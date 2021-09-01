--
-- Test suite for Dumb Lua Parser
--
local PRETTY_OUTPUT   = 1==1
local PRINT_IDS       = 1==1
local PRINT_LOCATIONS = 1==1
local EXIT_ON_ERROR   = 1==0

collectgarbage("stop")
io.stdout:setvbuf("no")
io.stderr:setvbuf("no")

local loadstring = loadstring or load

local parser          = require"dumbParser"
parser.printIds       = PRINT_IDS
parser.printLocations = PRINT_LOCATIONS

local testCount = 0
local failCount = 0
local results   = {}



local function assert(v, err)
	if v then  return v  end
	err = tostring(err or "Assertion failed!")
	error(debug.traceback(err, 2), (err:find"^Error @" and 0 or 2))
end

-- assertLua( lua, expectedLua [, expectedLuaAlt ] [, level=1 ] )
local function assertLua(lua, expectedLua, expectedLuaAlt, level)
	if type(expectedLuaAlt) ~= "string" then
		expectedLuaAlt, level = nil, expectedLuaAlt
	end

	lua            =                    lua           :gsub("^%s+", ""):gsub("%s+$", "")
	expectedLua    =                    expectedLua   :gsub("^%s+", ""):gsub("%s+$", "")
	expectedLuaAlt = expectedLuaAlt and expectedLuaAlt:gsub("^%s+", ""):gsub("%s+$", "")

	if lua == expectedLua    then  return  end
	if lua == expectedLuaAlt then  return  end

	error(string.format(
		"Unexpected Lua output.\n%s\nExpected:\n%s\n%s\nGot:\n%s\n%s",
		("-"):rep(64),
		expectedLua,
		("-"):rep(64),
		lua,
		("-"):rep(64)
	), 1+(level or 1))
end

local function debugExit()
	print("!!! DEBUG EXIT !!!")
	os.exit(2)
end



local function printStats(stats, locLevel)
	print("Stats:")
	print("  nodeRemoveCount:    "..stats.nodeRemoveCount)
	print("  renameCount:        "..stats.renameCount)
	print("  generatedNameCount: "..stats.generatedNameCount)

	if locLevel == 1 then
		print("  nodeReplacements:   "..#stats.nodeReplacements)
		print("  nodeRemovals:       "..#stats.nodeRemovals)
	else
		print("  nodeReplacements:")
		for i, loc in ipairs(stats.nodeReplacements) do
			if locLevel == 2 then
				print("    "..loc.sourcePath..":"..loc.line)
			else
				print("    "..parser.formatMessage(
					loc, "Node replacement %d/%d (%s -> %s)",
					i, #stats.nodeReplacements, loc.node.type, (loc.replacement and loc.replacement.type or "?")
				):gsub("\n", "\n    "))
			end
		end
		print("  nodeRemovals:")
		for i, loc in ipairs(stats.nodeRemovals) do
			if locLevel == 2 then
				print("    "..loc.sourcePath..":"..loc.line)
			else
				print("    "..parser.formatMessage(
					loc, "Node removal %d/%d (%s)",
					i, #stats.nodeRemovals, loc.node.type
				):gsub("\n", "\n    "))
			end
		end
	end
end



local function test(label, f)
	testCount = testCount + 1

	print("Running test: "..label)
	-- f() -- DEBUG
	local ok, err = pcall(f)

	if ok then
		print("Test succeeded!")

	else
		io.stderr:write(err, "\n")
		if EXIT_ON_ERROR then  os.exit(1)  end

		failCount = failCount + 1
		print("Test failed!")
	end

	table.insert(results, {label=label, ok=ok, err=err})
end



--[[ Benchmark.
do
	local PATH = "dumbParser.lua"
	-- local PATH = "test.lua"

	local file = assert(io.open(PATH, "r"))
	local lua  = file:read("*a")
	file:close()

	local time, tokens = os.clock()
	for i = 1, 10 do
		tokens = assert(parser.tokenize(lua, PATH))
	end
	local timeTokenize = os.clock() - time

	local time, ast = os.clock()
	for i = 1, 10 do
		ast = assert(parser.parse(tokens))
	end
	local timeParse = os.clock() - time

	print("tokenize="..timeTokenize.." parse="..timeParse)
	debugExit()
end
--]]



test("Test file / misc.", function()
	local PATH = "test.lua"
	-- local PATH = "dumbParser.lua"

	if _VERSION >= "Lua 5.4" then
		assert(loadfile("test.lua"))
	end

	local tokens = assert(parser.tokenizeFile(PATH))
	local ast    = assert(parser.parse(tokens))
	-- parser.printTree(ast)
	-- debugExit()

	assert(parser.validateTree(ast))

	--[[
	parser.traverseTree(ast, function(node, parent, container, k)
		io.write(tostring(container), ".", tostring(k), " ")
		parser.printNode(node)
		-- if container then
		-- 	container[k]        = parser.newNode("call")
		-- 	container[k].callee = parser.newNode("identifier", "foo")
		-- end
	end)
	--]]

	parser.traverseTree(ast, function(node)
		if node.type == "identifier" and node.name == "foo" then
			node.name = "dog"
		end
	end)

	parser.updateReferences(ast)
	-- parser.printTree(ast)
	-- print(parser.toLua(ast, 1==1))
	-- debugExit()

	-- local stats = parser.optimize(ast) ; printStats(stats, 3)
	-- parser.printTree(ast)
	-- print(parser.toLua(ast, 1==1))
	-- debugExit()

	-- local stats = parser.minify(ast, 1==1) ; printStats(stats, 3)
	-- parser.printTree(ast)
	-- print(parser.toLua(ast, 1==1))
	-- debugExit()

	local lua = assert(parser.toLua(ast, PRETTY_OUTPUT))

	if 1==1 then
		local luaEdit = lua
		-- luaEdit = luaEdit:gsub(("."):rep(250), "%0\0")
		-- luaEdit = luaEdit:gsub("([%w_]+)%z([%w_]+)", "%1%2\n")
		-- luaEdit = luaEdit:gsub("%z", "\n")
		print(luaEdit)
		-- debugExit()
	end

	if _VERSION >= "Lua 5.4" then
		assert(loadstring(lua, "@<luastring>"))
	end

	-- Round-trip.
	local tripAst = assert(parser.parse(lua))
	local tripLua = assert(parser.toLua(tripAst, PRETTY_OUTPUT))

	if tripLua ~= lua then
		print(("-"):rep(64))
		print(lua)
		print(("-"):rep(64))
		print(tripLua)
		print(("-"):rep(64))
		error("Failed AST round-trip.")
	end

	-- Callback for toLua().
	do
		local visited = {}
		local ok      = true

		local luaWithExtras = assert(parser.toLua(ast, true, function(node, buffer)
			if visited[node] then
				ok = false
				io.write("Multiple visits for: ")
				parser.printNode(node)
			else
				visited[node] = true
				if (buffer[#buffer] or ""):find"%-$" then  table.insert(buffer, " ")  end
				table.insert(buffer, "--[[")
				table.insert(buffer, node.type)
				table.insert(buffer, "]]")
			end
		end))

		parser.traverseTree(ast, function(node)
			if visited[node] then  return  end
			ok = false
			io.write("No callack for: ")
			parser.printNode(node)
		end)

		-- print(luaWithExtras)
		assert(ok)

		if _VERSION >= "Lua 5.4" then
			assert(loadstring(luaWithExtras, "@<luastring>"))
		end
	end
end)



test("Tokens", function()
	do
		local tokens = assert(parser.tokenizeFile("test.lua"))
		-- parser.printTokens(tokens)

		local concatted = parser.concatTokens(tokens)
		-- print(concatted)

		-- Round-trip.
		local tripTokens    = assert(parser.tokenize(concatted))
		local tripConcatted = parser.concatTokens(tripTokens)

		if concatted ~= tripConcatted then
			print(("-"):rep(64))
			print((concatted:gsub("\n", "\\n")))
			print(("-"):rep(64))
			print((tripConcatted:gsub("\n", "\\n")))
			print(("-"):rep(64))
			error("Failed token round-trip.")
		end
	end

	do
		local tokens = {}

		-- Construct a call.
		table.insert(tokens, parser.newToken("identifier",  "math"))
		table.insert(tokens, parser.newToken("punctuation", "."))
		table.insert(tokens, parser.newToken("identifier",  "abs"))
		table.insert(tokens, parser.newToken("whitespace",  " "))
		table.insert(tokens, parser.newToken("punctuation", "("))
		table.insert(tokens, parser.newToken("punctuation", "-"))
		table.insert(tokens, parser.newToken("number",      1.75))
		table.insert(tokens, parser.newToken("punctuation", ")"))

		-- Add the call to a declaration with an error.
		table.insert(tokens, 1, parser.newToken("keyword",     "local"))
		table.insert(tokens, 2, parser.newToken("identifier",  "n"))
		table.insert(tokens, 3, parser.newToken("punctuation", "="))
		table.insert(tokens, 4, parser.newToken("punctuation", "/")) -- Error!

		-- Remove the error.
		table.remove(tokens, 4)

		local ast = assert(parser.parse(tokens))
		local lua = assert(parser.toLua(ast))
		-- print(lua)
		assertLua(lua, [[ local n=math.abs(-1.75); ]])
	end

	do
		-- Test keepWhitespaceTokens flag.
		local tokens    = assert(parser.tokenize([[ local x --[=[ ]=] = foo ; ]], true))
		local concatted = parser.concatTokens(tokens)
		assert(concatted == [[ local x --[=[ ]=] = foo ; ]], concatted)
	end
end)



test("AST manipulation", function()
	-- Basics.
	do
		local block = assert(parser.parse([[
			local x = 49
		]], "<luastring>"))

		assert(block.type == "block")
		assert(block.statements[1])
		assert(block.statements[1].type == "declaration")
		assert(block.statements[1].names[1])
		assert(block.statements[1].names[1].type == "identifier")
		assert(block.statements[1].names[1].name == "x")
		assert(block.statements[1].values[1])
		assert(block.statements[1].values[1].type  == "literal")
		assert(block.statements[1].values[1].value == 49)

		block.statements[1].names[1]  = parser.newNode("identifier", "foo")
		block.statements[1].values[1] = parser.newNode("literal", 8.125)

		local lua = assert(parser.toLua(block, PRETTY_OUTPUT))
		print(lua)

		assert(loadstring(lua, "@<luastring>"))
	end
	do
		local binary = assert(parser.parseExpression([[
			x + y ^ 49
		]], "<luastring>"))

		assert(binary.type == "binary")
		assert(binary.left.type == "identifier")
		assert(binary.left.name == "x")
		assert(binary.right.type == "binary")
		assert(binary.right.left.type == "identifier")
		assert(binary.right.left.name == "y")
		assert(binary.right.right.type  == "literal")
		assert(binary.right.right.value == 49)

		local lua = assert(parser.toLua(binary, PRETTY_OUTPUT))
		print(lua)

		assert(loadstring("return "..lua, "@<luastring>"))
	end

	-- Node creation.
	do
		local identifier1  = parser.newNode("identifier", "foo")
		local identifier2  = parser.newNode("identifier", "foo", "const")
		local vararg       = parser.newNode("vararg")
		local literal1     = parser.newNode("literal", 8.125)
		local literal2     = parser.newNode("literal", "foo")
		local literal3     = parser.newNode("literal", true)
		local literal4     = parser.newNode("literal", nil)
		local tableNode    = parser.newNode("table")
		local lookup       = parser.newNode("lookup")
		local unary1       = parser.newNode("unary",  "#")
		local unary2       = parser.newNode("unary",  "not")
		local binary1      = parser.newNode("binary", "/")
		local binary2      = parser.newNode("binary", "<<")
		local call         = parser.newNode("call")
		local functionNode = parser.newNode("function")
		local breakNode    = parser.newNode("break")
		local returnNode   = parser.newNode("return")
		local label        = parser.newNode("label", "foo")
		local gotoNode     = parser.newNode("goto",  "foo")
		local block        = parser.newNode("block")
		local declaration  = parser.newNode("declaration")
		local assignment   = parser.newNode("assignment")
		local ifNode       = parser.newNode("if")
		local whileLoop    = parser.newNode("while")
		local repeatLoop   = parser.newNode("repeat")
		local forLoop      = parser.newNode("for", "numeric")

		parser.setChild(call,        "callee", identifier1)
		parser.setChild(declaration, "names",  1, identifier2)

		parser.addChild(tableNode, "fields", literal2, literal1)

		local node = parser.getChild(call,        "callee")             ; parser.printNode(node)
		local node = parser.getChild(declaration, "names", 1)           ; parser.printNode(node)
		local node = parser.getChild(tableNode,   "fields", 1, "value") ; parser.printNode(node)

		parser.removeChild(tableNode, "fields", 1)
	end

	-- Cloning.
	do
		local ast = assert(parser.parseFile("test.lua"))

		do
			local clone = parser.cloneNode(ast)
			assert(clone.type == "block")

			parser.traverseTree(clone, function(node)
				assert(node == clone)
			end)
		end

		local lua1  = assert(parser.toLua(ast))
		local clone = parser.cloneTree(ast)

		parser.traverseTreeReverse(ast, true, function(node, parent, container, key)
			if container then
				container[key] = nil -- This should not affect the clone.
			end
		end)

		local lua2 = assert(parser.toLua(clone))
		assertLua(lua1, lua2)
	end
end)



test("Call and lookup parsing", function()
	assert(    parser.parse([[ x =  tbl   () ]], "<luastring>"))
	assert(    parser.parse([[ x =  tbl   "" ]], "<luastring>"))
	assert(    parser.parse([[ x =  tbl .m() ]], "<luastring>"))
	assert(    parser.parse([[ x =  tbl :m"" ]], "<luastring>"))
	assert(    parser.parse([[ x = (tbl)  () ]], "<luastring>"))
	assert(    parser.parse([[ x = (tbl)  "" ]], "<luastring>"))
	assert(    parser.parse([[ x = (tbl).m() ]], "<luastring>"))
	assert(    parser.parse([[ x = (tbl):m"" ]], "<luastring>"))
	assert(not parser.parse([[ x =  " "   () ]], "<luastring>"))
	assert(not parser.parse([[ x =  " "   "" ]], "<luastring>"))
	assert(not parser.parse([[ x =  " " .m() ]], "<luastring>"))
	assert(not parser.parse([[ x =  " " :m"" ]], "<luastring>"))
	assert(    parser.parse([[ x = (" ")  () ]], "<luastring>"))
	assert(    parser.parse([[ x = (" ")  "" ]], "<luastring>"))
	assert(    parser.parse([[ x = (" ").m() ]], "<luastring>"))
	assert(    parser.parse([[ x = (" "):m"" ]], "<luastring>"))
	assert(not parser.parse([[ x =  { }   () ]], "<luastring>"))
	assert(not parser.parse([[ x =  { }   "" ]], "<luastring>"))
	assert(not parser.parse([[ x =  { } .m() ]], "<luastring>"))
	assert(not parser.parse([[ x =  { } :m"" ]], "<luastring>"))
	assert(    parser.parse([[ x = ({ })  () ]], "<luastring>"))
	assert(    parser.parse([[ x = ({ })  "" ]], "<luastring>"))
	assert(    parser.parse([[ x = ({ }).m() ]], "<luastring>"))
	assert(    parser.parse([[ x = ({ }):m"" ]], "<luastring>"))
	assert(not parser.parse([[ x =  123   () ]], "<luastring>"))
	assert(not parser.parse([[ x =  123   "" ]], "<luastring>"))
	assert(not parser.parse([[ x =  123 .m() ]], "<luastring>"))
	assert(not parser.parse([[ x =  123 :m"" ]], "<luastring>"))
	assert(    parser.parse([[ x = (123)  () ]], "<luastring>"))
	assert(    parser.parse([[ x = (123)  "" ]], "<luastring>"))
	assert(    parser.parse([[ x = (123).m() ]], "<luastring>"))
	assert(    parser.parse([[ x = (123):m"" ]], "<luastring>"))
	assert(not parser.parse([[ x =  nil   () ]], "<luastring>"))
	assert(not parser.parse([[ x =  nil   "" ]], "<luastring>"))
	assert(not parser.parse([[ x =  nil .m() ]], "<luastring>"))
	assert(not parser.parse([[ x =  nil :m"" ]], "<luastring>"))
	assert(    parser.parse([[ x = (nil)  () ]], "<luastring>"))
	assert(    parser.parse([[ x = (nil)  "" ]], "<luastring>"))
	assert(    parser.parse([[ x = (nil).m() ]], "<luastring>"))
	assert(    parser.parse([[ x = (nil):m"" ]], "<luastring>"))
end)



test("Simplify", function()
	local function testSimplify(lua, expectedLua, expectedLuaAlt)
		local ast = assert(parser.parse(lua, "<luastring>"))
		parser.simplify(ast)
		if expectedLuaAlt then
			assertLua(assert(parser.toLua(ast)), expectedLua, expectedLuaAlt, 2)
		else
			assertLua(assert(parser.toLua(ast)), expectedLua, 2)
		end
	end

	local ast = assert(parser.parseFile("test.lua"))
	parser.simplify(ast)
	assert(parser.toLua(ast))
	local ast = assert(parser.parseFile("dumbParser.lua"))
	parser.simplify(ast)
	assert(parser.toLua(ast))

	testSimplify([[ x = -0 ]], [[ x=0; ]])

	testSimplify([[ x = 1+2   ]], [[ x=3; ]])
	testSimplify([[ x = 1+2^3 ]], [[ x=9; ]], [[ x=9.0; ]]) -- In Lua 5.3+ the result of x^y is always a float.

	testSimplify([[ x = 1 << 8     ]], [[ x=256;   ]])
	testSimplify([[ x = 2^99999    ]], [[ x=(1/0); ]])
	testSimplify([[ x = 5 - - - -5 ]], [[ x=10;    ]])

	testSimplify([[ x = 2779          ]], [[ x=2779;  ]])
	testSimplify([[ x = ~2779         ]], [[ x=-2780; ]])
	testSimplify([[ x = 2779 & 0x1011 ]], [[ x=17;    ]])
	testSimplify([[ x = 2779 ~ 0x1011 ]], [[ x=6858;  ]])
	testSimplify([[ x = 2779 | 0x1011 ]], [[ x=6875;  ]])

	testSimplify([[ x =  2       ]], [[ x=2;  ]])
	testSimplify([[ x =  2 >>  1 ]], [[ x=1;  ]])
	testSimplify([[ x =  2 <<  1 ]], [[ x=4;  ]])
	testSimplify([[ x = -2       ]], [[ x=-2; ]])
	testSimplify([[ x = -2 >>  1 ]], (parser.INT_SIZE == 32 and [[ x=2147483647; ]] or [[ x=9223372036854775807; ]]))
	testSimplify([[ x = -2 <<  1 ]], [[ x=-4; ]])
	testSimplify([[ x =  2 >>  0 ]], [[ x=2;  ]])
	testSimplify([[ x =  2 <<  0 ]], [[ x=2;  ]])
	testSimplify([[ x = -2 << -1 ]], (parser.INT_SIZE == 32 and [[ x=2147483647; ]] or [[ x=9223372036854775807; ]]))
	testSimplify([[ x = -2 >> -1 ]], [[ x=-4; ]])

	testSimplify([[ x = 1/0 & 2 ]], [[ x=(1/0)&2; ]])

	testSimplify([[ x = "yes" == "no" ]], [[ x=false; ]])
	testSimplify([[ x = 5     ~= nil  ]], [[ x=true;  ]])
	testSimplify([[ x = 80.6  >= 34   ]], [[ x=true;  ]])

	testSimplify([[ x = "one" .. 2 .. "three" ]], [[ x="one2three"; ]])

	testSimplify([[ x = true  and always1() ]], [[ x=always1(); ]])
	testSimplify([[ x = true  or  never1()  ]], [[ x=true;      ]])
	testSimplify([[ x = 0     and always2() ]], [[ x=always2(); ]])
	testSimplify([[ x = 0     or  never2()  ]], [[ x=0;         ]])
	testSimplify([[ x = false and never3()  ]], [[ x=false;     ]])
	testSimplify([[ x = false or  always3() ]], [[ x=always3(); ]])
	testSimplify([[ x = nil   and never4()  ]], [[ x=nil;       ]])
	testSimplify([[ x = nil   or  always4() ]], [[ x=always4(); ]])

	testSimplify(
		[[
		if 9 > 1 then
			ifYes()
			if 1 == "foo" then  ifNo()  end
		end
		]],
		[[ ifYes(); ]]
	)
	testSimplify(
		[[
		while 9 > 1 do
			whileYes()
			while 1 == "foo" do  whileNo()  end
		end
		]],
		[[ while true do whileYes();end ]]
	)
	testSimplify(
		[[
		repeat
			repeatYes()
			repeat  repeatOnce()  until 1 ~= "foo"
		until 9 <= 1
		]],
		[[ repeat repeatYes();repeatOnce();until false ]]
	)
	testSimplify(
		[[
		for x = 1, 2 do
			forYes()
			for y = 1, 2, -1 do  forNo()  end
			for y = 2, 1     do  forNo()  end
			for y = 2, 1,  1 do  forNo()  end
		end
		]],
		[[ for x=1,2 do forYes();end ]]
	)
end)



test("Optimize", function()
	local function testOptimize(lua, expectedLua, expectedLuaAlt)
		local ast = assert(parser.parse(lua, "<luastring>"))
		parser.optimize(ast)

		lua = assert(parser.toLua(ast))
		-- print(lua)

		if expectedLuaAlt then
			assertLua(lua, expectedLua, expectedLuaAlt, 2)
		else
			assertLua(lua, expectedLua, 2)
		end
	end

	local ast = assert(parser.parseFile("test.lua"))
	parser.optimize(ast)
	assert(parser.toLua(ast))
	-- local ast = assert(parser.parseFile("dumbParser.lua"))
	-- parser.optimize(ast)
	-- assert(parser.toLua(ast))

	-- Unpack 'do' block.
	testOptimize(
		[[
		do
			globalFunc()
		end
		]],
		[[ globalFunc(); ]]
	)

	--
	-- Names.
	--

	-- Declarations.
	testOptimize(
		[[
		local useless, keep, remove = globalFunc1()
		globalFunc2(keep)
		]],
		[[ local useless,keep=globalFunc1();globalFunc2(keep); ]]
	)
	testOptimize(
		[[
		local useless, keep, remove
		useless, keep, remove = globalFunc1()
		globalFunc2(keep)
		]],
		[[ local useless,keep;useless,keep=globalFunc1();globalFunc2(keep); ]]
	)
	testOptimize(
		[[
		local useless, keep, remove = globalFunc()
		local function keepCalled()
			keep = global1
		end
		local function removeNotCalled()
			keep = global2
			return global3
		end
		useless, keep, remove = globalFunc()
		keepCalled()
		print(keep)
		]],
		[[ local useless,keep=globalFunc();local function keepCalled()keep=global1;end useless,keep=globalFunc();keepCalled();print(keep); ]]
	)

	-- Example from real code.
	testOptimize(
		[[
		local forward
		local function localFunc()  return forward()  end
		function globalFunc()  return localFunc()  end
		do
			local n = 0
			function forward()  n = n + 1 ; return n  end
		end
		]],
		[[ local forward;local function localFunc()return forward();end function globalFunc()return localFunc();end do local n=0;function forward()n=n+1;return n;end end ]]
	)

	-- Functions.
	testOptimize(
		[[
		function globalFunc(useless, keep, remove)
			global = keep
		end
		]],
		[[ function globalFunc(useless,keep)global=keep;end ]]
	)
	testOptimize(
		[[
		function globalFunc(useless1, useless2, ...)
			global = ...
		end
		]],
		[[ function globalFunc(useless1,useless2,...)global=...;end ]]
	)
	testOptimize(
		[[
		function globalFunc(useless1, useless2, ...)
			-- void
		end
		]],
		[[ function globalFunc()end ]]
	)

	-- For loops (generic).
	testOptimize(
		[[
		for useless, keep, remove in iterator() do
			global = keep
		end
		]],
		[[ for useless,keep in iterator()do global=keep;end ]]
	)
	testOptimize(
		[[
		for useless in iterator() do
			-- void
		end
		]],
		[[ for useless in iterator()do end ]]
	)

	-- Complete removal.
	testOptimize(
		[[
		local unused1 = 1 + 2
		local function unusedFunc()
			local unused2 = global
		end
		unusedFunc = nil
		]],
		[[ ]]
	)

	-- Complex removals.
	testOptimize( -- Remove everything.
		[[
		local unused1          = 1
		local unused1          = 1, 2
		local unused1, unused2 = 1
		local unused1, unused2 = 1, 2
		]],
		[[ ]]
	)
	testOptimize( -- Keep calls.
		[[
		local unused = globalFunc11()
		local unused = globalFunc12(), 2
		local unused = globalFunc13(), 2, 3
		local unused = 1, globalFunc21()
		local unused = 1, globalFunc22(), 3
		local unused = 1, 2, globalFunc31()
		]],
		[[ globalFunc11();globalFunc12();globalFunc13();globalFunc21();globalFunc22();globalFunc31(); ]]
	)
	testOptimize( -- Keep all calls.
		[[
		before()
		local unused = globalFunc1(), globalFunc2()
		after()
		]],
		[[ before();globalFunc1();globalFunc2();after(); ]]
	)
	testOptimize( -- Assignments, keep all calls.
		[[
		before()
		local useless = 1
		useless = globalFunc1(), globalFunc2()
		after()
		]],
		[[ before();globalFunc1();globalFunc2();after(); ]]
	)
	testOptimize( -- Keep call.
		[[
		local unused1, unused2, useless = 1, globalFunc2(), 3, 4
		useless                         = 1
		]],
		[[ globalFunc2(); ]]
	)
	testOptimize( -- Keep call, 'THREE' must be 3.
		[[
		local unused1, unused2, THREE = 1, globalFunc2(), 3, 4
		global                        = THREE
		]],
		[[ globalFunc2();global=3; ]]
	)
	testOptimize( -- 'third' must be third value returned from call.
		[[
		local useless1, useless2, third = globalFunc123()
		global                          = third
		]],
		[[ local useless1,useless2,third=globalFunc123();global=third; ]]
	)
	testOptimize( -- Keep call, 'NIL' must be nil.
		[[
		local unused1, unused2, NIL = globalFunc1(), 2
		global                      = NIL
		]],
		[[ globalFunc1();global=nil; ]]
	)
	testOptimize( -- Assignments, keep call.
		[[
		local useless = 11, 12
		useless       = 21, globalFunc2(), 23
		]],
		[[ globalFunc2(); ]]
	)
	testOptimize( -- Keep all calls, 'x' must be what globalFunc2() returns.
		[[
		local unused, x = globalFunc1(), globalFunc2(), globalFunc3()
		globalFuncX(x)
		]],
		[[ local unused,x=globalFunc1(),globalFunc2(),globalFunc3();globalFuncX(x); ]]
	)

	--
	-- Constants.
	--

	-- Zero and the elusive minus. (Coming to all theaters next summer!)
	testOptimize(
		[[
		local n = -0  -- The value will get normalized to 0.
		global(n)
		]],
		[[ global(0); ]]
	)
	testOptimize(
		[[
		local n = 0
		global(-n)
		]],
		_VERSION >= "Lua 5.3"
		and [[ global(0); ]] -- '-0' is not a thing in Lua 5.3+.
		or  [[ local n=0;global(-n); ]]
	)
end)



test("Minify", function()
	local function testMinify(lua, expectedLua, expectedLuaAlt)
		local ast = assert(parser.parse(lua, "<luastring>"))
		parser.minify(ast)

		lua = assert(parser.toLua(ast))
		-- print(lua)

		if expectedLuaAlt then
			assertLua(lua, expectedLua, expectedLuaAlt, 2)
		else
			assertLua(lua, expectedLua, 2)
		end
	end

	local ast = assert(parser.parseFile("test.lua"))
	parser.minify(ast)
	assert(parser.toLua(ast))
	-- local ast = assert(parser.parseFile("dumbParser.lua"))
	-- parser.minify(ast)
	-- assert(parser.toLua(ast))

	-- Letter frequencies: etaoinshrdlcumwfgypbvkxjqz

	testMinify(
		[[
		local x, y = 1, 2
		global     = x + y
		]],
		[[ local e,t=1,2;global=e+t; ]]
	)

	testMinify(
		[[
		function globalFunc(unused, x, ...)
			return x + ...
		end
		]],
		[[ function globalFunc(e,e,...)return e+...;end ]]
	)
	testMinify(
		[[
		function globalFunc(x, unused, ...)
			return x + ...
		end
		]],
		[[ function globalFunc(e,t,...)return e+...;end ]]
	)

	testMinify(
		[[
		for i, v in ipairs(global) do
			globalFunc1(v)
			for i = i, #global do
				globalFunc2(i)
			end
		end
		]],
		[[ for e,t in ipairs(global)do globalFunc1(t);for e=e,#global do globalFunc2(e);end end ]]
	)

	if _VERSION >= "Lua 5.2" then
		testMinify(
			[[
				local _ENV = _ENV
				local function getEnv()
					return _ENV
				end
			]],
			[[ local _ENV=_ENV;local function e()return _ENV;end ]]
		)
	end
end)



test("Validate", function()
	local function parseExpression(lua)
		local expr = assert(parser.parseExpression(lua))
		assert(parser.validateTree(expr))
		return expr
	end
	local function parseStatement(lua)
		local statement = assert(parser.parse(lua)).statements[1]
		assert(statement, "No statement.")
		assert(parser.validateTree(statement))
		return statement
	end
	local function testInvalid(ast)
		local isValid, err = parser.validateTree(ast)
		assert(not isValid)
		-- print(err)
	end

	local ident      = parseExpression[[ x ]] ; ident.name      = "%"   ; testInvalid(ident)
	local ident      = parseExpression[[ x ]] ; ident.name      = "if"  ; testInvalid(ident)
	local ident      = parseExpression[[ x ]] ; ident.attribute = "bad" ; testInvalid(ident)

	local literal    = parseExpression[[ 1 ]] ; literal.value = {} ; testInvalid(literal)

	local lookup     = parseExpression[[ x.y ]] ; lookup.object = nil                     ; testInvalid(lookup)
	local lookup     = parseExpression[[ x.y ]] ; lookup.object = parser.newNode("block") ; testInvalid(lookup)
	local lookup     = parseExpression[[ x.y ]] ; lookup.member = nil                     ; testInvalid(lookup)
	local lookup     = parseExpression[[ x.y ]] ; lookup.member = parser.newNode("block") ; testInvalid(lookup)

	local unary      = parseExpression[[ -x ]] ; unary.operator   = "bad"                   ; testInvalid(unary)
	local unary      = parseExpression[[ -x ]] ; unary.expression = nil                     ; testInvalid(unary)
	local unary      = parseExpression[[ -x ]] ; unary.expression = parser.newNode("block") ; testInvalid(unary)

	local binary     = parseExpression[[ x+y ]] ; binary.operator = "bad"                   ; testInvalid(binary)
	local binary     = parseExpression[[ x+y ]] ; binary.left     = nil                     ; testInvalid(binary)
	local binary     = parseExpression[[ x+y ]] ; binary.left     = parser.newNode("block") ; testInvalid(binary)
	local binary     = parseExpression[[ x+y ]] ; binary.right    = nil                     ; testInvalid(binary)
	local binary     = parseExpression[[ x+y ]] ; binary.right    = parser.newNode("block") ; testInvalid(binary)

	local call       = parseExpression[[ f(x) ]] ; call.callee       = nil                     ; testInvalid(call)
	local call       = parseExpression[[ f(x) ]] ; call.callee       = parser.newNode("block") ; testInvalid(call)
	local call       = parseExpression[[ f(x) ]] ; call.method       = true                    ; testInvalid(call)
	local call       = parseExpression[[ f(x) ]] ; call.arguments[1] = parser.newNode("block") ; testInvalid(call)

	local methodCall = parseExpression[[ o:m() ]] ; call.callee              = parser.newNode("block") ; testInvalid(call)
	local methodCall = parseExpression[[ o:m() ]] ; call.callee.member       = parser.newNode("block") ; testInvalid(call)
	local methodCall = parseExpression[[ o:m() ]] ; call.callee.member.value = 1                       ; testInvalid(call)
	local methodCall = parseExpression[[ o:m() ]] ; call.callee.member.value = "%"                     ; testInvalid(call)
	local methodCall = parseExpression[[ o:m() ]] ; call.callee.member.value = "if"                    ; testInvalid(call)

	local func       = parseExpression[[ function(x,...)end ]] ; func.parameters[1] = parser.newNode("block")  ; testInvalid(func)
	local func       = parseExpression[[ function(x,...)end ]] ; func.parameters[1] = parser.newNode("vararg") ; testInvalid(func)
	local func       = parseExpression[[ function(x,...)end ]] ; func.body          = nil                      ; testInvalid(func)
	local func       = parseExpression[[ function(x,...)end ]] ; func.body          = parser.newNode("vararg") ; testInvalid(func)

	local tableNode  = parseExpression[[ {x=y} ]] ; tableNode.fields[1].key       = nil                               ; testInvalid(tableNode)
	local tableNode  = parseExpression[[ {x=y} ]] ; tableNode.fields[1].key       = parser.newNode("block")           ; testInvalid(tableNode)
	local tableNode  = parseExpression[[ {x=y} ]] ; tableNode.fields[1].value     = nil                               ; testInvalid(tableNode)
	local tableNode  = parseExpression[[ {x=y} ]] ; tableNode.fields[1].value     = parser.newNode("block")           ; testInvalid(tableNode)
	local tableNode  = parseExpression[[ {x}   ]] ; tableNode.fields[1].key       = parser.newNode("identifier", "x") ; testInvalid(tableNode)
	local tableNode  = parseExpression[[ {x}   ]] ; tableNode.fields[1].key.value = "bad"                             ; testInvalid(tableNode)

	local label      = parseStatement[[ ::x:: ]] ; label.name = "%"  ; testInvalid(label)
	local label      = parseStatement[[ ::x:: ]] ; label.name = "if" ; testInvalid(label)

	local gotoNode   = parseStatement[[ goto x ]] ; gotoNode.name = "%"  ; testInvalid(gotoNode)
	local gotoNode   = parseStatement[[ goto x ]] ; gotoNode.name = "if" ; testInvalid(gotoNode)

	local returnNode = parseStatement[[ return x ]] ; returnNode.values[1] = parser.newNode("block") ; testInvalid(returnNode)

	local block      = parseStatement[[ do local x end ]] ; block.statements[1] = parser.newNode("vararg") ; testInvalid(block)

	local decl       = parseStatement[[ local x = 1 ]] ; decl.names[1]  = parser.newNode("vararg") ; testInvalid(decl)
	local decl       = parseStatement[[ local x = 1 ]] ; decl.values[1] = parser.newNode("block")  ; testInvalid(decl)

	local assignment = parseStatement[[ x      = 1     ]] ; assignment.targets[1] = nil                      ; testInvalid(assignment)
	local assignment = parseStatement[[ x      = 1     ]] ; assignment.values[1]  = nil                      ; testInvalid(assignment)
	local assignment = parseStatement[[ x, t.k = 1, "" ]] ; assignment.targets[1] = parser.newNode("vararg") ; testInvalid(assignment)
	local assignment = parseStatement[[ x, t.k = 1, "" ]] ; assignment.values[1]  = parser.newNode("block")  ; testInvalid(assignment)

	local ifNode     = parseStatement[[ if x then else end ]] ; ifNode.condition = nil                      ; testInvalid(ifNode)
	local ifNode     = parseStatement[[ if x then else end ]] ; ifNode.condition = parser.newNode("block")  ; testInvalid(ifNode)
	local ifNode     = parseStatement[[ if x then else end ]] ; ifNode.bodyTrue  = nil                      ; testInvalid(ifNode)
	local ifNode     = parseStatement[[ if x then else end ]] ; ifNode.bodyTrue  = parser.newNode("vararg") ; testInvalid(ifNode)
	local ifNode     = parseStatement[[ if x then else end ]] ; ifNode.bodyFalse = parser.newNode("vararg") ; testInvalid(ifNode)

	local whileLoop  = parseStatement[[ while x do end ]] ; whileLoop.condition = nil                      ; testInvalid(whileLoop)
	local whileLoop  = parseStatement[[ while x do end ]] ; whileLoop.condition = parser.newNode("block")  ; testInvalid(whileLoop)
	local whileLoop  = parseStatement[[ while x do end ]] ; whileLoop.body      = nil                      ; testInvalid(whileLoop)
	local whileLoop  = parseStatement[[ while x do end ]] ; whileLoop.body      = parser.newNode("vararg") ; testInvalid(whileLoop)

	local repeatLoop = parseStatement[[ repeat until x ]] ; repeatLoop.body      = nil                      ; testInvalid(repeatLoop)
	local repeatLoop = parseStatement[[ repeat until x ]] ; repeatLoop.body      = parser.newNode("vararg") ; testInvalid(repeatLoop)
	local repeatLoop = parseStatement[[ repeat until x ]] ; repeatLoop.condition = nil                      ; testInvalid(repeatLoop)
	local repeatLoop = parseStatement[[ repeat until x ]] ; repeatLoop.condition = parser.newNode("block")  ; testInvalid(repeatLoop)

	local forLoop    = parseStatement[[ for x = 1, 2    do end ]] ; forLoop.kind      = "bad"                             ; testInvalid(forLoop)
	local forLoop    = parseStatement[[ for x = 1, 2    do end ]] ; forLoop.names[1]  = nil                               ; testInvalid(forLoop)
	local forLoop    = parseStatement[[ for x = 1, 2    do end ]] ; forLoop.names[1]  = parser.newNode("vararg")          ; testInvalid(forLoop)
	local forLoop    = parseStatement[[ for x = 1, 2    do end ]] ; forLoop.names[2]  = parser.newNode("identifier", "y") ; testInvalid(forLoop) -- Too many names.
	local forLoop    = parseStatement[[ for x = 1, 2    do end ]] ; forLoop.values[1] = parser.newNode("block")           ; testInvalid(forLoop)
	local forLoop    = parseStatement[[ for x = 1, 2    do end ]] ; forLoop.values[2] = nil                               ; testInvalid(forLoop) -- Too few values.
	local forLoop    = parseStatement[[ for x = 1, 2, 3 do end ]] ; forLoop.values[4] = parser.newNode("literal", 4)      ; testInvalid(forLoop) -- Too many values.
	local forLoop    = parseStatement[[ for x = 1, 2    do end ]] ; forLoop.body      = nil                               ; testInvalid(forLoop)
	local forLoop    = parseStatement[[ for x = 1, 2    do end ]] ; forLoop.body      = parser.newNode("vararg")          ; testInvalid(forLoop)

	local forLoop    = parseStatement[[ for x, y, z in a, b, c, d do end ]] ; forLoop.kind      = "bad"                    ; testInvalid(forLoop)
	local forLoop    = parseStatement[[ for x       in a, b, c, d do end ]] ; forLoop.names[1]  = nil                      ; testInvalid(forLoop)
	local forLoop    = parseStatement[[ for x, y, z in a, b, c, d do end ]] ; forLoop.names[1]  = parser.newNode("vararg") ; testInvalid(forLoop)
	local forLoop    = parseStatement[[ for x, y, z in a          do end ]] ; forLoop.values[1] = nil                      ; testInvalid(forLoop)
	local forLoop    = parseStatement[[ for x, y, z in a, b, c, d do end ]] ; forLoop.values[1] = parser.newNode("block")  ; testInvalid(forLoop)
	local forLoop    = parseStatement[[ for x, y, z in a, b, c, d do end ]] ; forLoop.body      = nil                      ; testInvalid(forLoop)
	local forLoop    = parseStatement[[ for x, y, z in a, b, c, d do end ]] ; forLoop.body      = parser.newNode("vararg") ; testInvalid(forLoop)
end)



test("Selective pretty", function()
	local ast = assert(parser.parse[[
		local function foo(a, ...)
			print(a .. ...)
		end
		local x = 7 + foo("a", "\0322")
	]])

	ast.pretty                              = true
	ast.statements[2].values[1].body.pretty = false

	local lua = parser.toLua(ast, false)
	print(lua)
	assertLua(lua, [[
local function foo(a, ...)print(a.. ...);end
local x = 7 + foo("a", " 2");
	]])
end)



test("Soft LuaJIT", function()
	local ast = assert(parser.parse([[ x = 0b1001001111101011 ]], "<luastring>"))
	assertLua(assert(parser.toLua(ast)), [[ x=37867; ]])
end)

if jit then
	test("LuaJIT", function()
		local ast = assert(parser.parse([[ x = 0x0fffffffffffffffLL ]], "<luastring>"))
		assertLua(assert(parser.toLua(ast)), [[ x=1152921504606846975LL; ]])

		local ast = assert(parser.parse([[ x = 12.5i ]], "<luastring>"))
		assertLua(assert(parser.toLua(ast)), [[ x=12.5i; ]])

		local ast = assert(parser.parse([[ x = -3.75i ]], "<luastring>"))
		assertLua(assert(parser.toLua(ast)), [[ x=-3.75i; ]])

		local ast = assert(parser.parse([[ x = 94LL + 94LL ]], "<luastring>"))
		parser.simplify(ast)
		assertLua(assert(parser.toLua(ast)), [[ x=188LL; ]])
	end)
end



print(("="):rep(64))
for _, result in ipairs(results) do
	print(string.format("%-4s  %s", (result.ok and "ok" or "FAIL"), result.label))
end
print(("="):rep(64))

if failCount > 0 then
	print("Error: "..failCount.."/"..testCount.." tests failed!")
	os.exit(1)
else
	print("All tests passed successfully!")
end


