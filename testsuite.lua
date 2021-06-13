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



local function printStats(stats)
	local keys    = {}
	local kMaxLen = 0

	for k, v in pairs(stats) do
		table.insert(keys, k)
		kMaxLen = math.max(kMaxLen, #k)
	end

	table.sort(keys)

	print("Stats:")
	local format = "  %-"..kMaxLen.."s = %s"

	for _, k in ipairs(keys) do
		print(string.format(format, k, tostring(stats[k])))
	end
end



local function test(label, f)
	testCount = testCount + 1

	print("Running test: "..label)
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



test("Test file", function()
	local path = "test.lua"
	-- local path = "dumbParser.lua"

	-- assert(loadfile(path))

	local tokens = assert(parser.tokenizeFile(path))

	do
		local concatted = parser.concatTokens(tokens)
		-- print(concatted)

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

	local ast = assert(parser.parse(tokens))

	-- parser.printTokens(tokens)
	-- parser.printTree(ast)
	-- debugExit()

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

	-- local stats = parser.optimize(ast)
	-- printStats(stats)
	-- parser.printTree(ast)
	-- print(parser.toLua(ast, 1==1))
	-- debugExit()

	-- local stats = parser.minify(ast, 1==1)
	-- printStats(stats)
	-- parser.printTree(ast)
	-- print(parser.toLua(ast, 1==1))
	-- debugExit()

	local lua = assert(parser.toLua(ast, PRETTY_OUTPUT))

	do
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
end)



test("Token stream manipulations", function()
	local tokens = parser.newTokenStream()

	parser.insertToken(tokens, "identifier",  "math")
	parser.insertToken(tokens, "punctuation", ".")
	parser.insertToken(tokens, "identifier",  "abs")
	parser.insertToken(tokens, "punctuation", "(")
	parser.insertToken(tokens, "punctuation", "-")
	parser.insertToken(tokens, "number",      1.75)
	parser.insertToken(tokens, "punctuation", ")")

	parser.insertToken(tokens, 1, "keyword",     "local")
	parser.insertToken(tokens, 2, "identifier",  "n")
	parser.insertToken(tokens, 3, "punctuation", "=")
	parser.insertToken(tokens, 4, "punctuation", "/")

	parser.removeToken(tokens, 4)

	local ast = assert(parser.parse(tokens))
	assertLua(assert(parser.toLua(ast)), [[ local n=math.abs(-1.75); ]])
end)



test("AST manipulations", function()
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

	do
		parser.setChild(call,        "callee", identifier1)
		parser.setChild(declaration, "names",  1, identifier2)

		parser.addChild(tableNode, "fields", literal2, literal1)

		parser.printNode(parser.getChild(call,        "callee"))
		parser.printNode(parser.getChild(declaration, "names", 1))
		parser.printNode(parser.getChild(tableNode,   "fields", 1, "value"))

		parser.removeChild(tableNode, "fields", 1)
	end

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

	block.statements[1].names[1]  = identifier1
	block.statements[1].values[1] = literal1

	local lua = assert(parser.toLua(block, PRETTY_OUTPUT))
	print(lua)

	assert(loadstring(lua, "@<luastring>"))
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
end)



test("Optimize", function()
	local function testOptimize(lua, expectedLua, expectedLuaAlt)
		local ast = assert(parser.parse(lua, "<luastring>"))
		parser.optimize(ast)
		if expectedLuaAlt then
			assertLua(assert(parser.toLua(ast)), expectedLua, expectedLuaAlt, 2)
		else
			assertLua(assert(parser.toLua(ast)), expectedLua, 2)
		end
	end

	-- Constants.
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
		and [[ global(0); ]]
		or  [[ local n=0;global(-n); ]]
	)
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

test("Soft LuaJIT", function()
	local ast = assert(parser.parse([[ x = 0b1001001111101011 ]], "<luastring>"))
	assertLua(assert(parser.toLua(ast)), [[ x=37867; ]])
end)



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


