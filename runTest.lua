-- Parsing tests.
local PRETTY          = 1==1
local PRINT_IDS       = 1==1
local PRINT_LOCATIONS = 1==1

io.stdout:setvbuf("no")
io.stderr:setvbuf("no")

local loadLuaString   = loadstring or load
local parser          = require"dumbParser"
parser.printIds       = PRINT_IDS
parser.printLocations = PRINT_LOCATIONS

local function assert(v, err)
	if v then  return v  end
	io.stderr:write(debug.traceback(tostring(err or "Assertion failed!"), 2), "\n")
	os.exit(1)
end

do
	local path = "test.lua"
	-- local path = "dumbParser.lua"

	-- assert(loadfile(path))

	local tokens = assert(parser.tokenizeFile(path))
	local ast    = assert(parser.parse(tokens))

	-- parser.printTokens(tokens)
	-- parser.printTree(ast)
	-- error("DEBUG")

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
	-- error("DEBUG")

	if 1==1 then
		parser.simplify(ast)
		parser.clean(ast)
		parser.simplify(ast)
	else
		parser.minify(ast)
	end
	-- parser.printTree(ast)
	-- print(parser.toLua(ast, 1==1))
	-- error("DEBUG")

	local lua = assert(parser.toLua(ast, PRETTY))

	do
		local luaEdit = lua
		-- luaEdit = luaEdit:gsub(("."):rep(250), "%0\0")
		-- luaEdit = luaEdit:gsub("([%w_]+)%z([%w_]+)", "%1%2\n")
		-- luaEdit = luaEdit:gsub("%z", "\n")
		print(luaEdit)
	end

	assert(loadLuaString(lua, "@<luastring>"))

	-- Round-trip.
	local tripTokens = assert(parser.tokenize(lua))
	local tripAst    = assert(parser.parse(tripTokens))
	local tripLua    = assert(parser.toLua(tripAst, PRETTY))

	if tripLua ~= lua then
		print(("-"):rep(64))
		print(lua)
		print(("-"):rep(64))
		print(tripLua)
		print(("-"):rep(64))
		error("Failed round-trip.")
	end
end

-- Token stream manipulations.
do
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
	local lua = assert(parser.toLua(ast, PRETTY))
	print(lua)

	assert(loadLuaString(lua, "@<luastring>"))
end

-- AST manipulations.
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

	local lua = assert(parser.toLua(block, PRETTY))
	print(lua)

	assert(loadLuaString(lua, "@<luastring>"))
end

-- Tests that should fail.
do
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
end

print()
print("Tests passed successfully!")
print()
