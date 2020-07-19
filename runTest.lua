-- Parsing tests.
io.stdout:setvbuf("no")
io.stderr:setvbuf("no")

local loadLuaString = loadstring or load
local parser        = require"dumbParser"

local pretty = 1==1

do
	local path   = "./test.lua"
	-- local path   = "./dumbParser.lua"
	local tokens = assert(parser.tokenizeFile(path))
	local ast    = assert(parser.parse(tokens))

	-- parser.printTree(ast)

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
	parser.minify(ast)

	local lua = assert(parser.toLua(ast, pretty))

	do
		local luaEdit = lua
		-- luaEdit = luaEdit:gsub(("."):rep(250), "%0\0")
		-- luaEdit = luaEdit:gsub("([%w_]+)%z([%w_]+)", "%1%2\n")
		-- luaEdit = luaEdit:gsub("%z", "\n")
		print(luaEdit)
	end

	assert(loadLuaString(lua, "@lua"))

	-- Round-trip.
	local tripTokens = assert(parser.tokenizeString(lua))
	local tripAst    = assert(parser.parse(tripTokens))
	local tripLua    = assert(parser.toLua(tripAst, pretty))
	assert(tripLua == lua, tripLua)
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
	local lua = assert(parser.toLua(ast, pretty))
	print(lua)

	assert(loadLuaString(lua, "@lua"))
end

-- AST manipulations.
do
	local identifier   = parser.newNode("identifier", "foo")
	local vararg       = parser.newNode("vararg")
	local literal      = parser.newNode("literal", 8.125)
	local tableNode    = parser.newNode("table")
	local lookup       = parser.newNode("lookup")
	local unary        = parser.newNode("unary",  "not")
	local binary       = parser.newNode("binary", "/")
	local call         = parser.newNode("call")
	local functionNode = parser.newNode("function")
	local breakNode    = parser.newNode("break")
	local returnNode   = parser.newNode("return")
	local block        = parser.newNode("block")
	local declaration  = parser.newNode("declaration")
	local assignment   = parser.newNode("assignment")
	local ifNode       = parser.newNode("if")
	local whileLoop    = parser.newNode("while")
	local repeatLoop   = parser.newNode("repeat")
	local forLoop      = parser.newNode("for", "numeric")

	local block = assert(parser.parse([[
		local x = 49
	]], "<inline lua>"))

	assert(block.type == "block")
	assert(block.statements[1])
	assert(block.statements[1].type == "declaration")
	assert(block.statements[1].names[1])
	assert(block.statements[1].names[1].type == "identifier")
	assert(block.statements[1].names[1].name == "x")
	assert(block.statements[1].values[1])
	assert(block.statements[1].values[1].type  == "literal")
	assert(block.statements[1].values[1].value == 49)

	block.statements[1].names[1]  = identifier
	block.statements[1].values[1] = literal

	local lua = assert(parser.toLua(block, pretty))
	print(lua)

	assert(loadLuaString(lua, "@lua"))
end

print("Tests passed!")
