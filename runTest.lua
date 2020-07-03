-- Parsing tests.
io.stdout:setvbuf("no")
io.stderr:setvbuf("no")

local parser = require"dumbParser"

do
	local path   = "./test.lua"
	-- local path   = "./dumbParser.lua"
	local tokens = assert(parser.tokenizeFile(path))
	local ast    = assert(parser.parse(tokens))

	-- printTree(ast)

	local pretty = 1==1
	local lua    = parser.toLua(ast, pretty)

	do
		local luaEdit = lua
		-- luaEdit = luaEdit:gsub(("."):rep(250), "%0\0")
		-- luaEdit = luaEdit:gsub("([%w_]+)%z([%w_]+)", "%1%2\n")
		-- luaEdit = luaEdit:gsub("%z", "\n")
		print(luaEdit)
	end

	assert(loadstring(lua, "@lua"))

	-- Round-trip.
	local tripTokens = assert(parser.tokenizeString(lua))
	local tripAst    = assert(parser.parse(tripTokens))
	local tripLua    = parser.toLua(tripAst, pretty)
	assert(tripLua == lua, tripLua)
end

-- Token stream manipulation.
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
	local lua = parser.toLua(ast, pretty)
	print(lua)
end

print("Tests passed!")
