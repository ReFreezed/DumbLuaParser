--[[============================================================
--=
--=  Dumb Lua Parser - Lua parsing library
--=  by Marcus 'ReFreezed' Thunström
--=
--=  v1.2.1 (2021-06-01)
--=
--=  License: MIT (see the bottom of this file)
--=  Website: https://github.com/ReFreezed/DumbLuaParser
--=
--=  Supported Lua versions: 5.1, 5.2, 5.3
--=
--==============================================================


	Usage
	--------------------------------

	local parser = require("dumbParser")

	local tokens = parser.tokenizeFile("cool.lua")
	local ast    = parser.parse(tokens)

	parser.printTree(ast)

	local lua = parser.toLua(ast, true)
	print(lua)


	API
	--------------------------------

	tokenizeString()
		tokens, error = parser.tokenizeString( luaString [, pathForErrorMessages="?" ] )
		Convert a Lua string into tokens.
		Returns nil and an error message on error.

	tokenizeFile()
		tokens, error = parser.tokenizeFile( path )
		Convert the contents of a file into tokens. Uses io.open().
		Returns nil and an error message on error.

	newTokenStream()
		tokens = parser.newTokenStream( )
		Create a new token stream table. (See more info below.)

	insertToken()
		parser.insertToken( tokens, [ index=tokens.n+1, ] tokenType, tokenValue )
		Insert a new token. (Search for 'TokenInsertion' for more info.)

	removeToken()
		parser.removeToken( tokens [, index=1 ] )
		Remove a token.

	parse()
		astNode, error = parser.parse( tokens )
		astNode, error = parser.parse( luaString, pathForErrorMessages )
		astNode, error = parser.parse( path )
		Convert tokens or Lua code into an abstract syntax tree.
		Returns nil and an error message on error.

	newNode()
		astNode = parser.newNode( nodeType, arguments... )
		Create a new AST node. (Search for 'NodeCreation' for more info.)

	traverseTree()
		didBreak = parser.traverseTree( astNode, [ leavesFirst=false, ] callback [, topNodeParent=nil, topNodeContainer=nil, topNodeKey=nil ] )
		action   = callback( astNode, parent, container, key )
		action   = "stop"|"ignorechildren"|nil  -- Returning nil (or nothing) means continue traversal.
		Call a function on all nodes in an AST, going from astNode out to the leaf nodes (or from leaf nodes and inwards if leavesFirst is set).
		container[key] is the position of the current node in the tree and can be used to replace the node.

	updateReferences()
		parser.updateReferences( astNode )
		Update references between nodes in the tree.
		This function sets 'parent', 'container' and 'key' for all nodes and 'declaration' for identifiers.

	minify()
		parser.minify( astNode )
		Remove useless nodes from the tree and replace local variable names with short names.
		This function can be used to obfuscate the code to some extent.
		Note: Make sure to call updateReferences() first so minify() knows what identifiers refer to local declarations, otherwise all identifiers will be treated as globals and are thus left untouched.

	toLua()
		lua = parser.toLua( astNode [, prettyOuput=false ] )
		Convert an AST to Lua. Returns nil on error.

	printTokens()
		parser.printTokens( tokens )
		Print the contents of a token stream to stdout.

	printNode()
		parser.printNode( astNode )
		Print information about an AST node to stdout.

	printTree()
		parser.printTree( astNode )
		Print the structure of a whole AST to stdout.

	VERSION
		parser.VERSION
		The parser's version number (e.g. "1.0.2").


	Tokens
	--------------------------------

	Token stream table fields:

		n              -- Token count.
		sourceString   -- The original source string.
		sourcePath     -- Path to the source file.

		type           -- Array of token types.
		value          -- Array of token values. All token types have string values except "number" tokens.
		representation -- Array of token representations (i.e. strings have surrounding quotes etc.).
		lineStart      -- Array of token start line numbers.
		lineEnd        -- Array of token end line numbers.
		positionStart  -- Array of token start indices.
		positionEnd    -- Array of token end indices.

	Token types:

		"comment"     -- A comment.
		"identifier"  -- Word that is not a keyword.
		"keyword"     -- Lua keyword.
		"number"      -- Number literal.
		"punctuation" -- Any punctuation, like "." or "(".
		"string"      -- String value.


	AST (abstract syntax tree)
	--------------------------------

	Node types:

		"assignment"  -- Assignment of one or more values to one or more variables.
		"binary"      -- Binary expression (operation with two operands, e.g. "+" or "and").
		"block"       -- List of statements. Blocks inside blocks are 'do...end' statements. Can be a chuck.
		"break"       -- Loop break statement.
		"call"        -- Function call.
		"declaration" -- Declaration of one or more local variables, possibly with initial values.
		"for"         -- A 'for' loop.
		"function"    -- Anonymous function header and body.
		"goto"        -- A jump to a label.
		"identifier"  -- An identifier.
		"if"          -- If statement with a condition, a body if the condition is true, and possibly another body if the condition is false.
		"label"       -- Label for goto commands.
		"literal"     -- Number, string, boolean or nil literal.
		"lookup"      -- Field lookup on an object.
		"repeat"      -- A 'repeat' loop.
		"return"      -- Function/chunk return statement, possibly with values.
		"table"       -- Table constructor.
		"unary"       -- Unary expression (operation with one operand, e.g. "-" or "not").
		"vararg"      -- Vararg expression ("...").
		"while"       -- A 'while' loop.

	Node fields: (Search for 'NodeFields'.)


--============================================================]]

local PARSER_VERSION = "1.2.1"

local bytesToString = string.char
local F             = string.format
local find          = string.find
local getByte       = string.byte
local getSubstring  = string.sub
local gsub          = string.gsub
local match         = string.match
local repeatString  = string.rep

local floor         = math.floor
local getMax        = math.max
local getMin        = math.min

local loadLuaString = loadstring   or load
local unpack        = table.unpack or unpack

local assertArg
local countString
local formatNumber
local isToken, isTokenType, isTokenAnyValue
local itemWith1
local newId
local newTokenStream
local parse
local printError, printfError, reportErrorInFile, reportErrorAtToken
local printNode, printTree
local printTokens
local tokenizeString, tokenizeFile
local toLua
local traverseTree
local updateReferences

local parser



local KEYWORDS = {
	["and"]      = true,
	["break"]    = true,
	["do"]       = true,
	["else"]     = true,
	["elseif"]   = true,
	["end"]      = true,
	["false"]    = true,
	["for"]      = true,
	["function"] = true,
	["goto"]     = true,
	["if"]       = true,
	["in"]       = true,
	["local"]    = true,
	["nil"]      = true,
	["not"]      = true,
	["or"]       = true,
	["repeat"]   = true,
	["return"]   = true,
	["then"]     = true,
	["true"]     = true,
	["until"]    = true,
	["while"]    = true,
}
local PUNCTUATION = {
	["+"]=true,  ["-"]=true,  ["*"]=true,  ["/"]=true,  ["%"]=true,  ["^"]=true,   ["#"]=true,
	["&"]=true,  ["~"]=true,  ["|"]=true,  ["<<"]=true, [">>"]=true, ["//"]=true,
	["=="]=true, ["~="]=true, ["<="]=true, [">="]=true, ["<"]=true,  [">"]=true,   ["="]=true,
	["("]=true,  [")"]=true,  ["{"]=true,  ["}"]=true,  ["["]=true,  ["]"]=true,   ["::"]=true,
	[";"]=true,  [":"]=true,  [","]=true,  ["."]=true,  [".."]=true, ["..."]=true,
}
local OPERATORS_UNARY = {
	["-"]=true, ["not"]=true, ["#"]=true, ["~"]=true,
}
local OPERATORS_BINARY = {
	["+"]=true,   ["-"]=true,  ["*"]=true, ["/"]=true,  ["//"]=true, ["^"]=true,  ["%"]=true,
	["&"]=true,   ["~"]=true,  ["|"]=true, [">>"]=true, ["<<"]=true, [".."]=true,
	["<"]=true,   ["<="]=true, [">"]=true, [">="]=true, ["=="]=true, ["~="]=true,
	["and"]=true, ["or"]=true,
}
local OPERATOR_PRECEDENCE = {
	["or"]  = 1,
	["and"] = 2,
	["<"]   = 3,  [">"] = 3, ["<="] = 3, [">="] = 3, ["~="] = 3, ["=="] = 3,
	["|"]   = 4,
	["~"]   = 5,
	["&"]   = 6,
	["<<"]  = 7,  [">>"]  = 7,
	[".."]  = 8,
	["+"]   = 9,  ["-"] = 9,
	["*"]   = 10, ["/"] = 10, ["//"] = 10, ["%"] = 10,
	unary   = 11, -- "-", "not", "#", "~"
	["^"]   = 12,
}

local NUM_HEX_FRAC_EXP = gsub("^( 0[Xx] ([%dA-Fa-f]*) %.([%dA-Fa-f]+) [Pp]([-+]?[%dA-Fa-f]+) )", " +", "")
local NUM_HEX_FRAC     = gsub("^( 0[Xx] ([%dA-Fa-f]*) %.([%dA-Fa-f]+)                        )", " +", "")
local NUM_HEX_EXP      = gsub("^( 0[Xx] ([%dA-Fa-f]+) %.?             [Pp]([-+]?[%dA-Fa-f]+) )", " +", "")
local NUM_HEX          = gsub("^( 0[Xx]  [%dA-Fa-f]+  %.?                                    )", " +", "")
local NUM_DEC_FRAC_EXP = gsub("^(        %d*          %.%d+           [Ee][-+]?%d+           )", " +", "")
local NUM_DEC_FRAC     = gsub("^(        %d*          %.%d+                                  )", " +", "")
local NUM_DEC_EXP      = gsub("^(        %d+          %.?             [Ee][-+]?%d+           )", " +", "")
local NUM_DEC          = gsub("^(        %d+          %.?                                    )", " +", "")

local EMPTY_TABLE = {}



-- :NodeFields

-- All nodes also have a 'parent', 'container' and 'key' field (after updateReferences() has been called).
-- 'parent' refers to the node's parent in the tree.
-- 'container' refers to the specific table that the node is in, which could be the parent or a member of the parent.
-- 'key' refers to the specific field in the container that the node is in, which is either a string or an integer.

-- AST expressions.
local function AstIdentifier (tok)return{token=tok,
	type        = "identifier",
	id          = newId(),
	name        = "",
	declaration = nil, -- AstDeclaration, AstFunction or AstFor. Updated by updateReferences(). This is nil for globals.
}end
local function AstVararg (tok)return{token=tok,
	type        = "vararg",
	id          = newId(),
	adjustToOne = false, -- True if parentheses surround the vararg.
}end
local function AstLiteral (tok)return{token=tok,
	type        = "literal",
	id          = newId(),
	value       = nil, -- A number, string, boolean or nil.
}end
local function AstTable (tok)return{token=tok,
	type        = "table",
	id          = newId(),
	fields      = {}, -- Array of {key=expression, value=expression, generatedKey=bool}.
}end
local function AstLookup (tok)return{token=tok,
	type        = "lookup",
	id          = newId(),
	object      = nil, -- Expression.
	member      = nil, -- Expression.
}end
local function AstUnary (tok)return{token=tok,
	type        = "unary",
	id          = newId(),
	operator    = "",  -- "-" | "not" | "#" | "~"
	expression  = nil, -- Expression.
}end
local function AstBinary (tok)return{token=tok,
	type        = "binary",
	id          = newId(),
	operator    = "",  -- "+" | "-" | "*" | "/" | "//" | "^" | "%" | "&" | "~" | "|" | ">>" | "<<" | ".." | "<" | "<=" | ">" | ">=" | "==" | "~=" | "and" | "or"
	left        = nil, -- Expression.
	right       = nil, -- Expression.
}end
local function AstCall (tok)return{token=tok, -- Calls can be both expressions and statements.
	type        = "call",
	id          = newId(),
	callee      = nil,   -- Expression.
	arguments   = {},    -- Array of expressions.
	method      = false,
	adjustToOne = false, -- True if parentheses surround the call.
}end
local function AstFunction (tok)return{token=tok,
	type        = "function",
	id          = newId(),
	parameters  = {},  -- Array of AstIdentifier.
	vararg      = nil, -- AstVararg or nil.
	body        = nil, -- AstBlock.
}end

-- AST statements.
local function AstBreak (tok)return{token=tok,
	type        = "break",
	id          = newId(),
}end
local function AstReturn (tok)return{token=tok,
	type        = "return",
	id          = newId(),
	values      = {}, -- Array of expressions.
}end
local function AstLabel (tok)return{token=tok,
	type        = "label",
	id          = newId(),
	name        = nil, -- AstIdentifier
}end
local function AstGoto (tok)return{token=tok,
	type        = "goto",
	id          = newId(),
	name        = nil, -- AstIdentifier
}end
local function AstBlock (tok)return{token=tok,
	type        = "block",
	id          = newId(),
	statements  = {}, -- Array of statements.
}end
local function AstDeclaration (tok)return{token=tok,
	type        = "declaration",
	id          = newId(),
	names       = {}, -- Array of AstIdentifier.
	values      = {}, -- Array of expressions.

}end
local function AstAssignment (tok)return{token=tok,
	type        = "assignment",
	id          = newId(),
	targets     = {}, -- Mixed array of AstIdentifier and AstLookup.
	values      = {}, -- Array of expressions.
}end
local function AstIf (tok)return{token=tok,
	type        = "if",
	id          = newId(),
	condition   = nil, -- Expression.
	bodyTrue    = nil, -- AstBlock.
	bodyFalse   = nil, -- AstBlock. May be nil.
}end
local function AstWhile (tok)return{token=tok,
	type        = "while",
	id          = newId(),
	condition   = nil, -- Expression.
	body        = nil, -- AstBlock.
}end
local function AstRepeat (tok)return{token=tok,
	type        = "repeat",
	id          = newId(),
	body        = nil, -- AstBlock.
	condition   = nil, -- Expression.
}end
local function AstFor (tok)return{token=tok,
	type        = "for",
	id          = newId(),
	kind        = "",  -- "numeric" | "generic"
	names       = {},  -- Array of AstIdentifier.
	values      = {},  -- Array of expressions.
	body        = nil, -- AstBlock.
}end



-- count = countString( haystack, needle [, plain=false ] )
function countString(haystack, needle, plain)
	local count = 0
	local i     = 0
	local _

	while true do
		_, i = find(haystack, needle, i+1, plain)
		if not i then  return count  end

		count = count+1
	end
end

function printError(s)
	io.stderr:write(s, "\n")
end
function printfError(s, ...)
	io.stderr:write(s:format(...), "\n")
end

function reportErrorInFile(contents, path, ptr, agent, s, ...)
	s = F(s, ...)

	if contents == "" then
		printfError("Error @ %s: [%s] %s\n", path, agent, s)
		return s
	end

	local pre       = getSubstring(contents, 1, ptr-1)

	local lastLine1 = pre:reverse():match"^[^\n]*":reverse():gsub("\t", "    ")
	local lastLine2 = contents:match("^[^\n]*", ptr):gsub("\t", "    ")
	local lastLine  = lastLine1..lastLine2

	local ln        = countString(pre, "\n", true) + 1
	local col       = #lastLine1 + 1

	-- print(debug.traceback("", 2)) -- DEBUG

	printfError(
		"Error @ %s:%d: [%s] %s\n>\n> %s\n>%s^\n>\n",
		path, ln, agent, s, lastLine, repeatString("-", col)
	)

	return s
end

function reportErrorAtToken(tokens, tok, agent, s, ...)
	reportErrorInFile(tokens.sourceString, tokens.sourcePath, tokens.positionStart[tok], agent, s, ...)
end



local ERROR_UNFINISHED_VALUE = {}

-- success, equalSignCountIfLong|errorCode, ptr = parseStringlikeToken( s, ptr )
local function parseStringlikeToken(s, ptr)
	local longEqualSigns       = match(s, "^%[(=*)%[", ptr)
	local equalSignCountIfLong = longEqualSigns and #longEqualSigns

	-- Single line (comment).
	if not equalSignCountIfLong then
		local i1, i2 = find(s, "\n", ptr)
		ptr          = i2 and i2 + 1 or #s + 1

	-- Multiline.
	else
		ptr = ptr + 1 + #longEqualSigns + 1

		local i1, i2 = find(s, "%]"..longEqualSigns.."%]", ptr)
		if not i1 then
			return false, ERROR_UNFINISHED_VALUE, 0
		end

		ptr = i2 + 1
	end

	return true, equalSignCountIfLong, ptr
end

-- tokens, error = tokenizeString( luaString [, pathForErrorMessages="?" ] )
function tokenizeString(s, path)
	if find(s, "\r", 1, true) then
		s = gsub(s, "\r\n?", "\n")
	end
	path = path or "?"

	local tokens        = newTokenStream()
	tokens.sourceString = s
	tokens.sourcePath   = path

	local tokTypes  = tokens.type
	local tokValues = tokens.value
	local tokReprs  = tokens.representation
	local tokLine1  = tokens.lineStart
	local tokLine2  = tokens.lineEnd
	local tokPos1   = tokens.positionStart
	local tokPos2   = tokens.positionEnd

	local count = 0
	local ptr   = 1
	local ln    = 1

	while true do
		local i1, i2, spaces = find(s, "^(%s+)", ptr)
		if i1 then
			ln  = ln + countString(spaces, "\n", true)
			ptr = i2+1
		end

		if ptr > #s then  break  end

		local ptrStart = ptr
		local lnStart  = ln
		local tokType, tokRepr, tokValue

		-- Identifier/keyword.
		if find(s, "^[%a_]", ptr) then
			local i1, i2, word = find(s, "^([%a_][%w_]*)", ptr)
			ptr      = i2+1
			tokType  = KEYWORDS[word] and "keyword" or "identifier"
			tokRepr  = getSubstring(s, ptrStart, ptr-1)
			tokValue = tokRepr

		-- Comment.
		elseif find(s, "^%-%-", ptr) then
			ptr = ptr + 2

			local ok, equalSignCountIfLong
			ok, equalSignCountIfLong, ptr = parseStringlikeToken(s, ptr)

			if not ok then
				local errCode = equalSignCountIfLong
				if errCode == ERROR_UNFINISHED_VALUE then
					return nil, reportErrorInFile(s, path, ptrStart, "Tokenizer", "Unfinished long comment.")
				else
					return nil, reportErrorInFile(s, path, ptrStart, "Tokenizer", "Invalid comment.")
				end
			end

			-- Check for nesting of [[...]] which is depricated in Lua.
			if equalSignCountIfLong and equalSignCountIfLong == 0 then
				local pos = find(s, "[[", ptrStart+4, true)
				if pos and pos < ptr then
					return nil, reportErrorInFile(s, path, pos, "Tokenizer", "Cannot have nested comments.")
				end
			end

			tokType  = "comment"
			tokRepr  = getSubstring(s, ptrStart, ptr-1)
			tokValue = equalSignCountIfLong and getSubstring(tokRepr, 5+equalSignCountIfLong, -3-equalSignCountIfLong) or getSubstring(tokRepr, 3)
			tokValue = gsub(gsub(tokValue, "^%s+", ""), "%s+$", "")

		-- Number.
		elseif find(s, "^%.?%d", ptr) then
			local               lua52Hex, i1, i2, numStr = true,  find(s, NUM_HEX_FRAC_EXP, ptr)
			if not i1     then  lua52Hex, i1, i2, numStr = true,  find(s, NUM_HEX_FRAC,     ptr)
			if not i1     then  lua52Hex, i1, i2, numStr = true,  find(s, NUM_HEX_EXP,      ptr)
			if not i1     then  lua52Hex, i1, i2, numStr = false, find(s, NUM_HEX,          ptr)
			if not i1     then  lua52Hex, i1, i2, numStr = false, find(s, NUM_DEC_FRAC_EXP, ptr)
			if not i1     then  lua52Hex, i1, i2, numStr = false, find(s, NUM_DEC_FRAC,     ptr)
			if not i1     then  lua52Hex, i1, i2, numStr = false, find(s, NUM_DEC_EXP,      ptr)
			if not i1     then  lua52Hex, i1, i2, numStr = false, find(s, NUM_DEC,          ptr)
			if not numStr then  return nil, reportErrorInFile(s, path, ptrStart, "Tokenizer", "Malformed number.")
			end end end end end end end end

			local n = tonumber(numStr)

			-- Support hexadecimal floats if we're running Lua 5.1.
			if not n and lua52Hex then
				local               _, intStr, fracStr, expStr = match(numStr, NUM_HEX_FRAC_EXP)
				if not intStr then  _, intStr, fracStr         = match(numStr, NUM_HEX_FRAC) ; expStr  = "0"
				if not intStr then  _, intStr,          expStr = match(numStr, NUM_HEX_EXP)  ; fracStr = ""
				if not intStr then  return nil, reportErrorInFile(s, path, ptrStart, "Tokenizer", "Internal error parsing the number '%s'.", numStr)
				end end end

				n = tonumber(intStr, 16) or 0 -- intStr may be "".

				local fracValue = 1
				for i = 1, #fracStr do
					fracValue = fracValue / 16
					n         = n + tonumber(getSubstring(fracStr, i, i), 16) * fracValue
				end

				n = n * 2 ^ gsub(expStr, "^+", "")
			end

			if not n then
				return nil, reportErrorInFile(s, path, ptrStart, "Tokenizer", "Invalid number.")
			end

			ptr      = i2+1
			tokType  = "number"
			tokRepr  = numStr
			tokValue = n

			if find(s, "^%.", ptr) then
				return nil, reportErrorInFile(s, path, ptrStart, "Tokenizer", "Malformed number.")
			end

		-- String (short).
		elseif find(s, "^[\"']", ptr) then
			local quoteChar = getSubstring(s, ptr, ptr)
			ptr = ptr + 1

			while true do
				local c = getSubstring(s, ptr, ptr)

				if c == "" then
					return nil, reportErrorInFile(s, path, ptrStart, "Tokenizer", "Unfinished string.")

				elseif c == quoteChar then
					ptr = ptr + 1
					break

				elseif c == "\\" then
					-- Note: We don't have to look for multiple characters after
					-- the escape, like \nnn - this algorithm works anyway.
					if ptr+1 > #s then
						return nil, reportErrorInFile(s, path, ptrStart, "Tokenizer", "Unfinished string after escape character.")
					end
					ptr = ptr + 2

				elseif c == "\n" then
					-- Lua, this is silly!
					return nil, reportErrorInFile(s, path, ptrStart, "Tokenizer", "Unescaped newline in string.")

				else
					ptr = ptr + 1
				end
			end

			tokType = "string"
			tokRepr = getSubstring(s, ptrStart, ptr-1)

			local chunk, err = loadLuaString("return "..tokRepr, "@")
			if not chunk then
				err = gsub(err, "^:%d+: ", "")
				return nil, reportErrorInFile(s, path, ptrStart, "Tokenizer", "Could not convert string token to value. (%s)", err)
			end
			tokValue = assert(chunk)()
			assert(type(tokValue) == "string")

		-- Long string.
		elseif find(s, "^%[=*%[", ptr) then
			local ok, equalSignCountIfLong
			ok, equalSignCountIfLong, ptr = parseStringlikeToken(s, ptr)

			if not ok then
				local errCode = equalSignCountIfLong
				if errCode == ERROR_UNFINISHED_VALUE then
					return nil, reportErrorInFile(s, path, ptrStart, "Tokenizer", "Unfinished long string.")
				else
					return nil, reportErrorInFile(s, path, ptrStart, "Tokenizer", "Invalid long string.")
				end
			end

			tokType = "string"
			tokRepr = getSubstring(s, ptrStart, ptr-1)

			local chunk, err = loadLuaString("return "..tokRepr, "@")
			if not chunk then
				err = gsub(err, "^:%d+: ", "")
				return nil, reportErrorInFile(s, path, ptrStart, "Tokenizer", "Could not convert long string token to value. (%s)", err)
			end
			tokValue = assert(chunk)()
			assert(type(tokValue) == "string")

		-- Punctuation.
		elseif find(s, "^%.%.%.", ptr) then
			ptr      = ptr + 3
			tokType  = "punctuation"
			tokRepr  = getSubstring(s, ptrStart, ptr-1)
			tokValue = tokRepr
		elseif find(s, "^%.%.", ptr) or find(s, "^[=~<>]=", ptr) or find(s, "^::", ptr) or find(s, "^//", ptr) or find(s, "^<<", ptr) or find(s, "^>>", ptr) then
			ptr      = ptr + 2
			tokType  = "punctuation"
			tokRepr  = getSubstring(s, ptrStart, ptr-1)
			tokValue = tokRepr
		elseif find(s, "^[+%-*/%%^#<>=(){}[%];:,.&~|]", ptr) then
			ptr      = ptr + 1
			tokType  = "punctuation"
			tokRepr  = getSubstring(s, ptrStart, ptr-1)
			tokValue = tokRepr

		else
			return nil, reportErrorInFile(s, path, ptrStart, "Tokenizer", "Unknown character.")
		end

		if not tokType then
			return nil, reportErrorInFile(s, path, ptrStart, "Tokenizer", "Internal error: Got no token type.")
		end

		ln = ln + countString(tokRepr, "\n", true)

		count            = count + 1
		tokTypes [count] = tokType
		tokValues[count] = tokValue
		tokReprs [count] = tokRepr
		tokLine1 [count] = lnStart
		tokLine2 [count] = ln
		tokPos1  [count] = ptrStart
		tokPos2  [count] = ptr - 1

		-- print(F("%4d %-11s '%s'", count, tokType, (gsub(tokRepr, "\n", "\\n"))))
	end

	tokens.n = count
	return tokens
end

-- tokens, error = tokenizeFile( path )
function tokenizeFile(path)
	local file, err = io.open(path, "r")
	if not file then  return nil, err  end

	local s = file:read("*a")
	file:close()

	local tokens, err = tokenizeString(s, path)
	return tokens, err
end



function newTokenStream()
	return {
		n            = 0, -- Token count.
		sourceString = "",
		sourcePath   = "?",

		type           = {},
		value          = {},
		representation = {},
		lineStart      = {},
		lineEnd        = {},
		positionStart  = {},
		positionEnd    = {},
	}
end

--
-- :TokenInsertion
--
-- insertToken( tokens, [ index=atTheEnd, ] "comment",     contents )
-- insertToken( tokens, [ index=atTheEnd, ] "identifier",  name )
-- insertToken( tokens, [ index=atTheEnd, ] "keyword",     name )
-- insertToken( tokens, [ index=atTheEnd, ] "number",      number )
-- insertToken( tokens, [ index=atTheEnd, ] "punctuation", punctuationString )
-- insertToken( tokens, [ index=atTheEnd, ] "string",      stringValue )
--
local function insertToken(tokens, i, tokType, tokValue)
	if type(i) == "string" then
		i, tokType, tokValue = 1/0, i, tokType
	end
	i = getMin(getMax(i, 1), tokens.n+1)

	local tokRepr

	if tokType == "keyword" then
		if type(tokValue) ~= "string" then  error(F("Expected string value for 'keyword' token. (Got %s)", type(tokValue)))  end
		if not KEYWORDS[tokValue]     then  error(F("Invalid keyword '%s'.", tokValue))  end
		tokRepr = tokValue

	elseif tokType == "identifier" then
		if type(tokValue) ~= "string"          then  error(F("Expected string value for 'identifier' token. (Got %s)", type(tokValue)))  end
		if not find(tokValue, "^[%a_][%w_]*$") then  error(F("Invalid identifier '%s'.", tokValue))  end
		if KEYWORDS[tokValue]                  then  error(F("Invalid identifier '%s'.", tokValue))  end
		tokRepr = tokValue

	elseif tokType == "number" then
		if type(tokValue) ~= "number" then
			error(F("Expected number value for 'number' token. (Got %s)", type(tokValue)))
		end
		tokRepr = (
			tokValue == 0        and "0"      or -- Avoid '-0'.
			tokValue == 1/0      and "(1/0)"  or
			tokValue == -1/0     and "(-1/0)" or
			tokValue ~= tokValue and "(0/0)"  or
			formatNumber(tokValue)
		)

	elseif tokType == "string" then
		if type(tokValue) ~= "string" then  error(F("Expected string value for 'string' token. (Got %s)", type(tokValue)))  end
		tokRepr = gsub(F("%q", tokRepr), "\n", "n")

	elseif tokType == "punctuation" then
		if type(tokValue) ~= "string" then  error(F("Expected string value for 'punctuation' token. (Got %s)", type(tokValue)))  end
		if not PUNCTUATION[tokValue]  then  error(F("Invalid punctuation '%s'.", tokValue))  end
		tokRepr = tokValue

	elseif tokType == "comment" then
		if type(tokValue) ~= "string" then  error(F("Expected string value for 'comment' token. (Got %s)", type(tokValue)))  end

		if find(tokValue, "\n") then
			local equalSigns = find(tokValue, "[[", 1, true) and "=" or ""

			while find(tokValue, "]"..equalSigns.."]", 1, true) do
				equalSigns = equalSigns.."="
			end

			tokRepr = F("--[%s[%s]%s]", equalSigns, tokValue, equalSigns)

		else
			tokRepr = F("--%s\n", tokValue)
		end

	else
		error(F("Invalid token type '%s'.", tostring(tokType)))
	end

	local tokTypes  = tokens.type
	local tokValues = tokens.value
	local tokReprs  = tokens.representation
	local tokLine1  = tokens.lineStart
	local tokLine2  = tokens.lineEnd
	local tokPos1   = tokens.positionStart
	local tokPos2   = tokens.positionEnd

	for tok = tokens.n, i, -1 do
		tokTypes [tok+1] = tokTypes [tok]
		tokValues[tok+1] = tokValues[tok]
		tokReprs [tok+1] = tokReprs [tok]
		tokLine1 [tok+1] = tokLine1 [tok]
		tokLine2 [tok+1] = tokLine2 [tok]
		tokPos1  [tok+1] = tokPos1  [tok]
		tokPos2  [tok+1] = tokPos2  [tok]
	end

	tokTypes [i] = tokType
	tokValues[i] = tokValue
	tokReprs [i] = tokRepr
	tokLine1 [i] = 0
	tokLine2 [i] = 0
	tokPos1  [i] = 0
	tokPos2  [i] = 0

	tokens.n = tokens.n + 1
end

local function removeToken(tokens, i)
	i = i or tokens.n

	if i < 1 or i > tokens.n then  return  end

	local tokTypes  = tokens.type
	local tokValues = tokens.value
	local tokReprs  = tokens.representation
	local tokLine1  = tokens.lineStart
	local tokLine2  = tokens.lineEnd
	local tokPos1   = tokens.positionStart
	local tokPos2   = tokens.positionEnd

	for tok = i, tokens.n do
		tokTypes [tok] = tokTypes [tok+1]
		tokValues[tok] = tokValues[tok+1]
		tokReprs [tok] = tokReprs [tok+1]
		tokLine1 [tok] = tokLine1 [tok+1]
		tokLine2 [tok] = tokLine2 [tok+1]
		tokPos1  [tok] = tokPos1  [tok+1]
		tokPos2  [tok] = tokPos2  [tok+1]
	end

	tokens.n = tokens.n - 1
end



function isToken(tokens, tok, tokType, tokValue)
	return tokens.type[tok] == tokType and tokens.value[tok] == tokValue
end

function isTokenType(tokens, tok, tokType)
	return tokens.type[tok] == tokType
end

function isTokenAnyValue(tokens, tok, tokValueSet)
	return tokValueSet[tokens.value[tok]] == true
end



local parseExpression, parseExpressionList, parseFunctionParametersAndBody, parseBlock

local function parseIdentifier(tokens, tok) --> ident, token
	if not isTokenType(tokens, tok, "identifier") then
		reportErrorAtToken(tokens, tok, "Parser", "Expected an identifier.")
		return nil, tok
	end

	local ident = AstIdentifier(tok)
	ident.name  = tokens.value[tok]
	tok         = tok + 1

	return ident, tok
end

local function parseNameList(tokens, tok, names, allowVararg) --> success, token, vararg|nil
	while true do
		if allowVararg and isToken(tokens, tok, "punctuation", "...") then
			local vararg = AstVararg(tok)
			tok          = tok + 1 -- '...'
			return true, tok, vararg
		end

		local ident, tokNext = parseIdentifier(tokens, tok)
		if not ident then  return false, tok  end
		tok = tokNext

		table.insert(names, ident)

		if not isToken(tokens, tok, "punctuation", ",") then
			return true, tok, nil
		end
		tok = tok + 1 -- ','
	end

	return true, tok
end

local function parseTable(tokens, tok) --> tableNode, token
	local tableNode = AstTable(tok)
	tok             = tok + 1 -- '{'

	local generatedIndex = 0

	while true do
		if isToken(tokens, tok, "punctuation", "}") then
			tok = tok + 1 -- '}'
			break

		elseif isToken(tokens, tok, "punctuation", "[") then
			tok = tok + 1 -- '['

			local keyExpr, tokNext = parseExpression(tokens, tok, 0)
			if not keyExpr then  return nil, tok  end
			tok = tokNext

			if not isToken(tokens, tok, "punctuation", "]") then
				reportErrorAtToken(tokens, tok, "Parser", "Expected ']'.")
				return nil, tok
			end
			tok = tok + 1 -- ']'

			if not isToken(tokens, tok, "punctuation", "=") then
				reportErrorAtToken(tokens, tok, "Parser", "Expected '='.")
				return nil, tok
			end
			tok = tok + 1 -- '='

			local valueExpr, tokNext = parseExpression(tokens, tok, 0)
			if not valueExpr then  return nil, tok  end
			tok = tokNext

			local field = {key=keyExpr, value=valueExpr, generatedKey=false}
			table.insert(tableNode.fields, field)

		elseif isTokenType(tokens, tok, "identifier") and isToken(tokens, tok+1, "punctuation", "=") then
			local keyExpr = AstLiteral(tok)
			keyExpr.value = tokens.value[tok]
			tok           = tok + 1 -- identifier

			if not isToken(tokens, tok, "punctuation", "=") then
				reportErrorAtToken(tokens, tok, "Parser", "Expected '='.")
				return nil, tok
			end
			tok = tok + 1 -- '='

			local valueExpr, tokNext = parseExpression(tokens, tok, 0)
			if not valueExpr then  return nil, tok  end
			tok = tokNext

			local field = {key=keyExpr, value=valueExpr, generatedKey=false}
			table.insert(tableNode.fields, field)

		else
			generatedIndex = generatedIndex + 1

			local keyExpr = AstLiteral(tok)
			keyExpr.value = generatedIndex

			local valueExpr, tokNext = parseExpression(tokens, tok, 0)
			if not valueExpr then  return nil, tok  end
			tok = tokNext

			local field = {key=keyExpr, value=valueExpr, generatedKey=true}
			table.insert(tableNode.fields, field)
		end

		if isToken(tokens, tok, "punctuation", ",") or isToken(tokens, tok, "punctuation", ";") then
			tok = tok + 1 -- ',' or ';'
			-- Continue...

		elseif isToken(tokens, tok, "punctuation", "}") then
			tok = tok + 1 -- '}'
			break

		else
			reportErrorAtToken(tokens, tok, "Parser", "Expected ',' or '}'.")
			return nil, tok
		end
	end

	return tableNode, tok
end

function parseExpression(tokens, tok, lastPrecedence) --> expression, token
	local expr
	local canParseLookupOrCall = false

	-- identifier
	if isTokenType(tokens, tok, "identifier") then
		local ident, tokNext = parseIdentifier(tokens, tok)
		if not ident then  return false, tok  end
		tok = tokNext

		expr                 = ident
		canParseLookupOrCall = true

	-- ...
	elseif isToken(tokens, tok, "punctuation", "...") then
		local vararg = AstVararg(tok)
		tok          = tok + 1 -- '...'
		expr         = vararg

	-- literal
	elseif isTokenType(tokens, tok, "string") or isTokenType(tokens, tok, "number") then
		local literal = AstLiteral(tok)
		literal.value = tokens.value[tok]
		tok           = tok + 1 -- literal
		expr          = literal
	elseif isToken(tokens, tok, "keyword", "true") then
		local literal = AstLiteral(tok)
		literal.value = true
		tok           = tok + 1 -- 'true'
		expr          = literal
	elseif isToken(tokens, tok, "keyword", "false") then
		local literal = AstLiteral(tok)
		literal.value = false
		tok           = tok + 1 -- 'false'
		expr          = literal
	elseif isToken(tokens, tok, "keyword", "nil") then
		local literal = AstLiteral(tok)
		literal.value = nil
		tok           = tok + 1 -- 'nil'
		expr          = literal

	-- unary
	elseif
		(isToken(tokens, tok, "keyword", "not") or (isTokenType(tokens, tok, "punctuation") and isTokenAnyValue(tokens, tok, OPERATORS_UNARY)))
		and OPERATOR_PRECEDENCE.unary > lastPrecedence
	then
		local unary    = AstUnary(tok)
		unary.operator = tokens.value[tok]
		tok            = tok + 1 -- operator

		local subExpr, tokNext = parseExpression(tokens, tok, OPERATOR_PRECEDENCE.unary-1)
		if not subExpr then  return false, tok  end
		unary.expression = subExpr
		tok              = tokNext

		expr = unary

		-- Special rule: Treat '-n' as one literal (but not '-n^n' because of operator precedence).
		if unary.operator == "-" and subExpr.type == "literal" and type(subExpr.value) == "number" and isTokenType(tokens, subExpr.token, "number") then
			subExpr.value = -subExpr.value
			subExpr.token = unary.token
			expr          = subExpr
		end

	-- {...}
	elseif isToken(tokens, tok, "punctuation", "{") then
		local tableNode, tokNext = parseTable(tokens, tok)
		if not tableNode then  return false, tok  end
		tok = tokNext

		expr = tableNode

	-- function
	elseif isToken(tokens, tok, "keyword", "function") then
		tok = tok + 1 -- 'function'

		local func, tokNext = parseFunctionParametersAndBody(tokens, tok)
		if not func then  return false, tok  end
		func.token = tok
		tok        = tokNext

		expr = func

	-- (...)
	elseif isToken(tokens, tok, "punctuation", "(") then
		tok = tok + 1 -- '('

		local _expr, tokNext = parseExpression(tokens, tok, 0)
		if not _expr then  return false, tok  end
		tok = tokNext

		if _expr.type == "call" or _expr.type == "vararg" then
			_expr.adjustToOne = true
		end

		if not isToken(tokens, tok, "punctuation", ")") then
			reportErrorAtToken(tokens, tok, "Parser", "Expected ')'.")
			return false, tok
		end
		tok = tok + 1 -- ')'

		expr                 = _expr
		canParseLookupOrCall = true

	else
		reportErrorAtToken(tokens, tok, "Parser", "Failed parsing expression.")
		return false, tok
	end

	assert(expr)

	-- Binary expressions, including lookups and calls.
	while true do
		-- a + b
		if
			(
				(isTokenType(tokens, tok, "punctuation") and isTokenAnyValue(tokens, tok, OPERATORS_BINARY))
				or isToken(tokens, tok, "keyword", "and")
				or isToken(tokens, tok, "keyword", "or")
			)
			and OPERATOR_PRECEDENCE[tokens.value[tok]] > lastPrecedence
		then
			local rightAssociative = isToken(tokens, tok, "punctuation", "..") or isToken(tokens, tok, "punctuation", "^")

			local binary    = AstBinary(tok)
			binary.operator = tokens.value[tok]
			tok             = tok + 1 -- operator

			local lhsExpr = expr

			local rhsExpr, tokNext = parseExpression(tokens, tok, OPERATOR_PRECEDENCE[binary.operator] + (rightAssociative and -1 or 0))
			if not rhsExpr then  return false, tok  end
			tok = tokNext

			binary.left  = expr
			binary.right = rhsExpr

			expr = binary

			-- Special rule: Treat 'n/0' and '-n/0' as one literal (because that's how toLua() outputs infinity/NaN).
			if
				binary.operator  == "/"
				and lhsExpr.type == "literal" and type(lhsExpr.value) == "number"
				and rhsExpr.type == "literal" and      rhsExpr.value  == 0
				and (
					isTokenType(tokens, lhsExpr.token, "number")
					or (isToken(tokens, lhsExpr.token, "punctuation", "-") and isTokenType(tokens, lhsExpr.token+1, "number"))
				)
				and isTokenType(tokens, rhsExpr.token, "number")
			then
				lhsExpr.value = lhsExpr.value / 0
				expr          = lhsExpr
			end

		elseif not canParseLookupOrCall then
			break

		-- t.k
		elseif isToken(tokens, tok, "punctuation", ".") then
			local lookup = AstLookup(tok)
			tok          = tok + 1 -- '.'

			local ident, tokNext = parseIdentifier(tokens, tok)
			if not ident then  return false, tok  end
			tok = tokNext

			local literal = AstLiteral(ident.tok)
			literal.value = ident.name

			lookup.object = expr
			lookup.member = literal

			expr = lookup

		-- t[k]
		elseif isToken(tokens, tok, "punctuation", "[") then
			local lookup = AstLookup(tok)
			tok          = tok + 1 -- '['

			local memberExpr, tokNext = parseExpression(tokens, tok, 0)
			if not memberExpr then  return false, tok  end
			tok = tokNext

			if not isToken(tokens, tok, "punctuation", "]") then
				reportErrorAtToken(tokens, tok, "Parser", "Expected ']'.")
				return false, tok
			end
			tok = tok + 1 -- ']'

			lookup.object = expr
			lookup.member = memberExpr

			expr = lookup

		-- f""
		elseif isTokenType(tokens, tok, "string") then
			local call = AstCall(tok)

			local literal     = AstLiteral(tok)
			literal.value     = tokens.value[tok]
			tok               = tok + 1 -- string
			call.arguments[1] = literal

			call.callee = expr
			expr        = call

		-- f{}
		elseif isToken(tokens, tok, "punctuation", "{") then
			local call = AstCall(tok)

			local tableNode, tokNext = parseTable(tokens, tok)
			if not tableNode then  return false, tok  end
			call.arguments[1] = tableNode
			tok               = tokNext

			call.callee = expr
			expr        = call

		-- f()
		elseif isToken(tokens, tok, "punctuation", "(") then
			if tok >= 2 and tokens.lineStart[tok] > tokens.lineEnd[tok-1] then
				reportErrorAtToken(tokens, tok, "Parser", "Ambigous syntax. Is this a function call or a new statement?")
				return false, tok
			end

			local call = AstCall(tok)
			tok        = tok + 1 -- '('

			if not isToken(tokens, tok, "punctuation", ")") then
				local ok, tokNext = parseExpressionList(tokens, tok, call.arguments)
				if not ok then  return false, tok  end
				tok = tokNext
			end

			if not isToken(tokens, tok, "punctuation", ")") then
				reportErrorAtToken(tokens, tok, "Parser", "Expected ')'.")
				return false, tok
			end
			tok = tok + 1 -- ')'

			call.callee = expr
			expr        = call

		-- o:m()
		elseif isToken(tokens, tok, "punctuation", ":") then
			do
				local lookup = AstLookup(tok)
				tok          = tok + 1 -- ':'

				local ident, tokNext = parseIdentifier(tokens, tok)
				if not ident then  return false, tok  end
				tok = tokNext

				local literal = AstLiteral(ident.tok)
				literal.value = ident.name

				lookup.object = expr
				lookup.member = literal

				expr = lookup
			end

			do
				local call  = AstCall(tok)
				call.method = true

				if isTokenType(tokens, tok, "string") then
					local literal     = AstLiteral(tok)
					literal.value     = tokens.value[tok]
					tok               = tok + 1 -- string
					call.arguments[1] = literal

				elseif isToken(tokens, tok, "punctuation", "{") then
					local tableNode, tokNext = parseTable(tokens, tok)
					if not tableNode then  return false, tok  end
					call.arguments[1] = tableNode
					tok               = tokNext

				elseif isToken(tokens, tok, "punctuation", "(") then
					if tok >= 2 and tokens.lineStart[tok] > tokens.lineEnd[tok-1] then
						reportErrorAtToken(tokens, tok, "Parser", "Ambigous syntax. Is this a function call or a new statement?")
						return false, tok
					end

					tok = tok + 1 -- '('

					if not isToken(tokens, tok, "punctuation", ")") then
						local ok, tokNext = parseExpressionList(tokens, tok, call.arguments)
						if not ok then  return false, tok  end
						tok = tokNext
					end

					if not isToken(tokens, tok, "punctuation", ")") then
						reportErrorAtToken(tokens, tok, "Parser", "Expected ')'.")
						return false, tok
					end
					tok = tok + 1 -- ')'

				else
					reportErrorAtToken(tokens, tok, "Parser", "Expected '('.")
					return false, tok
				end

				call.callee = expr
				expr        = call
			end

		else
			break
		end

		assert(expr)
	end

	return expr, tok
end

function parseExpressionList(tokens, tok, expressions) --> success, token
	while true do
		local expr, tokNext = parseExpression(tokens, tok, 0)
		if not expr then  return false, tok  end
		tok = tokNext

		table.insert(expressions, expr)

		if not isToken(tokens, tok, "punctuation", ",") then
			return true, tok
		end
		tok = tok + 1 -- ','
	end
end

function parseFunctionParametersAndBody(tokens, tok)
	local func = AstFunction(tok)

	if not isToken(tokens, tok, "punctuation", "(") then
		reportErrorAtToken(tokens, tok, "Parser", "Expected '('.")
		return nil, tok
	end
	tok = tok + 1 -- '('

	if not isToken(tokens, tok, "punctuation", ")") then
		local ok, tokNext, vararg = parseNameList(tokens, tok, func.parameters, true)
		if not ok then  return nil, tok  end
		tok = tokNext

		func.vararg = vararg
	end

	if not isToken(tokens, tok, "punctuation", ")") then
		reportErrorAtToken(tokens, tok, "Parser", "Expected ')'.")
		return nil, tok
	end
	tok = tok + 1 -- ')'

	local block, tokNext = parseBlock(tokens, tok, true)
	if not block then  return nil, tok  end
	func.body = block
	tok       = tokNext

	if not isToken(tokens, tok, "keyword", "end") then
		reportErrorAtToken(tokens, tok, "Parser", "Expected 'end'.")
		return nil, tok
	end
	tok = tok + 1 -- 'end'

	return func, tok
end

local BLOCK_END_TOKEN_TYPES = {["end"]=true, ["else"]=true, ["elseif"]=true, ["until"]=true}

local function parseOneOrPossiblyMoreStatements(tokens, tok, statements) --> success, token
	--[[
	stat ::= varlist '=' explist |
	         functioncall |
	         do block end |
	         while exp do block end |
	         repeat block until exp |
	         if exp then block {elseif exp then block} [else block] end |
	         for Name '=' exp ',' exp [',' exp] do block end |
	         for namelist in explist do block end |
	         function funcname funcbody |
	         local function Name funcbody |
	         local namelist ['=' explist]

	laststat ::= return [explist] | break
	]]

	-- do
	if isToken(tokens, tok, "keyword", "do") then
		tok = tok + 1 -- 'do'

		local block, tokNext = parseBlock(tokens, tok, true)
		if not block then  return false, tok  end
		block.token = tok - 1
		tok         = tokNext

		if not isToken(tokens, tok, "keyword", "end") then
			reportErrorAtToken(tokens, tok, "Parser", "Expected 'end'.")
			return false, tok
		end
		tok = tok + 1 -- 'end'

		table.insert(statements, block)
		return true, tok

	-- while
	elseif isToken(tokens, tok, "keyword", "while") then
		local whileLoop = AstWhile(tok)
		tok             = tok + 1 -- 'while'

		local expr, tokNext = parseExpression(tokens, tok, 0)
		if not expr then  return false, tok  end
		whileLoop.condition = expr
		tok                 = tokNext

		if not isToken(tokens, tok, "keyword", "do") then
			reportErrorAtToken(tokens, tok, "Parser", "Expected 'do'.")
			return false, tok
		end
		tok = tok + 1 -- 'do'

		local block, tokNext = parseBlock(tokens, tok, true)
		if not block then  return false, tok  end
		block.token    = tok - 1
		whileLoop.body = block
		tok            = tokNext

		if not isToken(tokens, tok, "keyword", "end") then
			reportErrorAtToken(tokens, tok, "Parser", "Expected 'end'.")
			return false, tok
		end
		tok = tok + 1 -- 'end'

		table.insert(statements, whileLoop)
		return true, tok

	-- repeat
	elseif isToken(tokens, tok, "keyword", "repeat") then
		local repeatLoop = AstRepeat(tok)
		tok              = tok + 1 -- 'repeat'

		local block, tokNext = parseBlock(tokens, tok, true)
		if not block then  return false, tok  end
		repeatLoop.body = block
		tok             = tokNext

		if not isToken(tokens, tok, "keyword", "until") then
			reportErrorAtToken(tokens, tok, "Parser", "Expected 'until'.")
			return false, tok
		end
		tok = tok + 1 -- 'until'

		local expr, tokNext = parseExpression(tokens, tok, 0)
		if not expr then  return false, tok  end
		repeatLoop.condition = expr
		tok                  = tokNext

		table.insert(statements, repeatLoop)
		return true, tok

	-- if
	elseif isToken(tokens, tok, "keyword", "if") then
		local ifNode = AstIf(tok)
		tok          = tok + 1 -- 'if'

		local expr, tokNext = parseExpression(tokens, tok, 0)
		if not expr then  return false, tok  end
		ifNode.condition = expr
		tok              = tokNext

		if not isToken(tokens, tok, "keyword", "then") then
			reportErrorAtToken(tokens, tok, "Parser", "Expected 'then'.")
			return false, tok
		end
		tok = tok + 1 -- 'then'

		local block, tokNext = parseBlock(tokens, tok, true)
		if not block then  return false, tok  end
		ifNode.bodyTrue = block
		tok             = tokNext

		local ifNodeLeaf = ifNode

		while isToken(tokens, tok, "keyword", "elseif") do
			tok = tok + 1 -- 'elseif'

			ifNodeLeaf.bodyFalse               = AstBlock(tok)
			ifNodeLeaf.bodyFalse.statements[1] = AstIf(tok)
			ifNodeLeaf                         = ifNodeLeaf.bodyFalse.statements[1]

			local expr, tokNext = parseExpression(tokens, tok, 0)
			if not expr then  return false, tok  end
			ifNodeLeaf.condition = expr
			tok                  = tokNext

			if not isToken(tokens, tok, "keyword", "then") then
				reportErrorAtToken(tokens, tok, "Parser", "Expected 'then'.")
				return false, tok
			end
			tok = tok + 1 -- 'then'

			local block, tokNext = parseBlock(tokens, tok, true)
			if not block then  return false, tok  end
			ifNodeLeaf.bodyTrue = block
			tok                 = tokNext
		end

		if isToken(tokens, tok, "keyword", "else") then
			tok = tok + 1 -- 'else'

			local block, tokNext = parseBlock(tokens, tok, true)
			if not block then  return false, tok  end
			ifNodeLeaf.bodyFalse = block
			tok                  = tokNext
		end

		if not isToken(tokens, tok, "keyword", "end") then
			reportErrorAtToken(tokens, tok, "Parser", "Expected 'end'.")
			return false, tok
		end
		tok = tok + 1 -- 'end'

		table.insert(statements, ifNode)
		return true, tok

	-- for
	elseif isToken(tokens, tok, "keyword", "for") then
		local forLoop = AstFor(tok)
		tok           = tok + 1 -- 'for'

		local ok, tokNext = parseNameList(tokens, tok, forLoop.names, false)
		if not ok then  return false, tok  end
		tok = tokNext

		if isToken(tokens, tok, "keyword", "in") then
			forLoop.kind = "generic"
			tok          = tok + 1 -- 'in'

		elseif isToken(tokens, tok, "punctuation", "=") then
			if forLoop.names[2] then
				reportErrorAtToken(tokens, tok, "Parser", "Expected 'in'.")
				return false, tok
			end

			forLoop.kind = "numeric"
			tok          = tok + 1 -- '='

		else
			reportErrorAtToken(tokens, tok, "Parser", "Expected '=' or 'in'.")
			return false, tok
		end

		local ok, tokNext = parseExpressionList(tokens, tok, forLoop.values, 0)
		if not ok then  return false, tok  end
		tok = tokNext

		if forLoop.kind ~= "numeric" then
			-- void
		elseif not forLoop.values[2] then
			reportErrorAtToken(tokens, forLoop.values[1].token, "Parser", "Numeric loop: Too few values.")
			return false, tok
		elseif forLoop.values[4] then
			reportErrorAtToken(tokens, forLoop.values[4].token, "Parser", "Numeric loop: Too many values.")
			return false, tok
		end

		if not isToken(tokens, tok, "keyword", "do") then
			reportErrorAtToken(tokens, tok, "Parser", "Expected 'do'.")
			return false, tok
		end
		tok = tok + 1 -- 'do'

		local block, tokNext = parseBlock(tokens, tok, true)
		if not block then  return false, tok  end
		forLoop.body = block
		tok          = tokNext

		if not isToken(tokens, tok, "keyword", "end") then
			reportErrorAtToken(tokens, tok, "Parser", "Expected 'end'.")
			return false, tok
		end
		tok = tok + 1 -- 'end'

		table.insert(statements, forLoop)
		return true, tok

	-- function
	elseif isToken(tokens, tok, "keyword", "function") then
		local assignment = AstAssignment(tok)
		tok              = tok + 1 -- 'function'

		local targetExpr, tokNext = parseIdentifier(tokens, tok)
		if not targetExpr then  return false, tok  end
		tok = tokNext

		while isToken(tokens, tok, "punctuation", ".") do
			local lookup = AstLookup(tok)
			tok          = tok + 1 -- '.'

			local ident, tokNext = parseIdentifier(tokens, tok)
			if not ident then  return false, tok  end
			tok = tokNext

			local literal = AstLiteral(ident.tok)
			literal.value = ident.name
			lookup.member = literal

			lookup.object = targetExpr
			lookup.member = literal

			targetExpr = lookup
		end

		local isMethod = isToken(tokens, tok, "punctuation", ":")

		if isMethod then
			local lookup = AstLookup(tok)
			tok          = tok + 1 -- ':'

			local ident, tokNext = parseIdentifier(tokens, tok)
			if not ident then  return false, tok  end
			tok = tokNext

			local literal = AstLiteral(ident.tok)
			literal.value = ident.name
			lookup.member = literal

			lookup.object = targetExpr
			lookup.member = literal

			targetExpr = lookup
		end

		local func, tokNext = parseFunctionParametersAndBody(tokens, tok)
		if not func then  return false, tok  end
		tok = tokNext

		if isMethod then
			local ident = AstIdentifier(func.token)
			ident.name  = "self"
			table.insert(func.parameters, 1, ident)
		end

		assignment.targets[1] = targetExpr
		assignment.values[1]  = func

		table.insert(statements, assignment)
		return true, tok

	-- local function
	elseif isToken(tokens, tok, "keyword", "local") and isToken(tokens, tok+1, "keyword", "function") then
		local decl       = AstDeclaration(tok)
		local assignment = AstAssignment(tok)
		tok              = tok + 2 -- 'local' & 'function'

		local ident, tokNext = parseIdentifier(tokens, tok)
		local identCopy   = parseIdentifier(tokens, tok)
		if not ident then  return false, tok  end
		tok = tokNext

		local func, tokNext = parseFunctionParametersAndBody(tokens, tok)
		if not func then  return false, tok  end
		tok = tokNext

		decl.names[1]         = ident
		assignment.targets[1] = identCopy
		assignment.values[1]  = func

		table.insert(statements, decl)
		table.insert(statements, assignment)
		return true, tok

	-- local
	elseif isToken(tokens, tok, "keyword", "local") then
		local decl = AstDeclaration(tok)
		tok        = tok + 1 -- 'local'

		local ok, tokNext = parseNameList(tokens, tok, decl.names, false)
		if not ok then  return false, tok  end
		tok = tokNext

		if isToken(tokens, tok, "punctuation", "=") then
			tok = tok + 1 -- '='

			local ok, tokNext = parseExpressionList(tokens, tok, decl.values)
			if not ok then  return false, tok  end
			tok = tokNext
		end

		table.insert(statements, decl)
		return true, tok

	-- ::label::
	elseif isToken(tokens, tok, "punctuation", "::") then
		local label = AstLabel(tok)
		tok         = tok + 1 -- '::'

		local tokNext
		label.name, tokNext = parseIdentifier(tokens, tok)
		if not label.name then  return false, tok  end
		tok = tokNext

		if not isToken(tokens, tok, "punctuation", "::") then
			reportErrorAtToken(tokens, tok, "Parser", "Expected '::'.")
			return false, tok
		end
		tok = tok + 1 -- '::'

		table.insert(statements, label)
		return true, tok

	-- goto
	elseif isToken(tokens, tok, "keyword", "goto") then
		local gotoNode = AstGoto(tok)
		tok            = tok + 1 -- 'goto'

		local tokNext
		gotoNode.name, tokNext = parseIdentifier(tokens, tok)
		if not gotoNode.name then  return false, tok  end
		tok = tokNext

		table.insert(statements, gotoNode)
		return true, tok

	-- return (last)
	elseif isToken(tokens, tok, "keyword", "return") then
		local returnNode = AstReturn(tok)
		tok              = tok + 1 -- 'return'

		if tok <= tokens.n and not ((isTokenType(tokens, tok, "keyword") and isTokenAnyValue(tokens, tok, BLOCK_END_TOKEN_TYPES)) or isToken(tokens, tok, "punctuation", ";")) then
			local ok, tokNext = parseExpressionList(tokens, tok, returnNode.values)
			if not ok then  return false, tok  end
			tok = tokNext
		end

		table.insert(statements, returnNode)
		return true, tok

	-- break (last)
	elseif isToken(tokens, tok, "keyword", "break") then
		local breakNode = AstBreak(tok)
		tok             = tok + 1 -- 'break'

		table.insert(statements, breakNode)
		return true, tok

	elseif isTokenType(tokens, tok, "keyword") then
		return false, tok

	else
		local lookahead, tokNext = parseExpression(tokens, tok, 0)
		if not lookahead then  return false, tok  end

		if lookahead.type == "call" then
			local call = lookahead
			tok        = tokNext

			table.insert(statements, call)
			return true, tok

		elseif isToken(tokens, tokNext, "punctuation", "=") or isToken(tokens, tokNext, "punctuation", ",") then
			local assignment = AstAssignment(tokNext)

			local ok, tokNext = parseExpressionList(tokens, tok, assignment.targets)
			if not ok then  return false, tok  end
			tok = tokNext

			if not isToken(tokens, tok, "punctuation", "=") then
				reportErrorAtToken(tokens, tok, "Parser", "Expected '='.")
				return false, tok
			end
			tok = tok + 1 -- '='

			for _, targetExpr in ipairs(assignment.targets) do
				if not (targetExpr.type == "identifier" or targetExpr.type == "lookup") then
					reportErrorAtToken(tokens, targetExpr.token, "Parser", "Invalid assignment target.")
					return false, tok
				end
			end

			local ok, tokNext = parseExpressionList(tokens, tok, assignment.values)
			if not ok then  return false, tok  end
			tok = tokNext

			table.insert(statements, assignment)
			return true, tok

		else
			return false, tok
		end
	end

	assert(false)
end

function parseBlock(tokens, tok, stopAtEndKeyword) --> block, token
	local block      = AstBlock(tok)
	local statements = block.statements

	while tok <= tokens.n do
		while isToken(tokens, tok, "punctuation", ";") do
			-- Empty statements are valid in Lua 5.2+.
			tok = tok + 1 -- ';'
		end

		if stopAtEndKeyword and isTokenType(tokens, tok, "keyword") and isTokenAnyValue(tokens, tok, BLOCK_END_TOKEN_TYPES) then
			break
		end

		local ok, tokNext = parseOneOrPossiblyMoreStatements(tokens, tok, statements)
		if not ok then
			if not tokens.statementErrorReported then
				tokens.statementErrorReported = true
				reportErrorAtToken(tokens, tok, "Parser", "Failed parsing statement.")
			end
			return nil, tok
		end
		tok = tokNext

		if isToken(tokens, tok, "punctuation", ";") then
			tok = tok + 1 -- ';'
		end

		local lastStatement = statements[#statements]

		if lastStatement.type == "return" or lastStatement.type == "break" then
			break

		elseif lastStatement.type == "call" and lastStatement.adjustToOne then
			reportErrorAtToken(tokens, tok, "Parser", "Syntax error.")
			return nil, tok
		end
	end

	return block, tok
end

-- ast, error = parse( tokens )
-- ast, error = parse( luaString, pathForErrorMessages )
-- ast, error = parse( path )
function parse(tokens, path)
	if type(tokens) == "string" then
		local err

		if path then
			local lua   = tokens
			tokens, err = tokenizeString(lua, path)
		else
			path        = tokens
			tokens, err = tokenizeFile(path)
		end

		if not tokens then  return nil, err  end
	end

	local tokensPurged = {
		n            = 0,
		sourceString = tokens.sourceString,
		sourcePath   = tokens.sourcePath,

		type           = {},
		value          = {},
		representation = {},
		lineStart      = {},
		lineEnd        = {},
		positionStart  = {},
		positionEnd    = {},
	}

	for tok = 1, tokens.n do
		if tokens.type[tok] ~= "comment" then
			tokensPurged.n                              = tokensPurged.n+1
			tokensPurged.type          [tokensPurged.n] = tokens.type          [tok]
			tokensPurged.value         [tokensPurged.n] = tokens.value         [tok]
			tokensPurged.representation[tokensPurged.n] = tokens.representation[tok]
			tokensPurged.lineStart     [tokensPurged.n] = tokens.lineStart     [tok]
			tokensPurged.lineEnd       [tokensPurged.n] = tokens.lineEnd       [tok]
			tokensPurged.positionStart [tokensPurged.n] = tokens.positionStart [tok]
			tokensPurged.positionEnd   [tokensPurged.n] = tokens.positionEnd   [tok]
		end
	end

	local block = parseBlock(tokensPurged, 1, false)
	if not block then  return nil, "Failed parsing."  end

	return block
end



--
-- :NodeCreation
--
-- identifier   = newNode( "identifier", name )
-- vararg       = newNode( "vararg" )
-- literal      = newNode( "literal", value )
-- tableNode    = newNode( "table" )
-- lookup       = newNode( "lookup" )
-- unary        = newNode( "unary",  unaryOperator  )
-- binary       = newNode( "binary", binaryOperator )
-- call         = newNode( "call" )
-- functionNode = newNode( "function" )
-- breakNode    = newNode( "break" )
-- returnNode   = newNode( "return" )
-- label        = newNode( "label" )
-- gotoNode     = newNode( "goto" )
-- block        = newNode( "block" )
-- declaration  = newNode( "declaration" )
-- assignment   = newNode( "assignment" )
-- ifNode       = newNode( "if" )
-- whileLoop    = newNode( "while" )
-- repeatLoop   = newNode( "repeat" )
-- forLoop      = newNode( "for", forLoopKind )  -- forLoopKind can be "numeric" or "generic".
--
-- Search for 'NodeFields' for each node's fields.
--
local function newNode(nodeType, ...)
	local node

	if     nodeType == "vararg"      then  node = AstVararg(0)
	elseif nodeType == "table"       then  node = AstTable(0)
	elseif nodeType == "lookup"      then  node = AstLookup(0)
	elseif nodeType == "call"        then  node = AstCall(0)
	elseif nodeType == "function"    then  node = AstFunction(0)
	elseif nodeType == "break"       then  node = AstBreak(0)
	elseif nodeType == "return"      then  node = AstReturn(0)
	elseif nodeType == "label"       then  node = AstLabel(0)
	elseif nodeType == "goto"        then  node = AstGoto(0)
	elseif nodeType == "block"       then  node = AstBlock(0)
	elseif nodeType == "declaration" then  node = AstDeclaration(0)
	elseif nodeType == "assignment"  then  node = AstAssignment(0)
	elseif nodeType == "if"          then  node = AstIf(0)
	elseif nodeType == "while"       then  node = AstWhile(0)
	elseif nodeType == "repeat"      then  node = AstRepeat(0)

	elseif nodeType == "identifier" then
		if select("#", ...) == 0 then
			error(F("Missing name argument for identifier."))
		end

		local name = ...
		if type(name) ~= "string" then
			error(F("Invalid name argument value type '%s'. (Expected string)", type(name)))
		elseif not find(name, "^[%a_][%w_]*$") then
			error(F("Invalid identifier name '%s'.", name))
		elseif KEYWORDS[name] then
			error(F("Invalid identifier name '%s'.", name))
		end

		node       = AstIdentifier(0)
		node.name = name

	elseif nodeType == "literal" then
		if select("#", ...) == 0 then
			error(F("Missing value argument for literal."))
		end

		local value = ...
		if not (type(value) == "number" or type(value) == "string" or type(value) == "boolean" or type(value) == "nil") then
			error(F("Invalid literal value type '%s'. (Expected number, string, boolean or nil)", type(value)))
		end

		node       = AstLiteral(0)
		node.value = value

	elseif nodeType == "unary" then
		if select("#", ...) == 0 then
			error(F("Missing operator argument for unary expression."))
		end

		local op = ...
		if not OPERATORS_UNARY[op] then
			error(F("Invalid unary operator '%s'.", tostring(op)))
		end

		node          = AstUnary(0)
		node.operator = op

	elseif nodeType == "binary" then
		if select("#", ...) == 0 then
			error(F("Missing operator argument for binary expression."))
		end

		local op = ...
		if not OPERATORS_BINARY[op] then
			error(F("Invalid binary operator '%s'.", tostring(op)))
		end

		node          = AstBinary(0)
		node.operator = op

	elseif nodeType == "for" then
		if select("#", ...) == 0 then
			error(F("Missing kind argument for 'for' loop."))
		end

		local kind = ...
		if not (kind == "numeric" or kind == "generic") then
			error(F("Invalid for loop kind '%s'. (Must be 'numeric' or 'generic')", tostring(kind)))
		end

		node      = AstFor(0)
		node.kind = kind

	else
		error(F("Invalid node type '%s'.", tostring(nodeType)))
	end
	return node
end



do
	local ioWrite = io.write

	local function _printNode(node)
		local nodeType = node.type

		ioWrite(nodeType)
		if parser.printIds then  ioWrite("#", node.id)  end

		if nodeType == "identifier" then
			ioWrite(" (", node.name)
			if node.declaration then
				ioWrite(" local:", node.declaration.type)
				if parser.printIds then  ioWrite("#", node.declaration.id)  end
			end
			ioWrite(")")

		elseif nodeType == "vararg" then
			if node.adjustToOne then  ioWrite(" (adjustToOne)")  end

		elseif nodeType == "literal" then
			if node.value == nil or node.value == true or node.value == false then
				ioWrite(" (", tostring(node.value), ")")
			elseif type(node.value) == "string" then
				ioWrite(' (string="', node.value:gsub('\r','{CR}'):gsub('\n','{NL}'), '")')
			else
				ioWrite(" (", type(node.value), "=", tostring(node.value), ")")
			end

		elseif nodeType == "unary" then
			ioWrite(" (", node.operator, ")")

		elseif nodeType == "binary" then
			ioWrite(" (", node.operator, ")")

		elseif nodeType == "call" then
			if node.method      then  ioWrite(" (method)"     )  end
			if node.adjustToOne then  ioWrite(" (adjustToOne)")  end

		elseif nodeType == "function" then
			if node.vararg then  ioWrite(" (vararg)")  end

		elseif nodeType == "for" then
			ioWrite(" (", node.kind, ")")
		end

		ioWrite("\n")
	end

	local function _printTree(node, indent, key)
		for i = 1, indent do  ioWrite("    ")  end
		indent = indent+1

		if key ~= nil then
			ioWrite(tostring(key))
			ioWrite(" ")
		end

		_printNode(node)

		local nodeType = node.type

		if nodeType == "table" then
			for i, field in ipairs(node.fields) do
				if field.key   then  _printTree(field.key,   indent, i..(field.generatedKey and "KEYGEN" or "KEY   "))  end
				if field.value then  _printTree(field.value, indent, i..(                                   "VALUE "))  end
				local a =  {1, 5, g=6}
			end

		elseif nodeType == "lookup" then
			if node.object then  _printTree(node.object, indent, "OBJECT")  end
			if node.member then  _printTree(node.member, indent, "MEMBER")  end

		elseif nodeType == "unary" then
			if node.expression then  _printTree(node.expression, indent, nil)  end

		elseif nodeType == "binary" then
			if node.left  then  _printTree(node.left,  indent, nil)  end
			ioWrite(repeatString("    ", indent), node.operator, "\n")
			if node.right then  _printTree(node.right, indent, nil)  end

		elseif nodeType == "call" then
			if node.callee then  _printTree(node.callee, indent, "CALLEE")  end
			for i, expr in ipairs(node.arguments) do  _printTree(expr, indent, "ARG"..i)  end

		elseif nodeType == "function" then
			for i, ident in ipairs(node.parameters) do  _printTree(ident, indent, "PARAM"..i)  end
			if node.body then  _printTree(node.body, indent, "BODY")  end

		elseif nodeType == "return" then
			for i, expr in ipairs(node.values) do  _printTree(expr, indent, tostring(i))  end

		elseif nodeType == "label" then
			if node.name then  _printTree(node.name, indent, nil)  end

		elseif nodeType == "goto" then
			if node.name then  _printTree(node.name, indent, nil)  end

		elseif nodeType == "block" then
			for i, statement in ipairs(node.statements) do  _printTree(statement, indent, tostring(i))  end

		elseif nodeType == "declaration" then
			for i, ident in ipairs(node.names)  do  _printTree(ident, indent,  "NAME"..i)  end
			for i, expr  in ipairs(node.values) do  _printTree(expr,  indent, "VALUE"..i)  end

		elseif nodeType == "assignment" then
			for i, expr in ipairs(node.targets) do  _printTree(expr, indent, "TARGET"..i)  end
			for i, expr in ipairs(node.values)  do  _printTree(expr, indent,  "VALUE"..i)  end

		elseif nodeType == "if" then
			if node.condition then  _printTree(node.condition, indent, "CONDITION")  end
			if node.bodyTrue  then  _printTree(node.bodyTrue,  indent, "BODY"     )  end

			local i = 1

			while node.bodyFalse do
				-- Automatically detect what looks like 'elseif'.
				if #node.bodyFalse.statements == 1 and node.bodyFalse.statements[1].type == "if" then
					i    = i+1
					node = node.bodyFalse.statements[1]

					if node.condition then  _printTree(node.condition, indent, "ELSEIF" )  end
					if node.bodyTrue  then  _printTree(node.bodyTrue,  indent, "BODY"..i)  end

				else
					_printTree(node.bodyFalse, indent, "ELSE")
					break
				end
			end

		elseif nodeType == "while" then
			if node.condition then  _printTree(node.condition, indent, "CONDITION")  end
			if node.body      then  _printTree(node.body,      indent, "BODY"     )  end

		elseif nodeType == "repeat" then
			if node.body      then  _printTree(node.body,      indent, "BODY"     )  end
			if node.condition then  _printTree(node.condition, indent, "CONDITION")  end

		elseif nodeType == "for" then
			for i, ident in ipairs(node.names)  do  _printTree(ident, indent,  "NAME"..i)  end
			for i, expr  in ipairs(node.values) do  _printTree(expr,  indent, "VALUE"..i)  end
			if node.body then  _printTree(node.body, indent, "BODY")  end
		end
	end

	function printNode(node)
		_printNode(node)
	end

	function printTree(node)
		_printTree(node, 0, nil)
	end
end



-- didBreak = traverseTree( astNode, [ leavesFirst=false, ] callback [, topNodeParent=nil, topNodeContainer=nil, topNodeKey=nil ] )
-- action   = callback( astNode, parent, container, key )
-- action   = "stop"|"ignorechildren"|nil  -- Returning nil (or nothing) means continue traversal.
function traverseTree(node, leavesFirst, cb, parent, container, k)
	assertArg("traverseTree", 1, node, "table")

	if type(leavesFirst) == "boolean" then
		assertArg("traverseTree", 3, cb, "function")
	else
		leavesFirst, cb, parent, container, k = false, leavesFirst, cb, parent, container
		assertArg("traverseTree", 2, cb, "function")
	end

	if not leavesFirst then
		local action = cb(node, parent, container, k)
		if action == "stop"           then  return true   end
		if action == "ignorechildren" then  return false  end
		if action                     then  error(F("Unknown traversal action '%s' returned from callback.", tostring(action)))  end
	end

	local nodeType = node.type

	if nodeType == "identifier" or nodeType == "vararg" or nodeType == "literal" or nodeType == "break" then
		-- void  No child nodes.

	elseif nodeType == "table" then
		for _, field in ipairs(node.fields) do
			if field.key   and traverseTree(field.key,   leavesFirst, cb, node, field, "key")   then  return true  end
			if field.value and traverseTree(field.value, leavesFirst, cb, node, field, "value") then  return true  end
		end

	elseif nodeType == "lookup" then
		if node.object and traverseTree(node.object, leavesFirst, cb, node, node, "object") then  return true  end
		if node.member and traverseTree(node.member, leavesFirst, cb, node, node, "member") then  return true  end

	elseif nodeType == "unary" then
		if node.expression and traverseTree(node.expression, leavesFirst, cb, node, node, "expression") then  return true  end

	elseif nodeType == "binary" then
		if node.left  and traverseTree(node.left,  leavesFirst, cb, node, node, "left")  then  return true  end
		if node.right and traverseTree(node.right, leavesFirst, cb, node, node, "right") then  return true  end

	elseif nodeType == "call" then
		if node.callee and traverseTree(node.callee, leavesFirst, cb, node, node, "callee") then  return true  end
		for i, expr in ipairs(node.arguments) do
			if traverseTree(expr, leavesFirst, cb, node, node.arguments, i) then  return true  end
		end

	elseif nodeType == "function" then
		for i, name in ipairs(node.parameters) do
			if traverseTree(name, leavesFirst, cb, node, node.parameters, i) then  return true  end
		end
		if node.vararg and traverseTree(node.vararg, leavesFirst, cb, node, node, "vararg") then  return true  end
		if node.body   and traverseTree(node.body,   leavesFirst, cb, node, node, "body")   then  return true  end

	elseif nodeType == "return" then
		for i, expr in ipairs(node.values) do
			if traverseTree(expr, leavesFirst, cb, node, node.values, i) then  return true  end
		end

	elseif nodeType == "label" then
		if node.name and traverseTree(node.name, leavesFirst, cb, node, node, "name") then  return true  end

	elseif nodeType == "goto" then
		if node.name and traverseTree(node.name, leavesFirst, cb, node, node, "name") then  return true  end

	elseif nodeType == "block" then
		for i, statement in ipairs(node.statements) do
			if traverseTree(statement, leavesFirst, cb, node, node.statements, i) then  return true  end
		end

	elseif nodeType == "declaration" then
		for i, ident in ipairs(node.names) do
			if traverseTree(ident, leavesFirst, cb, node, node.names, i) then  return true  end
		end
		for i, expr in ipairs(node.values) do
			if traverseTree(expr, leavesFirst, cb, node, node.values, i) then  return true  end
		end

	elseif nodeType == "assignment" then
		for i, expr in ipairs(node.targets) do
			if traverseTree(expr, leavesFirst, cb, node, node.targets, i) then  return true  end
		end
		for i, expr in ipairs(node.values) do
			if traverseTree(expr, leavesFirst, cb, node, node.values, i) then  return true  end
		end

	elseif nodeType == "if" then
		if node.condition and traverseTree(node.condition, leavesFirst, cb, node, node, "condition") then  return true  end
		if node.bodyTrue  and traverseTree(node.bodyTrue,  leavesFirst, cb, node, node, "bodyTrue")  then  return true  end
		if node.bodyFalse and traverseTree(node.bodyFalse, leavesFirst, cb, node, node, "bodyFalse") then  return true  end

	elseif nodeType == "while" then
		if node.condition and traverseTree(node.condition, leavesFirst, cb, node, node, "condition") then  return true  end
		if node.body      and traverseTree(node.body,      leavesFirst, cb, node, node, "body")      then  return true  end

	elseif nodeType == "repeat" then
		if node.body      and traverseTree(node.body,      leavesFirst, cb, node, node, "body")      then  return true  end
		if node.condition and traverseTree(node.condition, leavesFirst, cb, node, node, "condition") then  return true  end

	elseif nodeType == "for" then
		for i, ident in ipairs(node.names) do
			if traverseTree(ident, leavesFirst, cb, node, node.names, i) then  return true  end
		end
		for i, expr in ipairs(node.values) do
			if traverseTree(expr, leavesFirst, cb, node, node.values, i) then  return true  end
		end
		if node.body and traverseTree(node.body, leavesFirst, cb, node, node, "body") then  return true  end

	else
		error(F("Invalid node type '%s'.", tostring(nodeType)))
	end

	if leavesFirst then
		local action = cb(node, parent, container, k)
		if action == "stop"           then  return true   end
		if action == "ignorechildren" then  error("Cannot ignore children when leavesFirst is set.")  end
		if action                     then  error(F("Unknown traversal action '%s' returned from callback.", tostring(action)))  end
	end

	return false
end



function updateReferences(node)
	local idents = {}

	traverseTree(node, function(node, parent, container, key)
		node.parent    = parent
		node.container = container
		node.key       = key

		if node.type == "identifier" then
			table.insert(idents, node)
		end
	end)

	for _, ident in ipairs(idents) do
		local name   = ident.name
		local parent = ident
		local lastChild

		while true do
			lastChild = parent
			parent    = parent.parent

			if not parent then  break  end

			if parent.type == "declaration" then
				local decl = parent
				if lastChild.container == decl.names and itemWith1(decl.names, "name", name) then
					ident.declaration = decl
					break
				end

			elseif parent.type == "function" then
				local func = parent
				if itemWith1(func.parameters, "name", name) then
					ident.declaration = func
					break
				end

			elseif parent.type == "for" then
				local forLoop = parent
				if itemWith1(forLoop.names, "name", name) then
					ident.declaration = forLoop
					break
				end

			elseif parent.type == "block" then
				local block = parent

				for i = lastChild.key-1, 1, -1 do
					local statement = block.statements[i]

					if statement.type == "declaration" then
						local decl = statement
						if itemWith1(decl.names, "name", name) then
							ident.declaration = decl
							break
						end
					end
				end

				if ident.declaration then  break  end

			elseif parent.type == "repeat" then
				local repeatLoop = parent

				if lastChild == repeatLoop.condition then
					local block = repeatLoop.body

					for i = #block.statements, 1, -1 do
						local statement = block.statements[i]

						if statement.type == "declaration" then
							local decl = statement
							if itemWith1(decl.names, "name", name) then
								ident.declaration = decl
								break
							end
						end
					end

					if ident.declaration then  break  end
				end
			end
		end--while true

		--[[ DEBUG
		print(F(
			"%-10s  %-12s  %s",
			name,
			(ident.declaration and ident.declaration.type or ""),
			tostring(ident.declaration and ident.declaration.id or "")
		))
		--]]
	end--for idents
end



local unaryFolders = {
	["-"] = function(unary, expr)
		if expr.type == "literal" and type(expr.value) == "number" then
			local literal = AstLiteral(unary.token)
			literal.value = -expr.value
			return literal
		end
		return nil
	end,
	["not"] = function(unary, expr)
		if expr.type == "literal" then
			local literal = AstLiteral(unary.token)
			literal.value = not expr.value
			return literal
		end
		return nil
	end,
	["#"] = function(unary, expr)
		-- I don't think there's ever anything to do here.
		return nil
	end,
	["~"] = function(unary, expr)
		-- @Incomplete
		return nil
	end,
}

local binaryFolders = {
	-- @Robustness: The bitwise operations are probably not very robust.
	["+"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and type(l.value) == type(r.value) and type(l.value) == "number" then
			local literal = AstLiteral(binary.token)
			literal.value = (l.value + r.value)
			return literal
		end
		return nil
	end,
	["-"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and type(l.value) == type(r.value) and type(l.value) == "number" then
			local literal = AstLiteral(binary.token)
			literal.value = (l.value - r.value)
			return literal
		end
		return nil
	end,
	["*"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and type(l.value) == type(r.value) and type(l.value) == "number" then
			local literal = AstLiteral(binary.token)
			literal.value = (l.value * r.value)
			return literal
		end
		return nil
	end,
	["/"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and type(l.value) == type(r.value) and type(l.value) == "number" then
			local literal = AstLiteral(binary.token)
			literal.value = (l.value / r.value)
			return literal
		end
		return nil
	end,
	["//"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and type(l.value) == type(r.value) and type(l.value) == "number" then
			local literal = AstLiteral(binary.token)
			literal.value = floor(l.value / r.value)
			return literal
		end
		return nil
	end,
	["^"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and type(l.value) == type(r.value) and type(l.value) == "number" then
			local literal = AstLiteral(binary.token)
			literal.value = (l.value ^ r.value)
			return literal
		end
		return nil
	end,
	["%"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and type(l.value) == type(r.value) and type(l.value) == "number" then
			local literal = AstLiteral(binary.token)
			literal.value = (l.value % r.value)
			return literal
		end
		return nil
	end,
	["&"] = function(binary, l, r)
		-- @Incomplete
		return nil
	end,
	["~"] = function(binary, l, r)
		-- @Incomplete
		return nil
	end,
	["|"] = function(binary, l, r)
		-- @Incomplete
		return nil
	end,
	[">>"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and type(l.value) == type(r.value) and type(l.value) == "number" and l.value >= 0 then
			local n = floor(l.value)

			if r.value > 0 then
				for _ = 1,  r.value do  n = floor(n*.5)  end
			else
				for _ = 1, -r.value do  n = n*2          end
			end

			local literal = AstLiteral(binary.token)
			literal.value = n
			return literal
		end
		return nil
	end,
	["<<"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and type(l.value) == type(r.value) and type(l.value) == "number" and l.value >= 0 then
			local n = floor(l.value)

			if r.value > 0 then
				for _ = 1,  r.value do  n = n*2          end
			else
				for _ = 1, -r.value do  n = floor(n*.5)  end
			end

			local literal = AstLiteral(binary.token)
			literal.value = n
			return literal
		end
		return nil
	end,
	[".."] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and (type(l.value) == "number" or type(l.value) == "string") and (type(r.value) == "number" or type(r.value) == "string") then
			local literal = AstLiteral(binary.token)
			literal.value = (l.value .. r.value)
			return literal
		end
		return nil
	end,
	["<"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and type(l.value) == type(r.value) and (type(l.value) == "number" or type(l.value) == "string") then
			local literal = AstLiteral(binary.token)
			literal.value = (l.value < r.value)
			return literal
		end
		return nil
	end,
	["<="] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and type(l.value) == type(r.value) and (type(l.value) == "number" or type(l.value) == "string") then
			local literal = AstLiteral(binary.token)
			literal.value = (l.value <= r.value)
			return literal
		end
		return nil
	end,
	[">"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and type(l.value) == type(r.value) and (type(l.value) == "number" or type(l.value) == "string") then
			local literal = AstLiteral(binary.token)
			literal.value = (l.value > r.value)
			return literal
		end
		return nil
	end,
	[">="] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and type(l.value) == type(r.value) and (type(l.value) == "number" or type(l.value) == "string") then
			local literal = AstLiteral(binary.token)
			literal.value = (l.value >= r.value)
			return literal
		end
		return nil
	end,
	["=="] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" then
			local literal = AstLiteral(binary.token)
			literal.value = (l.value == r.value)
			return literal
		end
		return nil
	end,
	["~="] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" then
			local literal = AstLiteral(binary.token)
			literal.value = (l.value ~= r.value)
			return literal
		end
		return nil
	end,
	["and"] = function(binary, l, r)
		if l.type == "literal" then  return l.value and r or l  end
		return nil
	end,
	["or"] = function(binary, l, r)
		if l.type == "literal" then  return l.value and l or r  end
		return nil
	end,
}

local function maybeReplace(parent, container, key, replacement)
	if not replacement then  return  end

	container[key]        = replacement
	replacement.parent    = parent
	replacement.container = container
	replacement.key       = key
end

local function foldTree(node) -- @Incomplete: Expose in API.
	traverseTree(node, true, function(node, parent, container, key)
		if not parent then
			-- void
		elseif node.type == "unary" then
			maybeReplace(parent, container, key, unaryFolders[node.operator](node, node.expression))
		elseif node.type == "binary" then
			maybeReplace(parent, container, key, binaryFolders[node.operator](node, node.left, node.right))
		end
	end)
end



-- (statement, block) | declLike | repeatLoop | func = findParentStatementAndBlockOrExpressionOfInterest( node, declLike )
local function findParentStatementAndBlockOrExpressionOfInterest(node, declLike)
	while true do
		local lastChild = node
		node            = node.parent

		if not node                then  return nil,       nil   end
		if node == declLike        then  return declLike,  nil   end
		if node.type == "block"    then  return lastChild, node  end
		if node.type == "function" then  return node,      nil   end
		if node.type == "for"      then  return node,      nil   end

		if node.type == "repeat" and lastChild == node.condition then  return node, nil  end
	end
end

-- foundCurrentDeclLike = lookForDeclarationLikesAndRegisterWatchers( declLikeWatchers, currentIdentInfo, block, statementStartIndex )
local function lookForDeclarationLikesAndRegisterWatchers(declLikeWatchers, identInfo, block, iStart)
	local statements      = block.statements
	local currentDeclLike = identInfo.ident.declaration

	for i = iStart, 1, -1 do
		local statement = statements[i]

		if statement.type == "declaration" or statement.type == "for" then
			local declLike = statement

			-- Note: Identifiers in declaration-likes also watch their own declaration-like. :DeclarationIdentifiersWatchTheirParent
			declLikeWatchers[declLike] = declLikeWatchers[declLike] or {}
			table.insert(declLikeWatchers[declLike], identInfo.ident)

			if declLike == currentDeclLike then  return true  end

			-- table.insert(identInfo.visibleDeclLikes, declLike)
		end
	end

	return false
end

local function getInformationAboutIdentifiers(node)
	-- Collect identifiers.
	local identInfos = {--[[ [ident1]=identInfo1, identInfo1, ... ]]} -- identInfo = {ident=ident, visibleDeclLikes=declLikes}

	traverseTree(node, function(node, parent, container, key)
		if node.type == "identifier" then
			local ident = node

			local identInfo = {ident=ident--[[, visibleDeclLikes={}]]}
			table.insert(identInfos, identInfo)
			identInfos[ident] = identInfo
		end
	end)

	-- Determine visible declarations for each identifier.
	local declLikeWatchers = {--[[ [declLike1]={ident1,...}, ... ]]}

	for _, identInfo in ipairs(identInfos) do
		local currentIdent    = identInfo.ident
		local currentDeclLike = currentIdent.declaration
		local block           = currentIdent -- Start node for while loop.

		while true do
			local statementOrInterest
			statementOrInterest, block = findParentStatementAndBlockOrExpressionOfInterest(block, currentDeclLike)

			if not statementOrInterest then
				assert(currentDeclLike == nil)
				break
			end

			if block then
				local statement = statementOrInterest

				assert(type(statement.key) == "number")
				assert(statement.container == block.statements)

				if lookForDeclarationLikesAndRegisterWatchers(declLikeWatchers, identInfo, block, statement.key-1) then
					break
				end

			elseif statementOrInterest.type == "function" or statementOrInterest.type == "for" then
				local declLike = statementOrInterest
				block          = declLike -- Start node for while loop.

				-- :DeclarationIdentifiersWatchTheirParent
				declLikeWatchers[declLike] = declLikeWatchers[declLike] or {}
				table.insert(declLikeWatchers[declLike], identInfo.ident)

				if declLike == currentDeclLike then  break  end

			elseif statementOrInterest.type == "repeat" then
				local repeatLoop = statementOrInterest
				block            = repeatLoop.body

				if lookForDeclarationLikesAndRegisterWatchers(declLikeWatchers, identInfo, block, #block.statements) then
					break
				end

			else
				local declLike = statementOrInterest
				assert(declLike == currentDeclLike)
				break
			end
		end
	end

	return identInfos, declLikeWatchers
end



local function optimizeTree(node) -- @Incomplete: Expose in API.
	-- @Incomplete
end



local function getNamesFromDeclarationLike(declLike)
	return declLike.names or declLike.parameters
end

local generateName
do
	local BANK_LETTERS  = "etaoinshrdlcumwfgypbvkxjqz_ETAOINSHRDLCUMWFGYPBVKXJQZ" -- http://en.wikipedia.org/wiki/Letter_frequencies
	local BANK_ALPHANUM = "etaoinshrdlcumwfgypbvkxjqz_ETAOINSHRDLCUMWFGYPBVKXJQZ0123456789"
	local cache         = {}

	function generateName(nameGeneration)
		if not cache[nameGeneration] then
			-- @Cleanup: Output the most significant byte first. (We need to know the length beforehand then, probably, so we use the correct bank.)
			local charBytes = {}

			for i = 1, 1/0 do
				nameGeneration  = nameGeneration - 1
				local charBank  = (i == 1) and BANK_LETTERS or BANK_ALPHANUM
				local charIndex = nameGeneration % #charBank + 1
				charBytes[i]    = charBank:byte(charIndex)
				nameGeneration  = floor(nameGeneration / #charBank)

				if nameGeneration == 0 then  break  end
			end

			cache[nameGeneration] = bytesToString(unpack(charBytes))
		end

		return cache[nameGeneration]
	end

	-- for nameGeneration = 1, 3500 do  print(generateName(nameGeneration))  end ; error("TEST")
	-- for pow = 0, 32 do  print(generateName(2^pow))  end ; error("TEST")
end

local function minify(node)
	foldTree(node)
	optimizeTree(node)

	local identInfos, declLikeWatchers = getInformationAboutIdentifiers(node)

	--
	-- Remember old declaration info before we start modifying things.
	--
	local declLikes_oldNameToIdent = {--[[ [declLike1]={[name1]=declIdent1,...}, ... ]]}

	for i, identInfo in ipairs(identInfos) do
		local declLike = identInfo.ident.declaration

		if declLike and not declLikes_oldNameToIdent[declLike] then
			local oldNameToIdent               = {}
			declLikes_oldNameToIdent[declLike] = oldNameToIdent

			for _, ident in ipairs(getNamesFromDeclarationLike(declLike)) do
				oldNameToIdent[ident.name] = ident
			end
		end
	end

	--
	-- Make sure frequencies affect who gets shorter names first. @Incomplete
	--
	--[[
	table.sort(identInfos, function(a, b)
		if #a.visibleDeclLikes ~= #b.visibleDeclLikes then
			-- I feel this is kinda reversed - it should be how many can see you, not how many you can see. Does this matter? This sure is confusing!
			return #a.visibleDeclLikes < #b.visibleDeclLikes
		end
		-- if (localCounts[a.ident.name] or 0) ~= (localCounts[b.ident.name] or 0) then -- This is most certainly incorrect, because the names are old!
		-- 	return (localCounts[a.ident.name] or 0) > (localCounts[b.ident.name] or 0)
		-- end
		return a.ident.id < b.ident.id
	end)
	--]]

	--
	-- Rename locals!
	--
	local renamed       = {--[[ [declIdent1]=true, ... ]]}
	-- local collisions = 0

	for _, identInfo in ipairs(identInfos) do
		local declLike = identInfo.ident.declaration

		if declLike then
			local oldName   = identInfo.ident.name
			local declIdent = declLikes_oldNameToIdent[declLike][oldName]

			if not renamed[declIdent] then
				local newName

				for nameGeneration = 1, 1/0 do
					newName         = generateName(nameGeneration)
					local collision = false

					for _, watcherIdent in ipairs(declLikeWatchers[declLike] or EMPTY_TABLE) do
						local watcherDeclLike = watcherIdent.declaration

						if watcherDeclLike then
							for _, watcherDeclIdent in ipairs(getNamesFromDeclarationLike(watcherDeclLike)) do
								if renamed[watcherDeclIdent] and watcherDeclIdent.name == newName then
									collision = true
									break
								end
							end--for names

						elseif watcherIdent.name == newName then
							collision = true
							break
						end
					end--for declLikeWatchers

					if not collision then  break  end

					-- collisions = collisions + 1
				end--for nameGeneration

				declIdent.name     = newName
				renamed[declIdent] = true
			end

			identInfo.ident.name = declIdent.name
		end
	end--for identInfos

	-- print("collisions", collisions) -- DEBUG
end



function printTokens(tokens)
	local tokTypes  = tokens.type
	local tokValues = tokens.value

	for tok = 1, tokens.n do
		local v = tostring(tokValues[tok])
		if #v > 200 then  v = getSubstring(v, 1, 200-3).."..."  end

		v = gsub(v, "\n", "\\n")
		print(F("%d. %-11s '%s'", tok, tokTypes[tok], v))
	end
end



do
	local writeNode
	local writeStatements

	local function canNodeBeName(node)
		return node.type == "literal" and type(node.value) == "string" and node.value:find"^[%a_][%w_]*$" and not KEYWORDS[node.value]
	end

	-- ensureSpaceIfNotPretty( buffer, pretty, lastOutput, value [, value2 ] )
	local function ensureSpaceIfNotPretty(buffer, pretty, lastOutput, value, value2)
		if not pretty and (lastOutput == value or lastOutput == value2) then
			table.insert(buffer, " ")
		end
	end

	local function writeLua(buffer, lua, lastOutput)
		table.insert(buffer, lua)
		return lastOutput
	end

	local function writeAlphanum(buffer, pretty, s, lastOutput)
		ensureSpaceIfNotPretty(buffer, pretty, lastOutput, "alphanum","number")
		lastOutput = writeLua(buffer, s, "alphanum")
		return "alphanum"
	end
	local function writeNumber(buffer, pretty, n, lastOutput)
		ensureSpaceIfNotPretty(buffer, pretty, lastOutput, "alphanum","number")
		lastOutput = writeLua(buffer, formatNumber(n), "number")
		return "number"
	end

	local function writeCommaSeparatedList(buffer, pretty, indent, lastOutput, expressions)
		for i, expr in ipairs(expressions) do
			if i > 1 then
				lastOutput = writeLua(buffer, ",", "")
				if pretty then  lastOutput = writeLua(buffer, " ", "")  end
			end

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, expr, true)
			if not ok then  return false, lastOutput  end
		end

		return true, lastOutput
	end

	local function isStatementFunctionDeclaration(statement, statementNext)
		return true
			and statement.type                == "declaration" and statementNext.type      == "assignment"
			and #statement.names              == 1             and #statement.values       == 0
			and #statementNext.targets        == 1             and #statementNext.values   == 1
			and statementNext.targets[1].type == "identifier"  and statement.names[1].name == statementNext.targets[1].name
			and statementNext.values [1].type == "function"
	end

	local function writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
		if pretty and indent > 0 then
			lastOutput = writeLua(buffer, repeatString("\t", indent), "")
		end
		return lastOutput
	end

	local function writeFunctionParametersAndBody(buffer, pretty, indent, lastOutput, func, explicitParams)
		lastOutput = writeLua(buffer, "(", "")

		local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, explicitParams)
		if not ok then  return false, lastOutput  end

		if func.vararg then
			if explicitParams[1] then
				lastOutput = writeLua(buffer, ",",   "")
				if pretty then  lastOutput = writeLua(buffer, " ", "")  end
			end
			lastOutput = writeLua(buffer, "...", "")
		end

		lastOutput = writeLua(buffer, ")", "")
		if pretty then  lastOutput = writeLua(buffer, "\n", "")  end

		local ok;ok, lastOutput = writeStatements(buffer, pretty, indent+1, lastOutput, func.body.statements)
		if not ok then  return false, lastOutput  end

		lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
		lastOutput = writeAlphanum(buffer, pretty, "end", lastOutput)

		return true, lastOutput
	end

	function writeStatements(buffer, pretty, indent, lastOutput, statements)
		local skipNext = false

		for i, statement in ipairs(statements) do
			if skipNext then
				skipNext = false

			else
				local statementNext = statements[i+1]
				lastOutput          = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)

				if statementNext and isStatementFunctionDeclaration(statement, statementNext) then
					local assignment = statementNext
					local func       = assignment.values[1]

					lastOutput = writeAlphanum(buffer, pretty, "local function", lastOutput)
					lastOutput = writeLua(buffer, " ", "")

					local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, assignment.targets[1], true)
					if not ok then  return false, lastOutput  end

					local ok;ok, lastOutput = writeFunctionParametersAndBody(buffer, pretty, indent, lastOutput, func, func.parameters)
					if not ok then  return false, lastOutput  end

					skipNext = true

				else
					local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, statement, true)
					if not ok then  return false, lastOutput  end

					if statement.type == "call" then
						lastOutput = writeLua(buffer, ";", "") -- @Ugly way of handling call statements. (But what way would be better?)
					end
				end

				if pretty then  lastOutput = writeLua(buffer, "\n", "")  end
			end
		end

		return true, lastOutput
	end

	local function writeLookup(buffer, pretty, indent, lastOutput, lookup, forMethodCall)
		local objIsLiteral = (lookup.object.type == "literal")
		if objIsLiteral then  lastOutput = writeLua(buffer, "(", "")  end

		local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, lookup.object, false)
		if not ok then  return false, lastOutput  end

		if objIsLiteral then  lastOutput = writeLua(buffer, ")", "")  end

		if canNodeBeName(lookup.member) then
			lastOutput = writeLua(buffer, (forMethodCall and ":" or "."), "")
			lastOutput = writeAlphanum(buffer, pretty, lookup.member.value, lastOutput)

		elseif forMethodCall then
			printfError("Error: AST: Callee for method call is not a lookup.")
			return false, lastOutput

		else
			lastOutput = writeLua(buffer, "[", "")

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, lookup.member, true)
			if not ok then  return false, lastOutput  end

			lastOutput = writeLua(buffer, "]", "")
		end

		return true, lastOutput
	end

	local function isAssignmentFunctionAssignment(assignment)
		if #assignment.targets       ~= 1          then  return false  end
		if #assignment.values        ~= 1          then  return false  end
		if assignment.values[1].type ~= "function" then  return false  end

		local targetExpr = assignment.targets[1]
		while true do
			if targetExpr.type == "identifier" then
				return true
			elseif not (targetExpr.type == "lookup" and canNodeBeName(targetExpr.member)) then
				return false
			end
			targetExpr = targetExpr.object
		end
	end

	local function writeBinaryOperatorChain(buffer, pretty, indent, lastOutput, binary)
		local l = binary.left
		local r = binary.right

		if l.type == "binary" and l.operator == binary.operator then
			local ok;ok, lastOutput = writeBinaryOperatorChain(buffer, pretty, indent, lastOutput, l)
			if not ok then  return false, lastOutput  end
		else
			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, l, false)
			if not ok then  return false, lastOutput  end
		end

		if pretty then  lastOutput = writeLua(buffer, " ", "")  end

		if binary.operator == ".." then  ensureSpaceIfNotPretty(buffer, pretty, lastOutput, "number")  end

		local nextOutput = ((binary.operator == "-" and "-") or (binary.operator:find"%w" and "alphanum") or (""))
		if nextOutput ~= "" then  ensureSpaceIfNotPretty(buffer, pretty, lastOutput, nextOutput)  end
		lastOutput = writeLua(buffer, binary.operator, nextOutput)

		if pretty then  lastOutput = writeLua(buffer, " ", "")  end

		if r.type == "binary" and r.operator == binary.operator then
			local ok;ok, lastOutput = writeBinaryOperatorChain(buffer, pretty, indent, lastOutput, r)
			if not ok then  return false, lastOutput  end
		else
			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, r, false)
			if not ok then  return false, lastOutput  end
		end

		return true, lastOutput
	end

	-- success, lastOutput = writeNode( buffer, pretty, indent, lastOutput, node, maySafelyOmitParens )
	-- lastOutput          = "" | "alphanum" | "number" | "-"
	function writeNode(buffer, pretty, indent, lastOutput, node, maySafelyOmitParens)
		local nodeType = node.type

		-- Expressions:

		if nodeType == "identifier" then
			lastOutput = writeAlphanum(buffer, pretty, node.name, lastOutput)

		elseif nodeType == "vararg" then
			if node.adjustToOne then  lastOutput = writeLua(buffer, "(", "")  end
			lastOutput = writeLua(buffer, "...", "")
			if node.adjustToOne then  lastOutput = writeLua(buffer, ")", "")  end

		elseif nodeType == "literal" then
			if node.value == 0 then
				lastOutput = writeNumber(buffer, pretty, 0, lastOutput) -- Avoid writing '-0'. (This rule might mess up round-trip tests in rare cases, I think.)

			elseif node.value == 1/0 then
				lastOutput = writeAlphanum(buffer, pretty, "(1/0)", lastOutput) -- Note: We parse this as one literal.

			elseif node.value == -1/0 then
				lastOutput = writeLua(buffer, "(-1/0)", "") -- Note: We parse this as one literal.

			elseif node.value ~= node.value then
				lastOutput = writeLua(buffer, "(0/0)", "") -- Note: We parse this as one literal.

			elseif node.value == nil or type(node.value) == "boolean" then
				lastOutput = writeAlphanum(buffer, pretty, tostring(node.value), lastOutput)

			elseif type(node.value) == "number" then
				lastOutput = writeNumber(buffer, pretty, node.value, lastOutput)

			elseif type(node.value) == "string" then
				local s = node.value

				s = s:gsub("(.)()", function(c, nextPos)
					-- Note: We assume the string is UTF-8, but nothing should
					-- break if it isn't - we should still get valid Lua code.
					local b = getByte(c)
					if     c == "\a" then  return [[\a]]
					elseif c == "\b" then  return [[\b]]
					elseif c == "\f" then  return [[\f]]
					elseif c == "\n" then  return [[\n]]
					elseif c == "\r" then  return [[\r]]
					elseif c == "\t" then  return [[\t]]
					elseif c == "\v" then  return [[\v]]
					elseif c == "\\" then  return [[\\]]
					elseif c == "\"" then  return [[\"]]
					elseif b == 127  then  return [[\127]]
					elseif b <= 31   then
						local nextByte = getByte(s, nextPos) or 0
						return nextByte >= 48 and nextByte <= 57 and F([[\%03d]], b) or F([[\%d]], b)
					end
				end)

				lastOutput = writeLua(buffer, F('"%s"', s), "")

			else
				printfError("Error: Failed outputting value '%s'.", node.value)
				return false, lastOutput
			end

		elseif nodeType == "table" then
			lastOutput = writeLua(buffer, "{", "")

			for i, field in ipairs(node.fields) do
				if i > 1 then
					lastOutput = writeLua(buffer, ",", "")
					if pretty then  lastOutput = writeLua(buffer, " ", "")  end
				end

				if not field.generatedKey then
					if canNodeBeName(field.key) then
						lastOutput = writeLua(buffer, field.key.value, "alphanum")

					else
						lastOutput = writeLua(buffer, "[", "")

						local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, field.key, true)
						if not ok then  return false, lastOutput  end

						lastOutput = writeLua(buffer, "]", "")
					end

					lastOutput = writeLua(buffer, "=", "")
				end

				local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, field.value, (not pretty))
				if not ok then  return false, lastOutput  end
			end

			lastOutput = writeLua(buffer, "}", "")

		elseif nodeType == "lookup" then
			local ok;ok, lastOutput = writeLookup(buffer, pretty, indent, lastOutput, node, false)
			if not ok then  return false, lastOutput  end

		elseif nodeType == "unary" then
			local operatorOutput    = ((node.operator == "-" and "-") or (node.operator:find"%w" and "alphanum") or (""))
			local prettyAndAlphanum = pretty and operatorOutput == "alphanum"

			if prettyAndAlphanum and not maySafelyOmitParens then  lastOutput = writeLua(buffer, "(", "")  end -- @Polish: Only output parentheses around child unaries/binaries if associativity requires it.

			if operatorOutput ~= "" then  ensureSpaceIfNotPretty(buffer, pretty, lastOutput, operatorOutput)  end
			lastOutput = writeLua(buffer, node.operator, operatorOutput)

			if prettyAndAlphanum then  lastOutput = writeLua(buffer, " ", "")  end

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.expression, false)
			if not ok then  return false, lastOutput  end

			if prettyAndAlphanum and not maySafelyOmitParens then  lastOutput = writeLua(buffer, ")", "")  end

		elseif nodeType == "binary" then
			if not maySafelyOmitParens then  lastOutput = writeLua(buffer, "(", "")  end -- @Polish: Only output parentheses around child unaries/binaries if associativity requires it.

			if node.operator == ".." or node.operator == "and" or node.operator == "or" then
				local ok;ok, lastOutput = writeBinaryOperatorChain(buffer, pretty, indent, lastOutput, node)
				if not ok then  return false, lastOutput  end

			else
				local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.left, false)
				if not ok then  return false, lastOutput  end

				local operatorOutput = ((node.operator == "-" and "-") or (node.operator:find"%w" and "alphanum") or (""))

				if pretty then  lastOutput = writeLua(buffer, " ", "")  end

				if operatorOutput ~= "" then  ensureSpaceIfNotPretty(buffer, pretty, lastOutput, operatorOutput)  end
				lastOutput = writeLua(buffer, node.operator, operatorOutput)

				if pretty then  lastOutput = writeLua(buffer, " ", "")  end

				local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.right, false)
				if not ok then  return false, lastOutput  end
			end

			if not maySafelyOmitParens then  lastOutput = writeLua(buffer, ")", "")  end

		elseif nodeType == "call" then
			if node.adjustToOne then  lastOutput = writeLua(buffer, "(", "")  end

			if node.method then
				local lookup = node.callee

				if lookup.type ~= "lookup" then
					printfError("Error: AST: Callee for method call is not a lookup.")
					return false, lastOutput
				end

				local ok;ok, lastOutput = writeLookup(buffer, pretty, indent, lastOutput, lookup, true)
				if not ok then  return false, lastOutput  end

			else
				local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.callee, false)
				if not ok then  return false, lastOutput  end
			end

			lastOutput = writeLua(buffer, "(", "")

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, node.arguments)
			if not ok then  return false, lastOutput  end

			lastOutput = writeLua(buffer, ")", "")
			if node.adjustToOne then  lastOutput = writeLua(buffer, ")", "")  end

		elseif nodeType == "function" then
			lastOutput = writeAlphanum(buffer, pretty, "function", lastOutput)

			local ok;ok, lastOutput = writeFunctionParametersAndBody(buffer, pretty, indent, lastOutput, node, node.parameters)
			if not ok then  return false, lastOutput  end

		-- Statements:

		elseif nodeType == "break" then
			lastOutput = writeAlphanum(buffer, pretty, "break", lastOutput)
			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "return" then
			lastOutput = writeAlphanum(buffer, pretty, "return", lastOutput)

			if node.values[1] then
				if pretty then  lastOutput = writeLua(buffer, " ", "")  end

				local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, node.values)
				if not ok then  return false, lastOutput  end
			end

			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "label" then
			local name = node.name.name
			if not (name:find"^[%a_][%w_]*$" and not KEYWORDS[name]) then
				printfError("Error: AST: Invalid label '%s'.", name)
				return false, lastOutput
			end
			lastOutput = writeLua(buffer, "::", "")
			lastOutput = writeAlphanum(buffer, pretty, name, lastOutput)
			lastOutput = writeLua(buffer, "::", "")
			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "goto" then
			local name = node.name.name
			if not (name:find"^[%a_][%w_]*$" and not KEYWORDS[name]) then
				printfError("Error: AST: Invalid label '%s'.", name)
				return false, lastOutput
			end
			lastOutput = writeAlphanum(buffer, pretty, "goto", lastOutput)
			lastOutput = writeLua(buffer, " ", "")
			lastOutput = writeAlphanum(buffer, pretty, name,   lastOutput)
			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "block" then
			lastOutput = writeAlphanum(buffer, pretty, "do", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, "\n", "")  end

			local ok;ok, lastOutput = writeStatements(buffer, pretty, indent+1, lastOutput, node.statements)
			if not ok then  return false, lastOutput  end

			lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
			lastOutput = writeAlphanum(buffer, pretty, "end", lastOutput)

		elseif nodeType == "declaration" then
			lastOutput = writeAlphanum(buffer, pretty, "local", lastOutput)
			lastOutput = writeLua(buffer, " ", "")

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, node.names)
			if not ok then  return false, lastOutput  end

			if node.values[1] then
				if pretty then  lastOutput = writeLua(buffer, " ", "")  end
				lastOutput = writeLua(buffer, "=", "")
				if pretty then  lastOutput = writeLua(buffer, " ", "")  end

				local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, node.values)
				if not ok then  return false, lastOutput  end
			end

			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "assignment" then
			if isAssignmentFunctionAssignment(node) then
				local func = node.values[1]
				lastOutput = writeAlphanum(buffer, pretty, "function", lastOutput)
				lastOutput = writeLua(buffer, " ", "")

				local implicitSelfParam
					=   #func.parameters >= 1
					and func.parameters[1].name == "self"
					and node.targets[1].type == "lookup"
					and canNodeBeName(node.targets[1].member)

				if implicitSelfParam then
					local ok;ok, lastOutput = writeLookup(buffer, pretty, indent, lastOutput, node.targets[1], true)
					if not ok then  return false, lastOutput  end
				else
					local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.targets[1], false)
					if not ok then  return false, lastOutput  end
				end

				local explicitParams = func.parameters
				if implicitSelfParam then  explicitParams = {unpack(explicitParams, 2)}  end

				local ok;ok, lastOutput = writeFunctionParametersAndBody(buffer, pretty, indent, lastOutput, func, explicitParams)
				if not ok then  return false, lastOutput  end

			else
				local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, node.targets)
				if not ok then  return false, lastOutput  end

				if pretty then  lastOutput = writeLua(buffer, " ", "")  end
				lastOutput = writeLua(buffer, "=", "")
				if pretty then  lastOutput = writeLua(buffer, " ", "")  end

				local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, node.values)
				if not ok then  return false, lastOutput  end

				lastOutput = writeLua(buffer, ";", "")
			end

		elseif nodeType == "if" then
			lastOutput = writeAlphanum(buffer, pretty, "if", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, " ", "")  end

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.condition, true)
			if not ok then  return false, lastOutput  end

			if pretty then  lastOutput = writeLua(buffer, " ", "")  end
			lastOutput = writeAlphanum(buffer, pretty, "then", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, "\n", "")  end

			local ok;ok, lastOutput = writeStatements(buffer, pretty, indent+1, lastOutput, node.bodyTrue.statements)
			if not ok then  return false, lastOutput  end

			while node.bodyFalse do
				-- Automatically detect what looks like 'elseif'.
				if #node.bodyFalse.statements == 1 and node.bodyFalse.statements[1].type == "if" then
					node = node.bodyFalse.statements[1]

					lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
					lastOutput = writeAlphanum(buffer, pretty, "elseif", lastOutput)
					if pretty then  lastOutput = writeLua(buffer, " ", "")  end

					local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.condition, true)
					if not ok then  return false, lastOutput  end

					if pretty then  lastOutput = writeLua(buffer, " ", "")  end
					lastOutput = writeAlphanum(buffer, pretty, "then", lastOutput)
					if pretty then  lastOutput = writeLua(buffer, "\n", "")  end

					local ok;ok, lastOutput = writeStatements(buffer, pretty, indent+1, lastOutput, node.bodyTrue.statements)
					if not ok then  return false, lastOutput  end

				else
					lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
					lastOutput = writeAlphanum(buffer, pretty, "else", lastOutput)
					if pretty then  lastOutput = writeLua(buffer, "\n", "")  end

					local ok;ok, lastOutput = writeStatements(buffer, pretty, indent+1, lastOutput, node.bodyFalse.statements)
					if not ok then  return false, lastOutput  end

					break
				end
			end

			lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
			lastOutput = writeAlphanum(buffer, pretty, "end", lastOutput)

		elseif nodeType == "while" then
			lastOutput = writeAlphanum(buffer, pretty, "while", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, " ", "")  end

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.condition, true)
			if not ok then  return false, lastOutput  end

			if pretty then  lastOutput = writeLua(buffer, " ", "")  end
			lastOutput = writeAlphanum(buffer, pretty, "do", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, "\n", "")  end

			local ok;ok, lastOutput = writeStatements(buffer, pretty, indent+1, lastOutput, node.body.statements)
			if not ok then  return false, lastOutput  end

			lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
			lastOutput = writeAlphanum(buffer, pretty, "end", lastOutput)

		elseif nodeType == "repeat" then
			lastOutput = writeAlphanum(buffer, pretty, "repeat", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, "\n", "")  end

			local ok;ok, lastOutput = writeStatements(buffer, pretty, indent+1, lastOutput, node.body.statements)
			if not ok then  return false, lastOutput  end

			lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
			lastOutput = writeAlphanum(buffer, pretty, "until", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, " ", "")  end

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.condition, true)
			if not ok then  return false, lastOutput  end

		elseif nodeType == "for" then
			lastOutput = writeAlphanum(buffer, pretty, "for", lastOutput)
			lastOutput = writeLua(buffer, " ", "")

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, node.names)
			if not ok then  return false, lastOutput  end

			if pretty then  lastOutput = writeLua(buffer, " ", "")  end

			if node.kind == "numeric" then
				lastOutput = writeLua(buffer, "=", "")

			elseif node.kind == "generic" then
				lastOutput = writeAlphanum(buffer, pretty, "in", lastOutput)

			else
				printfError("Error: Unknown 'for' loop kind '%s'.", node.kind)
				return false, lastOutput
			end

			if pretty then  lastOutput = writeLua(buffer, " ", "")  end

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, node.values)
			if not ok then  return false, lastOutput  end

			if pretty then  lastOutput = writeLua(buffer, " ", "")  end
			lastOutput = writeAlphanum(buffer, pretty, "do", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, "\n", "")  end

			local ok;ok, lastOutput = writeStatements(buffer, pretty, indent+1, lastOutput, node.body.statements)
			if not ok then  return false, lastOutput  end

			lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
			lastOutput = writeAlphanum(buffer, pretty, "end", lastOutput)

		else
			printfError("Error: Unknown node type '%s'.", tostring(nodeType))
			return false, lastOutput
		end
		return true, lastOutput
	end

	-- lua = toLua( astNode [, prettyOuput=false ] )
	-- Returns nil on error.
	function toLua(node, pretty)
		assertArg("toLua", 1, node, "table")

		local buffer = {}

		local ok
		if node.type == "block" then -- @Robustness: This exception isn't great. Should there be a file scope node?
			ok = writeStatements(buffer, pretty, 0, "", node.statements)
		else
			ok = writeNode(buffer, pretty, 0, "", node, true)
		end

		return ok and table.concat(buffer) or nil
	end
end



-- item, index = itemWith1( array, key, value )
function itemWith1(t, k, v)
	for i, item in ipairs(t) do
		if item[k] == v then  return item, i  end
	end
	return nil
end



do
	local lastId = 0

	function newId()
		lastId = lastId+1
		return lastId
	end
end



function assertArg(funcName, argNum, v, expectedType)
	if type(v) == expectedType then  return  end
	error(F("bad argument #%d to '%s' (%s expected, got %s)", argNum, funcName, expectedType, type(v)), 3)
end



function formatNumber(n)
	return (tostring(n)
		:gsub("(e[-+])0+(%d+)$", "%1%2") -- Remove unnecessary zeroes after 'e'.
		:gsub("e%+",             "e"   ) -- Remove plus after 'e'.
	)
end



parser = {
	-- Constants.
	VERSION          = PARSER_VERSION,

	-- Functions.
	tokenizeString   = tokenizeString,
	tokenizeFile     = tokenizeFile,

	newTokenStream   = newTokenStream,
	insertToken      = insertToken,
	removeToken      = removeToken,

	parse            = parse,
	newNode          = newNode,
	traverseTree     = traverseTree,
	updateReferences = updateReferences,
	minify           = minify,
	toLua            = toLua,

	printTokens      = printTokens,
	printNode        = printNode,
	printTree        = printTree,

	-- Settings.
	printIds = false, -- @Undocumented
}

return parser



--[=[===========================================================

Copyright © 2020-2021 Marcus 'ReFreezed' Thunström

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

=============================================================]=]
