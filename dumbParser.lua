--[[============================================================
--=
--=  Dumb Lua parsing library v0.1
--=  by Marcus 'ReFreezed' Thunström
--=
--=  Supported versions: 5.1
--=
--=  @Incomplete: Support Lua 5.2+.
--=
--==============================================================

	-- Functions:
	parse
	printNode, printTree
	tokenizeFile, tokenizeString
	toLua

--============================================================]]

local F       = string.format
local find    = string.find
local getByte = string.byte
local gsub    = string.gsub
local match   = string.match
local sub     = string.sub

local countString
local isToken
local isTokenAnyValue
local isTokenType
local parse
local parseBlock
local parseExpression
local parseExpressionList
local parseFunctionParametersAndBody
local parseIdentifier
local parseNameList
local parseOneOrPossiblyMoreStatements
local parseStringlikeToken
local parseTable
local printerr
local printNode
local printTree
local reportErrorAtToken
local reportErrorInFile
local tokenizeFile
local tokenizeString
local toLua



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
local OPERATOR_PRECEDENCE = {
	["or"]  = 1,
	["and"] = 2,
	["<"]   = 3, [">"] = 3, ["<="] = 3, [">="] = 3, ["~="] = 3, ["=="] = 3,
	[".."]  = 4,
	["+"]   = 5, ["-"] = 5,
	["*"]   = 6, ["/"] = 6, ["//"] = 6, ["%"] = 6,
	unary   = 7, -- "-", "#", "not"
	["^"]   = 8,
}

local NUM_HEX_FRAC_EXP = gsub("^( 0[Xx] ([%dA-Fa-f]*) %.([%dA-Fa-f]+) [Pp]([-+]?[%dA-Fa-f]+) )", " +", "")
local NUM_HEX_FRAC     = gsub("^( 0[Xx] ([%dA-Fa-f]*) %.([%dA-Fa-f]+)                        )", " +", "")
local NUM_HEX_EXP      = gsub("^( 0[Xx] ([%dA-Fa-f]+) %.?             [Pp]([-+]?[%dA-Fa-f]+) )", " +", "")
local NUM_HEX          = gsub("^( 0[Xx]  [%dA-Fa-f]+  %.?                                    )", " +", "")
local NUM_DEC_FRAC_EXP = gsub("^(        %d*          %.%d+           [Ee][-+]?%d+           )", " +", "")
local NUM_DEC_FRAC     = gsub("^(        %d*          %.%d+                                  )", " +", "")
local NUM_DEC_EXP      = gsub("^(        %d+          %.?             [Ee][-+]?%d+           )", " +", "")
local NUM_DEC          = gsub("^(        %d+          %.?                                    )", " +", "")

local ERROR_UNFINISHED_VALUE = {}



-- AST expressions.
local function AstIdentifier(tok) return {
	type  = "identifier",
	token = tok,
	name  = "",
} end
local function AstVararg(tok) return {
	type        = "vararg",
	token       = tok,
	adjustToOne = false, -- True if parentheses surround the vararg.
} end
local function AstLiteral(tok) return {
	type  = "literal",
	token = tok,
	value = nil, -- Number, string, boolean or nil.
} end
local function AstTable(tok) return {
	type   = "table",
	token  = tok,
	fields = {}, -- Array of {key=expression, value=expression, generatedKey=bool}.
} end
local function AstLookup(tok) return {
	type   = "lookup",
	token  = tok,
	object = nil, -- Expression.
	member = nil, -- Expression.
} end
local function AstUnary(tok) return {
	type       = "unary",
	token      = tok,
	operator   = "", -- "-"|"#"|"not"
	expression = nil,
} end
local function AstBinary(tok) return {
	type     = "binary",
	token    = tok,
	operator = "",  -- "+"|"-"|"*"|"/"|"//"|"^"|"%"|".."|"<"|"<="|">"|">="|"=="|"~="|"and"|"or"
	left     = nil, -- Expression.
	right    = nil, -- Expression.
} end
local function AstCall(tok) return {
	type        = "call",
	token       = tok,
	callee      = nil, -- Expression.
	arguments   = {},  -- Array of expressions.
	method      = false,
	adjustToOne = false, -- True if parentheses surround the call.
} end
local function AstFunction(tok) return {
	type       = "function",
	token      = tok,
	parameters = {},  -- Array of AstIdentifier.
	body       = nil, -- AstBlock.
	vararg     = false,
	method     = false,
} end

-- AST statements.
local function AstBreak(tok) return {
	type  = "break",
	token = tok,
} end
local function AstReturn(tok) return {
	type   = "return",
	token  = tok,
	values = {}, -- Array of expressions.
} end
local function AstBlock(tok) return {
	type       = "block",
	token      = tok,
	statements = {}, -- Array of statements.
} end
local function AstDeclaration(tok) return {
	type   = "declaration",
	token  = tok,
	names  = {}, -- Array of AstIdentifier.
	values = {}, -- Array of expressions.
} end
local function AstAssignment(tok) return {
	type    = "assignment",
	token   = tok,
	targets = {}, -- Mixed array of AstIdentifier and AstLookup.
	values  = {}, -- Array of expressions.
} end
local function AstIf(tok) return {
	type      = "if",
	token     = tok,
	condition = nil, -- Expression.
	bodyTrue  = nil, -- AstBlock.
	bodyFalse = nil, -- AstBlock. May be nil.
} end
local function AstWhile(tok) return {
	type      = "while",
	token     = tok,
	condition = nil, -- Expression.
	body      = nil, -- AstBlock.
} end
local function AstRepeat(tok) return {
	type      = "repeat",
	token     = tok,
	body      = nil, -- AstBlock.
	condition = nil, -- Expression.
} end
local function AstFor(tok) return {
	type   = "for",
	token  = tok,
	kind   = "",  -- "numeric"|"generic"
	names  = {},  -- Array of AstIdentifier.
	values = {},  -- Array of expressions.
	body   = nil, -- AstBlock.
} end



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

function printerr(s)
	io.stderr:write(s, "\n")
end

function reportErrorInFile(contents, path, ptr, agent, s, ...)
	s = F(s, ...)

	local pre       = contents:sub(1, ptr-1)

	local lastLine1 = pre:reverse():match"^[^\n]*":reverse():gsub("\t", "    ")
	local lastLine2 = contents:match("^[^\n]*", ptr):gsub("\t", "    ")
	local lastLine  = lastLine1..lastLine2

	local ln        = countString(pre, "\n", true) + 1
	local col       = #lastLine1 + 1

	-- print(debug.traceback("", 2)) -- DEBUG

	if agent then
		printerr(F(
			"Error @ %s:%d: [%s] %s\n>\n> %s\n>%s^\n>\n",
			path, ln, agent, s, lastLine, ("-"):rep(col)
		))
	else
		printerr(F(
			"Error @ %s:%d: %s\n>\n> %s\n> %s^\n>\n",
			path, ln,        s, lastLine, ("-"):rep(col-1)
		))
	end

	return s
end

function reportErrorAtToken(tokens, tok, agent, s, ...)
	reportErrorInFile(tokens.sourceString, tokens.sourcePath, tokens.positionStart[tok], agent, s, ...)
end



-- success, equalSignCountIfLong|errorCode, ptr = parseStringlikeToken( s, ptr )
function parseStringlikeToken(s, ptr)
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

-- tokens, error = tokenizeString( luaString, pathForError )
function tokenizeString(s, path)
	if find(s, "\r", 1, true) then
		s = gsub(s, "\r\n?", "\n")
	end

	local tokTypes  = {}
	local tokValues = {} -- @Incomplete: The resulting values.
	local tokReprs  = {}
	local tokLine1  = {}
	local tokLine2  = {}
	local tokPos1   = {}
	local tokPos2   = {}

	local tokens = {
		n            = 0, -- Token count.
		sourceString = s,
		sourcePath   = path,

		type           = tokTypes,
		value          = tokValues,
		representation = tokReprs,
		lineStart      = tokLine1,
		lineEnd        = tokLine2,
		positionStart  = tokPos1,
		positionEnd    = tokPos2,
	}

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
		local tokType

		-- Identifier/keyword.
		if find(s, "^[%a_]", ptr) then
			local i1, i2, word = find(s, "^([%a_][%w_]*)", ptr)
			ptr                = i2+1
			tokType            = KEYWORDS[word] and "keyword" or "identifier"

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
			if not numStr then  return nil, reportErrorInFile(s, path, ptr, "Tokenizer", "Malformed number.")
			end end end end end end end end

			ptr     = i2+1
			tokType = "number"

			if find(s, "^%.", ptr) then
				return nil, reportErrorInFile(s, path, ptr, "Tokenizer", "Malformed number.")
			end

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

			tokType = "comment"

		-- String (short).
		elseif find(s, "^[\"']", ptr) then
			local quoteChar = sub(s, ptr, ptr)
			ptr = ptr + 1

			while true do
				local c = sub(s, ptr, ptr)

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

		-- Punctuation.
		elseif find(s, "^%.%.%.", ptr) then
			ptr     = ptr + 3
			tokType = "punctuation"
		elseif find(s, "^%.%.", ptr) or find(s, "^[=~<>]=", ptr) or find(s, "^::", ptr) or find(s, "^//", ptr) or find(s, "^<<", ptr) or find(s, "^>>", ptr) then
			ptr     = ptr + 2
			tokType = "punctuation"
		elseif find(s, "^[+%-*/%%^#<>=(){}[%];:,.]", ptr) then
			ptr     = ptr + 1
			tokType = "punctuation"

		else
			return nil, reportErrorInFile(s, path, ptr, "Tokenizer", "Unknown character.")
		end

		if not tokType then
			return nil, reportErrorInFile(s, path, ptrStart, "Tokenizer", "Internal error: Got no token type.")
		end

		local tokRepr = sub(s, ptrStart, ptr-1)
		ln            = ln + countString(tokRepr, "\n", true)

		count           = count + 1
		tokTypes[count] = tokType
		tokReprs[count] = tokRepr
		tokLine1[count] = lnStart
		tokLine2[count] = ln
		tokPos1 [count] = ptrStart
		tokPos2 [count] = ptr - 1

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



--[=[
	chunk ::= {stat [';']} [laststat [';']]

	block ::= chunk

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



	funcname ::= Name {'.' Name} [':' Name]

	varlist ::= var {',' var}

	var ::= Name | prefixexp '[' exp ']' | prefixexp '.' Name

	namelist ::= Name {',' Name}

	explist ::= {exp ','} exp

	exp ::= nil | false | true | Number | String | '...' | function |
	        prefixexp | tableconstructor | exp binop exp | unop exp

	prefixexp ::= var | functioncall | '(' exp ')'

	functioncall ::= prefixexp args | prefixexp ':' Name args

	args ::= '(' [explist] ')' | tableconstructor | String

	function ::= function funcbody

	funcbody ::= '(' [parlist] ')' block end

	parlist ::= namelist [',' '...'] | '...'

	tableconstructor ::= '{' [fieldlist] '}'

	fieldlist ::= field {fieldsep field} [fieldsep]

	field ::= '[' exp ']' '=' exp | Name '=' exp | exp

	fieldsep ::= ',' | ';'

	binop ::= '+' | '-' | '*' | '/' | '^' | '%' | '..' |
	          '<' | '<=' | '>' | '>=' | '==' | '~=' |
	          and | or

	unop ::= '-' | not | '#'
]=]

function isToken(tokens, tok, tokType, tokValue)
	return tokens.type[tok] == tokType and tokens.representation[tok] == tokValue -- @Incomplete: Use token value.
end
function isTokenType(tokens, tok, tokType)
	return tokens.type[tok] == tokType
end
function isTokenAnyValue(tokens, tok, tokValueSet)
	local tokValue = tokens.representation[tok] -- @Incomplete: Use token value.
	return tokValueSet[tokValue] == true
end

function parseIdentifier(tokens, tok) --> ident, token
	if not isTokenType(tokens, tok, "identifier") then
		reportErrorAtToken(tokens, tok, "Parser", "Expected an identifier.")
		return nil, tok
	end

	local ident = AstIdentifier(tok)
	ident.name  = tokens.representation[tok] -- @Incomplete: Use token value.
	tok         = tok + 1

	return ident, tok
end

function parseNameList(tokens, tok, names, allowVararg) --> success, token
	while true do
		if allowVararg and isToken(tokens, tok, "punctuation", "...") then
			local vararg = AstVararg(tok)
			tok          = tok + 1 -- '...'
			table.insert(names, vararg)
			break
		end

		local ident, tokNext = parseIdentifier(tokens, tok)
		if not ident then  return false, tok  end
		tok = tokNext

		table.insert(names, ident)

		if not isToken(tokens, tok, "punctuation", ",") then
			break
		end
		tok = tok + 1 -- ','
	end

	return true, tok
end

function parseTable(tokens, tok) --> tableNode, token
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
			keyExpr.value = tokens.representation[tok] -- @Incomplete: Use token value.
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

local binaryTokenTypes = {
	["<"]   = true, [">"] = true, ["<="] = true, [">="] = true, ["~="] = true, ["=="] = true,
	[".."]  = true,
	["+"]   = true, ["-"] = true,
	["*"]   = true, ["/"] = true, ["//"] = true, ["%"] = true,
	["^"]   = true,
}

function parseExpression(tokens, tok, lastPrecedence) --> expression, token
	local expr

	-- identifier
	if isTokenType(tokens, tok, "identifier") then
		local ident, tokNext = parseIdentifier(tokens, tok)
		if not ident then  return false, tok  end
		tok = tokNext

		expr = ident

	-- ...
	elseif isToken(tokens, tok, "punctuation", "...") then
		local vararg = AstVararg(tok)
		tok          = tok + 1 -- '...'
		expr         = vararg

	-- literal
	elseif isTokenType(tokens, tok, "string") or isTokenType(tokens, tok, "number") then
		local literal = AstLiteral(tok)
		literal.value = assert(loadstring("return "..tokens.representation[tok]))()
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
		(isToken(tokens, tok, "keyword", "not") or isToken(tokens, tok, "punctuation", "-") or isToken(tokens, tok, "punctuation", "#"))
		and OPERATOR_PRECEDENCE.unary > lastPrecedence
	then
		local unary    = AstUnary(tok)
		unary.operator = tokens.representation[tok] -- @Incomplete: Use token value.
		tok            = tok + 1 -- operator

		local subExpr, tokNext = parseExpression(tokens, tok, OPERATOR_PRECEDENCE.unary)
		if not subExpr then  return false, tok  end
		unary.expression = subExpr
		tok              = tokNext

		expr = unary

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

		expr = _expr

	else
		reportErrorAtToken(tokens, tok, "Parser", "Failed parsing expression.")
		return false, tok
	end

	assert(expr)

	-- Binary expressions etc.
	while true do
		-- a + b
		if
			(
				(isTokenType(tokens, tok, "punctuation") and isTokenAnyValue(tokens, tok, binaryTokenTypes))
				or isToken(tokens, tok, "keyword", "and")
				or isToken(tokens, tok, "keyword", "or")
			)
			and OPERATOR_PRECEDENCE[tokens.representation[tok]] > lastPrecedence -- @Incomplete: Use token value.
		then
			local rightAssociative = isToken(tokens, tok, "punctuation", "..") or isToken(tokens, tok, "punctuation", "^")

			local binary    = AstBinary(tok)
			binary.operator = tokens.representation[tok] -- @Incomplete: Use token value.
			tok             = tok + 1 -- operator

			local rhsExpr, tokNext = parseExpression(tokens, tok, OPERATOR_PRECEDENCE[binary.operator] + (rightAssociative and -1 or 0))
			if not rhsExpr then  return false, tok  end
			tok = tokNext

			binary.left  = expr
			binary.right = rhsExpr

			expr = binary

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
			literal.value     = assert(loadstring("return "..tokens.representation[tok]))()
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
					literal.value     = assert(loadstring("return "..tokens.representation[tok]))()
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
		local ok, tokNext = parseNameList(tokens, tok, func.parameters, true)
		if not ok then  return nil, tok  end
		tok = tokNext
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

local blockEndTokenTypes = {["end"]=true, ["else"]=true, ["elseif"]=true, ["until"]=true}

function parseOneOrPossiblyMoreStatements(tokens, tok, statements) --> success, token
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
			func.method = true

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

	-- return (last)
	elseif isToken(tokens, tok, "keyword", "return") then
		local returnNode = AstReturn(tok)
		tok              = tok + 1 -- 'return'

		if tok <= tokens.n and not (isTokenType(tokens, tok, "keyword") and isTokenAnyValue(tokens, tok, blockEndTokenTypes)) then
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
		if stopAtEndKeyword and isTokenType(tokens, tok, "keyword") and isTokenAnyValue(tokens, tok, blockEndTokenTypes) then
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

		local lastStatement = statements[#statements]

		if lastStatement.type == "return" or lastStatement.type == "break" then
			break

		elseif lastStatement.type == "call" and lastStatement.adjustToOne then
			reportErrorAtToken(tokens, tok, "Parser", "Syntax error.")
			return nil, tok
		end

		if isToken(tokens, tok, "punctuation", ";") then
			tok = tok + 1 -- ';'
		end
	end

	return block, tok
end

-- ast, error = parse( tokens )
-- ast, error = parse( luaString, pathForError )
-- ast, error = parse( path )
function parse(tokens, path)
	if type(tokens) == "string" then
		local err

		if path then
			tokens, err = tokenizeString(tokens, path)
		else
			path = tokens
			local err
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



do
	local ioWrite = io.write

	local function _printNode(node)
		local nodeType = node.type
		ioWrite(nodeType)

		if nodeType == "identifier" then
			ioWrite(" (", node.name, ")")

		elseif nodeType == "vararg" then
			if node.adjustToOne then  ioWrite(" (adjustToOne)")  end

		elseif nodeType == "literal" then
			if node.value == nil or node.value == true or node.value == false then
				ioWrite(" (", tostring(node.value), ")")
			elseif type(node.value) == "string" then
				ioWrite(" (string='", node.value:gsub("\n","\\n"), "')")
			else
				ioWrite(" (", type(node.value), "=", tostring(node.value):gsub("\n","\\n"), ")")
			end

		elseif nodeType == "unary" then
			ioWrite(" (", node.operator, ")")

		elseif nodeType == "binary" then
			ioWrite(" (", node.operator, ")")

		elseif nodeType == "call" then
			if node.method      then  ioWrite(" (method)"     )  end
			if node.adjustToOne then  ioWrite(" (adjustToOne)")  end

		elseif nodeType == "function" then
			if node.method then  ioWrite(" (method)")  end
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
			ioWrite(("    "):rep(indent), node.operator, "\n")
			if node.right then  _printTree(node.right, indent, nil)  end

		elseif nodeType == "call" then
			if node.callee then  _printTree(node.callee, indent, "callee")  end
			for i, expr in ipairs(node.arguments) do  _printTree(expr, indent, "ARG"..i)  end

		elseif nodeType == "function" then
			for i, ident in ipairs(node.parameters) do  _printTree(ident, indent, "PARAM"..i)  end
			if node.body then  _printTree(node.body, indent, "body")  end

		elseif nodeType == "return" then
			for i, expr in ipairs(node.values) do  _printTree(expr, indent, nil)  end

		elseif nodeType == "block" then
			for i, statement in ipairs(node.statements) do  _printTree(statement, indent, nil)  end

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



do
	local writeNode

	local function canNodeBeName(node)
		return node.type == "literal" and type(node.value) == "string" and node.value:find"^[%a_][%w_]*$" and not KEYWORDS[node.value]
	end

	-- ensureSpace( buffer, lastOutput, value [, value2 ] )
	local function ensureSpace(buffer, lastOutput, value, value2)
		if lastOutput == value or lastOutput == value2 then
			table.insert(buffer, " ")
		end
	end

	local function writeLua(buffer, lua, lastOutput)
		table.insert(buffer, lua)
		return lastOutput
	end

	local function writeAlphanum(buffer, s, lastOutput)
		ensureSpace(buffer, lastOutput, "alphanum","number")
		lastOutput = writeLua(buffer, s, "alphanum")
		return "alphanum"
	end
	local function writeNumber(buffer, n, lastOutput)
		ensureSpace(buffer, lastOutput, "alphanum","number")
		lastOutput = writeLua(buffer, tostring(n), "number")
		return "number"
	end

	local function writeCommaSeparatedList(buffer, pretty, lastOutput, expressions)
		for i, expr in ipairs(expressions) do
			if i > 1 then  lastOutput = writeLua(buffer, ",", "")  end

			local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, expr, true)
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

	local function writeStatements(buffer, pretty, lastOutput, statements)
		local skipNext = false

		for i, statement in ipairs(statements) do
			local statementNext = statements[i+1]

			if skipNext then
				skipNext = false

			elseif statementNext and isStatementFunctionDeclaration(statement, statementNext) then
				local assignment = statementNext
				local func       = assignment.values[1]

				lastOutput = writeAlphanum(buffer, "local function", lastOutput)

				local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, assignment.targets[1], true)
				if not ok then  return false, lastOutput  end

				lastOutput = writeLua(buffer, "(", "")

				local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, lastOutput, func.parameters)
				if not ok then  return false, lastOutput  end

				lastOutput = writeLua(buffer, ")", "")

				local ok;ok, lastOutput = writeStatements(buffer, pretty, lastOutput, func.body.statements)
				if not ok then  return false, lastOutput  end

				lastOutput = writeAlphanum(buffer, "end", lastOutput)
				skipNext   = true

			else
				local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, statement, true)
				if not ok then  return false, lastOutput  end

				if statement.type == "call" then
					lastOutput = writeLua(buffer, ";", "")
				end

				if pretty then
					lastOutput = writeLua(buffer, "\n", "") -- DEBUG
				end
			end
		end

		return true, lastOutput
	end

	local function writeLookup(buffer, pretty, lastOutput, lookup, forMethodCall)
		local objIsLiteral = (lookup.object.type == "literal")
		if objIsLiteral then  lastOutput = writeLua(buffer, "(", "")  end

		local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, lookup.object, false)
		if not ok then  return false, lastOutput  end

		if objIsLiteral then  lastOutput = writeLua(buffer, ")", "")  end

		if canNodeBeName(lookup.member) then
			lastOutput = writeLua(buffer, (forMethodCall and ":" or "."), "")
			lastOutput = writeAlphanum(buffer, lookup.member.value, lastOutput)

		elseif forMethodCall then
			printerr(F("AST: Callee for method call is not a lookup."))
			return false, lastOutput

		else
			lastOutput = writeLua(buffer, "[", "")

			local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, lookup.member, true)
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

	local function writeBinaryOperatorChain(buffer, pretty, lastOutput, binary)
		local l = binary.left
		local r = binary.right

		if l.type == "binary" and l.operator == binary.operator then
			local ok;ok, lastOutput = writeBinaryOperatorChain(buffer, pretty, lastOutput, l)
			if not ok then  return false, lastOutput  end
		else
			local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, l, false)
			if not ok then  return false, lastOutput  end
		end

		if binary.operator == ".." then  ensureSpace(buffer, lastOutput, "number")  end

		local nextOutput = ((binary.operator == "-" and "-") or (binary.operator:find"%w" and "alphanum") or (""))
		if nextOutput ~= "" then  ensureSpace(buffer, lastOutput, nextOutput)  end
		lastOutput = writeLua(buffer, binary.operator, nextOutput)

		if r.type == "binary" and r.operator == binary.operator then
			local ok;ok, lastOutput = writeBinaryOperatorChain(buffer, pretty, lastOutput, r)
			if not ok then  return false, lastOutput  end
		else
			local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, r, false)
			if not ok then  return false, lastOutput  end
		end

		return true, lastOutput
	end

	-- success, lastOutput = writeNode( buffer, pretty, lastOutput, node, maySafelyOmitParens )
	-- lastOutput          = "" | "alphanum" | "number" | "-"
	function writeNode(buffer, pretty, lastOutput, node, maySafelyOmitParens)
		local nodeType = node.type

		-- Expressions:

		if nodeType == "identifier" then
			lastOutput = writeAlphanum(buffer, node.name, lastOutput)

		elseif nodeType == "vararg" then
			if node.adjustToOne then  lastOutput = writeLua(buffer, "(", "")  end
			lastOutput = writeLua(buffer, "...", "")
			if node.adjustToOne then  lastOutput = writeLua(buffer, ")", "")  end

		elseif nodeType == "literal" then
			if node.value == 0 then
				lastOutput = writeNumber(buffer, 0, lastOutput) -- Avoid writing '-0'.

			elseif node.value == math.huge then
				lastOutput = writeAlphanum(buffer, "math.huge", lastOutput)

			elseif node.value == -math.huge then
				lastOutput = writeLua(buffer, "(-math.huge)", "")

			elseif node.value ~= node.value then
				lastOutput = writeLua(buffer, "(0/0)", "")

			elseif node.value == nil or type(node.value) == "boolean" then
				lastOutput = writeAlphanum(buffer, tostring(node.value), lastOutput)

			elseif type(node.value) == "number" then
				lastOutput = writeNumber(buffer, node.value, lastOutput)

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
				printerr(F("Failed outputting value '%s'.", node.value))
				return false, lastOutput
			end

		elseif nodeType == "table" then
			lastOutput = writeLua(buffer, "{", "")

			for i, field in ipairs(node.fields) do
				if i > 1 then  lastOutput = writeLua(buffer, ",", "")  end

				if not field.generatedKey then
					if canNodeBeName(field.key) then
						lastOutput = writeLua(buffer, field.key.value, "alphanum")

					else
						lastOutput = writeLua(buffer, "[", "")

						local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, field.key, false)
						if not ok then  return false, lastOutput  end

						lastOutput = writeLua(buffer, "]", "")
					end

					lastOutput = writeLua(buffer, "=", "")
				end

				local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, field.value, false)
				if not ok then  return false, lastOutput  end
			end

			lastOutput = writeLua(buffer, "}", "")

		elseif nodeType == "lookup" then
			local ok;ok, lastOutput = writeLookup(buffer, pretty, lastOutput, node, false)
			if not ok then  return false, lastOutput  end

		elseif nodeType == "unary" then
			if not maySafelyOmitParens then  lastOutput = writeLua(buffer, "(", "")  end -- @Polish: Only output parentheses around child unaries/binaries if associativity requires it.

			local nextOutput = ((node.operator == "-" and "-") or (node.operator:find"%w" and "alphanum") or (""))
			if nextOutput ~= "" then  ensureSpace(buffer, lastOutput, nextOutput)  end
			lastOutput = writeLua(buffer, node.operator, nextOutput)

			local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, node.expression, false)
			if not ok then  return false, lastOutput  end

			if not maySafelyOmitParens then  lastOutput = writeLua(buffer, ")", "")  end

		elseif nodeType == "binary" then
			if not maySafelyOmitParens then  lastOutput = writeLua(buffer, "(", "")  end -- @Polish: Only output parentheses around child unaries/binaries if associativity requires it.

			if node.operator == ".." or node.operator == "and" or node.operator == "or" then
				local ok;ok, lastOutput = writeBinaryOperatorChain(buffer, pretty, lastOutput, node)
				if not ok then  return false, lastOutput  end

			else
				local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, node.left, false)
				if not ok then  return false, lastOutput  end

				local nextOutput = ((node.operator == "-" and "-") or (node.operator:find"%w" and "alphanum") or (""))
				if nextOutput ~= "" then  ensureSpace(buffer, lastOutput, nextOutput)  end
				lastOutput = writeLua(buffer, node.operator, nextOutput)

				local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, node.right, false)
				if not ok then  return false, lastOutput  end
			end

			if not maySafelyOmitParens then  lastOutput = writeLua(buffer, ")", "")  end

		elseif nodeType == "call" then
			if node.adjustToOne then  lastOutput = writeLua(buffer, "(", "")  end

			if node.method then
				local lookup = node.callee

				if lookup.type ~= "lookup" then
					printerr(F("AST: Callee for method call is not a lookup."))
					return false, lastOutput
				end

				local ok;ok, lastOutput = writeLookup(buffer, pretty, lastOutput, lookup, true)
				if not ok then  return false, lastOutput  end

			else
				local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, node.callee, false)
				if not ok then  return false, lastOutput  end
			end

			lastOutput = writeLua(buffer, "(", "")

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, lastOutput, node.arguments)
			if not ok then  return false, lastOutput  end

			lastOutput = writeLua(buffer, ")", "")
			if node.adjustToOne then  lastOutput = writeLua(buffer, ")", "")  end

		elseif nodeType == "function" then
			lastOutput = writeAlphanum(buffer, "function", lastOutput)
			lastOutput = writeLua(buffer, "(", "")

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, lastOutput, node.parameters)
			if not ok then  return false, lastOutput  end

			lastOutput = writeLua(buffer, ")", "")

			local ok;ok, lastOutput = writeStatements(buffer, pretty, lastOutput, node.body.statements)
			if not ok then  return false, lastOutput  end

			lastOutput = writeAlphanum(buffer, "end", lastOutput)

		-- Statements:

		elseif nodeType == "break" then
			lastOutput = writeAlphanum(buffer, "break", lastOutput)
			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "return" then
			lastOutput = writeAlphanum(buffer, "return", lastOutput)

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, lastOutput, node.values)
			if not ok then  return false, lastOutput  end

			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "block" then
			lastOutput = writeAlphanum(buffer, "do", lastOutput)

			local ok;ok, lastOutput = writeStatements(buffer, pretty, lastOutput, node.statements)
			if not ok then  return false, lastOutput  end

			lastOutput = writeAlphanum(buffer, "end", lastOutput)

		elseif nodeType == "declaration" then
			lastOutput = writeAlphanum(buffer, "local", lastOutput)

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, lastOutput, node.names)
			if not ok then  return false, lastOutput  end

			if node.values[1] then
				lastOutput = writeLua(buffer, "=", "")

				local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, lastOutput, node.values)
				if not ok then  return false, lastOutput  end
			end

			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "assignment" then
			if isAssignmentFunctionAssignment(node) then
				-- @Incomplete: Make implicit 'self' parameter actually implicit.
				local func = node.values[1]
				lastOutput = writeAlphanum(buffer, "function", lastOutput)

				local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, node.targets[1], false)
				if not ok then  return false, lastOutput  end

				lastOutput = writeLua(buffer, "(", "")

				local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, lastOutput, func.parameters)
				if not ok then  return false, lastOutput  end

				lastOutput = writeLua(buffer, ")", "")

				local ok;ok, lastOutput = writeStatements(buffer, pretty, lastOutput, func.body.statements)
				if not ok then  return false, lastOutput  end

				lastOutput = writeAlphanum(buffer, "end", lastOutput)

			else
				local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, lastOutput, node.targets)
				if not ok then  return false, lastOutput  end

				lastOutput = writeLua(buffer, "=", "")

				local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, lastOutput, node.values)
				if not ok then  return false, lastOutput  end

				lastOutput = writeLua(buffer, ";", "")
			end

		elseif nodeType == "if" then
			lastOutput = writeAlphanum(buffer, "if", lastOutput)

			local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, node.condition, true)
			if not ok then  return false, lastOutput  end

			lastOutput = writeAlphanum(buffer, "then", lastOutput)

			local ok;ok, lastOutput = writeStatements(buffer, pretty, lastOutput, node.bodyTrue.statements)
			if not ok then  return false, lastOutput  end

			while node.bodyFalse do
				-- Automatically detect what looks like 'elseif'.
				if #node.bodyFalse.statements == 1 and node.bodyFalse.statements[1].type == "if" then
					node = node.bodyFalse.statements[1]

					lastOutput = writeAlphanum(buffer, "elseif", lastOutput)

					local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, node.condition, true)
					if not ok then  return false, lastOutput  end

					lastOutput = writeAlphanum(buffer, "then", lastOutput)

					local ok;ok, lastOutput = writeStatements(buffer, pretty, lastOutput, node.bodyTrue.statements)
					if not ok then  return false, lastOutput  end

				else
					lastOutput = writeAlphanum(buffer, "else", lastOutput)

					local ok;ok, lastOutput = writeStatements(buffer, pretty, lastOutput, node.bodyFalse.statements)
					if not ok then  return false, lastOutput  end

					break
				end
			end

			lastOutput = writeAlphanum(buffer, "end", lastOutput)

		elseif nodeType == "while" then
			lastOutput = writeAlphanum(buffer, "while", lastOutput)

			local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, node.condition, true)
			if not ok then  return false, lastOutput  end

			lastOutput = writeAlphanum(buffer, "do", lastOutput)

			local ok;ok, lastOutput = writeStatements(buffer, pretty, lastOutput, node.body.statements)
			if not ok then  return false, lastOutput  end

			lastOutput = writeAlphanum(buffer, "end", lastOutput)

		elseif nodeType == "repeat" then
			lastOutput = writeAlphanum(buffer, "repeat", lastOutput)

			local ok;ok, lastOutput = writeStatements(buffer, pretty, lastOutput, node.body.statements)
			if not ok then  return false, lastOutput  end

			lastOutput = writeAlphanum(buffer, "until", lastOutput)

			local ok;ok, lastOutput = writeNode(buffer, pretty, lastOutput, node.condition, true)
			if not ok then  return false, lastOutput  end

		elseif nodeType == "for" then
			lastOutput = writeAlphanum(buffer, "for", lastOutput)

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, lastOutput, node.names)
			if not ok then  return false, lastOutput  end

			if node.kind == "numeric" then
				lastOutput = writeLua(buffer, "=", "")

			elseif node.kind == "generic" then
				lastOutput = writeAlphanum(buffer, "in", lastOutput)

			else
				printerr(F("Unknown 'for' loop kind '%s'.", node.kind))
				return false, lastOutput
			end

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, lastOutput, node.values)
			if not ok then  return false, lastOutput  end

			lastOutput = writeAlphanum(buffer, "do", lastOutput)

			local ok;ok, lastOutput = writeStatements(buffer, pretty, lastOutput, node.body.statements)
			if not ok then  return false, lastOutput  end

			lastOutput = writeAlphanum(buffer, "end", lastOutput)

		else
			printerr(F("Unknown node type '%s'.", nodeType))
			return false, lastOutput
		end
		return true, lastOutput
	end

	-- lua = toLua( ast [, prettyOuput=false ] )
	-- Returns nil on error.
	function toLua(node, pretty)
		-- @Incomplete: Support 'pretty'.
		local buffer = {}

		local ok
		if node.type == "block" then -- @Robustness: This exception isn't great. Should there be a file scope node?
			ok = writeStatements(buffer, pretty, "", node.statements)
		else
			ok = writeNode(buffer, pretty, "", node, true)
		end

		return ok and table.concat(buffer) or nil
	end
end



-- Test.
do
	io.stdout:setvbuf("no")
	io.stderr:setvbuf("no")

	local tokens = assert(tokenizeFile("./test.lua"))
	local ast    = assert(parse(tokens))

	-- printTree(ast)

	local lua
	lua = toLua(ast, true)
	-- lua = lua:gsub(("."):rep(250), "%0\0")
	-- lua = lua:gsub("([%w_]+)%z([%w_]+)", "%1%2\n")
	-- lua = lua:gsub("%z", "\n")
	print(lua)

	assert(loadstring(toLua(ast, true), "@lua"))
end

return {
	tokenizeString = tokenizeString,
	tokenizeFile   = tokenizeFile,
	parse          = parse,
	printNode      = printNode,
	printTree      = printTree,
	toLua          = toLua,
}
