--[[============================================================
--=
--=  Dumb Lua Parser - Lua parsing library
--=  by Marcus 'ReFreezed' Thunström
--=
--=  Tokenize Lua code or create ASTs (Abstract Syntax Trees)
--=  and convert the data back to Lua.
--=
--=  Version: 2.0-dev
--=
--=  License: MIT (see the bottom of this file)
--=  Website: https://github.com/ReFreezed/DumbLuaParser
--=
--=  Supported Lua versions: 5.1, 5.2, 5.3, 5.4
--=
--==============================================================

1 - Usage
2 - API
  2.1 - Functions
  2.2 - Constants
  2.3 - Settings
3 - Tokens
4 - AST


1 - Usage
================================================================

local parser = require("dumbParser")

local tokens = parser.tokenizeFile("cool.lua")
local ast    = parser.parse(tokens)

parser.printTree(ast)

local lua = parser.toLua(ast, true)
print(lua)


2 - API
================================================================


2.1 - Functions
----------------------------------------------------------------

tokenize, tokenizeFile
newTokenStream, insertToken, removeToken, concatTokens
parse, parseFile
newNode, getChild, setChild, addChild, removeChild
traverseTree, traverseTreeReverse
updateReferences
simplify, clean, minify
toLua
printTokens, printNode, printTree

tokenize()
	tokens, error = parser.tokenize( luaString [, pathForErrorMessages="?" ] )
	Convert a Lua string into tokens.
	Returns nil and a message on error.

tokenizeFile()
	tokens, error = parser.tokenizeFile( path )
	Convert the contents of a file into tokens. Uses io.open().
	Returns nil and a message on error.

newTokenStream()
	tokens = parser.newTokenStream( )
	Create a new token stream table. (See more info below.)

insertToken()
	parser.insertToken( tokens, [ index=tokens.n+1, ] tokenType, tokenValue )
	Insert a new token. (Search for 'TokenInsertion' for more info.)

removeToken()
	parser.removeToken( tokens [, index=tokens.n ] )
	Remove a token.

concatTokens()
	parser.concatTokens( tokens )
	Concatinate tokens.

parse()
	astNode, error = parser.parse( tokens )
	astNode, error = parser.parse( luaString [, pathForErrorMessages="?" ] )
	Convert tokens or Lua code into an AST.
	Returns nil and a message on error.

parseFile()
	astNode, error = parser.parseFile( path )
	Convert a Lua file into an AST. Uses io.open().
	Returns nil and a message on error.

newNode()
	astNode = parser.newNode( nodeType, arguments... )
	Create a new AST node. (Search for 'NodeCreation' for more info.)

getChild()
	node = parser.getChild( node, fieldName )
	node = parser.getChild( node, fieldName, index )                -- If the node field is an array.
	node = parser.getChild( node, fieldName, index, tableFieldKey ) -- If the node field is a table field array.
	tableFieldKey = "key"|"value"
	Get a child node. (Search for 'NodeFields' for field names.)
	@Incomplete: Better explanation.

setChild()
	parser.setChild( node, fieldName, childNode )
	parser.setChild( node, fieldName, index, childNode )                -- If the node field is an array.
	parser.setChild( node, fieldName, index, tableFieldKey, childNode ) -- If the node field is a table field array.
	tableFieldKey = "key"|"value"
	Set a child node. (Search for 'NodeFields' for field names.)
	@Incomplete: Better explanation.

addChild()
	parser.addChild( node, fieldName, [ index=atEnd, ] childNode )
	parser.addChild( node, fieldName, [ index=atEnd, ] keyNode, valueNode ) -- If the node field is a table field array.
	Add a child node to an array field. (Search for 'NodeFields' for field names.)
	@Incomplete: Better explanation.

removeChild()
	parser.removeChild( node, fieldName [, index=last ] )
	Remove a child node from an array field. (Search for 'NodeFields' for field names.)
	@Incomplete: Better explanation.

traverseTree()
	didStop = parser.traverseTree( astNode, [ leavesFirst=false, ] callback [, topNodeParent=nil, topNodeContainer=nil, topNodeKey=nil ] )
	action  = callback( astNode, parent, container, key )
	action  = "stop"|"ignorechildren"|nil  -- Returning nil (or nothing) means continue traversal.
	Call a function on all nodes in an AST, going from astNode out to the leaf nodes (or from leaf nodes and inwards if leavesFirst is set).
	container[key] is the position of the current node in the tree and can be used to replace the node.

traverseTreeReverse()
	didStop = parser.traverseTreeReverse( astNode, [ leavesFirst=false, ] callback [, topNodeParent=nil, topNodeContainer=nil, topNodeKey=nil ] )
	action  = callback( astNode, parent, container, key )
	action  = "stop"|"ignorechildren"|nil  -- Returning nil (or nothing) means continue traversal.
	Call a function on all nodes in reverse order in an AST, going from astNode out to the leaf nodes (or from leaf nodes and inwards if leavesFirst is set).
	container[key] is the position of the current node in the tree and can be used to replace the node.

updateReferences()
	parser.updateReferences( astNode [, updateTopNodePosition=true ] )
	Update references between nodes in the tree.
	This function sets 'parent', 'container' and 'key' for all nodes and 'declaration' for identifiers.
	If 'updateTopNodePosition' is false then 'parent', 'container' and 'key' will remain as-it for 'astNode' specifically.

simplify()
	simplify( astNode )
	Simplify/fold expressions and statements involving constants ('1+2' becomes '3', 'false and func()' becomes 'false' etc.).
	See the INT_SIZE constant for notes.

clean()
	clean( astNode )
	Attempt to remove nodes that aren't useful, like unused variables.
	This function can be quite slow!

minify()
	parser.minify( astNode [, optimize=false ] )
	Replace local variable names with short names.
	This function can be used to obfuscate the code to some extent.
	If 'optimize' is set then simplify() and clean() is also called automatically.

toLua()
	luaString, error = parser.toLua( astNode [, prettyOuput=false ] )
	Convert an AST to Lua. Returns nil and a message on error.

printTokens()
	parser.printTokens( tokens )
	Print the contents of a token stream to stdout.

printNode()
	parser.printNode( astNode )
	Print information about an AST node to stdout.

printTree()
	parser.printTree( astNode )
	Print the structure of a whole AST to stdout.


2.2 - Constants
----------------------------------------------------------------

INT_SIZE, MAX_INT, MIN_INT
VERSION

INT_SIZE
	parser.INT_SIZE = integer
	How many bits integers have. In Lua 5.3 and later this is usually 64, and in earlier versions it's 32.
	The int size may affect how bitwise operations involving only constants get simplified (see simplify()),
	e.g. the expression '-1>>1' becomes 2147483647 in Lua 5.2 but 9223372036854775807 in Lua 5.3.

MAX_INT
	parser.MAX_INT = integer
	The highest representable positive signed integer value, according to INT_SIZE.
	This is the same value as math.maxinteger in Lua 5.3 and later.
	This only affects simplification of some bitwise operations.

MIN_INT
	parser.MIN_INT = integer
	The highest representable negative signed integer value, according to INT_SIZE.
	This is the same value as math.mininteger in Lua 5.3 and later.
	This only affects simplification of some bitwise operations.

VERSION
	parser.VERSION
	The parser's version number (e.g. "1.0.2").


2.3 - Settings
----------------------------------------------------------------

printIds, printLocations
indentation

printIds
	parser.printIds = bool
	If AST node IDs should be printed. (All nodes gets assigned a unique ID when created.)
	Default: false.

printLocations
	parser.printLocations = bool
	If the file location (filename and line number) should be printed for each token or AST node.
	Default: false.

indentation
	parser.indentation = bool
	The indentation used when printing ASTs (with printTree()).
	Default: 4 spaces.


3 - Tokens
================================================================

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


4 - AST
================================================================

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

local PARSER_VERSION = "2.0.0-dev"

local assert       = assert
local error        = error
local ipairs       = ipairs
local loadstring   = loadstring or load
local pairs        = pairs
local select       = select
local tonumber     = tonumber
local tostring     = tostring
local type         = type

local io           = io
local ioOpen       = io.open
local ioWrite      = io.write

local jit          = jit

local mathFloor    = math.floor
local mathMax      = math.max
local mathMin      = math.min
local mathType     = math.type -- May be nil.

local F            = string.format
local stringByte   = string.byte
local stringChar   = string.char
local stringFind   = string.find
local stringGmatch = string.gmatch
local stringGsub   = string.gsub
local stringMatch  = string.match
local stringRep    = string.rep
local stringSub    = string.sub

local tableConcat  = table.concat
local tableInsert  = table.insert
local tableRemove  = table.remove
local tableSort    = table.sort
local tableUnpack  = table.unpack or unpack

local maybeWrapInt = (jit and function(n)return(n%2^32)end) or (_VERSION == "Lua 5.2" and bit32.band) or function(n)return(n)end

local assertArg1, assertArg, errorf
local countString, countSubString
local formatErrorInFile, formatErrorAtToken, formatErrorAfterToken, formatErrorAtNode -- @Incomplete: Should we expose these functions?
local formatNumber
local getChild, setChild, addChild, removeChild
local getLineNumber
local getNameArrayOfDeclarationLike
local getRelativeLocationText, getRelativeLocationTextForToken, getRelativeLocationTextForNode
local ipairsr
local isToken, isTokenType, isTokenAnyValue
local itemWith1
local mayNodeBeInvolvedInJump, mayAnyNodeBeInvolvedInJump
local newTokenStream, dummyTokens
local parse, parseFile
local printNode, printTree
local printTokens
local removeUnordered
local tokenize, tokenizeFile
local toLua
local traverseTree, traverseTreeReverse
local updateReferences

local parser



local function newSet(values)
	local set = {}
	for _, v in ipairs(values) do
		set[v] = true
	end
	return set
end

local KEYWORDS = newSet{
	"and", "break", "do", "else", "elseif", "end", "false", "for", "function", "goto", "if",
	"in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while",
}
local PUNCTUATION = newSet{
	"+",  "-",  "*",  "/",  "%",  "^",  "#",
	"&",  "~",  "|",  "<<", ">>", "//",
	"==", "~=", "<=", ">=", "<",  ">",  "=",
	"(",  ")",  "{",  "}",  "[",  "]",  "::",
	";",  ":",  ",",  ".",  "..", "...",
}
local OPERATORS_UNARY = newSet{
	"-", "not", "#", "~",
}
local OPERATORS_BINARY = newSet{
	"+",   "-",  "*", "/",  "//", "^", "%",
	"&",   "~",  "|", ">>", "<<", "..",
	"<",   "<=", ">", ">=", "==", "~=",
	"and", "or",
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

local NUM_HEX_FRAC_EXP = stringGsub("^( 0[Xx] (%x*) %.(%x+) [Pp]([-+]?%x+) )", " +", "")
local NUM_HEX_FRAC     = stringGsub("^( 0[Xx] (%x*) %.(%x+)                )", " +", "")
local NUM_HEX_EXP      = stringGsub("^( 0[Xx] (%x+) %.?     [Pp]([-+]?%x+) )", " +", "")
local NUM_HEX          = stringGsub("^( 0[Xx]  %x+  %.?                    )", " +", "")
local NUM_BIN          = stringGsub("^( 0[Bb]  [01]+                       )", " +", "")
local NUM_DEC_FRAC_EXP = stringGsub("^(        %d*  %.%d+   [Ee][-+]?%d+   )", " +", "")
local NUM_DEC_FRAC     = stringGsub("^(        %d*  %.%d+                  )", " +", "")
local NUM_DEC_EXP      = stringGsub("^(        %d+  %.?     [Ee][-+]?%d+   )", " +", "")
local NUM_DEC          = stringGsub("^(        %d+  %.?                    )", " +", "")

local INT_SIZE, MAX_INT, MIN_INT
do
	local hex = F("%x", maybeWrapInt(-1))
	INT_SIZE  = #hex * 4 -- This should generally be 64 for Lua 5.3+ and 32 for earlier.
	MAX_INT   = math.maxinteger or tonumber(stringGsub(hex, "f", "7", 1), 16)
	MIN_INT   = math.mininteger or -MAX_INT-1
end

local EMPTY_TABLE = {}

local nextSerialNumber = 1



-- :NodeFields

local function populateCommonNodeFields(tokens, tok, node)
	-- All nodes have these fields.
	node.id          = nextSerialNumber
	nextSerialNumber = nextSerialNumber + 1

	node.sourcePath   = tokens.sourcePath
	node.sourceString = tokens.sourceString

	node.token    = tok
	node.line     = tokens.lineStart    [tok] or 0
	node.position = tokens.positionStart[tok] or 0

	-- These fields are set by updateReferences():
	-- node.parent    = nil -- Refers to the node's parent in the tree.
	-- node.container = nil -- Refers to the specific table that the node is in, which could be the parent itself or a field in the parent.
	-- node.key       = nil -- Refers to the specific field in the container that the node is in (which is either a string or an integer).

	return node
end

-- AST expressions.
local function AstIdentifier (tokens,tok,name)return populateCommonNodeFields(tokens,tok,{
	type        = "identifier",
	name        = name,  -- String.
	attribute   = "",    -- "" | "close" | "const"
	declaration = nil,   -- AstDeclaration, AstFunction or AstFor. Updated by updateReferences(). This is nil for globals.
})end
local function AstVararg (tokens,tok)return populateCommonNodeFields(tokens,tok,{
	type        = "vararg",
	adjustToOne = false, -- True if parentheses surround the vararg.
})end
local function AstLiteral (tokens,tok,value)return populateCommonNodeFields(tokens,tok,{
	type        = "literal",
	value       = value, -- Number, string, boolean or nil.
})end
local function AstTable (tokens,tok)return populateCommonNodeFields(tokens,tok,{
	type        = "table",
	fields      = {},    -- Array of {key=expression, value=expression, generatedKey=bool}.
})end
local function AstLookup (tokens,tok)return populateCommonNodeFields(tokens,tok,{
	type        = "lookup",
	object      = nil,   -- Expression.
	member      = nil,   -- Expression.
})end
local function AstUnary (tokens,tok)return populateCommonNodeFields(tokens,tok,{
	type        = "unary",
	operator    = "",    -- "-" | "not" | "#" | "~"
	expression  = nil,   -- Expression.
})end
local function AstBinary (tokens,tok)return populateCommonNodeFields(tokens,tok,{
	type        = "binary",
	operator    = "",    -- "+" | "-" | "*" | "/" | "//" | "^" | "%" | "&" | "~" | "|" | ">>" | "<<" | ".." | "<" | "<=" | ">" | ">=" | "==" | "~=" | "and" | "or"
	left        = nil,   -- Expression.
	right       = nil,   -- Expression.
})end
local function AstCall (tokens,tok)return populateCommonNodeFields(tokens,tok,{ -- Calls can be both expressions and statements.
	type        = "call",
	callee      = nil,   -- Expression.
	arguments   = {},    -- Array of expressions.
	method      = false, -- True if the call is a method call. Method calls must have a callee that is a lookup with a member expression that is a string literal that can pass as an identifier.
	adjustToOne = false, -- True if parentheses surround the call.
})end
local function AstFunction (tokens,tok)return populateCommonNodeFields(tokens,tok,{
	type        = "function",
	parameters  = {},    -- Array of AstIdentifier.
	vararg      = nil,   -- AstVararg or nil.
	body        = nil,   -- AstBlock.
})end

-- AST statements.
local function AstBreak (tokens,tok)return populateCommonNodeFields(tokens,tok,{
	type        = "break",
})end
local function AstReturn (tokens,tok)return populateCommonNodeFields(tokens,tok,{
	type        = "return",
	values      = {},    -- Array of expressions.
})end
local function AstLabel (tokens,tok)return populateCommonNodeFields(tokens,tok,{
	type        = "label",
	name        = "",    -- The value must be able to pass as an identifier
})end
local function AstGoto (tokens,tok)return populateCommonNodeFields(tokens,tok,{
	type        = "goto",
	name        = "",    -- The value must be able to pass as an identifier
	label       = nil,   -- AstLabel. Updated by updateReferences().
})end
local function AstBlock (tokens,tok)return populateCommonNodeFields(tokens,tok,{
	type        = "block",
	statements  = {},    -- Array of statements.
})end
local function AstDeclaration (tokens,tok)return populateCommonNodeFields(tokens,tok,{
	type        = "declaration",
	names       = {},    -- Array of AstIdentifier.
	values      = {},    -- Array of expressions.
})end
local function AstAssignment (tokens,tok)return populateCommonNodeFields(tokens,tok,{
	type        = "assignment",
	targets     = {},    -- Mixed array of AstIdentifier and AstLookup.
	values      = {},    -- Array of expressions.
})end
local function AstIf (tokens,tok)return populateCommonNodeFields(tokens,tok,{
	type        = "if",
	condition   = nil,   -- Expression.
	bodyTrue    = nil,   -- AstBlock.
	bodyFalse   = nil,   -- AstBlock or nil.
})end
local function AstWhile (tokens,tok)return populateCommonNodeFields(tokens,tok,{
	type        = "while",
	condition   = nil,   -- Expression.
	body        = nil,   -- AstBlock.
})end
local function AstRepeat (tokens,tok)return populateCommonNodeFields(tokens,tok,{
	type        = "repeat",
	body        = nil,   -- AstBlock.
	condition   = nil,   -- Expression.
})end
local function AstFor (tokens,tok)return populateCommonNodeFields(tokens,tok,{
	type        = "for",
	kind        = "",    -- "numeric" | "generic"
	names       = {},    -- Array of AstIdentifier.
	values      = {},    -- Array of expressions.
	body        = nil,   -- AstBlock.
})end



local CHILD_FIELDS = {
	["identifier"]  = {declaration="node"},
	["vararg"]      = {},
	["literal"]     = {},
	["table"]       = {fields="tablefields"},
	["lookup"]      = {object="node", member="node"},
	["unary"]       = {expressions="node"},
	["binary"]      = {left="node", right="node"},
	["call"]        = {callee="node", arguments="nodearray"},
	["function"]    = {parameters="nodearray", vararg="node", body="node"},
	["break"]       = {},
	["return"]      = {values="nodearray"},
	["label"]       = {},
	["goto"]        = {},
	["block"]       = {statements="nodearray"},
	["declaration"] = {names="nodearray", values="nodearray"},
	["assignment"]  = {targets="nodearray", values="nodearray"},
	["if"]          = {condition="node", bodyTrue="node", bodyFalse="node"},
	["while"]       = {condition="node", body="node"},
	["repeat"]      = {body="node", condition="node"},
	["for"]         = {names="nodearray", values="nodearray", body="node"},
}



-- count = countString( haystack, needle [, plain=false ] )
function countString(haystack, needle, plain)
	local count = 0
	local pos   = 1

	while true do
		local _, i2 = stringFind(haystack, needle, pos, plain)
		if not i2 then  return count  end

		count = count + 1
		pos   = i2    + 1
	end
end

-- count = countSubString( haystack, startPosition, endPosition, needle [, plain=false ] )
function countSubString(haystack, pos, posEnd, needle, plain)
	local count = 0

	while true do
		local _, i2 = stringFind(haystack, needle, pos, plain)
		if not i2 or i2 > posEnd then  return count  end

		count = count + 1
		pos   = i2    + 1
	end
end



function getLineNumber(s, pos)
	return 1 + countSubString(s, 1, pos-1, "\n", true)
end



do
	local function findStartOfLine(s, pos, canBeEmpty)
		while pos > 1 do
			if stringByte(s, pos-1) == 10--[[\n]] and (canBeEmpty or stringByte(s, pos) ~= 10--[[\n]]) then  break  end
			pos = pos - 1
		end
		return mathMax(pos, 1)
	end
	local function findEndOfLine(s, pos)
		while pos < #s do
			if stringByte(s, pos+1) == 10--[[\n]] then  break  end
			pos = pos + 1
		end
		return mathMin(pos, #s)
	end

	local function getSubTextLength(s, pos, posEnd)
		local len = 0

		while pos <= posEnd do
			if stringByte(s, pos) == 9 then -- '\t'
				len = len + 4
				pos = pos + 1
			else
				local _, i2 = stringFind(s, "^[%z\1-\127\194-\253][\128-\191]*", pos)
				if i2 and i2 <= posEnd then
					len = len + 1
					pos = i2  + 1
				else
					len = len + 1
					pos = pos + 1
				end
			end
		end

		return len
	end

	function formatErrorInFile(contents, path, pos, agent, s, ...)
		s = F(s, ...)

		if contents == "" then
			return F("Error @ %s: [%s] %s", path, agent, s)
		end

		pos      = mathMin(mathMax(pos, 1), #contents+1)
		local ln = getLineNumber(contents, pos)

		local lineStart     = findStartOfLine(contents, pos, true)
		local lineEnd       = findEndOfLine  (contents, pos-1)
		local linePre1Start = findStartOfLine(contents, lineStart-1, false)
		local linePre1End   = findEndOfLine  (contents, linePre1Start-1)
		local linePre2Start = findStartOfLine(contents, linePre1Start-1, false)
		local linePre2End   = findEndOfLine  (contents, linePre2Start-1)
		-- print(F("pos %d | lines %d..%d, %d..%d, %d..%d", pos, linePre2Start,linePre2End+1, linePre1Start,linePre1End+1, lineStart,lineEnd+1)) -- DEBUG

		return F("Error @ %s:%d: [%s] %s\n>\n%s%s%s>-%s^",
			path, ln, agent, s,
			(linePre2Start < linePre1Start and linePre2Start <= linePre2End) and F("> %s\n", (stringGsub(stringSub(contents, linePre2Start, linePre2End), "\t", "    "))) or "",
			(linePre1Start < lineStart     and linePre1Start <= linePre1End) and F("> %s\n", (stringGsub(stringSub(contents, linePre1Start, linePre1End), "\t", "    "))) or "",
			(                                  lineStart     <= lineEnd    ) and F("> %s\n", (stringGsub(stringSub(contents, lineStart,     lineEnd    ), "\t", "    "))) or ">\n",
			stringRep("-", getSubTextLength(contents, lineStart, pos-1))
		)
	end
end

function formatErrorAtToken(tokens, tok, agent, s, ...)
	local pos = tokens.positionStart[tok] or (tok == 1 and 1 or #tokens.sourceString+1)
	return (formatErrorInFile(tokens.sourceString, tokens.sourcePath, pos, agent, s, ...))
end
function formatErrorAfterToken(tokens, tok, agent, s, ...)
	local pos = (tokens.positionStart[tok] and tokens.positionEnd[tok]+1) or (tok == 1 and 1 or #tokens.sourceString+1)
	return (formatErrorInFile(tokens.sourceString, tokens.sourcePath, pos, agent, s, ...))
end

function formatErrorAtNode(node, agent, s, ...)
	return (formatErrorInFile(node.sourceString, node.sourcePath, node.position, agent, s, ...))
end



-- text = getRelativeLocationText( sourcePathOfInterest, lineNumberOfInterest, otherSourcePath, otherLineNumber )
-- text = getRelativeLocationText( lineNumberOfInterest, otherLineNumber )
function getRelativeLocationText(sourcePath, ln, otherSourcePath, otherLn)
	if type(sourcePath) ~= "string" then
		sourcePath, ln, otherSourcePath, otherLn = "", sourcePath, "", ln
	end

	if not (ln > 0) then
		return "at <UnknownLocation>"
	end

	if sourcePath ~= otherSourcePath         then  return F("at %s:%d", sourcePath, ln)  end
	if ln+1       == otherLn and otherLn > 0 then  return F("on the previous line")  end
	if ln-1       == otherLn and otherLn > 0 then  return F("on the next line")  end
	if ln         ~= otherLn                 then  return F("on line %d", ln)  end
	return "on the same line"
end

-- text = getRelativeLocationTextForToken( tokens, tokenOfInterest, otherToken )
function getRelativeLocationTextForToken(tokens, tokOfInterest, otherTok)
	return getRelativeLocationText((tokens.lineStart[tokOfInterest] or 0), (tokens.lineStart[otherTok] or 0))
end

-- text = getRelativeLocationTextForNode( nodeOfInterest, otherNode )
-- text = getRelativeLocationTextForNode( nodeOfInterest, otherSourcePath, otherLineNumber )
function getRelativeLocationTextForNode(nodeOfInterest, otherSourcePath, otherLn)
	if type(otherSourcePath) == "table" then
		return getRelativeLocationTextForNode(nodeOfInterest, otherSourcePath.sourcePath, otherSourcePath.line)
	end

	if not (nodeOfInterest.sourcePath ~= "?" and nodeOfInterest.line > 0) then
		return "at <UnknownLocation>"
	end

	return getRelativeLocationText(nodeOfInterest.sourcePath, nodeOfInterest.line, otherSourcePath, otherLn)
end



local ERROR_UNFINISHED_VALUE = {}

-- success, equalSignCountIfLong|errorCode, ptr = parseStringlikeToken( s, ptr )
local function parseStringlikeToken(s, ptr)
	local longEqualSigns       = stringMatch(s, "^%[(=*)%[", ptr)
	local equalSignCountIfLong = longEqualSigns and #longEqualSigns

	-- Single line (comment).
	if not equalSignCountIfLong then
		local i1, i2 = stringFind(s, "\n", ptr)
		ptr          = i2 and i2 + 1 or #s + 1

	-- Multiline.
	else
		ptr = ptr + 1 + #longEqualSigns + 1

		local i1, i2 = stringFind(s, "%]"..longEqualSigns.."%]", ptr)
		if not i1 then
			return false, ERROR_UNFINISHED_VALUE, 0
		end

		ptr = i2 + 1
	end

	return true, equalSignCountIfLong, ptr
end

local function codepointToString(cp, buffer)
	if cp < 0 or cp > 0x10ffff then
		-- This error is actually incorrect as Lua supports codepoints up to 2^31.
		-- This is probably an issue that no one will ever encounter!
		return false, F("Codepoint 0x%X (%.0f) is outside the valid range (0..10FFFF).", cp, cp)
	end

	if cp < 128 then
		tableInsert(buffer, stringChar(cp))
		return true
	end

	local suffix = cp % 64
	local c4     = 128 + suffix
	cp           = (cp - suffix) / 64

	if cp < 32 then
		tableInsert(buffer, stringChar(192+cp))
		tableInsert(buffer, stringChar(c4))
		return true
	end

	suffix   = cp % 64
	local c3 = 128 + suffix
	cp       = (cp - suffix) / 64

	if cp < 16 then
		tableInsert(buffer, stringChar(224+cp))
		tableInsert(buffer, stringChar(c3))
		tableInsert(buffer, stringChar(c4))
		return true
	end

	suffix = cp % 64
	cp     = (cp - suffix) / 64

	tableInsert(buffer, stringChar(240+cp))
	tableInsert(buffer, stringChar(128+suffix))
	tableInsert(buffer, stringChar(c3))
	tableInsert(buffer, stringChar(c4))
	return true
end

local function parseStringContents(s, path, ptrStart, ptrEnd)
	local ptr    = ptrStart
	local buffer = {}

	while ptr <= ptrEnd do
		local i1 = stringFind(s, "\\", ptr, true)
		if not i1 or i1 > ptrEnd then  break  end

		if i1 > ptr then
			tableInsert(buffer, stringSub(s, ptr, i1-1))
		end
		ptr = i1 + 1

		-- local b1, b2, b3 = stringByte(s, ptr, ptr+2)

		if     stringFind(s, "^a", ptr) then  tableInsert(buffer, "\a") ; ptr = ptr + 1
		elseif stringFind(s, "^b", ptr) then  tableInsert(buffer, "\b") ; ptr = ptr + 1
		elseif stringFind(s, "^t", ptr) then  tableInsert(buffer, "\t") ; ptr = ptr + 1
		elseif stringFind(s, "^n", ptr) then  tableInsert(buffer, "\n") ; ptr = ptr + 1
		elseif stringFind(s, "^v", ptr) then  tableInsert(buffer, "\v") ; ptr = ptr + 1
		elseif stringFind(s, "^f", ptr) then  tableInsert(buffer, "\f") ; ptr = ptr + 1
		elseif stringFind(s, "^r", ptr) then  tableInsert(buffer, "\r") ; ptr = ptr + 1
		elseif stringFind(s, "^\\",ptr) then  tableInsert(buffer, "\\") ; ptr = ptr + 1
		elseif stringFind(s, '^"', ptr) then  tableInsert(buffer, "\"") ; ptr = ptr + 1
		elseif stringFind(s, "^'", ptr) then  tableInsert(buffer, "\'") ; ptr = ptr + 1
		elseif stringFind(s, "^\n",ptr) then  tableInsert(buffer, "\n") ; ptr = ptr + 1

		elseif stringFind(s, "^z", ptr) then
			local i1, i2 = stringFind(s, "^%s*", ptr+1)
			ptr          = i2 + 1

		elseif stringFind(s, "^%d", ptr) then
			local nStr = stringMatch(s, "^%d%d?%d?", ptr)
			local byte = tonumber(nStr)

			if byte > 255 then
				return nil, formatErrorInFile(
					s, path, ptr, "Tokenizer",
					"Byte value '%s' is out-of-range in decimal escape sequence. (String starting %s)",
					nStr, getRelativeLocationText(getLineNumber(s, ptrStart), getLineNumber(s, ptr))
				)
			end

			tableInsert(buffer, stringChar(byte))
			ptr = ptr + #nStr

		elseif stringFind(s, "^x%x%x", ptr) then
			local hexStr = stringSub(s, ptr+1, ptr+2)
			local byte   = tonumber(hexStr, 16)

			tableInsert(buffer, stringChar(byte))
			ptr = ptr + 3

		elseif stringFind(s, "^u{%x+}", ptr) then
			local hexStr = stringMatch(s, "^%x+", ptr+2)
			local cp     = tonumber(hexStr, 16)

			local ok, err = codepointToString(cp, buffer)
			if not ok then
				return nil, formatErrorInFile(
					s, path, ptr+2, "Tokenizer",
					"%s (String starting %s)",
					err, getRelativeLocationText(getLineNumber(s, ptrStart), getLineNumber(s, ptr))
				)
			end

			ptr = ptr + 3 + #hexStr

		else
			return nil, formatErrorInFile(
				s, path, ptr-1, "Tokenizer",
				"Invalid escape sequence. (String starting %s)",
				getRelativeLocationText(getLineNumber(s, ptrStart), getLineNumber(s, ptr))
			)
		end

	end

	if ptr <= ptrEnd then
		tableInsert(buffer, stringSub(s, ptr, ptrEnd))
	end

	return tableConcat(buffer)
end

-- tokens, error = tokenize( luaString [, pathForErrorMessages="?" ] )
function tokenize(s, path)
	assertArg1("tokenize", 1, s,    "string")
	assertArg ("tokenize", 2, path, "string","nil")

	if stringFind(s, "\r", 1, true) then
		s = stringGsub(s, "\r\n?", "\n")
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
		local i1, i2 = stringFind(s, "^%s+", ptr)
		if i1 then
			ln  = ln + countSubString(s, i1, i2, "\n", true)
			ptr = i2 + 1
		end

		if ptr > #s then  break  end

		local ptrStart = ptr
		local lnStart  = ln
		local tokType, tokRepr, tokValue

		-- Identifier/keyword.
		if stringFind(s, "^[%a_]", ptr) then
			local i1, i2, word = stringFind(s, "^([%a_][%w_]*)", ptr)
			ptr      = i2+1
			tokType  = KEYWORDS[word] and "keyword" or "identifier"
			tokRepr  = stringSub(s, ptrStart, ptr-1)
			tokValue = tokRepr

		-- Comment.
		elseif stringFind(s, "^%-%-", ptr) then
			ptr = ptr + 2

			local ok, equalSignCountIfLong
			ok, equalSignCountIfLong, ptr = parseStringlikeToken(s, ptr)

			if not ok then
				local errCode = equalSignCountIfLong
				if errCode == ERROR_UNFINISHED_VALUE then
					return nil, formatErrorInFile(s, path, ptrStart, "Tokenizer", "Unfinished long comment.")
				else
					return nil, formatErrorInFile(s, path, ptrStart, "Tokenizer", "Invalid comment.")
				end
			end

			-- Check for nesting of [[...]] which is deprecated in Lua. Sigh...
			if equalSignCountIfLong and equalSignCountIfLong == 0 then
				local pos = stringFind(s, "[[", ptrStart+4, true)
				if pos and pos < ptr then
					return nil, formatErrorInFile(s, path, pos, "Tokenizer", "Cannot have nested comments. (Comment starting %s)", getRelativeLocationText(lnStart, getLineNumber(s, pos)))
				end
			end

			tokType  = "comment"
			tokRepr  = stringSub(s, ptrStart, ptr-1)
			tokRepr  = equalSignCountIfLong and tokRepr or (stringFind(tokRepr, "\n$") and tokRepr or tokRepr.."\n") -- Make sure there's a newline at the end of single-line comments. (It may be missing if we've reached EOF.)
			tokValue = equalSignCountIfLong and stringSub(tokRepr, 5+equalSignCountIfLong, -3-equalSignCountIfLong) or stringSub(tokRepr, 3)

		-- Number.
		elseif stringFind(s, "^%.?%d", ptr) then
			local               pat, maybeInt, kind, i1, i2, numStr = NUM_HEX_FRAC_EXP, false, "lua52hex",  stringFind(s, NUM_HEX_FRAC_EXP, ptr)
			if not i1     then  pat, maybeInt, kind, i1, i2, numStr = NUM_HEX_FRAC,     false, "lua52hex",  stringFind(s, NUM_HEX_FRAC,     ptr)
			if not i1     then  pat, maybeInt, kind, i1, i2, numStr = NUM_HEX_EXP,      false, "lua52hex",  stringFind(s, NUM_HEX_EXP,      ptr)
			if not i1     then  pat, maybeInt, kind, i1, i2, numStr = NUM_HEX,          true,  "",          stringFind(s, NUM_HEX,          ptr)
			if not i1     then  pat, maybeInt, kind, i1, i2, numStr = NUM_BIN,          true,  "binary",    stringFind(s, NUM_BIN,          ptr) -- LuaJIT supports these, so why not.
			if not i1     then  pat, maybeInt, kind, i1, i2, numStr = NUM_DEC_FRAC_EXP, false, "",          stringFind(s, NUM_DEC_FRAC_EXP, ptr)
			if not i1     then  pat, maybeInt, kind, i1, i2, numStr = NUM_DEC_FRAC,     false, "",          stringFind(s, NUM_DEC_FRAC,     ptr)
			if not i1     then  pat, maybeInt, kind, i1, i2, numStr = NUM_DEC_EXP,      false, "",          stringFind(s, NUM_DEC_EXP,      ptr)
			if not i1     then  pat, maybeInt, kind, i1, i2, numStr = NUM_DEC,          true,  "",          stringFind(s, NUM_DEC,          ptr)
			if not numStr then  return nil, formatErrorInFile(s, path, ptrStart, "Tokenizer", "Malformed number.")
			end end end end end end end end end

			local numStrFallback = numStr

			if jit then
				if s:find("^[Ii]", i2+1) then -- Imaginary part of complex number.
					numStr = stringSub(s, i1, i2+1)
					i2     = i2 + 1

				elseif not maybeInt or numStr:find(".", 1, true) then
					-- void
				elseif s:find("^[Uu][Ll][Ll]", i2+1) then -- Unsigned 64-bit integer.
					numStr = stringSub(s, i1, i2+3)
					i2     = i2 + 3
				elseif s:find("^[Ll][Ll]", i2+1) then -- Signed 64-bit integer.
					numStr = stringSub(s, i1, i2+2)
					i2     = i2 + 2
				end
			end

			local n = tonumber(numStr)

			if not n and jit then
				local chunk = loadstring("return "..numStr)
				n           = chunk and chunk() or n
			end

			n = n or tonumber(numStrFallback)

			if not n then
				-- Note: We know we're not running LuaJIT here as it supports hexadecimal floats and binary notation, thus we use numStrFallback instead of numStr.

				-- Support hexadecimal floats if we're running Lua 5.1.
				if kind == "lua52hex" then
					local                                _, intStr, fracStr, expStr
					if     pat == NUM_HEX_FRAC_EXP then  _, intStr, fracStr, expStr = numStrFallback:match(NUM_HEX_FRAC_EXP)
					elseif pat == NUM_HEX_FRAC     then  _, intStr, fracStr         = numStrFallback:match(NUM_HEX_FRAC) ; expStr  = "0"
					elseif pat == NUM_HEX_EXP      then  _, intStr,          expStr = numStrFallback:match(NUM_HEX_EXP)  ; fracStr = ""
					else return nil, formatErrorInFile(s, path, ptrStart, "Tokenizer", "Internal error parsing the number '%s'.", numStrFallback) end

					n = tonumber(intStr, 16) or 0 -- intStr may be "".

					local fracValue = 1
					for i = 1, #fracStr do
						fracValue = fracValue / 16
						n         = n + tonumber(stringSub(fracStr, i, i), 16) * fracValue
					end

					n = n * 2 ^ stringGsub(expStr, "^+", "")

				elseif kind == "binary" then
					n = tonumber(numStrFallback:sub(3), 2)
				end
			end

			if not n then
				return nil, formatErrorInFile(s, path, ptrStart, "Tokenizer", "Invalid number.")
			end

			ptr      = i2+1
			tokType  = "number"
			tokRepr  = numStr
			tokValue = n

			if stringFind(s, "^[%w_.]", ptr) then
				local after = stringMatch(s, "^%.?%d+", ptr) or stringMatch(s, "^[%w_.][%w_.]?", ptr)
				return nil, formatErrorInFile(s, path, ptrStart, "Tokenizer", "Malformed number near '%s%s'.", numStr, after)
			end

		-- Quoted string.
		elseif stringFind(s, "^[\"']", ptr) then
			local quote     = stringSub(s, ptr, ptr)
			local quoteByte = stringByte(quote)
			ptr             = ptr + 1

			local pat = "["..quote.."\\\n]"

			while true do
				local i1 = stringFind(s, pat, ptr)
				if not i1 then
					return nil, formatErrorInFile(s, path, ptrStart, "Tokenizer", "Unfinished string.")
				end

				ptr          = i1
				local b1, b2 = stringByte(s, ptr, ptr+1)

				-- '"'
				if b1 == quoteByte then
					ptr = ptr + 1
					break

				-- '\'
				elseif b1 == 92 then
					ptr = ptr + 1

					if b2 == 122 then -- 'z'
						ptr         = ptr + 1
						local _, i2 = stringFind(s, "^%s*", ptr)
						ptr         = i2 + 1
					else
						-- Note: We don't have to look for multiple characters after the escape, like \nnn - this algorithm works anyway.
						if ptr > #s then
							return nil, formatErrorInFile(
								s, path, ptr, "Tokenizer",
								"Unfinished string after escape character. (String starting %s)",
								getRelativeLocationText(lnStart, getLineNumber(s, ptr))
							)
						end
						ptr = ptr + 1 -- Just skip the next character, whatever it might be.
					end

				-- '\n'
				elseif b1 == 10 then
					-- Lua, this is silly!
					return nil, formatErrorInFile(s, path, ptr, "Tokenizer", "Unescaped newline in string (starting %s).", getRelativeLocationText(lnStart, getLineNumber(s, ptr)))

				else
					assert(false)
				end
			end

			tokType = "string"
			tokRepr = stringSub(s, ptrStart, ptr-1)

			local chunk = loadstring("return "..tokRepr, "@") -- Try to make Lua parse the string value before we fall back to our own parser which is probably slower.
			if chunk then
				tokValue = chunk()
				assert(type(tokValue) == "string")
			else
				local stringValue, err = parseStringContents(s, path, ptrStart+1, ptr-2)
				if not stringValue then  return nil, err  end
				tokValue = stringValue
			end

		-- Long string.
		elseif stringFind(s, "^%[=*%[", ptr) then
			local ok, equalSignCountIfLong
			ok, equalSignCountIfLong, ptr = parseStringlikeToken(s, ptr)

			if not ok then
				local errCode = equalSignCountIfLong
				if errCode == ERROR_UNFINISHED_VALUE then
					return nil, formatErrorInFile(s, path, ptrStart, "Tokenizer", "Unfinished long string.")
				else
					return nil, formatErrorInFile(s, path, ptrStart, "Tokenizer", "Invalid long string.")
				end
			end

			tokType = "string"
			tokRepr = stringSub(s, ptrStart, ptr-1)

			local chunk, err = loadstring("return "..tokRepr, "@")
			if not chunk then
				err = stringGsub(err, "^:%d+: ", "")
				return nil, formatErrorInFile(s, path, ptrStart, "Tokenizer", "Could not convert long string token to value. (%s)", err)
			end
			tokValue = assert(chunk)()
			assert(type(tokValue) == "string")

		-- Punctuation.
		elseif stringFind(s, "^%.%.%.", ptr) then
			ptr      = ptr + 3
			tokType  = "punctuation"
			tokRepr  = stringSub(s, ptrStart, ptr-1)
			tokValue = tokRepr
		elseif stringFind(s, "^%.%.", ptr) or stringFind(s, "^[=~<>]=", ptr) or stringFind(s, "^::", ptr) or stringFind(s, "^//", ptr) or stringFind(s, "^<<", ptr) or stringFind(s, "^>>", ptr) then
			ptr      = ptr + 2
			tokType  = "punctuation"
			tokRepr  = stringSub(s, ptrStart, ptr-1)
			tokValue = tokRepr
		elseif stringFind(s, "^[-+*/%%^#<>=(){}[%];:,.&~|]", ptr) then
			ptr      = ptr + 1
			tokType  = "punctuation"
			tokRepr  = stringSub(s, ptrStart, ptr-1)
			tokValue = tokRepr

		else
			return nil, formatErrorInFile(s, path, ptrStart, "Tokenizer", "Unknown character.")
		end
		assert(tokType)

		ln = ln + countString(tokRepr, "\n", true)

		count            = count + 1
		tokTypes [count] = tokType
		tokValues[count] = tokValue
		tokReprs [count] = tokRepr
		tokLine1 [count] = lnStart
		tokLine2 [count] = ln
		tokPos1  [count] = ptrStart
		tokPos2  [count] = ptr - 1

		-- print(F("%4d %-11s '%s'", count, tokType, (stringGsub(tokRepr, "\n", "\\n"))))
	end

	tokens.n = count
	return tokens
end

-- tokens, error = tokenizeFile( path )
function tokenizeFile(path)
	assertArg1("tokenizeFile", 1, path, "string")

	local file, err = ioOpen(path, "r")
	if not file then  return nil, err  end

	local s = file:read("*a")
	file:close()

	return tokenize(s, path)
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
dummyTokens = newTokenStream()

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
	i = mathMin(mathMax(i, 1), tokens.n+1)

	local tokRepr

	if tokType == "keyword" then
		if type(tokValue) ~= "string" then  errorf(2, "Expected string value for 'keyword' token. (Got %s)", type(tokValue))  end
		if not KEYWORDS[tokValue]     then  errorf(2, "Invalid keyword '%s'.", tokValue)  end
		tokRepr = tokValue

	elseif tokType == "identifier" then
		if type(tokValue) ~= "string"                then  errorf(2, "Expected string value for 'identifier' token. (Got %s)", type(tokValue))  end
		if not stringFind(tokValue, "^[%a_][%w_]*$") then  errorf(2, "Invalid identifier '%s'.", tokValue)  end
		if KEYWORDS[tokValue]                        then  errorf(2, "Invalid identifier '%s'.", tokValue)  end
		tokRepr = tokValue

	elseif tokType == "number" then
		if type(tokValue) ~= "number" then
			errorf(2, "Expected number value for 'number' token. (Got %s)", type(tokValue))
		end
		tokRepr = (
			tokValue == 0        and "0"      or -- Avoid '-0'.
			tokValue == 1/0      and "(1/0)"  or
			tokValue == -1/0     and "(-1/0)" or
			tokValue ~= tokValue and "(0/0)"  or
			formatNumber(tokValue)
		)

	elseif tokType == "string" then
		if type(tokValue) ~= "string" then  errorf(2, "Expected string value for 'string' token. (Got %s)", type(tokValue))  end
		tokRepr = stringGsub(F("%q", tokRepr), "\n", "n")

	elseif tokType == "punctuation" then
		if type(tokValue) ~= "string" then  errorf(2, "Expected string value for 'punctuation' token. (Got %s)", type(tokValue))  end
		if not PUNCTUATION[tokValue]  then  errorf(2, "Invalid punctuation '%s'.", tokValue)  end
		tokRepr = tokValue

	elseif tokType == "comment" then
		if type(tokValue) ~= "string" then  errorf(2, "Expected string value for 'comment' token. (Got %s)", type(tokValue))  end

		if stringFind(tokValue, "\n") then
			local equalSigns = stringFind(tokValue, "[[", 1, true) and "=" or ""

			while stringFind(tokValue, "]"..equalSigns.."]", 1, true) do
				equalSigns = equalSigns.."="
			end

			tokRepr = F("--[%s[%s]%s]", equalSigns, tokValue, equalSigns)

		else
			tokRepr = F("--%s\n", tokValue)
		end

	else
		errorf(2, "Invalid token type '%s'.", tostring(tokType))
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

local function concatTokens(tokens)
	local tokReprs = tokens.representation
	local tokTypes = tokens.type
	local parts    = {}

	for tok = 1, tokens.n do
		local tokRepr     = tokReprs[tok]
		local lastTokRepr = tokReprs[tok-1]

		if lastTokRepr and (
			(stringFind(tokRepr, "^[%w_]") and stringFind(lastTokRepr, "[%w_]$")) or
			(stringFind(tokRepr, "^%."   ) and stringFind(lastTokRepr, "%.$"   )) or
			(stringFind(tokRepr, "^%-"   ) and stringFind(lastTokRepr, "%-$"   )) or
			(stringFind(tokRepr, "^/"    ) and stringFind(lastTokRepr, "/$"    )) or

			(tokTypes[tok-1] == "number" and stringFind(tokRepr,     "^[%w_.]")) or
			(tokTypes[tok  ] == "number" and stringFind(lastTokRepr, "%.$") and not stringFind(lastTokRepr, "%.%.$"))
		) then
			tableInsert(parts, " ")
		end
		tableInsert(parts, tokRepr)
	end

	return tableConcat(parts)
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



local function getLeftmostToken(node)
	if node.type == "binary" then
		return getLeftmostToken(node.left)
	else
		return node.token
	end
end



local parseExpression, parseExpressionList, parseFunctionParametersAndBody, parseBlock

local function parseIdentifier(tokens, tok) --> ident, token, error
	if not isTokenType(tokens, tok, "identifier") then
		return nil, tok, formatErrorAtToken(tokens, tok, "Parser", "Expected an identifier.")
	end

	local ident = AstIdentifier(tokens, tok, tokens.value[tok])
	tok         = tok + 1

	return ident, tok
end

local function parseNameList(tokens, tok, names, allowVararg, allowAttributes) --> success, token, vararg|error|nil
	while true do
		if allowVararg and isToken(tokens, tok, "punctuation", "...") then
			local vararg = AstVararg(tokens, tok)
			tok          = tok + 1 -- '...'
			return true, tok, vararg
		end

		local ident, tokNext, err = parseIdentifier(tokens, tok)
		if not ident then  return false, tok, err  end
		tok = tokNext

		if allowAttributes and isToken(tokens, tok, "punctuation", "<") then
			tok = tok + 1 -- '<'

			local attrIdent, tokNext, err = parseIdentifier(tokens, tok)
			if not attrIdent then
				return false, tok, err
			elseif not (attrIdent.name == "close" or attrIdent.name == "const") then
				return false, tok, formatErrorAtToken(tokens, tok, "Parser", "Expected 'close' or 'const' for attribute name.")
			end
			tok = tokNext

			ident.attribute = attrIdent.name

			if not isToken(tokens, tok, "punctuation", ">") then
				return nil, tok, formatErrorAfterToken(tokens, tok-1, "Parser", "Expected '>' after attribute name.")
			end
			tok = tok + 1 -- '>'
		end

		tableInsert(names, ident)

		if not isToken(tokens, tok, "punctuation", ",") then
			return true, tok
		end
		tok = tok + 1 -- ','
	end

	return true, tok
end

local function parseTable(tokens, tokStart) --> tableNode, token, error
	local tok       = tokStart
	local tableNode = AstTable(tokens, tok)
	tok             = tok + 1 -- '{'

	local generatedIndex = 0

	while true do
		if isToken(tokens, tok, "punctuation", "}") then
			tok = tok + 1 -- '}'
			break

		elseif isToken(tokens, tok, "punctuation", "[") then
			tok = tok + 1 -- '['

			local keyExpr, tokNext, err = parseExpression(tokens, tok, 0)
			if not keyExpr then  return nil, tok, err  end
			tok = tokNext

			if not isToken(tokens, tok, "punctuation", "]") then
				return nil, tok, formatErrorAfterToken(tokens, tok-1, "Parser", "Expected ']' after key value.")
			end
			tok = tok + 1 -- ']'

			if not isToken(tokens, tok, "punctuation", "=") then
				return nil, tok, formatErrorAfterToken(tokens, tok-1, "Parser", "Expected '=' after key.")
			end
			tok = tok + 1 -- '='

			local valueExpr, tokNext, err = parseExpression(tokens, tok, 0)
			if not valueExpr then  return nil, tok, err  end
			tok = tokNext

			local tableField = {key=keyExpr, value=valueExpr, generatedKey=false}
			tableInsert(tableNode.fields, tableField)

		elseif isTokenType(tokens, tok, "identifier") and isToken(tokens, tok+1, "punctuation", "=") then
			local keyExpr = AstLiteral(tokens, tok, tokens.value[tok])
			tok           = tok + 1 -- identifier

			if not isToken(tokens, tok, "punctuation", "=") then
				return nil, tok, formatErrorAfterToken(tokens, tok-1, "Parser", "Expected '=' after key name.")
			end
			tok = tok + 1 -- '='

			local valueExpr, tokNext, err = parseExpression(tokens, tok, 0)
			if not valueExpr then  return nil, tok, err  end
			tok = tokNext

			local tableField = {key=keyExpr, value=valueExpr, generatedKey=false}
			tableInsert(tableNode.fields, tableField)

		else
			generatedIndex = generatedIndex + 1
			local keyExpr  = AstLiteral(tokens, tok, generatedIndex)

			local valueExpr, tokNext, err = parseExpression(tokens, tok, 0)
			if not valueExpr then  return nil, tok, err  end
			tok = tokNext

			local tableField = {key=keyExpr, value=valueExpr, generatedKey=true}
			tableInsert(tableNode.fields, tableField)
		end

		if isToken(tokens, tok, "punctuation", ",") or isToken(tokens, tok, "punctuation", ";") then
			tok = tok + 1 -- ',' or ';'
			-- Continue...

		elseif isToken(tokens, tok, "punctuation", "}") then
			tok = tok + 1 -- '}'
			break

		else
			return nil, tok, formatErrorAfterToken(
				tokens, tok-1, "Parser",
				"Expected ',' or '}' after value in table constructor (starting %s).",
				getRelativeLocationTextForToken(tokens, tokStart, tok-1)
			)
		end
	end

	return tableNode, tok
end

function parseExpression(tokens, tokStart, lastPrecedence) --> expression, token, error
	local tok                  = tokStart
	local canParseLookupOrCall = false
	local expr

	-- identifier
	if isTokenType(tokens, tok, "identifier") then
		local ident, tokNext, err = parseIdentifier(tokens, tok)
		if not ident then  return nil, tok, err  end
		tok = tokNext

		expr                 = ident
		canParseLookupOrCall = true

	-- ...
	elseif isToken(tokens, tok, "punctuation", "...") then
		local vararg = AstVararg(tokens, tok)
		tok          = tok + 1 -- '...'
		expr         = vararg

	-- literal
	elseif isTokenType(tokens, tok, "string") or isTokenType(tokens, tok, "number") then
		local literal = AstLiteral(tokens, tok, tokens.value[tok])
		tok           = tok + 1 -- literal
		expr          = literal
	elseif isToken(tokens, tok, "keyword", "true") then
		local literal = AstLiteral(tokens, tok, true)
		tok           = tok + 1 -- 'true'
		expr          = literal
	elseif isToken(tokens, tok, "keyword", "false") then
		local literal = AstLiteral(tokens, tok, false)
		tok           = tok + 1 -- 'false'
		expr          = literal
	elseif isToken(tokens, tok, "keyword", "nil") then
		local literal = AstLiteral(tokens, tok, nil)
		tok           = tok + 1 -- 'nil'
		expr          = literal

	-- unary
	elseif
		(isToken(tokens, tok, "keyword", "not") or (isTokenType(tokens, tok, "punctuation") and isTokenAnyValue(tokens, tok, OPERATORS_UNARY)))
		and OPERATOR_PRECEDENCE.unary > lastPrecedence
	then
		local unary    = AstUnary(tokens, tok)
		unary.operator = tokens.value[tok]
		tok            = tok + 1 -- operator

		local subExpr, tokNext, err = parseExpression(tokens, tok, OPERATOR_PRECEDENCE.unary-1)
		if not subExpr then  return nil, tok, err  end
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
		local tableNode, tokNext, err = parseTable(tokens, tok)
		if not tableNode then  return nil, tok, err  end
		tok = tokNext

		expr = tableNode

	-- function
	elseif isToken(tokens, tok, "keyword", "function") then
		tok = tok + 1 -- 'function'

		local func, tokNext, err = parseFunctionParametersAndBody(tokens, tok)
		if not func then  return nil, tok, err  end
		func.token = tok
		tok        = tokNext

		expr = func

	-- (...)
	elseif isToken(tokens, tok, "punctuation", "(") then
		tok = tok + 1 -- '('

		local _expr, tokNext, err = parseExpression(tokens, tok, 0)
		if not _expr then  return nil, tok, err  end
		tok = tokNext

		if _expr.type == "call" or _expr.type == "vararg" then
			_expr.adjustToOne = true
		end

		if not isToken(tokens, tok, "punctuation", ")") then
			return nil, tok, formatErrorAfterToken(tokens, tok-1, "Parser", "Expected ')' (to end parenthesis expression starting %s).", getRelativeLocationTextForToken(tokens, tokStart, tok-1))
		end
		tok = tok + 1 -- ')'

		expr                 = _expr
		canParseLookupOrCall = true

	else
		return nil, tok, formatErrorAtToken(tokens, tok, "Parser", "Failed parsing expression.")
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

			local binary    = AstBinary(tokens, tok)
			binary.operator = tokens.value[tok]
			tok             = tok + 1 -- operator

			local lhsExpr = expr

			local rhsExpr, tokNext, err = parseExpression(tokens, tok, OPERATOR_PRECEDENCE[binary.operator] + (rightAssociative and -1 or 0))
			if not rhsExpr then  return nil, tok, err  end
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
			local lookup = AstLookup(tokens, tok)
			tok          = tok + 1 -- '.'

			local ident, tokNext, err = parseIdentifier(tokens, tok)
			if not ident then  return nil, tok, err  end
			tok = tokNext

			local literal = AstLiteral(tokens, ident.tok, ident.name)

			lookup.object = expr
			lookup.member = literal

			expr = lookup

		-- t[k]
		elseif isToken(tokens, tok, "punctuation", "[") then
			local lookup = AstLookup(tokens, tok)
			tok          = tok + 1 -- '['

			local memberExpr, tokNext, err = parseExpression(tokens, tok, 0)
			if not memberExpr then  return nil, tok, err  end
			tok = tokNext

			if not isToken(tokens, tok, "punctuation", "]") then
				return nil, tok, formatErrorAfterToken(tokens, tok-1, "Parser", "Expected ']' after lookup key value.")
			end
			tok = tok + 1 -- ']'

			lookup.object = expr
			lookup.member = memberExpr

			expr = lookup

		-- f""
		elseif isTokenType(tokens, tok, "string") then
			local call = AstCall(tokens, tok)

			local literal     = AstLiteral(tokens, tok, tokens.value[tok])
			tok               = tok + 1 -- string
			call.arguments[1] = literal

			call.callee = expr
			expr        = call

		-- f{}
		elseif isToken(tokens, tok, "punctuation", "{") then
			local call = AstCall(tokens, tok)

			local tableNode, tokNext, err = parseTable(tokens, tok)
			if not tableNode then  return nil, tok, err  end
			call.arguments[1] = tableNode
			tok               = tokNext

			call.callee = expr
			expr        = call

		-- f()
		elseif isToken(tokens, tok, "punctuation", "(") then
			if tok >= 2 and tokens.lineStart[tok] > tokens.lineEnd[tok-1] then
				return nil, tok, formatErrorAtToken(tokens, tok, "Parser", "Ambigous syntax. Is this a function call or a new statement?")
			end

			local call = AstCall(tokens, tok)
			tok        = tok + 1 -- '('

			if not isToken(tokens, tok, "punctuation", ")") then
				local ok, tokNext, err = parseExpressionList(tokens, tok, call.arguments)
				if not ok then  return nil, tok, err  end
				tok = tokNext
			end

			if not isToken(tokens, tok, "punctuation", ")") then
				return nil, tok, formatErrorAfterToken(tokens, tok-1, "Parser", "Expected ')' to end argument list for call.")
			end
			tok = tok + 1 -- ')'

			call.callee = expr
			expr        = call

		-- o:m()
		elseif isToken(tokens, tok, "punctuation", ":") then
			do
				local lookup = AstLookup(tokens, tok)
				tok          = tok + 1 -- ':'

				local ident, tokNext, err = parseIdentifier(tokens, tok)
				if not ident then  return nil, tok, err  end
				tok = tokNext

				local literal = AstLiteral(tokens, ident.tok, ident.name)

				lookup.object = expr
				lookup.member = literal

				expr = lookup
			end

			do
				local call  = AstCall(tokens, tok)
				call.method = true

				if isTokenType(tokens, tok, "string") then
					local literal     = AstLiteral(tokens, tok, tokens.value[tok])
					tok               = tok + 1 -- string
					call.arguments[1] = literal

				elseif isToken(tokens, tok, "punctuation", "{") then
					local tableNode, tokNext, err = parseTable(tokens, tok)
					if not tableNode then  return nil, tok, err  end
					call.arguments[1] = tableNode
					tok               = tokNext

				elseif isToken(tokens, tok, "punctuation", "(") then
					if tok >= 2 and tokens.lineStart[tok] > tokens.lineEnd[tok-1] then
						return nil, tok, formatErrorAtToken(tokens, tok, "Parser", "Ambigous syntax. Is this a function call or a new statement?")
					end

					tok = tok + 1 -- '('

					if not isToken(tokens, tok, "punctuation", ")") then
						local ok, tokNext, err = parseExpressionList(tokens, tok, call.arguments)
						if not ok then  return nil, tok, err  end
						tok = tokNext
					end

					if not isToken(tokens, tok, "punctuation", ")") then
						return nil, tok, formatErrorAfterToken(tokens, tok-1, "Parser", "Expected ')' after argument list for method call.")
					end
					tok = tok + 1 -- ')'

				else
					return nil, tok, formatErrorAfterToken(tokens, tok-1, "Parser", "Expected '(' to start argument list for method call.")
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

function parseExpressionList(tokens, tok, expressions) --> success, token, error
	while true do
		local expr, tokNext, err = parseExpression(tokens, tok, 0)
		if not expr then  return false, tok, err  end
		tok = tokNext

		tableInsert(expressions, expr)

		if not isToken(tokens, tok, "punctuation", ",") then
			return true, tok
		end
		tok = tok + 1 -- ','
	end
end

function parseFunctionParametersAndBody(tokens, tokStart) --> func, token, error
	local tok  = tokStart
	local func = AstFunction(tokens, tok)

	if not isToken(tokens, tok, "punctuation", "(") then
		return nil, tok, formatErrorAfterToken(tokens, tok-1, "Parser", "Expected '(' to start parameter list for function.")
	end
	tok = tok + 1 -- '('

	if not isToken(tokens, tok, "punctuation", ")") then
		local ok, tokNext, varargOrErr = parseNameList(tokens, tok, func.parameters, true, false)
		if not ok then  return nil, tok, varargOrErr  end
		tok = tokNext

		func.vararg = varargOrErr
	end

	if not isToken(tokens, tok, "punctuation", ")") then
		return nil, tok, formatErrorAfterToken(tokens, tok-1, "Parser", "Expected ')' to end parameter list for function.")
	end
	tok = tok + 1 -- ')'

	local block, tokNext, err = parseBlock(tokens, tok, tok-1, true)
	if not block then  return nil, tok, err  end
	func.body = block
	tok       = tokNext

	if not isToken(tokens, tok, "keyword", "end") then
		return nil, tok, formatErrorAtToken(tokens, tok, "Parser", "Expected 'end' to end function (starting %s).", getRelativeLocationTextForToken(tokens, tokStart, tok))
	end
	tok = tok + 1 -- 'end'

	return func, tok
end

local BLOCK_END_TOKEN_TYPES = newSet{ "end", "else", "elseif", "until" }

local function parseOneOrPossiblyMoreStatements(tokens, tokStart, statements) --> success, token, error  -- The error message may be empty.
	--[[
	stat ::= ';'
	         varlist '=' explist |
	         functioncall |
	         label |
	         break |
	         goto Name |
	         do block end |
	         while exp do block end |
	         repeat block until exp |
	         if exp then block {elseif exp then block} [else block] end |
	         for Name '=' exp ',' exp [',' exp] do block end |
	         for namelist in explist do block end |
	         function funcname funcbody |
	         local function Name funcbody |
	         local attnamelist ['=' explist]

	retstat ::= return [explist] [';']
	]]
	local tok = tokStart

	-- do
	if isToken(tokens, tok, "keyword", "do") then
		tok = tok + 1 -- 'do'

		local block, tokNext, err = parseBlock(tokens, tok, tok-1, true)
		if not block then  return false, tok, err  end
		block.token = tok - 1
		tok         = tokNext

		if not isToken(tokens, tok, "keyword", "end") then
			return false, tok, formatErrorAtToken(tokens, tok, "Parser", "Expected 'end' to end 'do' block (starting %s).", getRelativeLocationTextForToken(tokens, tokStart, tok))
		end
		tok = tok + 1 -- 'end'

		tableInsert(statements, block)
		return true, tok

	-- while
	elseif isToken(tokens, tok, "keyword", "while") then
		local whileLoop = AstWhile(tokens, tok)
		tok             = tok + 1 -- 'while'

		local expr, tokNext, err = parseExpression(tokens, tok, 0)
		if not expr then  return false, tok, err  end
		whileLoop.condition = expr
		tok                 = tokNext

		if not isToken(tokens, tok, "keyword", "do") then
			return false, tok, formatErrorAfterToken(tokens, tok-1, "Parser", "Expected 'do' to start body for 'while' loop.")
		end
		tok = tok + 1 -- 'do'

		local block, tokNext, err = parseBlock(tokens, tok, tok-1, true)
		if not block then  return false, tok, err  end
		block.token    = tok - 1
		whileLoop.body = block
		tok            = tokNext

		if not isToken(tokens, tok, "keyword", "end") then
			return false, tok, formatErrorAtToken(tokens, tok, "Parser", "Expected 'end' to end 'while' loop (starting %s).", getRelativeLocationTextForToken(tokens, tokStart, tok))
		end
		tok = tok + 1 -- 'end'

		tableInsert(statements, whileLoop)
		return true, tok

	-- repeat
	elseif isToken(tokens, tok, "keyword", "repeat") then
		local repeatLoop = AstRepeat(tokens, tok)
		tok              = tok + 1 -- 'repeat'

		local block, tokNext, err = parseBlock(tokens, tok, tok-1, true)
		if not block then  return false, tok, err  end
		repeatLoop.body = block
		tok             = tokNext

		if not isToken(tokens, tok, "keyword", "until") then
			return false, tok, formatErrorAtToken(tokens, tok, "Parser", "Expected 'until' at the end of 'repeat' loop (starting %s).", getRelativeLocationTextForToken(tokens, tokStart, tok))
		end
		tok = tok + 1 -- 'until'

		local expr, tokNext, err = parseExpression(tokens, tok, 0)
		if not expr then  return false, tok, err  end
		repeatLoop.condition = expr
		tok                  = tokNext

		tableInsert(statements, repeatLoop)
		return true, tok

	-- if
	elseif isToken(tokens, tok, "keyword", "if") then
		local ifNode = AstIf(tokens, tok)
		tok          = tok + 1 -- 'if'

		local expr, tokNext, err = parseExpression(tokens, tok, 0)
		if not expr then  return false, tok, err  end
		ifNode.condition = expr
		tok              = tokNext

		if not isToken(tokens, tok, "keyword", "then") then
			return false, tok, formatErrorAtToken(tokens, tok, "Parser", "Expected 'then' after 'if' condition.")
		end
		tok = tok + 1 -- 'then'

		local block, tokNext, err = parseBlock(tokens, tok, tok-1, true)
		if not block then  return false, tok, err  end
		ifNode.bodyTrue = block
		tok             = tokNext

		local ifNodeLeaf = ifNode

		while isToken(tokens, tok, "keyword", "elseif") do
			tok = tok + 1 -- 'elseif'

			ifNodeLeaf.bodyFalse               = AstBlock(tokens, tok)
			ifNodeLeaf.bodyFalse.statements[1] = AstIf(tokens, tok)
			ifNodeLeaf                         = ifNodeLeaf.bodyFalse.statements[1]

			local expr, tokNext, err = parseExpression(tokens, tok, 0)
			if not expr then  return false, tok, err  end
			ifNodeLeaf.condition = expr
			tok                  = tokNext

			if not isToken(tokens, tok, "keyword", "then") then
				return false, tok, formatErrorAtToken(tokens, tok, "Parser", "Expected 'then' after 'elseif' condition.")
			end
			tok = tok + 1 -- 'then'

			local block, tokNext, err = parseBlock(tokens, tok, tok-1, true)
			if not block then  return false, tok, err  end
			ifNodeLeaf.bodyTrue = block
			tok                 = tokNext
		end

		if isToken(tokens, tok, "keyword", "else") then
			tok = tok + 1 -- 'else'

			local block, tokNext, err = parseBlock(tokens, tok, tok-1, true)
			if not block then  return false, tok, err  end
			ifNodeLeaf.bodyFalse = block
			tok                  = tokNext
		end

		if not isToken(tokens, tok, "keyword", "end") then
			return false, tok, formatErrorAtToken(tokens, tok, "Parser", "Expected 'end' to end 'if' statement (starting %s).", getRelativeLocationTextForToken(tokens, tokStart, tok))
		end
		tok = tok + 1 -- 'end'

		tableInsert(statements, ifNode)
		return true, tok

	-- for
	elseif isToken(tokens, tok, "keyword", "for") then
		local forLoop = AstFor(tokens, tok)
		tok           = tok + 1 -- 'for'

		local ok, tokNext, err = parseNameList(tokens, tok, forLoop.names, false, false)
		if not ok then  return false, tok, err  end
		tok = tokNext

		if isToken(tokens, tok, "keyword", "in") then
			forLoop.kind = "generic"
			tok          = tok + 1 -- 'in'

		elseif isToken(tokens, tok, "punctuation", "=") then
			if forLoop.names[2] then
				return false, tok, formatErrorAtToken(tokens, tok, "Parser", "Expected 'in' for generic loop.")
			end

			forLoop.kind = "numeric"
			tok          = tok + 1 -- '='

		else
			return false, tok, formatErrorAfterToken(tokens, tok-1, "Parser", "Expected '=' or 'in' for 'for' loop.")
		end

		local valuesStartTok = tok

		local ok, tokNext, err = parseExpressionList(tokens, tok, forLoop.values, 0)
		if not ok then  return false, tok, err  end
		tok = tokNext

		if forLoop.kind ~= "numeric" then
			-- void
		elseif not forLoop.values[2] then
			return false, tok, formatErrorAtToken(tokens, valuesStartTok, "Parser", "Numeric loop: Too few values.")
		elseif forLoop.values[4] then
			-- @Cleanup: Instead of using getLeftmostToken(), make parseExpressionList() return a list of expression start tokens.
			return false, tok, formatErrorAtToken(tokens, getLeftmostToken(forLoop.values[4]), "Parser", "Numeric loop: Too many values.")
		end

		if not isToken(tokens, tok, "keyword", "do") then
			return false, tok, formatErrorAfterToken(tokens, tok-1, "Parser", "Expected 'do' to start body for 'for' loop.")
		end
		tok = tok + 1 -- 'do'

		local block, tokNext, err = parseBlock(tokens, tok, tok-1, true)
		if not block then  return false, tok, err  end
		forLoop.body = block
		tok          = tokNext

		if not isToken(tokens, tok, "keyword", "end") then
			return false, tok, formatErrorAtToken(tokens, tok, "Parser", "Expected 'end' to end 'for' loop (starting %s).", getRelativeLocationTextForToken(tokens, tokStart, tok))
		end
		tok = tok + 1 -- 'end'

		tableInsert(statements, forLoop)
		return true, tok

	-- function
	elseif isToken(tokens, tok, "keyword", "function") then
		local assignment = AstAssignment(tokens, tok)
		tok              = tok + 1 -- 'function'

		local targetExpr, tokNext, err = parseIdentifier(tokens, tok)
		if not targetExpr then  return false, tok, err  end
		tok = tokNext

		while isToken(tokens, tok, "punctuation", ".") do
			local lookup = AstLookup(tokens, tok)
			tok          = tok + 1 -- '.'

			local ident, tokNext, err = parseIdentifier(tokens, tok)
			if not ident then  return false, tok, err  end
			tok = tokNext

			local literal = AstLiteral(tokens, ident.tok, ident.name)
			lookup.member = literal

			lookup.object = targetExpr
			lookup.member = literal

			targetExpr = lookup
		end

		local isMethod = isToken(tokens, tok, "punctuation", ":")

		if isMethod then
			local lookup = AstLookup(tokens, tok)
			tok          = tok + 1 -- ':'

			local ident, tokNext, err = parseIdentifier(tokens, tok)
			if not ident then  return false, tok, err  end
			tok = tokNext

			local literal = AstLiteral(tokens, ident.tok, ident.name)
			lookup.member = literal

			lookup.object = targetExpr
			lookup.member = literal

			targetExpr = lookup
		end

		local func, tokNext, err = parseFunctionParametersAndBody(tokens, tok)
		if not func then  return false, tok, err  end
		tok = tokNext

		if isMethod then
			local ident = AstIdentifier(tokens, func.token, "self")
			tableInsert(func.parameters, 1, ident)
		end

		assignment.targets[1] = targetExpr
		assignment.values[1]  = func

		tableInsert(statements, assignment)
		return true, tok

	-- local function
	elseif isToken(tokens, tok, "keyword", "local") and isToken(tokens, tok+1, "keyword", "function") then
		local decl       = AstDeclaration(tokens, tok)
		local assignment = AstAssignment(tokens, tok)
		tok              = tok + 2 -- 'local function'

		local ident, tokNext, err = parseIdentifier(tokens, tok)
		if not ident then  return false, tok, err  end
		local identCopy = parseIdentifier(tokens, tok)
		tok             = tokNext

		local func, tokNext, err = parseFunctionParametersAndBody(tokens, tok)
		if not func then  return false, tok, err  end
		tok = tokNext

		decl.names[1]         = ident
		assignment.targets[1] = identCopy
		assignment.values[1]  = func

		tableInsert(statements, decl)
		tableInsert(statements, assignment)
		return true, tok

	-- local
	elseif isToken(tokens, tok, "keyword", "local") then
		local decl = AstDeclaration(tokens, tok)
		tok        = tok + 1 -- 'local'

		local ok, tokNext, err = parseNameList(tokens, tok, decl.names, false, true)
		if not ok then  return false, tok, err  end
		tok = tokNext

		if isToken(tokens, tok, "punctuation", "=") then
			tok = tok + 1 -- '='

			local ok, tokNext, err = parseExpressionList(tokens, tok, decl.values)
			if not ok then  return false, tok, err  end
			tok = tokNext
		end

		tableInsert(statements, decl)
		return true, tok

	-- ::label::
	elseif isToken(tokens, tok, "punctuation", "::") then
		local label = AstLabel(tokens, tok)
		tok         = tok + 1 -- '::'

		local labelIdent, tokNext, err = parseIdentifier(tokens, tok)
		if not labelIdent then  return false, tok, err  end
		tok = tokNext

		label.name = labelIdent.name

		if not isToken(tokens, tok, "punctuation", "::") then
			return false, tok, formatErrorAfterToken(tokens, tok-1, "Parser", "Expected '::' after label name.")
		end
		tok = tok + 1 -- '::'

		tableInsert(statements, label)
		return true, tok

	-- goto
	elseif isToken(tokens, tok, "keyword", "goto") then
		local gotoNode = AstGoto(tokens, tok)
		tok            = tok + 1 -- 'goto'

		local labelIdent, tokNext, err = parseIdentifier(tokens, tok)
		if not labelIdent then  return false, tok, err  end
		tok = tokNext

		gotoNode.name = labelIdent.name

		tableInsert(statements, gotoNode)
		return true, tok

	-- return (last)
	elseif isToken(tokens, tok, "keyword", "return") then
		local returnNode = AstReturn(tokens, tok)
		tok              = tok + 1 -- 'return'

		if tok <= tokens.n and not ((isTokenType(tokens, tok, "keyword") and isTokenAnyValue(tokens, tok, BLOCK_END_TOKEN_TYPES)) or isToken(tokens, tok, "punctuation", ";")) then
			local ok, tokNext, err = parseExpressionList(tokens, tok, returnNode.values)
			if not ok then  return false, tok, err  end
			tok = tokNext
		end

		tableInsert(statements, returnNode)
		return true, tok

	-- break (last)
	elseif isToken(tokens, tok, "keyword", "break") then
		local breakNode = AstBreak(tokens, tok)
		tok             = tok + 1 -- 'break'

		tableInsert(statements, breakNode)
		return true, tok

	elseif isTokenType(tokens, tok, "keyword") then
		return false, tok, ""

	else
		local lookahead, tokNext, err = parseExpression(tokens, tok, 0)
		if not lookahead then  return false, tok, err  end

		if lookahead.type == "call" then
			local call = lookahead
			tok        = tokNext

			tableInsert(statements, call)
			return true, tok

		elseif isToken(tokens, tokNext, "punctuation", "=") or isToken(tokens, tokNext, "punctuation", ",") then
			local assignment = AstAssignment(tokens, tokNext)

			local ok, tokNext, err = parseExpressionList(tokens, tok, assignment.targets)
			if not ok then  return false, tok, err  end
			tok = tokNext

			if not isToken(tokens, tok, "punctuation", "=") then
				return false, tok, formatErrorAfterToken(tokens, tok-1, "Parser", "Expected '=' for an assignment.")
			end
			tok = tok + 1 -- '='

			for _, targetExpr in ipairs(assignment.targets) do
				if not (targetExpr.type == "identifier" or targetExpr.type == "lookup") then
					return false, tok, formatErrorAtNode(targetExpr, "Parser", "Invalid assignment target.")
				end
			end

			local ok, tokNext, err = parseExpressionList(tokens, tok, assignment.values)
			if not ok then  return false, tok, err  end
			tok = tokNext

			tableInsert(statements, assignment)
			return true, tok

		else
			return false, tok, ""
		end
	end

	assert(false)
end

local statementErrorReported = false

function parseBlock(tokens, tok, blockTok, stopAtEndKeyword) --> block, token, error
	local block      = AstBlock(tokens, blockTok)
	local statements = block.statements

	while tok <= tokens.n do
		while isToken(tokens, tok, "punctuation", ";") do
			-- Empty statements are valid in Lua 5.2+.
			tok = tok + 1 -- ';'
		end

		local statementStartTok = tok

		if stopAtEndKeyword and isTokenType(tokens, tok, "keyword") and isTokenAnyValue(tokens, tok, BLOCK_END_TOKEN_TYPES) then
			break
		end

		local ok, tokNext, err = parseOneOrPossiblyMoreStatements(tokens, tok, statements)
		if not ok then
			if not statementErrorReported then
				statementErrorReported = true
				err                    = (err ~= "" and err.."\n" or "") .. formatErrorAtToken(tokens, tok, "Parser", "Failed parsing statement.")
			end
			return nil, tok, err
		end
		tok = tokNext

		if isToken(tokens, tok, "punctuation", ";") then
			tok = tok + 1 -- ';'
		end

		local lastAddedStatement = statements[#statements]

		if lastAddedStatement.type == "return" then -- Note: 'break' statements are allowed in the middle of blocks as of Lua 5.2.
			break

		elseif lastAddedStatement.type == "call" and lastAddedStatement.adjustToOne then
			statementErrorReported = true
			return nil, tok, formatErrorAtToken(tokens, statementStartTok, "Parser", "Syntax error.")
		end
	end

	return block, tok
end

-- block, error = tokensToAst( tokens )
local function tokensToAst(tokens)
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

	statementErrorReported = false

	local block, _, err = parseBlock(tokensPurged, 1, 1, false)
	if not block then  return nil, err  end

	return block
end

-- ast, error = parse( tokens )
-- ast, error = parse( luaString [, pathForErrorMessages="?" ] )
function parse(luaOrTokens, path)
	assertArg("parse", 1, luaOrTokens, "string","table")

	-- ast, error = parse( tokens )
	if type(luaOrTokens) == "table" then
		assertArg1("parse", 2, path, "nil")

		return tokensToAst(luaOrTokens)

	-- ast, error = parse( luaString, pathForErrorMessages )
	else
		if path == nil then
			path = "?"
		else
			assertArg1("parse", 2, path, "string")
		end

		local tokens, err = tokenize(luaOrTokens, path)
		if not tokens then  return nil, err  end

		return tokensToAst(tokens)
	end
end

-- ast, error = parseFile( path )
function parseFile(path)
	assertArg1("parseFile", 1, path, "string")

	local tokens, err = tokenizeFile(path)
	if not tokens then  return nil, err  end

	return tokensToAst(tokens)
end



--
-- :NodeCreation
--
-- identifier   = newNode( "identifier", name [, attributeName="" ] )  -- 'attributeName' can be "close", "const" or "".
-- vararg       = newNode( "vararg" )
-- literal      = newNode( "literal", value )  -- 'value' must be a number, a string, a boolean or nil.
-- tableNode    = newNode( "table" )
-- lookup       = newNode( "lookup" )
-- unary        = newNode( "unary",  unaryOperator  )
-- binary       = newNode( "binary", binaryOperator )
-- call         = newNode( "call" )
-- functionNode = newNode( "function" )
-- breakNode    = newNode( "break" )
-- returnNode   = newNode( "return" )
-- label        = newNode( "label", labelName )
-- gotoNode     = newNode( "goto",  labelName )
-- block        = newNode( "block" )
-- declaration  = newNode( "declaration" )
-- assignment   = newNode( "assignment" )
-- ifNode       = newNode( "if" )
-- whileLoop    = newNode( "while" )
-- repeatLoop   = newNode( "repeat" )
-- forLoop      = newNode( "for", forLoopKind )  -- 'forLoopKind' can be "numeric" or "generic".
--
-- Search for 'NodeFields' for each node's fields.
--
local function newNode(nodeType, ...)
	local node

	if     nodeType == "vararg"      then  node = AstVararg     (dummyTokens, 0)
	elseif nodeType == "table"       then  node = AstTable      (dummyTokens, 0)
	elseif nodeType == "lookup"      then  node = AstLookup     (dummyTokens, 0)
	elseif nodeType == "call"        then  node = AstCall       (dummyTokens, 0)
	elseif nodeType == "function"    then  node = AstFunction   (dummyTokens, 0)
	elseif nodeType == "break"       then  node = AstBreak      (dummyTokens, 0)
	elseif nodeType == "return"      then  node = AstReturn     (dummyTokens, 0)
	elseif nodeType == "block"       then  node = AstBlock      (dummyTokens, 0)
	elseif nodeType == "declaration" then  node = AstDeclaration(dummyTokens, 0)
	elseif nodeType == "assignment"  then  node = AstAssignment (dummyTokens, 0)
	elseif nodeType == "if"          then  node = AstIf         (dummyTokens, 0)
	elseif nodeType == "while"       then  node = AstWhile      (dummyTokens, 0)
	elseif nodeType == "repeat"      then  node = AstRepeat     (dummyTokens, 0)

	elseif nodeType == "identifier" then
		if select("#", ...) == 0 then
			errorf(2, "Missing name argument for identifier.")
		end

		local name, attribute = ...

		if type(name) ~= "string" then
			errorf(2, "Invalid name argument value type '%s'. (Expected string)", type(name))
		elseif not stringFind(name, "^[%a_][%w_]*$") or KEYWORDS[name] then
			errorf(2, "Invalid identifier name '%s'.", name)
		end

		if attribute == nil or attribute == "" then
			-- void
		elseif type(attribute) ~= "string" then
			errorf(2, "Invalid attribute argument value type '%s'. (Expected string)", type(attribute))
		elseif not (attribute == "close" or attribute == "const") then
			errorf(2, "Invalid attribute name '%s'. (Must be 'close' or 'const'.)", attribute)
		end

		node           = AstIdentifier(dummyTokens, 0, name)
		node.attribute = attribute or ""

	elseif nodeType == "label" then
		if select("#", ...) == 0 then
			errorf(2, "Missing name argument for label.")
		end

		local name = ...
		if type(name) ~= "string" then
			errorf(2, "Invalid name argument value type '%s'. (Expected string)", type(name))
		elseif not stringFind(name, "^[%a_][%w_]*$") or KEYWORDS[name] then
			errorf(2, "Invalid label name '%s'.", name)
		end

		node      = AstLabel(dummyTokens, 0)
		node.name = name

	elseif nodeType == "goto" then
		if select("#", ...) == 0 then
			errorf(2, "Missing label name argument for goto.")
		end

		local name = ...
		if type(name) ~= "string" then
			errorf(2, "Invalid label name argument value type '%s'. (Expected string)", type(name))
		elseif not stringFind(name, "^[%a_][%w_]*$") or KEYWORDS[name] then
			errorf(2, "Invalid label name '%s'.", name)
		end

		node      = AstGoto(dummyTokens, 0)
		node.name = name

	elseif nodeType == "literal" then
		if select("#", ...) == 0 then
			errorf(2, "Missing value argument for literal.")
		end

		local value = ...
		if not (type(value) == "number" or type(value) == "string" or type(value) == "boolean" or type(value) == "nil") then
			errorf(2, "Invalid literal value type '%s'. (Expected number, string, boolean or nil)", type(value))
		end

		node = AstLiteral(dummyTokens, 0, value)

	elseif nodeType == "unary" then
		if select("#", ...) == 0 then
			errorf(2, "Missing operator argument for unary expression.")
		end

		local op = ...
		if not OPERATORS_UNARY[op] then
			errorf(2, "Invalid unary operator '%s'.", tostring(op))
		end

		node          = AstUnary(dummyTokens, 0)
		node.operator = op

	elseif nodeType == "binary" then
		if select("#", ...) == 0 then
			errorf(2, "Missing operator argument for binary expression.")
		end

		local op = ...
		if not OPERATORS_BINARY[op] then
			errorf(2, "Invalid binary operator '%s'.", tostring(op))
		end

		node          = AstBinary(dummyTokens, 0)
		node.operator = op

	elseif nodeType == "for" then
		if select("#", ...) == 0 then
			errorf(2, "Missing kind argument for 'for' loop.")
		end

		local kind = ...
		if not (kind == "numeric" or kind == "generic") then
			errorf(2, "Invalid for loop kind '%s'. (Must be 'numeric' or 'generic')", tostring(kind))
		end

		node      = AstFor(dummyTokens, 0)
		node.kind = kind

	else
		errorf(2, "Invalid node type '%s'.", tostring(nodeType))
	end
	return node
end



do
	local NL_AND_CR_TO_READABLE = {
		["\n"] = "{NL}",
		["\r"] = "{CR}",
	}

	local function _printNode(node)
		local nodeType = node.type

		ioWrite(nodeType)

		if parser.printIds then  ioWrite("#", node.id)  end

		-- if mayNodeBeInvolvedInJump(node) then  ioWrite("[MAYJUMP]")  end -- DEBUG

		if nodeType == "identifier" then
			ioWrite(" (", node.name, ")")

			if node.declaration then
				ioWrite(" (decl=", node.declaration.type)
				if parser.printIds then  ioWrite("#", node.declaration.id)  end
				ioWrite(")")
			end

		elseif nodeType == "vararg" then
			if node.adjustToOne then  ioWrite(" (adjustToOne)")  end

		elseif nodeType == "literal" then
			if node.value == nil or node.value == true or node.value == false then
				ioWrite(" (", tostring(node.value), ")")
			elseif type(node.value) == "string" then
				ioWrite(' (string="', stringGsub(node.value, "[\n\r]", NL_AND_CR_TO_READABLE), '")')
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

		elseif nodeType == "label" then
			ioWrite(" (", node.name, ")")

		elseif nodeType == "goto" then
			ioWrite(" (", node.name, ")")

			if node.label then
				ioWrite(" (label")
				if parser.printIds then  ioWrite("#", node.label.id)  end
				ioWrite(")")
			end
		end

		if parser.printLocations then  ioWrite(" @ ", node.sourcePath, ":", node.line)  end

		ioWrite("\n")
	end

	local function _printTree(node, indent, key)
		for i = 1, indent do  ioWrite(parser.indentation)  end
		indent = indent+1

		if key ~= nil then
			ioWrite(tostring(key))
			ioWrite(" ")
		end

		_printNode(node)

		local nodeType = node.type

		if nodeType == "table" then
			for i, tableField in ipairs(node.fields) do
				if tableField.key   then  _printTree(tableField.key,   indent, i..(tableField.generatedKey and "KEYGEN" or "KEY  "))  end
				if tableField.value then  _printTree(tableField.value, indent, i..(                                   "VALUE"))  end
				local a =  {1, 5, g=6}
			end

		elseif nodeType == "lookup" then
			if node.object then  _printTree(node.object, indent, "OBJECT")  end
			if node.member then  _printTree(node.member, indent, "MEMBER")  end

		elseif nodeType == "unary" then
			if node.expression then  _printTree(node.expression, indent, nil)  end

		elseif nodeType == "binary" then
			if node.left  then  _printTree(node.left,  indent, nil)  end
			for i = 1, indent do  ioWrite(parser.indentation)  end ; ioWrite(node.operator, "\n")
			if node.right then  _printTree(node.right, indent, nil)  end

		elseif nodeType == "call" then
			if node.callee then  _printTree(node.callee, indent, "CALLEE")  end
			for i, expr in ipairs(node.arguments) do  _printTree(expr, indent, "ARG"..i)  end

		elseif nodeType == "function" then
			for i, ident in ipairs(node.parameters) do  _printTree(ident, indent, "PARAM"..i)  end
			if node.body then  _printTree(node.body, indent, "BODY")  end

		elseif nodeType == "return" then
			for i, expr in ipairs(node.values) do  _printTree(expr, indent, tostring(i))  end

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



-- didStop = traverseTree( astNode, [ leavesFirst=false, ] callback [, topNodeParent=nil, topNodeContainer=nil, topNodeKey=nil ] )
-- action  = callback( astNode, parent, container, key )
-- action  = "stop"|"ignorechildren"|nil  -- Returning nil (or nothing) means continue traversal.
function traverseTree(node, leavesFirst, cb, parent, container, k)
	assertArg1("traverseTree", 1, node, "table")

	if type(leavesFirst) == "boolean" then
		assertArg1("traverseTree", 3, cb, "function")
	else
		leavesFirst, cb, parent, container, k = false, leavesFirst, cb, parent, container
		assertArg1("traverseTree", 2, cb, "function")
	end

	if not leavesFirst then
		local action = cb(node, parent, container, k)
		if action == "stop"           then  return true   end
		if action == "ignorechildren" then  return false  end
		if action                     then  errorf("Unknown traversal action '%s' returned from callback.", tostring(action))  end
	end

	local nodeType = node.type

	if nodeType == "identifier" or nodeType == "vararg" or nodeType == "literal" or nodeType == "break" or nodeType == "label" or nodeType == "goto" then
		-- void  No child nodes.

	elseif nodeType == "table" then
		for _, tableField in ipairs(node.fields) do
			if tableField.key   and traverseTree(tableField.key,   leavesFirst, cb, node, tableField, "key")   then  return true  end
			if tableField.value and traverseTree(tableField.value, leavesFirst, cb, node, tableField, "value") then  return true  end
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
		errorf("Invalid node type '%s'.", tostring(nodeType))
	end

	if leavesFirst then
		local action = cb(node, parent, container, k)
		if action == "stop"           then  return true   end
		if action == "ignorechildren" then  errorf("Cannot ignore children when leavesFirst is set.")  end
		if action                     then  errorf("Unknown traversal action '%s' returned from callback.", tostring(action))  end
	end

	return false
end

-- didStop = traverseTreeReverse( astNode, [ leavesFirst=false, ] callback [, topNodeParent=nil, topNodeContainer=nil, topNodeKey=nil ] )
-- action  = callback( astNode, parent, container, key )
-- action  = "stop"|"ignorechildren"|nil  -- Returning nil (or nothing) means continue traversal.
function traverseTreeReverse(node, leavesFirst, cb, parent, container, k)
	assertArg1("traverseTreeReverse", 1, node, "table")

	if type(leavesFirst) == "boolean" then
		assertArg1("traverseTreeReverse", 3, cb, "function")
	else
		leavesFirst, cb, parent, container, k = false, leavesFirst, cb, parent, container
		assertArg1("traverseTreeReverse", 2, cb, "function")
	end

	if not leavesFirst then
		local action = cb(node, parent, container, k)
		if action == "stop"           then  return true   end
		if action == "ignorechildren" then  return false  end
		if action                     then  errorf("Unknown traversal action '%s' returned from callback.", tostring(action))  end
	end

	local nodeType = node.type

	if nodeType == "identifier" or nodeType == "vararg" or nodeType == "literal" or nodeType == "break" or nodeType == "label" or nodeType == "goto" then
		-- void  No child nodes.

	elseif nodeType == "table" then
		for _, tableField in ipairsr(node.fields) do
			if tableField.value and traverseTreeReverse(tableField.value, leavesFirst, cb, node, tableField, "value") then  return true  end
			if tableField.key   and traverseTreeReverse(tableField.key,   leavesFirst, cb, node, tableField, "key")   then  return true  end
		end

	elseif nodeType == "lookup" then
		if node.member and traverseTreeReverse(node.member, leavesFirst, cb, node, node, "member") then  return true  end
		if node.object and traverseTreeReverse(node.object, leavesFirst, cb, node, node, "object") then  return true  end

	elseif nodeType == "unary" then
		if node.expression and traverseTreeReverse(node.expression, leavesFirst, cb, node, node, "expression") then  return true  end

	elseif nodeType == "binary" then
		if node.right and traverseTreeReverse(node.right, leavesFirst, cb, node, node, "right") then  return true  end
		if node.left  and traverseTreeReverse(node.left,  leavesFirst, cb, node, node, "left")  then  return true  end

	elseif nodeType == "call" then
		for i, expr in ipairsr(node.arguments) do
			if traverseTreeReverse(expr, leavesFirst, cb, node, node.arguments, i) then  return true  end
		end
		if node.callee and traverseTreeReverse(node.callee, leavesFirst, cb, node, node, "callee") then  return true  end

	elseif nodeType == "function" then
		if node.body   and traverseTreeReverse(node.body,   leavesFirst, cb, node, node, "body")   then  return true  end
		if node.vararg and traverseTreeReverse(node.vararg, leavesFirst, cb, node, node, "vararg") then  return true  end
		for i, name in ipairsr(node.parameters) do
			if traverseTreeReverse(name, leavesFirst, cb, node, node.parameters, i) then  return true  end
		end

	elseif nodeType == "return" then
		for i, expr in ipairsr(node.values) do
			if traverseTreeReverse(expr, leavesFirst, cb, node, node.values, i) then  return true  end
		end

	elseif nodeType == "block" then
		for i, statement in ipairsr(node.statements) do
			if traverseTreeReverse(statement, leavesFirst, cb, node, node.statements, i) then  return true  end
		end

	elseif nodeType == "declaration" then
		for i, expr in ipairsr(node.values) do
			if traverseTreeReverse(expr, leavesFirst, cb, node, node.values, i) then  return true  end
		end
		for i, ident in ipairsr(node.names) do
			if traverseTreeReverse(ident, leavesFirst, cb, node, node.names, i) then  return true  end
		end

	elseif nodeType == "assignment" then
		for i, expr in ipairsr(node.values) do
			if traverseTreeReverse(expr, leavesFirst, cb, node, node.values, i) then  return true  end
		end
		for i, expr in ipairsr(node.targets) do
			if traverseTreeReverse(expr, leavesFirst, cb, node, node.targets, i) then  return true  end
		end

	elseif nodeType == "if" then
		if node.bodyFalse and traverseTreeReverse(node.bodyFalse, leavesFirst, cb, node, node, "bodyFalse") then  return true  end
		if node.bodyTrue  and traverseTreeReverse(node.bodyTrue,  leavesFirst, cb, node, node, "bodyTrue")  then  return true  end
		if node.condition and traverseTreeReverse(node.condition, leavesFirst, cb, node, node, "condition") then  return true  end

	elseif nodeType == "while" then
		if node.body      and traverseTreeReverse(node.body,      leavesFirst, cb, node, node, "body")      then  return true  end
		if node.condition and traverseTreeReverse(node.condition, leavesFirst, cb, node, node, "condition") then  return true  end

	elseif nodeType == "repeat" then
		if node.condition and traverseTreeReverse(node.condition, leavesFirst, cb, node, node, "condition") then  return true  end
		if node.body      and traverseTreeReverse(node.body,      leavesFirst, cb, node, node, "body")      then  return true  end

	elseif nodeType == "for" then
		if node.body and traverseTreeReverse(node.body, leavesFirst, cb, node, node, "body") then  return true  end
		for i, expr in ipairsr(node.values) do
			if traverseTreeReverse(expr, leavesFirst, cb, node, node.values, i) then  return true  end
		end
		for i, ident in ipairsr(node.names) do
			if traverseTreeReverse(ident, leavesFirst, cb, node, node.names, i) then  return true  end
		end

	else
		errorf("Invalid node type '%s'.", tostring(nodeType))
	end

	if leavesFirst then
		local action = cb(node, parent, container, k)
		if action == "stop"           then  return true   end
		if action == "ignorechildren" then  errorf("Cannot ignore children when leavesFirst is set.")  end
		if action                     then  errorf("Unknown traversal action '%s' returned from callback.", tostring(action))  end
	end

	return false
end



-- decl|func|forLoop|nil = findDeclaration(ident)
local function findDeclaration(ident)
	local name   = ident.name
	local parent = ident

	while true do
		local lastChild = parent
		parent          = parent.parent

		if not parent then  return nil  end

		if parent.type == "declaration" then
			local decl = parent
			if lastChild.container == decl.names and itemWith1(decl.names, "name", name) then
				return decl
			end

		elseif parent.type == "function" then
			local func = parent
			if itemWith1(func.parameters, "name", name) then
				return func
			end

		elseif parent.type == "for" then
			local forLoop = parent
			if itemWith1(forLoop.names, "name", name) then
				return forLoop
			end

		elseif parent.type == "block" then
			local block = parent

			for i = lastChild.key-1, 1, -1 do
				local statement = block.statements[i]

				if statement.type == "declaration" then
					local decl = statement
					if itemWith1(decl.names, "name", name) then
						return decl
					end
				end
			end

		elseif parent.type == "repeat" then
			local repeatLoop = parent

			if lastChild == repeatLoop.condition then
				local block = repeatLoop.body

				for i = #block.statements, 1, -1 do
					local statement = block.statements[i]

					if statement.type == "declaration" then
						local decl = statement
						if itemWith1(decl.names, "name", name) then  return decl  end
					end
				end
			end
		end
	end
end

local function findLabel(gotoNode)
	local name   = gotoNode.name
	local parent = gotoNode

	while true do
		local lastChild = parent
		parent          = parent.parent

		if not parent then  return nil  end

		if parent.type == "block" then
			local block = parent

			for _, statement in ipairs(block.statements) do
				if statement.type == "label" and statement.name == name then
					return statement
				end
			end

		elseif parent.type == "function" then
			return nil
		end
	end
end

function updateReferences(node, updateTopNodePosition)
	local topNodeParent    = nil
	local topNodeContainer = nil
	local topNodeKey       = nil

	if updateTopNodePosition == false then
		topNodeParent    = node.parent
		topNodeContainer = node.container
		topNodeKey       = node.key
	end

	traverseTree(node, function(node, parent, container, key)
		node.parent    = parent
		node.container = container
		node.key       = key

		if node.type == "identifier" then
			local ident       = node
			ident.declaration = findDeclaration(ident) -- We can call this because all parents and previous nodes already have their references updated at this point.

			--[[ DEBUG
			print(F(
				"%-10s  %-12s  %s",
				ident.name,
				(        ident.declaration and ident.declaration.type or ""),
				tostring(ident.declaration and ident.declaration.id   or "")
			))
			--]]

		elseif node.type == "goto" then
			local gotoNode = node
			gotoNode.label = findLabel(gotoNode) -- We can call this because all relevant 'parent' references have been updated at this point.
		end
	end, topNodeParent, topNodeContainer, topNodeKey)
end



local PATTERN_INT_TO_HEX = F("%%0%dx", INT_SIZE/4)

local HEX_DIGIT_TO_BITS = {
	["0"]={0,0,0,0}, ["1"]={0,0,0,1}, ["2"]={0,0,1,0}, ["3"]={0,0,1,1}, ["4"]={0,1,0,0}, ["5"]={0,1,0,1}, ["6"]={0,1,1,0}, ["7"]={0,1,1,1},
	["8"]={1,0,0,0}, ["9"]={1,0,0,1}, ["a"]={1,0,1,0}, ["b"]={1,0,1,1}, ["c"]={1,1,0,0}, ["d"]={1,1,0,1}, ["e"]={1,1,1,0}, ["f"]={1,1,1,1},
}

local function intToBits(n, bitsOut)
	local hexNumber = stringSub(F(PATTERN_INT_TO_HEX, maybeWrapInt(n)), -INT_SIZE/4) -- The stringSub() call is probably not needed, but just to be safe.
	local i         = 1

	for hexDigit in stringGmatch(hexNumber, ".") do
		local bits = HEX_DIGIT_TO_BITS[hexDigit]

		for iOffset = 1, 4 do
			bitsOut[i] = bits[iOffset]
			i          = i + 1
		end
	end
end

local function bitsToInt(bits)
	local n     = 0
	local multi = 1

	for i = INT_SIZE, 2, -1 do
		n     = n + multi * bits[i]
		multi = multi * 2
	end

	return (bits[1] == 1 and MIN_INT+n or n)
end

local function isValueNumberOrString(v)
	return type(v) == "number" or type(v) == "string"
end
local function isValueFiniteNumber(v)
	return type(v) == "number" and v == v and v ~= 1/0 and v ~= -1/0
end
local function isValueNumberLike(v)
	return tonumber(v) ~= nil
end
local function areValuesNumbersOrStringsAndOfSameType(v1, v2)
	local type1 = type(v1)
	return type1 == type(v2) and (type1 == "number" or type1 == "string")
end

local bits1 = {}
local bits2 = {}

local unaryFolders = {
	["-"] = function(unary, expr)
		if expr.type == "literal" and isValueNumberLike(expr.value) then
			return AstLiteral(dummyTokens, unary.token, -expr.value)
		end
		return nil
	end,
	["not"] = function(unary, expr)
		if expr.type == "literal" then
			return AstLiteral(dummyTokens, unary.token, not expr.value)
		end
		return nil
	end,
	["#"] = function(unary, expr)
		-- I don't think there's ever anything to do here.
		return nil
	end,
	["~"] = function(unary, expr)
		if expr.type == "literal" and isValueFiniteNumber(expr.value) then
			intToBits(expr.value, bits1)
			for i = 1, INT_SIZE do
				bits1[i] = 1 - bits1[i]
			end
			return AstLiteral(dummyTokens, unary.token, bitsToInt(bits1))
		end
		return nil
	end,
}

local binaryFolders = {
	["+"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueNumberLike(l.value) and isValueNumberLike(r.value) then
			return AstLiteral(dummyTokens, binary.token, l.value+r.value)
		end
		return nil
	end,
	["-"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueNumberLike(l.value) and isValueNumberLike(r.value) then
			return AstLiteral(dummyTokens, binary.token, l.value-r.value)
		end
		return nil
	end,
	["*"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueNumberLike(l.value) and isValueNumberLike(r.value) then
			return AstLiteral(dummyTokens, binary.token, l.value*r.value)
		end
		return nil
	end,
	["/"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueNumberLike(l.value) and isValueNumberLike(r.value) then
			return AstLiteral(dummyTokens, binary.token, l.value/r.value)
		end
		return nil
	end,
	["//"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueNumberLike(l.value) and isValueNumberLike(r.value) then
			return AstLiteral(dummyTokens, binary.token, mathFloor(l.value/r.value))
		end
		return nil
	end,
	["^"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueNumberLike(l.value) and isValueNumberLike(r.value) then
			return AstLiteral(dummyTokens, binary.token, l.value^r.value)
		end
		return nil
	end,
	["%"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueNumberLike(l.value) and isValueNumberLike(r.value) then
			return AstLiteral(dummyTokens, binary.token, l.value%r.value)
		end
		return nil
	end,
	["&"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueFiniteNumber(l.value) and isValueFiniteNumber(r.value) then
			intToBits(l.value, bits1)
			intToBits(r.value, bits2)
			for i = 1, INT_SIZE do
				bits1[i] = (bits1[i] == 1 and bits2[i] == 1) and 1 or 0
			end
			return AstLiteral(dummyTokens, binary.token, bitsToInt(bits1))
		end
		return nil
	end,
	["~"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueFiniteNumber(l.value) and isValueFiniteNumber(r.value) then
			intToBits(l.value, bits1)
			intToBits(r.value, bits2)
			for i = 1, INT_SIZE do
				bits1[i] = (bits1[i] == 1) == (bits2[i] ~= 1) and 1 or 0
			end
			return AstLiteral(dummyTokens, binary.token, bitsToInt(bits1))
		end
		return nil
	end,
	["|"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueFiniteNumber(l.value) and isValueFiniteNumber(r.value) then
			intToBits(l.value, bits1)
			intToBits(r.value, bits2)
			for i = 1, INT_SIZE do
				if bits1[i] == 0 then  bits1[i] = bits2[i]  end
			end
			return AstLiteral(dummyTokens, binary.token, bitsToInt(bits1))
		end
		return nil
	end,
	[">>"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueFiniteNumber(l.value) and type(r.value) == "number" then
			intToBits(l.value, bits1)

			local shift = mathFloor(r.value)
			local i1    = INT_SIZE
			local i2    = 1
			local step  = -1

			if shift < 0 then
				i1, i2  = i2, i1
				step    = -step
			end

			for i = i1, i2, step do
				bits1[i] = bits1[i-shift] or 0
			end

			return AstLiteral(dummyTokens, binary.token, bitsToInt(bits1))
		end

		return nil
	end,
	["<<"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueFiniteNumber(l.value) and type(r.value) == "number" then
			intToBits(l.value, bits1)

			local shift = mathFloor(r.value)
			local i1    = 1
			local i2    = INT_SIZE
			local step  = 1

			if shift < 0 then
				i1, i2 = i2, i1
				step   = -step
			end

			for i = i1, i2, step do
				bits1[i] = bits1[i+shift] or 0
			end

			return AstLiteral(dummyTokens, binary.token, bitsToInt(bits1))
		end

		return nil
	end,
	[".."] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueNumberOrString(l.value) and isValueNumberOrString(r.value) then
			return AstLiteral(dummyTokens, binary.token, l.value..r.value)
		end
		return nil
	end,
	["<"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and areValuesNumbersOrStringsAndOfSameType(l.value, r.value) then
			return AstLiteral(dummyTokens, binary.token, (l.value < r.value))
		end
		return nil
	end,
	["<="] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and areValuesNumbersOrStringsAndOfSameType(l.value, r.value) then
			return AstLiteral(dummyTokens, binary.token, (l.value <= r.value))
		end
		return nil
	end,
	[">"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and areValuesNumbersOrStringsAndOfSameType(l.value, r.value) then
			return AstLiteral(dummyTokens, binary.token, (l.value > r.value))
		end
		return nil
	end,
	[">="] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and areValuesNumbersOrStringsAndOfSameType(l.value, r.value) then
			return AstLiteral(dummyTokens, binary.token, (l.value >= r.value))
		end
		return nil
	end,
	["=="] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" then
			return AstLiteral(dummyTokens, binary.token, (l.value == r.value))
		end
		return nil
	end,
	["~="] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" then
			return AstLiteral(dummyTokens, binary.token, (l.value ~= r.value))
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

local function replace(node, replacement, parent, container, key)
	container[key] = replacement

	replacement.sourcePath   = node.sourcePath
	replacement.sourceString = node.sourceString

	replacement.token    = node.token
	replacement.line     = node.line
	replacement.position = node.position

	replacement.parent    = parent
	replacement.container = container
	replacement.key       = key
end

local function simplifyNode(node, parent, container, key)
	if not parent then
		-- void

	elseif node.type == "unary" then
		-- Note: We don't fold e.g. '- - -expr' into '-expr' because metamethods may
		-- be called, and folding '- -expr' into 'expr' would remove what could be a
		-- runtime error if 'expr' didn't contain a number.
		local replacement = unaryFolders[node.operator](node, node.expression)
		if replacement then  replace(node, replacement, parent, container, key)  end

	elseif node.type == "binary" then
		-- @Incomplete: Fold 'expr - -n' into 'expr + n' etc.
		local replacement = binaryFolders[node.operator](node, node.left, node.right)
		if replacement then  replace(node, replacement, parent, container, key)  end

	elseif node.type == "if" then
		local ifNode = node

		if ifNode.condition.type == "literal" then -- @Incomplete: There are more values that make simplification possible (e.g. functions, but who would put that here anyway). :SimplifyTruthfulValues
			local replacement = ifNode.condition.value and ifNode.bodyTrue or ifNode.bodyFalse

			if replacement and replacement.statements[1] then
				replace(ifNode, replacement, parent, container, key)
				return simplifyNode(replacement, parent, container, key)
			else
				tableRemove(container, key)
			end
		end

	elseif node.type == "while" then
		local whileLoop = node

		if whileLoop.condition.type == "literal" then -- :SimplifyTruthfulValues
			if whileLoop.condition.value then
				whileLoop.condition.value = true
			else
				tableRemove(container, key)
			end
		end

	elseif node.type == "repeat" then
		local repeatLoop = node

		if repeatLoop.condition.type == "literal" then -- :SimplifyTruthfulValues
			if repeatLoop.condition.value then
				replace(repeatLoop, repeatLoop.body, parent, container, key)
				return simplifyNode(repeatLoop.body, parent, container, key)
			else
				repeatLoop.condition.value = false
			end
		end

	elseif node.type == "block" then
		if parent.type == "block" then
			local block           = node
			local hasDeclarations = false

			for _, statement in ipairs(block.statements) do
				if statement.type == "declaration" then
					hasDeclarations = true
					break
				end
			end

			if not hasDeclarations then
				-- Blocks without declarations don't need a scope.
				tableRemove(parent.statements, key)

				for i, statement in ipairs(block.statements) do
					tableInsert(parent.statements, key+i-1, statement)
				end
			end
		end
	end
end

local function simplify(node)
	traverseTreeReverse(node, true, simplifyNode)
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

			-- Note: Identifiers in declaration-likes also watch their own declaration-like. (Is this good?) :DeclarationIdentifiersWatchTheirParent
			declLikeWatchers[declLike] = declLikeWatchers[declLike] or {}
			tableInsert(declLikeWatchers[declLike], identInfo.ident)

			if declLike == currentDeclLike then  return true  end

			-- tableInsert(identInfo.visibleDeclLikes, declLike)
		end
	end

	return false
end

local function getInformationAboutIdentifiersAndUpdateMostReferences(node)
	local identInfos       = {--[[ [ident1]=identInfo1, identInfo1, ... ]]} -- identInfo = {ident=ident, visibleDeclLikes=declLikes}
	local declLikeWatchers = {--[[ [declLike1]={ident1,...}, ... ]]}

	traverseTree(node, function(node, parent, container, key)
		node.parent    = parent
		node.container = container
		node.key       = key

		if node.type == "identifier" then
			local currentIdent       = node
			local currentDeclLike    = findDeclaration(currentIdent) -- We can call this because all parents and previous nodes already have their references updated at this point.
			currentIdent.declaration = currentDeclLike

			local identType = (
				(parent and (
					(parent.type == "declaration" and container == parent.names     ) or
					(parent.type == "assignment"  and container == parent.targets   ) or
					(parent.type == "function"    and container == parent.parameters) or
					(parent.type == "for"         and container == parent.names     )
				))
				and "lvalue"
				or  "rvalue"
			)

			-- if currentDeclLike then  print(F("%s:%d: %s '%s'", currentIdent.sourcePath, currentIdent.line, identType, currentIdent.name))  end -- DEBUG

			local identInfo = {ident=currentIdent, type=identType--[[, visibleDeclLikes={}]]}
			tableInsert(identInfos, identInfo)
			identInfos[currentIdent] = identInfo

			-- Determine visible declarations for the identifier.
			local block = currentIdent -- Start node for while loop.

			while true do
				local statementOrInterest
				statementOrInterest, block = findParentStatementAndBlockOrExpressionOfInterest(block, currentDeclLike)

				if not statementOrInterest then
					assert(not currentDeclLike)
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
					tableInsert(declLikeWatchers[declLike], currentIdent)

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

					-- :DeclarationIdentifiersWatchTheirParent
					declLikeWatchers[declLike] = declLikeWatchers[declLike] or {}
					tableInsert(declLikeWatchers[declLike], currentIdent)

					break
				end
			end

		elseif node.type == "goto" then
			local gotoNode = node
			gotoNode.label = findLabel(gotoNode) -- We can call this because all relevant 'parent' references have been updated at this point.
		end
	end, node.parent, node.container, node.key)

	return identInfos, declLikeWatchers
end



local INVOLVED_NEVER  = newSet{ "function", "literal", "vararg" }
local INVOLVED_ALWAYS = newSet{ "break", "call", "goto", "label", "lookup", "return" }

function mayNodeBeInvolvedInJump(node)
	if INVOLVED_NEVER[node.type] then
		return false

	elseif INVOLVED_ALWAYS[node.type] then
		return true

	elseif node.type == "identifier" then
		return (node.declaration == nil) -- Globals may invoke a metamethod.

	elseif node.type == "binary" then
		return mayNodeBeInvolvedInJump(node.left) or mayNodeBeInvolvedInJump(node.right)
	elseif node.type == "unary" then
		return mayNodeBeInvolvedInJump(node.expression)

	elseif node.type == "block" then
		return mayAnyNodeBeInvolvedInJump(node.statements)

	elseif node.type == "if" then
		return mayNodeBeInvolvedInJump(node.condition) or mayNodeBeInvolvedInJump(node.bodyTrue) or (node.bodyFalse ~= nil and mayNodeBeInvolvedInJump(node.bodyFalse))

	elseif node.type == "for" then
		return mayAnyNodeBeInvolvedInJump(node.values) or mayNodeBeInvolvedInJump(node.body)
	elseif node.type == "repeat" or node.type == "while" then
		return mayNodeBeInvolvedInJump(node.condition) or mayNodeBeInvolvedInJump(node.body)

	elseif node.type == "declaration" then
		return mayAnyNodeBeInvolvedInJump(node.values)
	elseif node.type == "assignment" then
		return mayAnyNodeBeInvolvedInJump(node.targets) or mayAnyNodeBeInvolvedInJump(node.values) -- Targets may be identifiers or lookups.

	elseif node.type == "table" then
		for _, tableField in ipairs(node.fields) do
			if mayNodeBeInvolvedInJump(tableField.key)   then  return true  end
			if mayNodeBeInvolvedInJump(tableField.value) then  return true  end
		end
		return false

	else
		errorf("Invalid/unhandled node type '%s'.", tostring(node.type))
	end
end

function mayAnyNodeBeInvolvedInJump(nodes)
	for _, node in ipairs(nodes) do
		if mayNodeBeInvolvedInJump(node) then  return true  end
	end
	return false
end



local function hasNodeType(theNode, nodeType)
	return (traverseTree(theNode, function(node)
		if node.type == nodeType then  return "stop"  end
	end))
end

local function isDeclaredIdentifierReferenced(declLikeWatchers, declLike, declIdent)
	for _, watcherIdent in ipairs(declLikeWatchers[declLike]) do
		if
			watcherIdent.declaration == declLike
			and watcherIdent.name == declIdent.name
			and watcherIdent ~= declIdent
		then
			return true
		end
	end
	return false
end
local function isDeclaredIdentifierLookedUp(identInfos, declLikeWatchers, ident)
	local decl = ident.declaration
	if not decl then  return false  end -- We don't care about globals.

	for _, identInfo in ipairs(identInfos) do -- @Speed: Use declLikeWatchers.
		if identInfo.ident.declaration == decl and identInfo.type == "rvalue" then
			return true
		end
	end

	return false
end

local function isAnyDeclaredIdentifierReferenced(identInfos, declLikeWatchers, declLike)
	for _, watcherIdent in ipairs(declLikeWatchers[declLike] --[[or EMPTY_TABLE]]) do
		if
			watcherIdent.declaration == declLike
			and watcherIdent.parent ~= declLike -- We look at the parent because :DeclarationIdentifiersWatchTheirParent.
		then
			return true
		end
	end
	return false
end

local function isAssignmentSignificant(identInfos, declLikeWatchers, assignment)
	if hasNodeType(assignment, "function") then  return true  end -- This isn't smart enough.

	for _, targetExpr in ipairs(assignment.targets) do
		if not (targetExpr.type == "identifier" and targetExpr.declaration and not isDeclaredIdentifierLookedUp(identInfos, declLikeWatchers, targetExpr)) then
			return true
		end
	end

	return false
end

local function unregisterWatchers(identInfos, declLikeWatchers, theNode)
	-- ioWrite("unregister ") ; printNode(theNode) -- DEBUG

	traverseTree(theNode, true, function(node) -- @Speed
		if node.type == "identifier" then
			local currentIdent = node

			for _, watcherIdents in pairs(declLikeWatchers) do -- @Speed
				for i, watcherIdent in ipairs(watcherIdents) do
					if watcherIdent == currentIdent then
						removeUnordered(watcherIdents, i)
						break
					end
				end
			end

			for i, identInfo in ipairs(identInfos) do
				if identInfo.ident == currentIdent then
					removeUnordered(identInfos, i)
					break
				end
			end
			identInfos[currentIdent] = nil

		else
			declLikeWatchers[node] = nil -- In case it's a declLike. This does nothing otherwise, which is OK.
		end
	end)
end

-- Note: References need to be updated after calling this!
local function clean(theNode)
	local identInfos, declLikeWatchers = getInformationAboutIdentifiersAndUpdateMostReferences(theNode)

	--
	-- Find functions
	--
	local funcInfos = {}

	do
		-- We assume theNode is a block, but it's fine if it isn't.
		local funcInfo = {node=theNode, declLikes={}, assignments={}, locals={}, upvalues={}, globals={}}
		tableInsert(funcInfos, funcInfo)
	end

	traverseTree(theNode, function(node)
		if node == theNode then  return  end

		if node.type == "function" then
			local funcInfo = {node=node, declLikes={node}, assignments={}, locals={}, upvalues={}, globals={}}
			tableInsert(funcInfos, funcInfo)
		end
	end)

	for _, funcInfo in ipairs(funcInfos) do
		traverseTree(funcInfo.node, function(node)
			if node      == funcInfo.node then  return                   end
			if node.type == "function"    then  return "ignorechildren"  end

			if node.type == "identifier" then
				local ident    = node
				local declLike = ident.declaration

				if declLike then
					local isInFunc = true
					local parent   = ident.parent

					while parent do
						if parent == declLike then -- declLike may be a function itself.
							break
						elseif parent.type == "function" then
							isInFunc = false
							break
						end
						parent = parent.parent
					end

					tableInsert((isInFunc and funcInfo.locals or funcInfo.upvalues), ident)

				else
					tableInsert(funcInfo.globals, ident)
				end

			elseif node.type == "declaration" or node.type == "for" then
				tableInsert(funcInfo.declLikes, node)

			elseif node.type == "assignment" then
				tableInsert(funcInfo.assignments, node)
			end
		end)

		--[[ DEBUG
		print("--------------")
		printNode(funcInfo.node)
		for i, ident in ipairs(funcInfo.locals) do
			ioWrite("local   ", i, "  ") ; printNode(ident)
		end
		for i, ident in ipairs(funcInfo.upvalues) do
			ioWrite("upvalue ", i, "  ") ; printNode(ident)
		end
		for i, ident in ipairs(funcInfo.globals) do
			ioWrite("global  ", i, "  ") ; printNode(ident)
		end
	end
	print("--------------")
	--[==[]]
	end
	--]==]

	--
	-- Remove useless declaration-likes
	--
	for _, funcInfo in ipairs(funcInfos) do
		for _, declLike in ipairs(funcInfo.declLikes) do
			if declLike.type == "declaration" then -- @Incomplete: Handle other declaration-likes.
				local decl                  = declLike
				local declIdents            = decl.names--getNameArrayOfDeclarationLike(declLike)
				local usedIdentCount        = 0
				local significantValueCount = 0
				local significantCall       = nil

				-- Save some adjustment information.
				local madeToAdjusted = {}

				for i = 1, #decl.values-1 do -- Skip the last value.
					local valueExpr = decl.values[i]
					if (valueExpr.type == "call" or valueExpr.type == "vararg") and not valueExpr.adjustToOne then
						valueExpr.adjustToOne     = true
						madeToAdjusted[valueExpr] = true
					end
				end

				-- Remove useless extra values.
				for i = #decl.values, #declIdents+1, -1 do
					local valueExpr = decl.values[i]

					if mayNodeBeInvolvedInJump(valueExpr) then
						significantValueCount = significantValueCount + 1
						significantCall       = significantCall or (valueExpr.type == "call" and valueExpr) or nil
					else
						tableRemove(decl.values, i)
					end
				end

				for i = #declIdents+1, #decl.values do
					decl.values[i].key = i
				end

				-- Remove useles declared identifiers.
				local declIsRemoved = false

				for slot, declIdent in ipairsr(declIdents) do
					local identIsReadFrom   = false
					local identIsAssignedTo = false

					for _, watcherIdent in ipairs(declLikeWatchers[decl]) do
						if watcherIdent.declaration == decl and watcherIdent.name == declIdent.name then
							local identInfo = identInfos[watcherIdent]

							if identInfo.type == "rvalue" then
								identIsReadFrom = true
								if identIsAssignedTo then  break  end

							elseif identInfo.ident.parent.type == "assignment" then
								identIsAssignedTo = true
								if identIsReadFrom then  break  end
							end
						end
					end

					if not identIsReadFrom then
						-- ioWrite("useless ") ; printNode(declIdent) -- DEBUG

						local valueExpr          = decl.values[slot]
						local valueExprEffective = valueExpr or decl.values[#decl.values]

						local wantToRemoveIdent = not identIsAssignedTo -- :RemoveConnectedAssigments
						local wantToRemoveValue = not (
							(valueExpr and mayNodeBeInvolvedInJump(valueExpr))
							or (
								valueExprEffective
								and (valueExprEffective.type == "call" or valueExprEffective.type == "vararg")
								and not valueExprEffective.adjustToOne
							)
						)

						if not wantToRemoveIdent then
							usedIdentCount = usedIdentCount + 1
						end

						if not wantToRemoveValue then
							significantValueCount = significantValueCount + 1
							significantCall       = significantCall or (valueExpr and valueExpr.type == "call" and valueExpr) or nil
						end

						-- @Incomplete: Remove connected assigments. Note that if the identifier cannot be removed from some assignment then we cannot remove the slot! :RemoveConnectedAssigments
						-- @Incomplete: Update funcInfo.locals and whatever else.
						-- @Incomplete: Replace 'local unused, useless = func()' with 'local useless = func()' etc.
						local canRemoveSlot = (wantToRemoveIdent and wantToRemoveValue)

						-- Remove the whole declaration.
						if canRemoveSlot and not (declIdents[2] or decl.values[2]) then
							unregisterWatchers(identInfos, declLikeWatchers, decl)

							local block = decl.parent

							for i = decl.key, #block.statements do
								local statement     = block.statements[i+1]
								block.statements[i] = statement

								if statement then  statement.key = i  end
							end

							declIsRemoved = true

						-- Replace 'local unused = func()' with just 'func()'. This is a unique case as call expressions can also be statements.
						elseif slot == 1 and wantToRemoveIdent and not declIdents[2] and significantCall and significantValueCount == 1 then
							unregisterWatchers(identInfos, declLikeWatchers, declIdent)
							declLikeWatchers[decl] = nil

							local block                = decl.parent
							block.statements[decl.key] = significantCall
							significantCall.parent     = block
							significantCall.container  = decl.container
							significantCall.key        = decl.key

							declIsRemoved = true

						-- Remove whatever we can.
						else
							if canRemoveSlot and #declIdents > 1 then
								unregisterWatchers(identInfos, declLikeWatchers, declIdent)
								tableRemove(declIdents, slot)
								for slot = slot, #declIdents do
									declIdents[slot].key = slot
								end
							end

							if wantToRemoveValue and valueExpr then
								if canRemoveSlot or not decl.values[slot+1] then
									unregisterWatchers(identInfos, declLikeWatchers, valueExpr)
									tableRemove(decl.values, slot)
									for slot = slot, #decl.values do
										decl.values[slot]. key = slot
									end

								elseif not (valueExpr.type == "literal" and valueExpr.value == nil) then
									unregisterWatchers(identInfos, declLikeWatchers, valueExpr)
									replace(valueExpr, AstLiteral(dummyTokens, valueExpr.token, nil), valueExpr.parent, valueExpr.container, valueExpr.key)
								end
							end
						end
					end--if not identIsReadFrom
				end--for names

				-- Restore or remove adjusted flags.
				for i = 1, #decl.values do
					local valueExpr = decl.values[i]
					if (valueExpr.type == "call" or valueExpr.type == "vararg") then
						if declIsRemoved or decl.values[i+1] or not decl.names[i+1] or not madeToAdjusted[valueExpr] then
							valueExpr.adjustToOne = false
						end
					end
				end
			end
		end--for declLikes
	end--for funcInfos

	-- @Incomplete: Remove useless return statements etc.
end

--[[
local function clean_OLD(theNode)
	local identInfos, declLikeWatchers = getInformationAboutIdentifiersAndUpdateMostReferences(theNode)

	traverseTreeReverse(theNode, true, function(node)
		----------------------------------------------------------------

		if node.type == "declaration" or node.type == "function" or node.type == "for" then
			if node.type == "function" and node.vararg then
				local func               = node
				local varargIsReferenced = false

				traverseTree(func.body, function(nodeInFuncBody)
					if nodeInFuncBody.type == "vararg" then
						varargIsReferenced = true
						return "stop"
					elseif nodeInFuncBody.type == "function" then
						return "ignorechildren"
					end
				end)

				if not varargIsReferenced then
					func.vararg = nil
				end
			end

			if not (node.type == "function" and node.vararg) then
				local declLike   = node
				local declIdents = getNameArrayOfDeclarationLike(declLike)

				for i = #declIdents, 2, -1 do
					local declIdent = declIdents[i]
					if isDeclaredIdentifierReferenced(declLikeWatchers, declLike, declIdent) then  break  end

					unregisterWatchers(identInfos, declLikeWatchers, declIdent)
					declIdents[i] = nil
				end
			end

		----------------------------------------------------------------

		elseif node.type == "assignment" then
			local assignment = node
			local targets    = assignment.targets

			for i = #targets, 2, -1 do
				local targetExpr = targets[i]
				if targetExpr.type ~= "identifier" then  break  end

				local targetIdent = targetExpr
				if not targetIdent.declaration or isDeclaredIdentifierLookedUp(identInfos, declLikeWatchers, targetIdent) then
					break
				end

				unregisterWatchers(identInfos, declLikeWatchers, targetIdent)
				targets[i] = nil
			end

		----------------------------------------------------------------

		elseif node.type == "block" then
			local block = node

			for i = #block.statements, 1, -1 do
				local statement = block.statements[i]

				local mustKeep = (
					mayNodeBeInvolvedInJump(statement) -- I feel like this is insufficient...
					or hasNodeType(statement, "function") -- This is overkill, but we currently need it, otherwise too much is removed. We need to propagate upwards to some extent what's significant.
					or statement.type == "declaration" and (isAnyDeclaredIdentifierReferenced(identInfos, declLikeWatchers, statement) or hasNodeType(statement, "function")) -- @Incomplete: Unsure about the hasNodeType(). Look at assignments!
					or statement.type == "assignment"  and isAssignmentSignificant(identInfos, declLikeWatchers, statement)
				)

				-- if statement.line == 1804 then  print("mustKeep", mustKeep) ; printTree(statement)  end -- DEBUG

				if not mustKeep then
					-- if statement.line == 1804 then  ioWrite(">>>>>>> ") ; printNode(statement)  end -- DEBUG

					unregisterWatchers(identInfos, declLikeWatchers, statement)
					tableRemove(block.statements, i)

				elseif statement.type == "declaration" then
					local decl = statement

					-- Replace 'local unused = func()' with just 'func()'. This is a unique case as call expressions can also be statements.
					if
						not decl.names[2]
						and #decl.values == 1
						and decl.values[1].type == "call"
						and not isDeclaredIdentifierReferenced(declLikeWatchers, decl, decl.names[1])
					then
						unregisterWatchers(identInfos, declLikeWatchers, decl.names[1])
						declLikeWatchers[decl] = nil
						block.statements[i]    = decl.values[1]
					end

				elseif statement.type == "assignment" then
					local assignment  = statement
					local targetExpr1 = assignment.targets[1]
					local valueExpr1  = assignment.values[1]

					-- Replace 'finalReference = func()' with just 'func()'. This is a unique case as call expressions can also be statements.
					if
						not assignment.targets[2]
						and targetExpr1.type == "identifier"
						and targetExpr1.declaration
						and not assignment.values[2]
						and valueExpr1.type == "call"
						and not isDeclaredIdentifierLookedUp(identInfos, declLikeWatchers, targetExpr1)
					then
						unregisterWatchers(identInfos, declLikeWatchers, targetExpr1)
						block.statements[i] = valueExpr1
					end

				elseif statement.type == "block" then
					local subBlock        = statement
					local hasDeclarations = false

					for _, subStatement in ipairs(subBlock.statements) do
						if subStatement.type == "declaration" then
							hasDeclarations = true
							break
						end
					end

					if not hasDeclarations then
						-- Blocks without declarations don't need a scope.
						tableRemove(block.statements, i)

						for subIndex, subStatement in ipairs(subBlock.statements) do
							tableInsert(block.statements, i+subIndex-1, subStatement)
						end
					end
				end
			end--for statements

		----------------------------------------------------------------
		end
	end)
end
--]]



function getNameArrayOfDeclarationLike(declLike)
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
				charBytes[i]    = stringByte(charBank, charIndex)
				nameGeneration  = mathFloor(nameGeneration / #charBank)

				if nameGeneration == 0 then  break  end
			end

			cache[nameGeneration] = stringChar(tableUnpack(charBytes))
		end

		return cache[nameGeneration]
	end

	-- for nameGeneration = 1, 3500 do  print(generateName(nameGeneration))  end ; error("TEST")
	-- for pow = 0, 32 do  print(generateName(2^pow))  end ; error("TEST")
end

local function minify(node, optimize)
	if optimize then
		simplify(node)
		clean(node)
		simplify(node) -- clean() may have created more situations that can be simplified.
	end

	local identInfos, declLikeWatchers = getInformationAboutIdentifiersAndUpdateMostReferences(node)

	--
	-- Remember old declaration info before we start modifying things.
	--
	local declLikes_oldNameToIdent = {--[[ [declLike1]={[name1]=declIdent1,...}, ... ]]}

	for i, identInfo in ipairs(identInfos) do
		local declLike = identInfo.ident.declaration

		if declLike and not declLikes_oldNameToIdent[declLike] then
			local oldNameToIdent               = {}
			declLikes_oldNameToIdent[declLike] = oldNameToIdent

			for _, ident in ipairs(getNameArrayOfDeclarationLike(declLike)) do
				oldNameToIdent[ident.name] = ident
			end
		end
	end

	--
	-- Make sure frequencies affect who gets shorter names first. @Incomplete
	--
	--[[
	tableSort(identInfos, function(a, b)
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

					for _, watcherIdent in ipairs(declLikeWatchers[declLike] --[[or EMPTY_TABLE]]) do
						local watcherDeclLike = watcherIdent.declaration

						if watcherDeclLike then
							for _, watcherDeclIdent in ipairs(getNameArrayOfDeclarationLike(watcherDeclLike)) do
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
	local printLocs  = parser.printLocations
	local sourcePath = tokens.sourcePath
	local tokLine    = tokens.lineStart
	local tokTypes   = tokens.type
	local tokValues  = tokens.value

	for tok = 1, tokens.n do
		local v = tostring(tokValues[tok])
		if #v > 200 then  v = stringSub(v, 1, 200-3).."..."  end

		v = stringGsub(v, "\n", "\\n")

		if printLocs then  ioWrite(sourcePath, ":", tokLine[tok], ": ")  end
		ioWrite(tok, ". ", F("%-11s", tokTypes[tok]), " '", v, "'\n")
	end
end



do
	local writeNode
	local writeStatements

	-- lastOutput = "alpha"|"alphanum"|""

	local function isNumberInRange(n, min, max)
		return n ~= nil and n >= min and n <= max
	end

	local function canNodeBeName(node)
		return node.type == "literal" and type(node.value) == "string" and stringFind(node.value, "^[%a_][%w_]*$") and not KEYWORDS[node.value]
	end

	-- ensureSpaceIfNotPretty( buffer, pretty, lastOutput, value [, value2 ] )
	local function ensureSpaceIfNotPretty(buffer, pretty, lastOutput, value, value2)
		if not pretty and (lastOutput == value or lastOutput == value2) then
			tableInsert(buffer, " ")
		end
	end

	local function writeLua(buffer, lua, lastOutput)
		tableInsert(buffer, lua)
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

	-- Returns nil and a message or error.
	local function writeCommaSeparatedList(buffer, pretty, indent, lastOutput, expressions, writeAttributes)
		for i, expr in ipairs(expressions) do
			if i > 1 then
				lastOutput = writeLua(buffer, ",", "")
				if pretty then  lastOutput = writeLua(buffer, " ", "")  end
			end

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, expr, true)
			if not ok then  return nil, lastOutput  end

			if writeAttributes and expr.type == "identifier" and expr.attribute ~= "" then
				lastOutput = writeLua(buffer, "<", "")
				lastOutput = writeAlphanum(buffer, pretty, expr.attribute, lastOutput)
				lastOutput = writeLua(buffer, ">", "")
			end
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
			lastOutput = writeLua(buffer, stringRep("\t", indent), "")
		end
		return lastOutput
	end

	-- Returns nil and a message or error.
	local function writeFunctionParametersAndBody(buffer, pretty, indent, lastOutput, func, explicitParams)
		lastOutput = writeLua(buffer, "(", "")

		local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, explicitParams, false)
		if not ok then  return nil, lastOutput  end

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
		if not ok then  return nil, lastOutput  end

		lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
		lastOutput = writeAlphanum(buffer, pretty, "end", lastOutput)

		return true, lastOutput
	end

	-- Returns nil and a message or error.
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
					if not ok then  return nil, lastOutput  end

					local ok;ok, lastOutput = writeFunctionParametersAndBody(buffer, pretty, indent, lastOutput, func, func.parameters)
					if not ok then  return nil, lastOutput  end

					skipNext = true

				else
					local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, statement, true)
					if not ok then  return nil, lastOutput  end

					if statement.type == "call" then
						lastOutput = writeLua(buffer, ";", "") -- @Ugly way of handling call statements. (But what way would be better?)
					end
				end

				if pretty then  lastOutput = writeLua(buffer, "\n", "")  end
			end
		end

		return true, lastOutput
	end

	-- Returns nil and a message or error.
	local function writeLookup(buffer, pretty, indent, lastOutput, lookup, forMethodCall)
		local objIsLiteral = (lookup.object.type == "literal")
		if objIsLiteral then  lastOutput = writeLua(buffer, "(", "")  end

		local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, lookup.object, false)
		if not ok then  return nil, lastOutput  end

		if objIsLiteral then  lastOutput = writeLua(buffer, ")", "")  end

		if canNodeBeName(lookup.member) then
			lastOutput = writeLua(buffer, (forMethodCall and ":" or "."), "")
			lastOutput = writeAlphanum(buffer, pretty, lookup.member.value, lastOutput)

		elseif forMethodCall then
			return nil, "Error: AST: Callee for method call is not a lookup."

		else
			lastOutput = writeLua(buffer, "[", "")

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, lookup.member, true)
			if not ok then  return nil, lastOutput  end

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

	-- Returns nil and a message or error.
	local function writeBinaryOperatorChain(buffer, pretty, indent, lastOutput, binary)
		local l = binary.left
		local r = binary.right

		if l.type == "binary" and l.operator == binary.operator then
			local ok;ok, lastOutput = writeBinaryOperatorChain(buffer, pretty, indent, lastOutput, l)
			if not ok then  return nil, lastOutput  end
		else
			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, l, false)
			if not ok then  return nil, lastOutput  end
		end

		if pretty then  lastOutput = writeLua(buffer, " ", "")  end

		if binary.operator == ".." then  ensureSpaceIfNotPretty(buffer, pretty, lastOutput, "number")  end

		local nextOutput = ((binary.operator == "-" and "-") or (stringFind(binary.operator, "%w") and "alphanum") or (""))
		if nextOutput ~= "" then  ensureSpaceIfNotPretty(buffer, pretty, lastOutput, nextOutput)  end
		lastOutput = writeLua(buffer, binary.operator, nextOutput)

		if pretty then  lastOutput = writeLua(buffer, " ", "")  end

		if r.type == "binary" and r.operator == binary.operator then
			local ok;ok, lastOutput = writeBinaryOperatorChain(buffer, pretty, indent, lastOutput, r)
			if not ok then  return nil, lastOutput  end
		else
			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, r, false)
			if not ok then  return nil, lastOutput  end
		end

		return true, lastOutput
	end

	-- success, lastOutput = writeNode( buffer, pretty, indent, lastOutput, node, maySafelyOmitParens )
	-- lastOutput          = "" | "alphanum" | "number" | "-"
	-- Returns nil and a message or error.
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

			elseif type(node.value) == "number" or (jit and type(node.value) == "cdata" and tonumber(node.value)) then
				lastOutput = writeNumber(buffer, pretty, node.value, lastOutput)

			elseif type(node.value) == "string" then
				local R         = isNumberInRange
				local s         = node.value
				local quote     = stringFind(s, '"', 1, true) and not stringFind(s, "'", 1, true) and "'" or '"'
				local quoteByte = stringByte(quote)
				local pos       = 1

				lastOutput = writeLua(buffer, quote, "")

				while pos <= #s do
					local b1, b2, b3, b4 = stringByte(s, pos, pos+3)

					-- Printable ASCII.
					if R(b1,32,126) and b1 ~= 92 then
						if     b1 == quoteByte then  tableInsert(buffer, "\\") ; tableInsert(buffer, quote) ; pos = pos + 1
						elseif b1 == 92        then  tableInsert(buffer, [[\\]])                            ; pos = pos + 1
						else                         tableInsert(buffer, stringSub(s, pos, pos))            ; pos = pos + 1
						end

					-- Multi-byte UTF-8 sequence.
					elseif b2 and R(b1,194,223) and R(b2,128,191)                                     then  tableInsert(buffer, stringSub(s, pos, pos+1)) ; pos = pos + 2
					elseif b3 and b1== 224      and R(b2,160,191) and R(b3,128,191)                   then  tableInsert(buffer, stringSub(s, pos, pos+2)) ; pos = pos + 3
					elseif b3 and R(b1,225,236) and R(b2,128,191) and R(b3,128,191)                   then  tableInsert(buffer, stringSub(s, pos, pos+2)) ; pos = pos + 3
					elseif b3 and b1== 237      and R(b2,128,159) and R(b3,128,191)                   then  tableInsert(buffer, stringSub(s, pos, pos+2)) ; pos = pos + 3
					elseif b3 and R(b1,238,239) and R(b2,128,191) and R(b3,128,191)                   then  tableInsert(buffer, stringSub(s, pos, pos+2)) ; pos = pos + 3
					elseif b4 and b1== 240      and R(b2,144,191) and R(b3,128,191) and R(b4,128,191) then  tableInsert(buffer, stringSub(s, pos, pos+3)) ; pos = pos + 4
					elseif b4 and R(b1,241,243) and R(b2,128,191) and R(b3,128,191) and R(b4,128,191) then  tableInsert(buffer, stringSub(s, pos, pos+3)) ; pos = pos + 4
					elseif b4 and b1== 244      and R(b2,128,143) and R(b3,128,191) and R(b4,128,191) then  tableInsert(buffer, stringSub(s, pos, pos+3)) ; pos = pos + 4

					-- Escape sequence.
					elseif b1 == 7  then  tableInsert(buffer, [[\a]]) ; pos = pos + 1
					elseif b1 == 8  then  tableInsert(buffer, [[\b]]) ; pos = pos + 1
					elseif b1 == 9  then  tableInsert(buffer, [[\t]]) ; pos = pos + 1
					elseif b1 == 10 then  tableInsert(buffer, [[\n]]) ; pos = pos + 1
					elseif b1 == 11 then  tableInsert(buffer, [[\v]]) ; pos = pos + 1
					elseif b1 == 12 then  tableInsert(buffer, [[\f]]) ; pos = pos + 1
					elseif b1 == 13 then  tableInsert(buffer, [[\r]]) ; pos = pos + 1

					-- Other control character or anything else.
					elseif b2 and R(b2,48,57) then  tableInsert(buffer, F([[\%03d]], b1)) ; pos = pos + 1
					else                            tableInsert(buffer, F([[\%d]],   b1)) ; pos = pos + 1
					end
				end

				lastOutput = writeLua(buffer, quote, "")

			else
				return nil, F("Error: Failed outputting '%s' value '%s'.", type(node.value), tostring(node.value))
			end

		elseif nodeType == "table" then
			lastOutput = writeLua(buffer, "{", "")

			for i, tableField in ipairs(node.fields) do
				if i > 1 then
					lastOutput = writeLua(buffer, ",", "")
					if pretty then  lastOutput = writeLua(buffer, " ", "")  end
				end

				if not tableField.generatedKey then
					if canNodeBeName(tableField.key) then
						lastOutput = writeLua(buffer, tableField.key.value, "alphanum")

					else
						lastOutput = writeLua(buffer, "[", "")

						local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, tableField.key, true)
						if not ok then  return nil, lastOutput  end

						lastOutput = writeLua(buffer, "]", "")
					end

					lastOutput = writeLua(buffer, "=", "")
				end

				local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, tableField.value, (not pretty))
				if not ok then  return nil, lastOutput  end
			end

			lastOutput = writeLua(buffer, "}", "")

		elseif nodeType == "lookup" then
			local ok;ok, lastOutput = writeLookup(buffer, pretty, indent, lastOutput, node, false)
			if not ok then  return nil, lastOutput  end

		elseif nodeType == "unary" then
			local operatorOutput    = ((node.operator == "-" and "-") or (stringFind(node.operator, "%w") and "alphanum") or (""))
			local prettyAndAlphanum = pretty and operatorOutput == "alphanum"

			if prettyAndAlphanum and not maySafelyOmitParens then  lastOutput = writeLua(buffer, "(", "")  end -- @Polish: Only output parentheses around child unaries/binaries if associativity requires it.

			if operatorOutput ~= "" then  ensureSpaceIfNotPretty(buffer, pretty, lastOutput, operatorOutput)  end
			lastOutput = writeLua(buffer, node.operator, operatorOutput)

			if prettyAndAlphanum then  lastOutput = writeLua(buffer, " ", "")  end

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.expression, false)
			if not ok then  return nil, lastOutput  end

			if prettyAndAlphanum and not maySafelyOmitParens then  lastOutput = writeLua(buffer, ")", "")  end

		elseif nodeType == "binary" then
			if not maySafelyOmitParens then  lastOutput = writeLua(buffer, "(", "")  end -- @Polish: Only output parentheses around child unaries/binaries if associativity requires it.

			if node.operator == ".." or node.operator == "and" or node.operator == "or" then
				local ok;ok, lastOutput = writeBinaryOperatorChain(buffer, pretty, indent, lastOutput, node)
				if not ok then  return nil, lastOutput  end

			else
				local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.left, false)
				if not ok then  return nil, lastOutput  end

				local operatorOutput = ((node.operator == "-" and "-") or (stringFind(node.operator, "%w") and "alphanum") or (""))

				if pretty then  lastOutput = writeLua(buffer, " ", "")  end

				if operatorOutput ~= "" then  ensureSpaceIfNotPretty(buffer, pretty, lastOutput, operatorOutput)  end
				lastOutput = writeLua(buffer, node.operator, operatorOutput)

				if pretty then  lastOutput = writeLua(buffer, " ", "")  end

				local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.right, false)
				if not ok then  return nil, lastOutput  end
			end

			if not maySafelyOmitParens then  lastOutput = writeLua(buffer, ")", "")  end

		elseif nodeType == "call" then
			if node.adjustToOne then  lastOutput = writeLua(buffer, "(", "")  end

			if node.method then
				local lookup = node.callee

				if lookup.type ~= "lookup" then
					return nil, "Error: AST: Callee for method call is not a lookup."
				end

				local ok;ok, lastOutput = writeLookup(buffer, pretty, indent, lastOutput, lookup, true)
				if not ok then  return nil, lastOutput  end

			else
				local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.callee, false)
				if not ok then  return nil, lastOutput  end
			end

			lastOutput = writeLua(buffer, "(", "")

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, node.arguments, false)
			if not ok then  return nil, lastOutput  end

			lastOutput = writeLua(buffer, ")", "")
			if node.adjustToOne then  lastOutput = writeLua(buffer, ")", "")  end

		elseif nodeType == "function" then
			lastOutput = writeAlphanum(buffer, pretty, "function", lastOutput)

			local ok;ok, lastOutput = writeFunctionParametersAndBody(buffer, pretty, indent, lastOutput, node, node.parameters)
			if not ok then  return nil, lastOutput  end

		-- Statements:

		elseif nodeType == "break" then
			lastOutput = writeAlphanum(buffer, pretty, "break", lastOutput)
			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "return" then
			lastOutput = writeAlphanum(buffer, pretty, "return", lastOutput)

			if node.values[1] then
				if pretty then  lastOutput = writeLua(buffer, " ", "")  end

				local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, node.values, false)
				if not ok then  return nil, lastOutput  end
			end

			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "label" then
			local name = node.name
			if not (stringFind(name, "^[%a_][%w_]*$") and not KEYWORDS[name]) then
				return nil, F("Error: AST: Invalid label '%s'.", name)
			end
			lastOutput = writeLua(buffer, "::", "")
			lastOutput = writeAlphanum(buffer, pretty, name, lastOutput)
			lastOutput = writeLua(buffer, "::", "")
			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "goto" then
			local name = node.name
			if not (stringFind(name, "^[%a_][%w_]*$") and not KEYWORDS[name]) then
				return nil, F("Error: AST: Invalid label '%s'.", name)
			end
			lastOutput = writeAlphanum(buffer, pretty, "goto", lastOutput)
			lastOutput = writeLua(buffer, " ", "")
			lastOutput = writeAlphanum(buffer, pretty, name,   lastOutput)
			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "block" then
			lastOutput = writeAlphanum(buffer, pretty, "do", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, "\n", "")  end

			local ok;ok, lastOutput = writeStatements(buffer, pretty, indent+1, lastOutput, node.statements)
			if not ok then  return nil, lastOutput  end

			lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
			lastOutput = writeAlphanum(buffer, pretty, "end", lastOutput)

		elseif nodeType == "declaration" then
			lastOutput = writeAlphanum(buffer, pretty, "local", lastOutput)
			lastOutput = writeLua(buffer, " ", "")

			if not node.names[1] then  return nil, "Error: AST: Missing name(s) for declaration."  end

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, node.names, true)
			if not ok then  return nil, lastOutput  end

			if node.values[1] then
				if pretty then  lastOutput = writeLua(buffer, " ", "")  end
				lastOutput = writeLua(buffer, "=", "")
				if pretty then  lastOutput = writeLua(buffer, " ", "")  end

				local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, node.values, false)
				if not ok then  return nil, lastOutput  end
			end

			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "assignment" then
			if not node.targets[1] then  return nil, "Error: AST: Missing target expression(s) for assignment."  end
			if not node.values[1]  then  return nil, "Error: AST: Missing value(s) for assignment."  end

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
					if not ok then  return nil, lastOutput  end
				else
					local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.targets[1], false)
					if not ok then  return nil, lastOutput  end
				end

				local explicitParams = func.parameters
				if implicitSelfParam then  explicitParams = {tableUnpack(explicitParams, 2)}  end

				local ok;ok, lastOutput = writeFunctionParametersAndBody(buffer, pretty, indent, lastOutput, func, explicitParams)
				if not ok then  return nil, lastOutput  end

			else
				local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, node.targets, false)
				if not ok then  return nil, lastOutput  end

				if pretty then  lastOutput = writeLua(buffer, " ", "")  end
				lastOutput = writeLua(buffer, "=", "")
				if pretty then  lastOutput = writeLua(buffer, " ", "")  end

				local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, node.values, false)
				if not ok then  return nil, lastOutput  end

				lastOutput = writeLua(buffer, ";", "")
			end

		elseif nodeType == "if" then
			lastOutput = writeAlphanum(buffer, pretty, "if", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, " ", "")  end

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.condition, true)
			if not ok then  return nil, lastOutput  end

			if pretty then  lastOutput = writeLua(buffer, " ", "")  end
			lastOutput = writeAlphanum(buffer, pretty, "then", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, "\n", "")  end

			local ok;ok, lastOutput = writeStatements(buffer, pretty, indent+1, lastOutput, node.bodyTrue.statements)
			if not ok then  return nil, lastOutput  end

			while node.bodyFalse do
				-- Automatically detect what looks like 'elseif'.
				if #node.bodyFalse.statements == 1 and node.bodyFalse.statements[1].type == "if" then
					node = node.bodyFalse.statements[1]

					lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
					lastOutput = writeAlphanum(buffer, pretty, "elseif", lastOutput)
					if pretty then  lastOutput = writeLua(buffer, " ", "")  end

					local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.condition, true)
					if not ok then  return nil, lastOutput  end

					if pretty then  lastOutput = writeLua(buffer, " ", "")  end
					lastOutput = writeAlphanum(buffer, pretty, "then", lastOutput)
					if pretty then  lastOutput = writeLua(buffer, "\n", "")  end

					local ok;ok, lastOutput = writeStatements(buffer, pretty, indent+1, lastOutput, node.bodyTrue.statements)
					if not ok then  return nil, lastOutput  end

				else
					lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
					lastOutput = writeAlphanum(buffer, pretty, "else", lastOutput)
					if pretty then  lastOutput = writeLua(buffer, "\n", "")  end

					local ok;ok, lastOutput = writeStatements(buffer, pretty, indent+1, lastOutput, node.bodyFalse.statements)
					if not ok then  return nil, lastOutput  end

					break
				end
			end

			lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
			lastOutput = writeAlphanum(buffer, pretty, "end", lastOutput)

		elseif nodeType == "while" then
			lastOutput = writeAlphanum(buffer, pretty, "while", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, " ", "")  end

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.condition, true)
			if not ok then  return nil, lastOutput  end

			if pretty then  lastOutput = writeLua(buffer, " ", "")  end
			lastOutput = writeAlphanum(buffer, pretty, "do", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, "\n", "")  end

			local ok;ok, lastOutput = writeStatements(buffer, pretty, indent+1, lastOutput, node.body.statements)
			if not ok then  return nil, lastOutput  end

			lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
			lastOutput = writeAlphanum(buffer, pretty, "end", lastOutput)

		elseif nodeType == "repeat" then
			lastOutput = writeAlphanum(buffer, pretty, "repeat", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, "\n", "")  end

			local ok;ok, lastOutput = writeStatements(buffer, pretty, indent+1, lastOutput, node.body.statements)
			if not ok then  return nil, lastOutput  end

			lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
			lastOutput = writeAlphanum(buffer, pretty, "until", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, " ", "")  end

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, node.condition, true)
			if not ok then  return nil, lastOutput  end

		elseif nodeType == "for" then
			if not node.names[1]  then  return nil, "Error: AST: Missing name(s) for 'for' loop."   end
			if not node.values[1] then  return nil, "Error: AST: Missing value(s) for 'for' loop."  end

			lastOutput = writeAlphanum(buffer, pretty, "for", lastOutput)
			lastOutput = writeLua(buffer, " ", "")

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, node.names, false)
			if not ok then  return nil, lastOutput  end

			if pretty then  lastOutput = writeLua(buffer, " ", "")  end

			if node.kind == "numeric" then
				lastOutput = writeLua(buffer, "=", "")
			elseif node.kind == "generic" then
				lastOutput = writeAlphanum(buffer, pretty, "in", lastOutput)
			else
				return nil, F("Error: Unknown 'for' loop kind '%s'.", node.kind)
			end

			if pretty then  lastOutput = writeLua(buffer, " ", "")  end

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, node.values, false)
			if not ok then  return nil, lastOutput  end

			if pretty then  lastOutput = writeLua(buffer, " ", "")  end
			lastOutput = writeAlphanum(buffer, pretty, "do", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, "\n", "")  end

			local ok;ok, lastOutput = writeStatements(buffer, pretty, indent+1, lastOutput, node.body.statements)
			if not ok then  return nil, lastOutput  end

			lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
			lastOutput = writeAlphanum(buffer, pretty, "end", lastOutput)

		else
			return false, F("Error: Unknown node type '%s'.", tostring(nodeType))
		end
		return true, lastOutput
	end

	-- lua = toLua( astNode [, prettyOuput=false ] )
	-- Returns nil and a message on error.
	function toLua(node, pretty)
		assertArg1("toLua", 1, node, "table")

		local buffer = {}

		local ok, err
		if node.type == "block" then -- @Robustness: This exception isn't great. Should there be a file scope node?
			ok, err = writeStatements(buffer, pretty, 0, "", node.statements)
		else
			ok, err = writeNode(buffer, pretty, 0, "", node, true)
		end

		if ok then
			return tableConcat(buffer)
		else
			return nil, err
		end
	end
end



-- item, index = itemWith1( array, key, value )
function itemWith1(t, k, v)
	for i, item in ipairs(t) do
		if item[k] == v then  return item, i  end
	end
	return nil
end



-- assertArg1( functionName, argumentNumber, value, expectedType )
function assertArg1(funcName, argNum, v, expectedType)
	if type(v) == expectedType then  return  end
	errorf(3, "Bad argument #%d to '%s'. (Expected %s, got %s)", argNum, funcName, expectedType, type(v))
end

-- assertArg( functionName, argumentNumber, value, expectedType1, ... )
function assertArg(funcName, argNum, v, ...)
	for i = 1, select("#", ...) do
		if type(v) == select(i, ...) then  return  end
	end
	errorf(3, "Bad argument #%d to '%s'. (Expected %s, got %s)", argNum, funcName, tableConcat({...}, " or "), type(v))
end

-- errorf( [ level=1, ] format, ... )
function errorf(level, s, ...)
	if type(level) == "number" then
		error(F(s, ...), (level == 0 and 0 or (1+level)))
	else
		error(F(level, s, ...), 2)
	end
end



function formatNumber(n)
	-- 64-bit int in LuaJIT (is what we assume, anyway).
	if jit and type(n) == "cdata" then
		local nStr = tostring(n)

		if stringFind(nStr, "i$") then
			if stringFind(nStr, "^0[-+]") then
				nStr = stringGsub(nStr, "^0%+?", "")
			else
				--
				-- LuaJIT doesn't seem to be able to parse nStr if we output it as-is.
				-- What is even the notation for complex numbers with a non-zero real part?
				-- Oh LuaJIT, you're so mysterious...
				--
				-- @Robustness: Make sure we don't choke when trying to simplify() complex numbers.
				--
				errorf(2, "Cannot output complex number '%s'.", nStr)
			end
		end

		return nStr
	end

	-- Int (including 64-bit ints in Lua 5.3+, and excluding whole floats).
	if n == mathFloor(n) and not (mathType and mathType(n) == "float") then
		local nStr = F("%.0f", n)
		if tonumber(nStr) == n then  return nStr  end
	end

	-- Anything else.
	return (tostring(n)
		:gsub("(e[-+])0+(%d+)$", "%1%2") -- Remove unnecessary zeroes after 'e'.
		:gsub("e%+",             "e"   ) -- Remove plus after 'e'.
	)
end



local function iprev(t, i)
	i       = i-1
	local v = t[i]

	if v ~= nil then  return i, v  end
end

function ipairsr(t)
	return iprev, t, #t+1
end



function removeUnordered(t, i)
	local len = #t
	if i > len or i < 1 then  return  end

	t[len], t[i] = nil, t[len]
end



-- node = getChild( node, fieldName )
-- node = getChild( node, fieldName, index )                -- If the node field is an array.
-- node = getChild( node, fieldName, index, tableFieldKey ) -- If the node field is a table field array.
function getChild(node, fieldName, i, tableFieldKey)
	assertArg1("getChild", 1, node,      "table")
	assertArg1("getChild", 2, fieldName, "string")

	local nodeType       = node.type
	local childFields    = CHILD_FIELDS[nodeType] or errorf(2, "Unknown node type '%s'.", tostring(nodeType))
	local childFieldType = childFields[fieldName] or errorf(2, "Unknown node field '%s.%s'.", nodeType, tostring(fieldName))

	if childFieldType == "node" then
		return node[fieldName]

	elseif childFieldType == "nodearray" then
		assertArg1("getChild", 3, i, "number")

		return node[fieldName][i]

	elseif childFieldType == "tablefields" then
		assertArg1("getChild", 3, i,             "number")
		assertArg1("getChild", 4, tableFieldKey, "string")

		if not (tableFieldKey == "key" or tableFieldKey == "value") then
			errorf(2, "Bad argument #4 to 'getChild'. (Expected %q or %q, got %q)", "key", "value", tableFieldKey)
		end

		local field = node[fieldName][i]
		return field and field[tableFieldKey]

	else
		error(childFieldType)
	end
end

-- setChild( node, fieldName, childNode )
-- setChild( node, fieldName, index, childNode )                -- If the node field is an array.
-- setChild( node, fieldName, index, tableFieldKey, childNode ) -- If the node field is a table field array.
function setChild(node, fieldName, i, tableFieldKey, childNode)
	assertArg1("setChild", 1, node,      "table")
	assertArg1("setChild", 2, fieldName, "string")

	local nodeType       = node.type
	local childFields    = CHILD_FIELDS[nodeType] or errorf(2, "Unknown node type '%s'.", tostring(nodeType))
	local childFieldType = childFields[fieldName] or errorf(2, "Unknown node field '%s.%s'.", nodeType, tostring(fieldName))

	if childFieldType == "node" then
		childNode = i

		if childNode ~= nil then  assertArg1("setChild", 3, childNode, "table")  end

		node[fieldName] = childNode

	elseif childFieldType == "nodearray" then
		childNode = tableFieldKey

		assertArg1("setChild", 3, i,         "number")
		assertArg1("setChild", 4, childNode, "table")

		node[fieldName][i] = childNode

	elseif childFieldType == "tablefields" then
		assertArg1("setChild", 3, i,             "number")
		assertArg1("setChild", 4, tableFieldKey, "string")
		assertArg1("setChild", 5, childNode,     "table")

		if not (tableFieldKey == "key" or tableFieldKey == "value") then
			errorf(2, "Bad argument #4 to 'setChild'. (Expected %q or %q, got %q)", "key", "value", tableFieldKey)
		end

		local field = node[fieldName][i] or errorf(2, "No table field at index %d in %s.%s.", i, nodeType, fieldName)
		field[tableFieldKey] = childNode

	else
		error(childFieldType)
	end
end

-- addChild( node, fieldName, [ index=atEnd, ] childNode )
-- addChild( node, fieldName, [ index=atEnd, ] keyNode, valueNode ) -- If the node field is a table field array.
function addChild(node, fieldName, i, childNode, extraChildNode)
	assertArg1("addChild", 1, node,      "table")
	assertArg1("addChild", 2, fieldName, "string")

	if type(i) ~= "number" then
		i, childNode, extraChildNode = nil, i, childNode
	end
	local postIndexArgOffset = i and 0 or -1

	local nodeType       = node.type
	local childFields    = CHILD_FIELDS[nodeType] or errorf(2, "Unknown node type '%s'.", tostring(nodeType))
	local childFieldType = childFields[fieldName] or errorf(2, "Unknown node field '%s.%s'.", nodeType, tostring(fieldName))

	if childFieldType == "nodearray" then
		if i ~= nil then  assertArg1("addChild", 3, i, "number")  end
		assertArg1("addChild", 4+postIndexArgOffset, childNode, "table")

		i = i or #node[fieldName]+1
		tableInsert(node[fieldName], i, childNode)

	elseif childFieldType == "tablefields" then
		if i ~= nil then  assertArg1("addChild", 3, i, "number")  end
		assertArg1("addChild", 4+postIndexArgOffset, childNode,      "table")
		assertArg1("addChild", 5+postIndexArgOffset, extraChildNode, "table")

		i = i or #node[fieldName]+1
		tableInsert(node[fieldName], i, {key=childNode, value=extraChildNode, generatedKey=false})

	else
		errorf(2, "Node field '%s.%s' is not an array.", nodeType, tostring(fieldName))
	end
end

-- removeChild( node, fieldName [, index=last ] )
function removeChild(node, fieldName, i)
	assertArg1("removeChild", 1, node,      "table")
	assertArg1("removeChild", 2, fieldName, "string")
	assertArg ("removeChild", 3, i,         "number","nil")

	local nodeType       = node.type
	local childFields    = CHILD_FIELDS[nodeType] or errorf(2, "Unknown node type '%s'.", tostring(nodeType))
	local childFieldType = childFields[fieldName] or errorf(2, "Unknown node field '%s.%s'.", nodeType, tostring(fieldName))

	if childFieldType == "nodearray" or childFieldType == "tablefields" then
		tableRemove(node[fieldName], i) -- This also works if i is nil.
	else
		errorf(2, "Node field '%s.%s' is not an array.", nodeType, tostring(fieldName))
	end
end



parser = {
	-- Constants.
	VERSION             = PARSER_VERSION,

	INT_SIZE            = INT_SIZE,
	MAX_INT             = MAX_INT,
	MIN_INT             = MIN_INT,

	-- Functions.
	tokenize            = tokenize,
	tokenizeFile        = tokenizeFile,

	newTokenStream      = newTokenStream,
	insertToken         = insertToken,
	removeToken         = removeToken,
	concatTokens        = concatTokens,

	parse               = parse,
	parseFile           = parseFile,

	newNode             = newNode,
	getChild            = getChild,
	setChild            = setChild,
	addChild            = addChild,
	removeChild         = removeChild,

	traverseTree        = traverseTree,
	traverseTreeReverse = traverseTreeReverse,
	updateReferences    = updateReferences,

	simplify            = simplify,
	clean               = clean,
	minify              = minify,

	toLua               = toLua,

	printTokens         = printTokens,
	printNode           = printNode,
	printTree           = printTree,

	-- Settings.
	printIds            = false,
	printLocations      = false,
	indentation         = "    ",
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
