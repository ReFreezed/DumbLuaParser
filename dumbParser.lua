--[=[===========================================================
--=
--=  Dumb Lua Parser - Lua parsing library
--=  by Marcus 'ReFreezed' Thunström
--=
--=  Tokenize Lua code or create ASTs (Abstract Syntax Trees)
--=  and convert the data back to Lua.
--=
--=  Version: 2.2-dev
--=
--=  License: MIT (see the bottom of this file)
--=  Website: http://refreezed.com/luaparser/
--=  Documentation: http://refreezed.com/luaparser/docs/
--=
--=  Supported Lua versions: 5.1, 5.2, 5.3, 5.4, LuaJIT
--=
--==============================================================

1 - Usage
2 - API
  2.1 - Functions
  2.2 - Constants
  2.3 - Settings
3 - Tokens
4 - AST
5 - Other Objects
  5.1 - Stats
  5.2 - Locations
6 - Notes


1 - Usage
================================================================

local parser = require("dumbParser")

local tokens = parser.tokenizeFile("cool.lua")
local ast    = parser.parse(tokens)

parser.simplify(ast)
parser.printTree(ast)

local lua = parser.toLua(ast, true)
print(lua)


2 - API
================================================================


2.1 - Functions
----------------------------------------------------------------

tokenize, tokenizeFile
newToken, updateToken, cloneToken, concatTokens
parse, parseExpression, parseFile
newNode, newNodeFast, valueToAst, cloneNode, cloneTree, getChild, setChild, addChild, removeChild
validateTree
traverseTree, traverseTreeReverse
updateReferences
simplify, optimize, minify
toLua
printTokens, printNode, printTree
formatMessage
findDeclaredNames, findGlobalReferences, findShadows

tokenize()
	tokens = parser.tokenize( luaString [, pathForErrorMessages="?" ] )
	tokens = parser.tokenize( luaString [, keepWhitespaceTokens=false, pathForErrorMessages="?" ] )
	Convert a Lua string into an array of tokens. (See below for more info.)
	Returns nil and a message on error.

tokenizeFile()
	tokens = parser.tokenizeFile( path [, keepWhitespaceTokens=false ] )
	Convert the contents of a file into an array of tokens. (See below for more info.) Uses io.open().
	Returns nil and a message on error.

newToken()
	token = parser.newToken( tokenType, tokenValue )
	Create a new token. (See below or search for 'TokenCreation' for more info.)

updateToken()
	parser.updateToken( token, tokenValue )
	Update the value and representation of an existing token. (Search for 'TokenModification' for more info.)

cloneToken()
	tokenClone = parser.cloneToken( token )
	Clone an existing token.

concatTokens()
	luaString = parser.concatTokens( tokens )
	Concatenate tokens. Whitespace is added between tokens when necessary.

parse()
	astNode = parser.parse( tokens )
	astNode = parser.parse( luaString [, pathForErrorMessages="?" ] )
	Convert tokens or Lua code into an AST representing a block of code. (See below for more info.)
	Returns nil and a message on error.

parseExpression()
	astNode = parser.parseExpression( tokens )
	astNode = parser.parseExpression( luaString [, pathForErrorMessages="?" ] )
	Convert tokens or Lua code into an AST representing a value expression. (See below for more info.)
	Returns nil and a message on error.

parseFile()
	astNode = parser.parseFile( path )
	Convert a Lua file into an AST. (See below for more info.) Uses io.open().
	Returns nil and a message on error.

newNode()
	astNode = parser.newNode( nodeType, arguments... )
	Create a new AST node. (Search for 'NodeCreation' for more info.)

newNodeFast()
	astNode = parser.newNodeFast( nodeType, arguments... )
	Same as newNode() but without any validation. (Search for 'NodeCreation' for more info.)

valueToAst()
	astNode = parser.valueToAst( value [, sortTableKeys=false ] )
	Convert a Lua value (number, string, boolean, nil or table) to an AST.

cloneNode()
	astNode = parser.cloneNode( astNode )
	Clone an existing AST node (but not any children).

cloneTree()
	astNode = parser.cloneTree( astNode )
	Clone an existing AST node and its children.

getChild()
	childNode = parser.getChild( astNode, fieldName )
	childNode = parser.getChild( astNode, fieldName, index )                -- If the node field is an array.
	childNode = parser.getChild( astNode, fieldName, index, tableFieldKey ) -- If the node field is a table field array.
	tableFieldKey = "key"|"value"
	Get a child node. (Search for 'NodeFields' for field names.)

	The result is the same as doing the following, but with more error checking:
	childNode = astNode[fieldName]
	childNode = astNode[fieldName][index]
	childNode = astNode[fieldName][index][tableFieldKey]

setChild()
	parser.setChild( astNode, fieldName, childNode )
	parser.setChild( astNode, fieldName, index, childNode )                -- If the node field is an array.
	parser.setChild( astNode, fieldName, index, tableFieldKey, childNode ) -- If the node field is a table field array.
	tableFieldKey = "key"|"value"
	Set a child node. (Search for 'NodeFields' for field names.)

	The result is the same as doing the following, but with more error checking:
	astNode[fieldName]                       = childNode
	astNode[fieldName][index]                = childNode
	astNode[fieldName][index][tableFieldKey] = childNode

addChild()
	parser.addChild( astNode, fieldName, [ index=atEnd, ] childNode )
	parser.addChild( astNode, fieldName, [ index=atEnd, ] keyNode, valueNode ) -- If the node field is a table field array.
	Add a child node to an array field. (Search for 'NodeFields' for field names.)

	The result is the same as doing the following, but with more error checking:
	table.insert(astNode[fieldName], index, childNode)
	table.insert(astNode[fieldName], index, {key=keyNode, value=valueNode, generatedKey=false})

removeChild()
	parser.removeChild( astNode, fieldName [, index=last ] )
	Remove a child node from an array field. (Search for 'NodeFields' for field names.)

	The result is the same as doing the following, but with more error checking:
	table.remove(astNode[fieldName], index)

isExpression()
	bool = parser.isExpression( astNode )
	Returns true for expression nodes and false for statements.
	Note that call nodes count as expressions for this function, i.e. return true.

isStatement()
	bool = parser.isStatement( astNode )
	Returns true for statements and false for expression nodes.
	Note that call nodes count as statements for this function, i.e. return true.

validateTree()
	isValid, errorMessages = parser.validateTree( astNode )
	Check for errors in an AST (e.g. missing condition expressions for if statements).
	errorMessages is a multi-line string if isValid is false.

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
	parser.updateReferences( astNode [, updateTopNodePositionInfo=true ] )
	Update references between nodes in the tree.
	This function sets 'parent'+'container'+'key' for all nodes, 'declaration' for identifiers and vararg nodes, and 'label' for goto nodes.
	If 'updateTopNodePositionInfo' is false then 'parent', 'container' and 'key' will remain as-is for 'astNode' specifically.

simplify()
	stats = parser.simplify( astNode )
	Simplify/fold expressions and statements involving constants ('1+2' becomes '3', 'false and func()' becomes 'false' etc.).
	See the INT_SIZE constant for notes.
	See below for more info about stats.

optimize()
	stats = parser.optimize( astNode )
	Attempt to remove nodes that aren't useful, like unused variables, or variables that are essentially constants.
	Calls simplify() internally.
	This function can be quite slow!
	See below for more info about stats.
	Note: References may be out-of-date after calling this.

minify()
	stats = parser.minify( astNode [, optimize=false ] )
	Replace local variable names with short names.
	This function can be used to obfuscate the code to some extent.
	If 'optimize' is set then optimize() is also called automatically.
	See below for more info about stats.
	Note: References may be out-of-date after calling this.

toLua()
	luaString    = parser.toLua( astNode [, prettyOuput=false, nodeCallback ] )
	nodeCallback = function( node, outputBuffer )
	Convert an AST to Lua, optionally call a function on each node before they are turned into Lua.
	Any node in the tree with a .pretty attribute will override the 'prettyOuput' flag for itself and its children.
	Nodes can also have a .prefix and/or .suffix attribute with Lua code to output before/after the node (e.g. declaration.names[1].suffix="--[[foo]]").
	outputBuffer is an array of Lua code that has been output so far.
	Returns nil and a message on error.

printTokens()
	parser.printTokens( tokens )
	Print tokens to stdout.

printNode()
	parser.printNode( astNode )
	Print information about an AST node to stdout.

printTree()
	parser.printTree( astNode )
	Print the structure of a whole AST to stdout.

formatMessage()
	message = parser.formatMessage( [ prefix="Info", ] token,    formatString, ... )
	message = parser.formatMessage( [ prefix="Info", ] astNode,  formatString, ... )
	message = parser.formatMessage( [ prefix="Info", ] location, formatString, ... )
	Format a message to contain a code preview window with an arrow pointing at the target token, node or location.
	This is used internally for formatting error messages.

	-- Example:
	if identifier.name ~= "good" then
		print(parser.formatMessage("Error", identifier, "This identifier is not good!"))
		print(parser.formatMessage(currentStatement, "Current statement."))
	end

findDeclaredNames()
	identifiers = parser.findDeclaredNames( astNode )
	Find all declared names in the tree (i.e. identifiers from AstDeclaration, AstFunction and AstFor nodes).

findGlobalReferences()
	identifiers = parser.findGlobalReferences( astNode )
	Find all identifiers not referring to local variables in the tree.
	Note: updateReferences() must be called at some point before you call this - otherwise all variables will be seen as globals!

findShadows()
	shadowSequences = parser.findShadows( astNode )
	shadowSequences = { shadowSequence1, ... }
	shadowSequence  = { shadowingIdentifier, shadowedIdentifier1, ... }
	Find local variable shadowing in the tree. Each shadowSequence is an array of declared identifiers where each identifier shadows the next one.
	Note: updateReferences() must be called at some point before you call this - otherwise all variables will be seen as globals!
	Note: Shadowing of globals cannot be detected by the function as that would require knowledge of all potential globals in your program. (See findGlobalReferences())


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
constantNameReplacementStringMaxLength

printIds
	parser.printIds = bool
	If AST node IDs should be printed. (All nodes gets assigned a unique ID when created.)
	Default: false.

printLocations
	parser.printLocations = bool
	If the file location (filename and line number) should be printed for each token or AST node.
	Default: false.

indentation
	parser.indentation = string
	The indentation used when printing ASTs (with printTree()).
	Default: 4 spaces.

constantNameReplacementStringMaxLength
	parser.constantNameReplacementStringMaxLength = length
	Normally optimize() replaces variable names that are effectively constants with their value.
	The exception is if the value is a string that's longer than what this setting specifies.
	Default: 200.

	-- Example:
	local ast = parser.parse[==[
		local short = "a"
		local long  = "xy"
		func(short, long)
	]==]
	parser.constantNameReplacementStringMaxLength = 1
	parser.optimize(ast)
	print(parser.toLua(ast)) -- local long="xy";func("a",long);


3 - Tokens
================================================================

Tokens are represented by tables.

Token fields:

	type           -- Token type. (See below.)
	value          -- Token value. All token types have a string value, except "number" tokens which have a number value.
	representation -- The token's code representation. (Strings have surrounding quotes, comments start with "--" etc.)

	sourceString   -- The original source string, or "" if there is none.
	sourcePath     -- Path to the source file, or "?" if there is none.

	lineStart      -- Start line number in sourceString, or 0 by default.
	lineEnd        -- End line number in sourceString, or 0 by default.
	positionStart  -- Start byte position in sourceString, or 0 by default.
	positionEnd    -- End byte position in sourceString, or 0 by default.

Token types:

	"comment"     -- A comment.
	"identifier"  -- Word that is not a keyword.
	"keyword"     -- Lua keyword.
	"number"      -- Number literal.
	"punctuation" -- Any punctuation, e.g. ".." or "(".
	"string"      -- String value.
	"whitespace"  -- Sequence of whitespace characters.


4 - AST
================================================================

AST nodes are represented by tables.

Node types:

	"assignment"  -- Assignment of one or more values to one or more variables.
	"binary"      -- Binary expression (operation with two operands, e.g. "+" or "and").
	"block"       -- List of statements. Blocks inside blocks are 'do...end' statements.
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


5 - Other Objects
================================================================


5.1 - Stats
----------------------------------------------------------------

Some functions return a stats table which contains these fields:

	nodeReplacements   -- Array of locations. Locations of nodes that were replaced. (See below for location info.)
	nodeRemovals       -- Array of locations. Locations of nodes or tree branches that were removed. (See below for location info.)
	nodeRemoveCount    -- Number. How many nodes were removed, including subnodes of nodeRemovals.

	renameCount        -- Number. How many identifiers were renamed.
	generatedNameCount -- Number. How many unique names were generated.


5.2 - Locations
----------------------------------------------------------------

Locations are tables with these fields:

	sourceString -- The original source string, or "" if there is none.
	sourcePath   -- Path to the source file, or "?" if there is none.

	line         -- Line number in sourceString, or 0 by default.
	position     -- Byte position in sourceString, or 0 by default.

	node         -- The node the location points to, or nil if there is none.
	replacement  -- The node that replaced 'node', or nil if there is none. (This is set for stats.nodeReplacements.)


6 - Notes
================================================================

Special number notation rules.

	The expression '-n' is parsed as a single number literal if 'n' is a
	numeral (i.e. the result is a negative number).

	The expression 'n/0' is parsed as a single number literal if 'n' is a
	numeral. If 'n' is positive then the result is math.huge, if 'n' is
	negative then the result is -math.huge, or if 'n' is 0 then the result is
	NaN.


-============================================================]=]

local PARSER_VERSION = "2.2.0-dev"

local NORMALIZE_MINUS_ZERO, HANDLE_ENV
do
	local n              = 0
	NORMALIZE_MINUS_ZERO = tostring(-n) == "0" -- Lua 5.3+ normalizes -0 to 0.
end
do
	local pcall = pcall
	local _ENV  = nil
	HANDLE_ENV  = not pcall(function() return _G end) -- Looking up the global _G will raise an error if _ENV is supported (Lua 5.2+).
end

local assert       = assert
local error        = error
local ipairs       = ipairs
local loadstring   = loadstring or load
local pairs        = pairs
local print        = print
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

local maybeWrapInt = (
	(jit and function(n)
		-- 'n' might be cdata (i.e. a 64-bit integer) here. We have to limit the range
		-- with mod once before we convert it to a Lua number to not lose precision,
		-- but the result might be negative (and still out of range somehow!) so we
		-- have to use mod again. Gah!
		return tonumber(n % 0x100000000) % 0x100000000 -- 0x100000000 == 2^32
	end)
	or (_VERSION == "Lua 5.2" and require"bit32".band)
	or function(n)  return n  end
)

local parser



local function newSet(values)
	local set = {}
	for _, v in ipairs(values) do
		set[v] = true
	end
	return set
end
local function newCharSet(chars)
	return newSet{ stringByte(chars, 1, #chars) }
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

local EXPRESSION_NODES = newSet{ "binary", "call", "function", "identifier", "literal", "lookup", "table", "unary", "vararg" }
local STATEMENT_NODES  = newSet{ "assignment", "block", "break", "call", "declaration", "for", "goto", "if", "label", "repeat", "return", "while" }

local TOKEN_BYTES = {
	NAME_START      = newCharSet"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_",
	DASH            = newCharSet"-",
	NUM             = newCharSet"0123456789",
	-- NUM_OR_DOT   = newCharSet"0123456789.",
	QUOTE           = newCharSet"\"'",
	SQUARE          = newCharSet"[",
	DOT             = newCharSet".",
	PUNCT_TWO_CHARS = newCharSet".=~<>:/<>",
	PUNCT_ONE_CHAR  = newCharSet"+-*/%^#<>=(){}[];:,.&~|",
}

local NUMERAL_PATTERNS = {
	HEX_FRAC_EXP = stringGsub("^( 0[Xx] (%x*) %.(%x+) [Pp]([-+]?%x+) )", " +", ""),
	HEX_FRAC     = stringGsub("^( 0[Xx] (%x*) %.(%x+)                )", " +", ""),
	HEX_EXP      = stringGsub("^( 0[Xx] (%x+) %.?     [Pp]([-+]?%x+) )", " +", ""),
	HEX          = stringGsub("^( 0[Xx]  %x+  %.?                    )", " +", ""),
	BIN          = stringGsub("^( 0[Bb]  [01]+                       )", " +", ""),
	DEC_FRAC_EXP = stringGsub("^(        %d*  %.%d+   [Ee][-+]?%d+   )", " +", ""),
	DEC_FRAC     = stringGsub("^(        %d*  %.%d+                  )", " +", ""),
	DEC_EXP      = stringGsub("^(        %d+  %.?     [Ee][-+]?%d+   )", " +", ""),
	DEC          = stringGsub("^(        %d+  %.?                    )", " +", ""),
}

local INT_SIZE, MAX_INT, MIN_INT
do
	local hex = F("%x", maybeWrapInt(-1))
	INT_SIZE  = #hex * 4 -- This should generally be 64 for Lua 5.3+ and 32 for earlier.
	MAX_INT   = math.maxinteger or tonumber(stringGsub(hex, "f", "7", 1), 16)
	MIN_INT   = math.mininteger or -MAX_INT-1
end

-- local EMPTY_TABLE = {}

local nextSerialNumber = 1



-- :NodeFields

local function populateCommonNodeFields(token, node)
	-- All nodes have these fields.
	node.id          = nextSerialNumber
	nextSerialNumber = nextSerialNumber + 1

	node.sourceString = token and token.sourceString or ""
	node.sourcePath   = token and token.sourcePath   or "?"

	node.token    = token
	node.line     = token and token.lineStart     or 0
	node.position = token and token.positionStart or 0

	-- These fields are set by updateReferences():
	-- node.parent    = nil -- Refers to the node's parent in the tree.
	-- node.container = nil -- Refers to the specific table that the node is in, which could be the parent itself or a field in the parent.
	-- node.key       = nil -- Refers to the specific field in the container that the node is in (which is either a string or an integer).

	-- toLua() uses these fields if present:
	-- node.pretty = bool
	-- node.prefix = luaString
	-- node.suffix = luaString

	return node
end

-- AST expressions.
local function AstIdentifier (token,name)return populateCommonNodeFields(token,{
	type        = "identifier",
	name        = name,  -- String.
	attribute   = "",    -- "" | "close" | "const"  -- Only used in declarations.
	declaration = nil,   -- AstIdentifier (whose parent is an AstDeclaration, AstFunction or AstFor). Updated by updateReferences(). This is nil for globals.
})end
local function AstVararg (token)return populateCommonNodeFields(token,{
	type        = "vararg",
	declaration = nil,   -- AstVararg (whose parent is an AstFunction). Updated by updateReferences(). This is nil in the main chunk (or in a non-vararg function, which is probably an error).
	adjustToOne = false, -- True if parentheses surround the vararg.
})end
local function AstLiteral (token,v)return populateCommonNodeFields(token,{
	type        = "literal",
	value       = v,     -- Number, string, boolean or nil.
})end
local function AstTable (token)return populateCommonNodeFields(token,{
	type        = "table",
	fields      = {},    -- Array of {key=expression, value=expression, generatedKey=bool}. generatedKey is true for implicit keys (i.e. {x,y}) and false for explicit keys (i.e. {a=x,b=y}). Note that the state of generatedKey affects the output of toLua()! 'key' may be nil if generatedKey is true.
})end
local function AstLookup (token)return populateCommonNodeFields(token,{
	type        = "lookup",
	object      = nil,   -- Expression.
	member      = nil,   -- Expression.
})end
local function AstUnary (token,op)return populateCommonNodeFields(token,{
	type        = "unary",
	operator    = op,    -- "-" | "not" | "#" | "~"
	expression  = nil,   -- Expression.
})end
local function AstBinary (token,op)return populateCommonNodeFields(token,{
	type        = "binary",
	operator    = op,    -- "+" | "-" | "*" | "/" | "//" | "^" | "%" | "&" | "~" | "|" | ">>" | "<<" | ".." | "<" | "<=" | ">" | ">=" | "==" | "~=" | "and" | "or"
	left        = nil,   -- Expression.
	right       = nil,   -- Expression.
})end
local function AstCall (token)return populateCommonNodeFields(token,{ -- Calls can be both expressions and statements.
	type        = "call",
	callee      = nil,   -- Expression.
	arguments   = {},    -- Array of expressions.
	method      = false, -- True if the call is a method call. Method calls must have a callee that is a lookup with a member expression that is a string literal that can pass as an identifier.
	adjustToOne = false, -- True if parentheses surround the call.
})end
local function AstFunction (token)return populateCommonNodeFields(token,{
	type        = "function",
	parameters  = {},    -- Array of AstIdentifier and maybe an AstVararg at the end.
	body        = nil,   -- AstBlock.
})end

-- AST statements.
local function AstBreak (token)return populateCommonNodeFields(token,{
	type        = "break",
})end
local function AstReturn (token)return populateCommonNodeFields(token,{
	type        = "return",
	values      = {},    -- Array of expressions.
})end
local function AstLabel (token,name)return populateCommonNodeFields(token,{
	type        = "label",
	name        = name,  -- String. The value must be able to pass as an identifier.
})end
local function AstGoto (token,name)return populateCommonNodeFields(token,{
	type        = "goto",
	name        = name,  -- String. The value must be able to pass as an identifier.
	label       = nil,   -- AstLabel. Updated by updateReferences().
})end
local function AstBlock (token)return populateCommonNodeFields(token,{
	type        = "block",
	statements  = {},    -- Array of statements.
})end
local function AstDeclaration (token)return populateCommonNodeFields(token,{
	type        = "declaration",
	names       = {},    -- Array of AstIdentifier.
	values      = {},    -- Array of expressions.
})end
local function AstAssignment (token)return populateCommonNodeFields(token,{
	type        = "assignment",
	targets     = {},    -- Mixed array of AstIdentifier and AstLookup.
	values      = {},    -- Array of expressions.
})end
local function AstIf (token)return populateCommonNodeFields(token,{
	type        = "if",
	condition   = nil,   -- Expression.
	bodyTrue    = nil,   -- AstBlock.
	bodyFalse   = nil,   -- AstBlock or nil.
})end
local function AstWhile (token)return populateCommonNodeFields(token,{
	type        = "while",
	condition   = nil,   -- Expression.
	body        = nil,   -- AstBlock.
})end
local function AstRepeat (token)return populateCommonNodeFields(token,{
	type        = "repeat",
	body        = nil,   -- AstBlock.
	condition   = nil,   -- Expression.
})end
local function AstFor (token,kind)return populateCommonNodeFields(token,{
	type        = "for",
	kind        = kind,  -- "numeric" | "generic"
	names       = {},    -- Array of AstIdentifier.
	values      = {},    -- Array of expressions.
	body        = nil,   -- AstBlock.
})end



local CHILD_FIELDS = {
	["identifier"]  = {},
	["vararg"]      = {},
	["literal"]     = {},
	["table"]       = {fields="tablefields"},
	["lookup"]      = {object="node", member="node"},
	["unary"]       = {expressions="node"},
	["binary"]      = {left="node", right="node"},
	["call"]        = {callee="node", arguments="nodearray"},
	["function"]    = {parameters="nodearray", body="node"},
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



local function Stats()
	return {
		-- simplify() and optimize():
		nodeReplacements   = {--[[ location1, ... ]]},
		nodeRemovals       = {--[[ location1, ... ]]},
		nodeRemoveCount    = 0,

		-- minify():
		renameCount        = 0,
		generatedNameCount = 0,
	}
end

-- location = Location( sourceLocation [, extraKey, extraValue ] )
-- location = Location( sourceNode     [, extraKey, extraValue ] )
local function Location(sourceLocOrNode, extraK, extraV)
	local loc = {
		sourceString = sourceLocOrNode.sourceString,
		sourcePath   = sourceLocOrNode.sourcePath,

		line     = sourceLocOrNode.line,
		position = sourceLocOrNode.position,

		node = sourceLocOrNode.type and sourceLocOrNode or nil,
	}
	if extraK then
		loc[extraK] = extraV
	end
	return loc
end



-- count = countString( haystack, needle [, plain=false ] )
local function countString(haystack, needle, plain)
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
local function countSubString(haystack, pos, posEnd, needle, plain)
	local count = 0

	while true do
		local _, i2 = stringFind(haystack, needle, pos, plain)
		if not i2 or i2 > posEnd then  return count  end

		count = count + 1
		pos   = i2    + 1
	end
end



-- errorf( [ level=1, ] format, ... )
local function errorf(level, s, ...)
	if type(level) == "number" then
		error(F(s, ...), (level == 0 and 0 or (1+level)))
	else
		error(F(level, s, ...), 2)
	end
end

-- assertArg1( functionName, argumentNumber, value, expectedType                 [, level=2 ] )
-- assertArg2( functionName, argumentNumber, value, expectedType1, expectedType2 [, level=2 ] )
local function assertArg1(funcName, argNum, v, expectedType, level)
	if type(v) == expectedType then  return  end
	errorf(1+(level or 2), "Bad argument #%d to '%s'. (Expected %s, got %s)", argNum, funcName, expectedType, type(v))
end
local function assertArg2(funcName, argNum, v, expectedType1, expectedType2, level)
	if type(v) == expectedType1 or type(v) == expectedType2 then  return  end
	errorf(1+(level or 2), "Bad argument #%d to '%s'. (Expected %s or %s, got %s)", argNum, funcName, expectedType1, expectedType2, type(v))
end



local ensurePrintable
do
	local CONTROL_TO_READABLE = {
		["\0"] = "{NUL}",
		["\n"] = "{NL}",
		["\r"] = "{CR}",
	}

	function ensurePrintable(s)
		return (stringGsub(s, "[%z\1-\31\127-\255]", function(c)
			return CONTROL_TO_READABLE[c] or (stringByte(c) <= 31 or stringByte(c) >= 127) and F("{%d}", stringByte(c)) or nil
		end))
	end
end



local function removeUnordered(t, i)
	local len = #t
	if i > len or i < 1 then  return  end

	-- Note: This does the correct thing if i==len too.
	t[i]   = t[len]
	t[len] = nil
end

local function removeItemUnordered(t, v)
	for i = 1, #t do
		if t[i] == v then
			removeUnordered(t, i)
			return
		end
	end
end



local function getLineNumber(s, pos)
	return 1 + countSubString(s, 1, pos-1, "\n", true)
end



local formatMessageInFile
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

	function formatMessageInFile(prefix, contents, path, pos, agent, s, ...)
		if agent ~= "" then
			agent = "["..agent.."] "
		end

		s = F(s, ...)

		if contents == "" then
			return F("%s @ %s: %s%s", prefix, path, agent, s)
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

		return F("%s @ %s:%d: %s%s\n>\n%s%s%s>-%s^",
			prefix, path, ln, agent, s,
			(linePre2Start < linePre1Start and linePre2Start <= linePre2End) and F("> %s\n", (stringGsub(stringSub(contents, linePre2Start, linePre2End), "\t", "    "))) or "",
			(linePre1Start < lineStart     and linePre1Start <= linePre1End) and F("> %s\n", (stringGsub(stringSub(contents, linePre1Start, linePre1End), "\t", "    "))) or "",
			(                                  lineStart     <= lineEnd    ) and F("> %s\n", (stringGsub(stringSub(contents, lineStart,     lineEnd    ), "\t", "    "))) or ">\n",
			stringRep("-", getSubTextLength(contents, lineStart, pos-1))
		)
	end
end

local function formatMessageAtToken(prefix, token, agent, s, ...)
	return (formatMessageInFile(prefix, (token and token.sourceString or ""), (token and token.sourcePath or "?"), (token and token.positionStart or 0), agent, s, ...))
end
local function formatMessageAfterToken(prefix, token, agent, s, ...)
	return (formatMessageInFile(prefix, (token and token.sourceString or ""), (token and token.sourcePath or "?"), (token and token.positionEnd+1 or 0), agent, s, ...))
end

local function formatMessageAtNode(prefix, node, agent, s, ...)
	return (formatMessageInFile(prefix, node.sourceString, node.sourcePath, node.position, agent, s, ...))
end

local function formatMessageHelper(argNumOffset, prefix, nodeOrLocOrToken, s, ...)
	assertArg1("formatMessage", 1+argNumOffset, prefix,           "string", 3)
	assertArg1("formatMessage", 2+argNumOffset, nodeOrLocOrToken, "table",  3)
	assertArg1("formatMessage", 3+argNumOffset, s,                "string", 3)

	local formatter = nodeOrLocOrToken.representation and formatMessageAtToken or formatMessageAtNode
	return (formatter(prefix, nodeOrLocOrToken, "", s, ...))
end

-- message = formatMessage( [ prefix="Info", ] token,    s, ... )
-- message = formatMessage( [ prefix="Info", ] astNode,  s, ... )
-- message = formatMessage( [ prefix="Info", ] location, s, ... )
local function formatMessage(prefix, ...)
	if type(prefix) == "string" then
		return (formatMessageHelper(0, prefix, ...))
	else
		return (formatMessageHelper(-1, "Info", prefix, ...))
	end

end



local function formatErrorInFile    (...)  return formatMessageInFile    ("Error", ...)  end
local function formatErrorAtToken   (...)  return formatMessageAtToken   ("Error", ...)  end
local function formatErrorAfterToken(...)  return formatMessageAfterToken("Error", ...)  end
local function formatErrorAtNode    (...)  return formatMessageAtNode    ("Error", ...)  end



local function where(node, s, ...)
	if not node then
		print("[Where] No node here!")
	elseif s then
		print(formatMessageAtNode("Info", node, "Where", s, ...))
	else
		print(formatMessageAtNode("Info", node, "Where", "Here!"))
	end
end



local function iprev(t, i)
	i       = i-1
	local v = t[i]

	if v ~= nil then  return i, v  end
end

local function ipairsr(t)
	return iprev, t, #t+1
end



-- index = indexOf( array, value [, startIndex=1, endIndex=#array ] )
local function indexOf(t, v, i1, i2)
	for i = (i1 or 1), (i2 or #t) do
		if t[i] == v then  return i  end
	end
	return nil
end

-- item, index = itemWith1    ( array, key, value )
-- item, index = lastItemWith1( array, key, value )
local function itemWith1(t, k, v)
	for i, item in ipairs(t) do
		if item[k] == v then  return item, i  end
	end
	return nil
end
local function lastItemWith1(t, k, v)
	for i, item in ipairsr(t) do
		if item[k] == v then  return item, i  end
	end
	return nil
end



-- text = getRelativeLocationText( sourcePathOfInterest, lineNumberOfInterest, otherSourcePath, otherLineNumber )
-- text = getRelativeLocationText( lineNumberOfInterest, otherLineNumber )
local function getRelativeLocationText(sourcePath, ln, otherSourcePath, otherLn)
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
local function getRelativeLocationTextForToken(tokens, tokOfInterest, otherTok)
	return getRelativeLocationText((tokens[tokOfInterest] and tokens[tokOfInterest].lineStart or 0), (tokens[otherTok] and tokens[otherTok].lineStart or 0))
end

--[[
-- text = getRelativeLocationTextForNode( nodeOfInterest, otherNode )
-- text = getRelativeLocationTextForNode( nodeOfInterest, otherSourcePath, otherLineNumber )
local function getRelativeLocationTextForNode(nodeOfInterest, otherSourcePath, otherLn)
	if type(otherSourcePath) == "table" then
		return getRelativeLocationTextForNode(nodeOfInterest, otherSourcePath.sourcePath, otherSourcePath.line)
	end

	if not (nodeOfInterest.sourcePath ~= "?" and nodeOfInterest.line > 0) then
		return "at <UnknownLocation>"
	end

	return getRelativeLocationText(nodeOfInterest.sourcePath, nodeOfInterest.line, otherSourcePath, otherLn)
end
]]



local function formatNumber(n)
	-- @Speed: Cache!

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



local tokenize
do
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

	-- tokens, error = tokenize( luaString [, keepWhitespaceTokens=false ] [, pathForErrorMessages="?" ] )
	function tokenize(s, keepWhitespaceTokens, path)
		assertArg1("tokenize", 1, s, "string")

		if type(keepWhitespaceTokens) == "string" then
			path                 = keepWhitespaceTokens
			keepWhitespaceTokens = false
		else
			assertArg2("tokenize", 2, keepWhitespaceTokens, "boolean","nil")
			assertArg2("tokenize", 3, path,                 "string","nil")
		end

		if stringFind(s, "\r", 1, true) then
			s = stringGsub(s, "\r\n?", "\n")
		end
		path = path or "?"

		local tokens = {}
		local count  = 0
		local ptr    = 1
		local ln     = 1

		local BYTES_NAME_START      = TOKEN_BYTES.NAME_START
		local BYTES_DASH            = TOKEN_BYTES.DASH
		local BYTES_NUM             = TOKEN_BYTES.NUM
		-- local BYTES_NUM_OR_DOT   = TOKEN_BYTES.NUM_OR_DOT
		local BYTES_QUOTE           = TOKEN_BYTES.QUOTE
		local BYTES_SQUARE          = TOKEN_BYTES.SQUARE
		local BYTES_DOT             = TOKEN_BYTES.DOT
		local BYTES_PUNCT_TWO_CHARS = TOKEN_BYTES.PUNCT_TWO_CHARS
		local BYTES_PUNCT_ONE_CHAR  = TOKEN_BYTES.PUNCT_ONE_CHAR

		local NUM_HEX_FRAC_EXP      = NUMERAL_PATTERNS.HEX_FRAC_EXP
		local NUM_HEX_FRAC          = NUMERAL_PATTERNS.HEX_FRAC
		local NUM_HEX_EXP           = NUMERAL_PATTERNS.HEX_EXP
		local NUM_HEX               = NUMERAL_PATTERNS.HEX
		local NUM_BIN               = NUMERAL_PATTERNS.BIN
		local NUM_DEC_FRAC_EXP      = NUMERAL_PATTERNS.DEC_FRAC_EXP
		local NUM_DEC_FRAC          = NUMERAL_PATTERNS.DEC_FRAC
		local NUM_DEC_EXP           = NUMERAL_PATTERNS.DEC_EXP
		local NUM_DEC               = NUMERAL_PATTERNS.DEC

		while true do
			local i1, i2 = stringFind(s, "^[ \t\n]+", ptr)

			if i1 then
				if keepWhitespaceTokens then
					local lnStart = ln
					local tokRepr = stringSub(s, i1, i2)
					ln            = ln + countString(tokRepr, "\n", true)
					count         = count + 1

					tokens[count] = {
						type           = "whitespace",
						value          = tokRepr,
						representation = tokRepr,

						sourceString   = s,
						sourcePath     = path,

						lineStart      = lnStart,
						lineEnd        = ln,
						positionStart  = i1,
						positionEnd    = i2,
					}

				else
					ln = ln + countSubString(s, i1, i2, "\n", true)
				end

				ptr = i2 + 1
			end

			if ptr > #s then  break  end

			local ptrStart = ptr
			local lnStart  = ln
			local b1, b2   = stringByte(s, ptr, ptr+1)
			local tokType, tokRepr, tokValue

			-- Identifier/keyword.
			if BYTES_NAME_START[b1] then
				local i1, i2, word = stringFind(s, "^(.[%w_]*)", ptr)
				ptr      = i2+1
				tokType  = KEYWORDS[word] and "keyword" or "identifier"
				tokRepr  = stringSub(s, ptrStart, ptr-1)
				tokValue = tokRepr

			-- Comment.
			elseif BYTES_DASH[b1] and BYTES_DASH[b2] then
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
				tokValue = equalSignCountIfLong and stringSub(tokRepr, 5+equalSignCountIfLong, -3-equalSignCountIfLong) or stringSub(tokRepr, 3, -2)

			-- Number.
			elseif BYTES_NUM[b1] or (BYTES_DOT[b1] and BYTES_NUM[b2]) then
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
					if stringFind(s, "^[Ii]", i2+1) then -- Imaginary part of complex number.
						numStr = stringSub(s, i1, i2+1)
						i2     = i2 + 1

					elseif not maybeInt or stringFind(numStr, ".", 1, true) then
						-- void
					elseif stringFind(s, "^[Uu][Ll][Ll]", i2+1) then -- Unsigned 64-bit integer.
						numStr = stringSub(s, i1, i2+3)
						i2     = i2 + 3
					elseif stringFind(s, "^[Ll][Ll]", i2+1) then -- Signed 64-bit integer.
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
						if     pat == NUM_HEX_FRAC_EXP then  _, intStr, fracStr, expStr = stringMatch(numStrFallback, NUM_HEX_FRAC_EXP)
						elseif pat == NUM_HEX_FRAC     then  _, intStr, fracStr         = stringMatch(numStrFallback, NUM_HEX_FRAC) ; expStr  = "0"
						elseif pat == NUM_HEX_EXP      then  _, intStr,          expStr = stringMatch(numStrFallback, NUM_HEX_EXP)  ; fracStr = ""
						else return nil, formatErrorInFile(s, path, ptrStart, "Tokenizer", "Internal error parsing the number '%s'.", numStrFallback) end

						n = tonumber(intStr, 16) or 0 -- intStr may be "".

						local fracValue = 1
						for i = 1, #fracStr do
							fracValue = fracValue / 16
							n         = n + tonumber(stringSub(fracStr, i, i), 16) * fracValue
						end

						n = n * 2 ^ stringGsub(expStr, "^+", "")

					elseif kind == "binary" then
						n = tonumber(stringSub(numStrFallback, 3), 2)
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
			elseif BYTES_QUOTE[b1] then
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
			elseif BYTES_SQUARE[b1] and stringFind(s, "^=*%[", ptr+1) then
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
			elseif BYTES_DOT[b1] and stringFind(s, "^%.%.", ptr+1) then
				ptr      = ptr + 3
				tokType  = "punctuation"
				tokRepr  = stringSub(s, ptrStart, ptr-1)
				tokValue = tokRepr
			elseif BYTES_PUNCT_TWO_CHARS[b1] and stringFind(s, "^%.%.", ptr) or stringFind(s, "^[=~<>]=", ptr) or stringFind(s, "^::", ptr) or stringFind(s, "^//", ptr) or stringFind(s, "^<<", ptr) or stringFind(s, "^>>", ptr) then
				ptr      = ptr + 2
				tokType  = "punctuation"
				tokRepr  = stringSub(s, ptrStart, ptr-1)
				tokValue = tokRepr
			elseif BYTES_PUNCT_ONE_CHAR[b1] then
				ptr      = ptr + 1
				tokType  = "punctuation"
				tokRepr  = stringSub(s, ptrStart, ptr-1)
				tokValue = tokRepr

			else
				return nil, formatErrorInFile(s, path, ptrStart, "Tokenizer", "Unknown character.")
			end
			assert(tokType)

			ln    = ln + countString(tokRepr, "\n", true)
			count = count + 1

			tokens[count] = {
				type           = tokType,
				value          = tokValue,
				representation = tokRepr,

				sourceString   = s,
				sourcePath     = path,

				lineStart      = lnStart,
				lineEnd        = ln,
				positionStart  = ptrStart,
				positionEnd    = ptr - 1,
			}

			-- print(F("%4d %-11s '%s'", count, tokType, (stringGsub(tokRepr, "\n", "\\n"))))
		end

		return tokens
	end
end

-- tokens, error = tokenizeFile( path [, keepWhitespaceTokens=false ] )
local function tokenizeFile(path, keepWhitespaceTokens)
	assertArg1("tokenizeFile", 1, path,                 "string")
	assertArg2("tokenizeFile", 2, keepWhitespaceTokens, "boolean","nil")

	local file, err = ioOpen(path, "r")
	if not file then  return nil, F("Could not open file '%s'. (%s)", ensurePrintable(path), ensurePrintable(err))  end

	local s = file:read("*a")
	file:close()

	return tokenize(s, (keepWhitespaceTokens or false), path)
end

--
-- :TokenCreation
--
-- commentToken     = newToken( "comment",     contents )
-- identifierToken  = newToken( "identifier",  name )
-- keywordToken     = newToken( "keyword",     name )
-- numberToken      = newToken( "number",      number )
-- punctuationToken = newToken( "punctuation", punctuationString )
-- stringToken      = newToken( "string",      stringValue )
-- whitespaceToken  = newToken( "whitespace",  contents )
--
local function newToken(tokType, tokValue)
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
			tokValue == 0 and NORMALIZE_MINUS_ZERO and "0"      or -- Avoid '-0' sometimes.
			tokValue == 1/0                        and "(1/0)"  or
			tokValue == -1/0                       and "(-1/0)" or
			tokValue ~= tokValue                   and "(0/0)"  or
			formatNumber(tokValue)
		)

	elseif tokType == "string" then
		if type(tokValue) ~= "string" then  errorf(2, "Expected string value for 'string' token. (Got %s)", type(tokValue))  end
		tokRepr = stringGsub(F("%q", tokValue), "\n", "n")

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

	elseif tokType == "whitespace" then
		if type(tokValue) ~= "string"       then  errorf(2, "Expected string value for 'whitespace' token. (Got %s)", type(tokValue))  end
		if tokValue == ""                   then  errorf(2, "Value is empty.")  end -- Having a token that is zero characters long would be weird, so we disallow it.
		if stringFind(tokValue, "[^ \t\n]") then  errorf(2, "Value has non-whitespace characters.")  end
		tokRepr = tokValue

	else
		errorf(2, "Invalid token type '%s'.", tostring(tokType))
	end

	return {
		type           = tokType,
		value          = tokValue,
		representation = tokRepr,

		sourceString   = "",
		sourcePath     = "?",

		lineStart      = 0,
		lineEnd        = 0,
		positionStart  = 0,
		positionEnd    = 0,
	}
end

--
-- :TokenModification
--
-- updateToken( commentToken,     contents )
-- updateToken( identifierToken,  name )
-- updateToken( keywordToken,     name )
-- updateToken( numberToken,      number )
-- updateToken( punctuationToken, punctuationString )
-- updateToken( stringToken,      stringValue )
-- updateToken( whitespaceToken,  contents )
--
local function updateToken(tok, tokValue)
	if tok.type == "keyword" then
		if type(tokValue) ~= "string" then  errorf(2, "Expected string value for 'keyword' token. (Got %s)", type(tokValue))  end
		if not KEYWORDS[tokValue]     then  errorf(2, "Invalid keyword '%s'.", tokValue)  end
		tok.representation = tokValue

	elseif tok.type == "identifier" then
		if type(tokValue) ~= "string"                then  errorf(2, "Expected string value for 'identifier' token. (Got %s)", type(tokValue))  end
		if not stringFind(tokValue, "^[%a_][%w_]*$") then  errorf(2, "Invalid identifier '%s'.", tokValue)  end
		if KEYWORDS[tokValue]                        then  errorf(2, "Invalid identifier '%s'.", tokValue)  end
		tok.representation = tokValue

	elseif tok.type == "number" then
		if type(tokValue) ~= "number" then
			errorf(2, "Expected number value for 'number' token. (Got %s)", type(tokValue))
		end
		tok.representation = (
			tokValue == 0 and NORMALIZE_MINUS_ZERO and "0"      or -- Avoid '-0' sometimes.
			tokValue == 1/0                        and "(1/0)"  or
			tokValue == -1/0                       and "(-1/0)" or
			tokValue ~= tokValue                   and "(0/0)"  or
			formatNumber(tokValue)
		)

	elseif tok.type == "string" then
		if type(tokValue) ~= "string" then  errorf(2, "Expected string value for 'string' token. (Got %s)", type(tokValue))  end
		tok.representation = stringGsub(F("%q", tokValue), "\n", "n")

	elseif tok.type == "punctuation" then
		if type(tokValue) ~= "string" then  errorf(2, "Expected string value for 'punctuation' token. (Got %s)", type(tokValue))  end
		if not PUNCTUATION[tokValue]  then  errorf(2, "Invalid punctuation '%s'.", tokValue)  end
		tok.representation = tokValue

	elseif tok.type == "comment" then
		if type(tokValue) ~= "string" then  errorf(2, "Expected string value for 'comment' token. (Got %s)", type(tokValue))  end

		if stringFind(tokValue, "\n") then
			local equalSigns = stringFind(tokValue, "[[", 1, true) and "=" or ""

			while stringFind(tokValue, "]"..equalSigns.."]", 1, true) do
				equalSigns = equalSigns.."="
			end

			tok.representation = F("--[%s[%s]%s]", equalSigns, tokValue, equalSigns)

		else
			tok.representation = F("--%s\n", tokValue)
		end

	elseif tok.type == "whitespace" then
		if type(tokValue) ~= "string"       then  errorf(2, "Expected string value for 'whitespace' token. (Got %s)", type(tokValue))  end
		if tokValue == ""                   then  errorf(2, "Value is empty.")  end -- Having a token that is zero characters long would be weird, so we disallow it.
		if stringFind(tokValue, "[^ \t\n]") then  errorf(2, "Value has non-whitespace characters.")  end
		tok.representation = tokValue

	else
		errorf(2, "Internal error: Invalid token type '%s'.", tostring(tok.type))
	end

	tok.value = tokValue
end

local function cloneToken(tok)
	return {
		type           = tok.type,
		value          = tok.value,
		representation = tok.representation,

		sourceString   = tok.sourceString,
		sourcePath     = tok.sourcePath,

		lineStart      = tok.lineStart,
		lineEnd        = tok.lineEnd,
		positionStart  = tok.positionStart,
		positionEnd    = tok.positionEnd,
	}
end

local function concatTokens(tokens)
	local parts = {}

	for tok = 1, #tokens do
		local tokRepr     =             tokens[tok  ].representation
		local lastTokRepr = tok > 1 and tokens[tok-1].representation

		if lastTokRepr and (
			(stringFind(tokRepr, "^[%w_]") and stringFind(lastTokRepr, "[%w_]$")) or
			(stringFind(tokRepr, "^%."   ) and stringFind(lastTokRepr, "%.$"   )) or
			(stringFind(tokRepr, "^%-"   ) and stringFind(lastTokRepr, "%-$"   )) or
			(stringFind(tokRepr, "^/"    ) and stringFind(lastTokRepr, "/$"    )) or

			(tok > 1 and tokens[tok-1].type == "number" and stringFind(tokRepr,     "^[%w_.]")) or
			(            tokens[tok  ].type == "number" and stringFind(lastTokRepr, "%.$") and not stringFind(lastTokRepr, "%.%.$"))
		) then
			tableInsert(parts, " ")
		end
		tableInsert(parts, tokRepr)
	end

	return tableConcat(parts)
end



local function isToken(token, tokType, tokValue)
	return token ~= nil and token.type == tokType and token.value == tokValue
end

local function isTokenType(token, tokType)
	return token ~= nil and token.type == tokType
end

local function isTokenAnyValue(token, tokValueSet)
	return token ~= nil and tokValueSet[token.value] == true
end



local function getLeftmostToken(node)
	if node.type == "binary" then
		return getLeftmostToken(node.left)
	else
		return node.token
	end
end



local parseExpressionInternal, parseExpressionList, parseFunctionParametersAndBody, parseBlock

local function parseIdentifier(tokens, tok) --> ident, token, error
	if not isTokenType(tokens[tok], "identifier") then
		return nil, tok, formatErrorAtToken(tokens[tok], "Parser", "Expected an identifier.")
	end

	local ident = AstIdentifier(tokens[tok], tokens[tok].value)
	tok         = tok + 1

	return ident, tok
end

local function parseNameList(tokens, tok, names, allowVararg, allowAttributes) --> success, token, error|nil
	while true do
		if allowVararg and isToken(tokens[tok], "punctuation", "...") then
			local vararg = AstVararg(tokens[tok])
			tok          = tok + 1 -- '...'

			tableInsert(names, vararg)

			return true, tok
		end

		local ident, tokNext, err = parseIdentifier(tokens, tok)
		if not ident then  return false, tok, err  end
		tok = tokNext

		if allowAttributes and isToken(tokens[tok], "punctuation", "<") then
			tok = tok + 1 -- '<'

			local attrIdent, tokNext, err = parseIdentifier(tokens, tok)
			if not attrIdent then
				return false, tok, err
			elseif not (attrIdent.name == "close" or attrIdent.name == "const") then
				return false, tok, formatErrorAtToken(tokens[tok], "Parser", "Expected 'close' or 'const' for attribute name.")
			end
			tok = tokNext

			ident.attribute = attrIdent.name

			if not isToken(tokens[tok], "punctuation", ">") then
				return nil, tok, formatErrorAfterToken(tokens[tok-1], "Parser", "Expected '>' after attribute name.")
			end
			tok = tok + 1 -- '>'
		end

		tableInsert(names, ident)

		if not isToken(tokens[tok], "punctuation", ",") then
			return true, tok
		end
		tok = tok + 1 -- ','
	end

	return true, tok
end

local function parseTable(tokens, tokStart) --> tableNode, token, error
	local tok       = tokStart
	local tableNode = AstTable(tokens[tok])
	tok             = tok + 1 -- '{'

	local generatedIndex = 0

	while true do
		if isToken(tokens[tok], "punctuation", "}") then
			tok = tok + 1 -- '}'
			break

		elseif isToken(tokens[tok], "punctuation", "[") then
			tok = tok + 1 -- '['

			local keyExpr, tokNext, err = parseExpressionInternal(tokens, tok, 0)
			if not keyExpr then  return nil, tok, err  end
			tok = tokNext

			if not isToken(tokens[tok], "punctuation", "]") then
				return nil, tok, formatErrorAfterToken(tokens[tok-1], "Parser", "Expected ']' after key value.")
			end
			tok = tok + 1 -- ']'

			if not isToken(tokens[tok], "punctuation", "=") then
				return nil, tok, formatErrorAfterToken(tokens[tok-1], "Parser", "Expected '=' after key.")
			end
			tok = tok + 1 -- '='

			local valueExpr, tokNext, err = parseExpressionInternal(tokens, tok, 0)
			if not valueExpr then  return nil, tok, err  end
			tok = tokNext

			local tableField = {key=keyExpr, value=valueExpr, generatedKey=false}
			tableInsert(tableNode.fields, tableField)

		elseif isTokenType(tokens[tok], "identifier") and isToken(tokens[tok+1], "punctuation", "=") then
			local keyExpr = AstLiteral(tokens[tok], tokens[tok].value)
			tok           = tok + 1 -- identifier

			if not isToken(tokens[tok], "punctuation", "=") then
				return nil, tok, formatErrorAfterToken(tokens[tok-1], "Parser", "Expected '=' after key name.")
			end
			tok = tok + 1 -- '='

			local valueExpr, tokNext, err = parseExpressionInternal(tokens, tok, 0)
			if not valueExpr then  return nil, tok, err  end
			tok = tokNext

			local tableField = {key=keyExpr, value=valueExpr, generatedKey=false}
			tableInsert(tableNode.fields, tableField)

		else
			generatedIndex = generatedIndex + 1
			local keyExpr  = AstLiteral(tokens[tok], generatedIndex)

			local valueExpr, tokNext, err = parseExpressionInternal(tokens, tok, 0)
			if not valueExpr then  return nil, tok, err  end
			tok = tokNext

			local tableField = {key=keyExpr, value=valueExpr, generatedKey=true}
			tableInsert(tableNode.fields, tableField)
		end

		if isToken(tokens[tok], "punctuation", ",") or isToken(tokens[tok], "punctuation", ";") then
			tok = tok + 1 -- ',' or ';'
			-- Continue...

		elseif isToken(tokens[tok], "punctuation", "}") then
			tok = tok + 1 -- '}'
			break

		else
			return nil, tok, formatErrorAfterToken(
				tokens[tok-1], "Parser",
				"Expected ',' or '}' after value in table constructor (starting %s).",
				getRelativeLocationTextForToken(tokens, tokStart, tok-1)
			)
		end
	end

	return tableNode, tok
end

function parseExpressionInternal(tokens, tokStart, lastPrecedence) --> expression, token, error
	local tok                  = tokStart
	local canParseLookupOrCall = false
	local currentToken         = tokens[tok]
	local expr

	-- identifier
	if isTokenType(currentToken, "identifier") then
		local ident, tokNext, err = parseIdentifier(tokens, tok)
		if not ident then  return nil, tok, err  end
		tok = tokNext

		expr                 = ident
		canParseLookupOrCall = true

	-- ...
	elseif isToken(currentToken, "punctuation", "...") then
		local vararg = AstVararg(currentToken)
		tok          = tok + 1 -- '...'
		expr         = vararg

	-- literal
	elseif isTokenType(currentToken, "string") or isTokenType(currentToken, "number") then
		local literal = AstLiteral(currentToken, currentToken.value)
		tok           = tok + 1 -- literal
		expr          = literal
	elseif isToken(currentToken, "keyword", "true") then
		local literal = AstLiteral(currentToken, true)
		tok           = tok + 1 -- 'true'
		expr          = literal
	elseif isToken(currentToken, "keyword", "false") then
		local literal = AstLiteral(currentToken, false)
		tok           = tok + 1 -- 'false'
		expr          = literal
	elseif isToken(currentToken, "keyword", "nil") then
		local literal = AstLiteral(currentToken, nil)
		tok           = tok + 1 -- 'nil'
		expr          = literal

	-- unary
	elseif
		(isToken(currentToken, "keyword", "not") or (isTokenType(currentToken, "punctuation") and isTokenAnyValue(currentToken, OPERATORS_UNARY)))
		and OPERATOR_PRECEDENCE.unary > lastPrecedence
	then
		local unary = AstUnary(currentToken, currentToken.value)
		tok         = tok + 1 -- operator

		local subExpr, tokNext, err = parseExpressionInternal(tokens, tok, OPERATOR_PRECEDENCE.unary-1)
		if not subExpr then  return nil, tok, err  end
		unary.expression = subExpr
		tok              = tokNext

		expr = unary

		-- Special rule: Treat '-n' as one literal (but not '-n^n' because of operator precedence).
		if
			unary.operator == "-"
			and subExpr.type == "literal"
			and type(subExpr.value) == "number"
			and isTokenType(subExpr.token, "number")
			and not (subExpr.value == 0 and NORMALIZE_MINUS_ZERO) -- We cannot store -0 in Lua 5.3+, thus we need to keep the unary expression.
		then
			subExpr.value = -subExpr.value
			subExpr.token = unary.token
			expr          = subExpr
		end

	-- {...}
	elseif isToken(currentToken, "punctuation", "{") then
		local tableNode, tokNext, err = parseTable(tokens, tok)
		if not tableNode then  return nil, tok, err  end
		tok = tokNext

		expr = tableNode

	-- function
	elseif isToken(currentToken, "keyword", "function") then
		local funcTok = tok
		tok           = tok + 1 -- 'function'

		local func, tokNext, err = parseFunctionParametersAndBody(tokens, tok, funcTok)
		if not func then  return nil, tok, err  end
		func.token = tok
		tok        = tokNext

		expr = func

	-- (...)
	elseif isToken(currentToken, "punctuation", "(") then
		tok = tok + 1 -- '('

		local _expr, tokNext, err = parseExpressionInternal(tokens, tok, 0)
		if not _expr then  return nil, tok, err  end
		tok = tokNext

		if _expr.type == "call" or _expr.type == "vararg" then
			_expr.adjustToOne = true
		end

		if not isToken(tokens[tok], "punctuation", ")") then
			return nil, tok, formatErrorAfterToken(tokens[tok-1], "Parser", "Expected ')' (to end parenthesis expression starting %s).", getRelativeLocationTextForToken(tokens, tokStart, tok-1))
		end
		tok = tok + 1 -- ')'

		expr                 = _expr
		canParseLookupOrCall = true

	else
		return nil, tok, formatErrorAtToken(currentToken, "Parser", "Failed parsing expression.")
	end

	assert(expr)

	-- Binary expressions, including lookups and calls.
	while true do
		currentToken = tokens[tok]

		-- a + b
		if
			(
				(isTokenType(currentToken, "punctuation") and isTokenAnyValue(currentToken, OPERATORS_BINARY))
				or isToken(currentToken, "keyword", "and")
				or isToken(currentToken, "keyword", "or")
			)
			and OPERATOR_PRECEDENCE[currentToken.value] > lastPrecedence
		then
			local rightAssociative = isToken(currentToken, "punctuation", "..") or isToken(currentToken, "punctuation", "^")

			local tokOp  = tok
			local binary = AstBinary(currentToken, currentToken.value)
			tok          = tok + 1 -- operator

			local lhsExpr = expr

			local rhsExpr, tokNext, err = parseExpressionInternal(tokens, tok, OPERATOR_PRECEDENCE[binary.operator] + (rightAssociative and -1 or 0))
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
					isTokenType(lhsExpr.token, "number")
					or (
						isToken(lhsExpr.token, "punctuation", "-")
						and isTokenType(tokens[indexOf(tokens, lhsExpr.token, tokStart, tokOp-1) + 1], "number") -- @Speed: Don't use indexOf().
					)
				)
				and isTokenType(rhsExpr.token, "number")
			then
				lhsExpr.value = lhsExpr.value / 0
				expr          = lhsExpr
			end

		elseif not canParseLookupOrCall then
			break

		-- t.k
		elseif isToken(currentToken, "punctuation", ".") then
			local lookup = AstLookup(currentToken)
			tok          = tok + 1 -- '.'

			local ident, tokNext, err = parseIdentifier(tokens, tok)
			if not ident then  return nil, tok, err  end
			tok = tokNext

			local literal = AstLiteral(ident.token, ident.name)

			lookup.object = expr
			lookup.member = literal

			expr = lookup

		-- t[k]
		elseif isToken(currentToken, "punctuation", "[") then
			local lookup = AstLookup(currentToken)
			tok          = tok + 1 -- '['

			local memberExpr, tokNext, err = parseExpressionInternal(tokens, tok, 0)
			if not memberExpr then  return nil, tok, err  end
			tok = tokNext

			if not isToken(tokens[tok], "punctuation", "]") then
				return nil, tok, formatErrorAfterToken(tokens[tok-1], "Parser", "Expected ']' after lookup key value.")
			end
			tok = tok + 1 -- ']'

			lookup.object = expr
			lookup.member = memberExpr

			expr = lookup

		-- f""
		elseif isTokenType(currentToken, "string") then
			local call = AstCall(currentToken)

			local literal     = AstLiteral(currentToken, currentToken.value)
			tok               = tok + 1 -- string
			call.arguments[1] = literal

			call.callee = expr
			expr        = call

		-- f{}
		elseif isToken(currentToken, "punctuation", "{") then
			local call = AstCall(currentToken)

			local tableNode, tokNext, err = parseTable(tokens, tok)
			if not tableNode then  return nil, tok, err  end
			call.arguments[1] = tableNode
			tok               = tokNext

			call.callee = expr
			expr        = call

		-- f()
		elseif isToken(currentToken, "punctuation", "(") then
			if tok >= 2 and currentToken.lineStart > tokens[tok-1].lineEnd then
				return nil, tok, formatErrorAtToken(currentToken, "Parser", "Ambigous syntax. Is this a function call or a new statement?")
			end

			local call = AstCall(currentToken)
			tok        = tok + 1 -- '('

			if not isToken(tokens[tok], "punctuation", ")") then
				local ok, tokNext, err = parseExpressionList(tokens, tok, call.arguments)
				if not ok then  return nil, tok, err  end
				tok = tokNext
			end

			if not isToken(tokens[tok], "punctuation", ")") then
				return nil, tok, formatErrorAfterToken(tokens[tok-1], "Parser", "Expected ')' to end argument list for call.")
			end
			tok = tok + 1 -- ')'

			call.callee = expr
			expr        = call

		-- o:m()
		elseif isToken(currentToken, "punctuation", ":") then
			do
				local lookup = AstLookup(currentToken)
				tok          = tok + 1 -- ':'

				local ident, tokNext, err = parseIdentifier(tokens, tok)
				if not ident then  return nil, tok, err  end
				tok = tokNext

				local literal = AstLiteral(ident.token, ident.name)

				lookup.object = expr
				lookup.member = literal

				expr = lookup
			end

			do
				local call  = AstCall(tokens[tok])
				call.method = true

				if isTokenType(tokens[tok], "string") then
					local literal     = AstLiteral(tokens[tok], tokens[tok].value)
					tok               = tok + 1 -- string
					call.arguments[1] = literal

				elseif isToken(tokens[tok], "punctuation", "{") then
					local tableNode, tokNext, err = parseTable(tokens, tok)
					if not tableNode then  return nil, tok, err  end
					call.arguments[1] = tableNode
					tok               = tokNext

				elseif isToken(tokens[tok], "punctuation", "(") then
					if tok >= 2 and tokens[tok].lineStart > tokens[tok-1].lineEnd then
						return nil, tok, formatErrorAtToken(tokens[tok], "Parser", "Ambigous syntax. Is this a function call or a new statement?")
					end

					tok = tok + 1 -- '('

					if not isToken(tokens[tok], "punctuation", ")") then
						local ok, tokNext, err = parseExpressionList(tokens, tok, call.arguments)
						if not ok then  return nil, tok, err  end
						tok = tokNext
					end

					if not isToken(tokens[tok], "punctuation", ")") then
						return nil, tok, formatErrorAfterToken(tokens[tok-1], "Parser", "Expected ')' after argument list for method call.")
					end
					tok = tok + 1 -- ')'

				else
					return nil, tok, formatErrorAfterToken(tokens[tok-1], "Parser", "Expected '(' to start argument list for method call.")
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
		local expr, tokNext, err = parseExpressionInternal(tokens, tok, 0)
		if not expr then  return false, tok, err  end
		tok = tokNext

		tableInsert(expressions, expr)

		if not isToken(tokens[tok], "punctuation", ",") then
			return true, tok
		end
		tok = tok + 1 -- ','
	end
end

function parseFunctionParametersAndBody(tokens, tokStart, funcTok) --> func, token, error
	local tok  = tokStart
	local func = AstFunction(tokens[funcTok])

	if not isToken(tokens[tok], "punctuation", "(") then
		return nil, tok, formatErrorAfterToken(tokens[tok-1], "Parser", "Expected '(' to start parameter list for function.")
	end
	tok = tok + 1 -- '('

	if not isToken(tokens[tok], "punctuation", ")") then
		local ok, tokNext, err = parseNameList(tokens, tok, func.parameters, true, false)
		if not ok then  return nil, tok, err  end
		tok = tokNext
		-- @Cleanup: Move the vararg parameter parsing here.
	end

	if not isToken(tokens[tok], "punctuation", ")") then
		return nil, tok, formatErrorAfterToken(tokens[tok-1], "Parser", "Expected ')' to end parameter list for function.")
	end
	tok = tok + 1 -- ')'

	local block, tokNext, err = parseBlock(tokens, tok, tok-1, true)
	if not block then  return nil, tok, err  end
	func.body = block
	tok       = tokNext

	if not isToken(tokens[tok], "keyword", "end") then
		return nil, tok, formatErrorAtToken(tokens[tok], "Parser", "Expected 'end' to end function (starting %s).", getRelativeLocationTextForToken(tokens, tokStart, tok))
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
	local tok          = tokStart
	local currentToken = tokens[tok]

	-- do
	if isToken(currentToken, "keyword", "do") then
		tok = tok + 1 -- 'do'

		local block, tokNext, err = parseBlock(tokens, tok, tok-1, true)
		if not block then  return false, tok, err  end
		block.token = tok - 1
		tok         = tokNext

		if not isToken(tokens[tok], "keyword", "end") then
			return false, tok, formatErrorAtToken(tokens[tok], "Parser", "Expected 'end' to end 'do' block (starting %s).", getRelativeLocationTextForToken(tokens, tokStart, tok))
		end
		tok = tok + 1 -- 'end'

		tableInsert(statements, block)
		return true, tok

	-- while
	elseif isToken(currentToken, "keyword", "while") then
		local whileLoop = AstWhile(currentToken)
		tok             = tok + 1 -- 'while'

		local expr, tokNext, err = parseExpressionInternal(tokens, tok, 0)
		if not expr then  return false, tok, err  end
		whileLoop.condition = expr
		tok                 = tokNext

		if not isToken(tokens[tok], "keyword", "do") then
			return false, tok, formatErrorAfterToken(tokens[tok-1], "Parser", "Expected 'do' to start body for 'while' loop.")
		end
		tok = tok + 1 -- 'do'

		local block, tokNext, err = parseBlock(tokens, tok, tok-1, true)
		if not block then  return false, tok, err  end
		block.token    = tok - 1
		whileLoop.body = block
		tok            = tokNext

		if not isToken(tokens[tok], "keyword", "end") then
			return false, tok, formatErrorAtToken(tokens[tok], "Parser", "Expected 'end' to end 'while' loop (starting %s).", getRelativeLocationTextForToken(tokens, tokStart, tok))
		end
		tok = tok + 1 -- 'end'

		tableInsert(statements, whileLoop)
		return true, tok

	-- repeat
	elseif isToken(currentToken, "keyword", "repeat") then
		local repeatLoop = AstRepeat(currentToken)
		tok              = tok + 1 -- 'repeat'

		local block, tokNext, err = parseBlock(tokens, tok, tok-1, true)
		if not block then  return false, tok, err  end
		repeatLoop.body = block
		tok             = tokNext

		if not isToken(tokens[tok], "keyword", "until") then
			return false, tok, formatErrorAtToken(tokens[tok], "Parser", "Expected 'until' at the end of 'repeat' loop (starting %s).", getRelativeLocationTextForToken(tokens, tokStart, tok))
		end
		tok = tok + 1 -- 'until'

		local expr, tokNext, err = parseExpressionInternal(tokens, tok, 0)
		if not expr then  return false, tok, err  end
		repeatLoop.condition = expr
		tok                  = tokNext

		tableInsert(statements, repeatLoop)
		return true, tok

	-- if
	elseif isToken(currentToken, "keyword", "if") then
		local ifNode = AstIf(currentToken)
		tok          = tok + 1 -- 'if'

		local expr, tokNext, err = parseExpressionInternal(tokens, tok, 0)
		if not expr then  return false, tok, err  end
		ifNode.condition = expr
		tok              = tokNext

		if not isToken(tokens[tok], "keyword", "then") then
			return false, tok, formatErrorAtToken(tokens[tok], "Parser", "Expected 'then' after 'if' condition.")
		end
		tok = tok + 1 -- 'then'

		local block, tokNext, err = parseBlock(tokens, tok, tok-1, true)
		if not block then  return false, tok, err  end
		ifNode.bodyTrue = block
		tok             = tokNext

		local ifNodeLeaf = ifNode

		while isToken(tokens[tok], "keyword", "elseif") do
			tok = tok + 1 -- 'elseif'

			ifNodeLeaf.bodyFalse               = AstBlock(tokens[tok])
			ifNodeLeaf.bodyFalse.statements[1] = AstIf   (tokens[tok])
			ifNodeLeaf                         = ifNodeLeaf.bodyFalse.statements[1]

			local expr, tokNext, err = parseExpressionInternal(tokens, tok, 0)
			if not expr then  return false, tok, err  end
			ifNodeLeaf.condition = expr
			tok                  = tokNext

			if not isToken(tokens[tok], "keyword", "then") then
				return false, tok, formatErrorAtToken(tokens[tok], "Parser", "Expected 'then' after 'elseif' condition.")
			end
			tok = tok + 1 -- 'then'

			local block, tokNext, err = parseBlock(tokens, tok, tok-1, true)
			if not block then  return false, tok, err  end
			ifNodeLeaf.bodyTrue = block
			tok                 = tokNext
		end

		if isToken(tokens[tok], "keyword", "else") then
			tok = tok + 1 -- 'else'

			local block, tokNext, err = parseBlock(tokens, tok, tok-1, true)
			if not block then  return false, tok, err  end
			ifNodeLeaf.bodyFalse = block
			tok                  = tokNext
		end

		if not isToken(tokens[tok], "keyword", "end") then
			return false, tok, formatErrorAtToken(tokens[tok], "Parser", "Expected 'end' to end 'if' statement (starting %s).", getRelativeLocationTextForToken(tokens, tokStart, tok))
		end
		tok = tok + 1 -- 'end'

		tableInsert(statements, ifNode)
		return true, tok

	-- for
	elseif isToken(currentToken, "keyword", "for") then
		local forLoop = AstFor(currentToken, "")
		tok           = tok + 1 -- 'for'

		local ok, tokNext, err = parseNameList(tokens, tok, forLoop.names, false, false)
		if not ok then  return false, tok, err  end
		tok = tokNext

		if isToken(tokens[tok], "keyword", "in") then
			forLoop.kind = "generic"
			tok          = tok + 1 -- 'in'

		elseif isToken(tokens[tok], "punctuation", "=") then
			if forLoop.names[2] then
				return false, tok, formatErrorAtToken(tokens[tok], "Parser", "Expected 'in' for generic loop.")
			end

			forLoop.kind = "numeric"
			tok          = tok + 1 -- '='

		else
			return false, tok, formatErrorAfterToken(tokens[tok-1], "Parser", "Expected '=' or 'in' for 'for' loop.")
		end

		local valuesStartTok = tok

		local ok, tokNext, err = parseExpressionList(tokens, tok, forLoop.values, 0)
		if not ok then  return false, tok, err  end
		tok = tokNext

		if forLoop.kind ~= "numeric" then
			-- void
		elseif not forLoop.values[2] then
			return false, tok, formatErrorAtToken(tokens[valuesStartTok], "Parser", "Numeric loop: Too few values.")
		elseif forLoop.values[4] then
			-- @Cleanup: Instead of using getLeftmostToken(), make parseExpressionList() return a list of expression start tokens.
			return false, tok, formatErrorAtToken(getLeftmostToken(forLoop.values[4]), "Parser", "Numeric loop: Too many values.")
		end

		if not isToken(tokens[tok], "keyword", "do") then
			return false, tok, formatErrorAfterToken(tokens[tok-1], "Parser", "Expected 'do' to start body for 'for' loop.")
		end
		tok = tok + 1 -- 'do'

		local block, tokNext, err = parseBlock(tokens, tok, tok-1, true)
		if not block then  return false, tok, err  end
		forLoop.body = block
		tok          = tokNext

		if not isToken(tokens[tok], "keyword", "end") then
			return false, tok, formatErrorAtToken(tokens[tok], "Parser", "Expected 'end' to end 'for' loop (starting %s).", getRelativeLocationTextForToken(tokens, tokStart, tok))
		end
		tok = tok + 1 -- 'end'

		tableInsert(statements, forLoop)
		return true, tok

	-- function
	elseif isToken(currentToken, "keyword", "function") then
		local funcTok    = tok
		local assignment = AstAssignment(currentToken)
		tok              = tok + 1 -- 'function'

		local targetExpr, tokNext, err = parseIdentifier(tokens, tok)
		if not targetExpr then  return false, tok, err  end
		tok = tokNext

		while isToken(tokens[tok], "punctuation", ".") do
			local lookup = AstLookup(tokens[tok])
			tok          = tok + 1 -- '.'

			local ident, tokNext, err = parseIdentifier(tokens, tok)
			if not ident then  return false, tok, err  end
			tok = tokNext

			local literal = AstLiteral(ident.token, ident.name)
			lookup.member = literal

			lookup.object = targetExpr
			lookup.member = literal

			targetExpr = lookup
		end

		local isMethod = isToken(tokens[tok], "punctuation", ":")

		if isMethod then
			local lookup = AstLookup(tokens[tok])
			tok          = tok + 1 -- ':'

			local ident, tokNext, err = parseIdentifier(tokens, tok)
			if not ident then  return false, tok, err  end
			tok = tokNext

			local literal = AstLiteral(ident.token, ident.name)
			lookup.member = literal

			lookup.object = targetExpr
			lookup.member = literal

			targetExpr = lookup
		end

		local func, tokNext, err = parseFunctionParametersAndBody(tokens, tok, funcTok)
		if not func then  return false, tok, err  end
		tok = tokNext

		if isMethod then
			local ident = AstIdentifier(func.token, "self")
			tableInsert(func.parameters, 1, ident)
		end

		assignment.targets[1] = targetExpr
		assignment.values[1]  = func

		tableInsert(statements, assignment)
		return true, tok

	-- local function
	elseif isToken(currentToken, "keyword", "local") and isToken(tokens[tok+1], "keyword", "function") then
		local funcTok    = tok + 1
		local decl       = AstDeclaration(currentToken)
		local assignment = AstAssignment(currentToken)
		tok              = tok + 2 -- 'local function'

		local ident, tokNext, err = parseIdentifier(tokens, tok)
		if not ident then  return false, tok, err  end
		local identCopy = parseIdentifier(tokens, tok)
		tok             = tokNext

		local func, tokNext, err = parseFunctionParametersAndBody(tokens, tok, funcTok)
		if not func then  return false, tok, err  end
		tok = tokNext

		decl.names[1]         = ident
		assignment.targets[1] = identCopy
		assignment.values[1]  = func

		tableInsert(statements, decl)
		tableInsert(statements, assignment)
		return true, tok

	-- local
	elseif isToken(currentToken, "keyword", "local") then
		local decl = AstDeclaration(currentToken)
		tok        = tok + 1 -- 'local'

		local ok, tokNext, err = parseNameList(tokens, tok, decl.names, false, true)
		if not ok then  return false, tok, err  end
		tok = tokNext

		if isToken(tokens[tok], "punctuation", "=") then
			tok = tok + 1 -- '='

			local ok, tokNext, err = parseExpressionList(tokens, tok, decl.values)
			if not ok then  return false, tok, err  end
			tok = tokNext
		end

		tableInsert(statements, decl)
		return true, tok

	-- ::label::
	elseif isToken(currentToken, "punctuation", "::") then
		local label = AstLabel(currentToken, "")
		tok         = tok + 1 -- '::'

		local labelIdent, tokNext, err = parseIdentifier(tokens, tok)
		if not labelIdent then  return false, tok, err  end
		tok = tokNext

		label.name = labelIdent.name

		if not isToken(tokens[tok], "punctuation", "::") then
			return false, tok, formatErrorAfterToken(tokens[tok-1], "Parser", "Expected '::' after label name.")
		end
		tok = tok + 1 -- '::'

		tableInsert(statements, label)
		return true, tok

	-- goto
	elseif isToken(currentToken, "keyword", "goto") then
		local gotoNode = AstGoto(currentToken, "")
		tok            = tok + 1 -- 'goto'

		local labelIdent, tokNext, err = parseIdentifier(tokens, tok)
		if not labelIdent then  return false, tok, err  end
		tok = tokNext

		gotoNode.name = labelIdent.name

		tableInsert(statements, gotoNode)
		return true, tok

	-- break
	elseif isToken(currentToken, "keyword", "break") then
		local breakNode = AstBreak(currentToken)
		tok             = tok + 1 -- 'break'

		tableInsert(statements, breakNode)
		return true, tok

	-- return (last)
	elseif isToken(currentToken, "keyword", "return") then
		local returnNode = AstReturn(currentToken)
		tok              = tok + 1 -- 'return'

		if tokens[tok] and not (
			(isTokenType(tokens[tok], "keyword") and isTokenAnyValue(tokens[tok], BLOCK_END_TOKEN_TYPES))
			or isToken(tokens[tok], "punctuation", ";")
			or isTokenType(tokens[tok], "end")
		) then
			local ok, tokNext, err = parseExpressionList(tokens, tok, returnNode.values)
			if not ok then  return false, tok, err  end
			tok = tokNext
		end

		tableInsert(statements, returnNode)
		return true, tok

	elseif isTokenType(currentToken, "keyword") then
		return false, tok, ""

	else
		local lookahead, tokNext, err = parseExpressionInternal(tokens, tok, 0)
		if not lookahead then  return false, tok, err  end

		if lookahead.type == "call" then
			local call = lookahead
			tok        = tokNext

			tableInsert(statements, call)
			return true, tok

		elseif isToken(tokens[tokNext], "punctuation", "=") or isToken(tokens[tokNext], "punctuation", ",") then
			local assignment = AstAssignment(tokens[tokNext])

			local ok, tokNext, err = parseExpressionList(tokens, tok, assignment.targets)
			if not ok then  return false, tok, err  end
			tok = tokNext

			if not isToken(tokens[tok], "punctuation", "=") then
				return false, tok, formatErrorAfterToken(tokens[tok-1], "Parser", "Expected '=' for an assignment.")
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
	local block      = AstBlock(tokens[blockTok])
	local statements = block.statements

	while tok <= #tokens and tokens[tok].type ~= "end" do
		while isToken(tokens[tok], "punctuation", ";") do
			-- Empty statements are valid in Lua 5.2+.
			tok = tok + 1 -- ';'
		end

		local statementStartTok = tok

		if stopAtEndKeyword and isTokenType(tokens[tok], "keyword") and isTokenAnyValue(tokens[tok], BLOCK_END_TOKEN_TYPES) then
			break
		end

		local ok, tokNext, err = parseOneOrPossiblyMoreStatements(tokens, tok, statements)
		if not ok then
			if not statementErrorReported then
				statementErrorReported = true
				err                    = (err ~= "" and err.."\n" or "") .. formatErrorAtToken(tokens[tok], "Parser", "Failed parsing statement.")
			end
			return nil, tok, err
		end
		tok = tokNext

		if isToken(tokens[tok], "punctuation", ";") then
			tok = tok + 1 -- ';'
		end

		local lastAddedStatement = statements[#statements]

		if lastAddedStatement.type == "return" then -- Note: 'break' statements are allowed in the middle of blocks as of Lua 5.2.
			break

		elseif lastAddedStatement.type == "call" and lastAddedStatement.adjustToOne then
			statementErrorReported = true
			return nil, tok, formatErrorAtToken(tokens[statementStartTok], "Parser", "Syntax error.")
		end
	end

	return block, tok
end

-- block, error = tokensToAst( tokens, asBlock )
local function tokensToAst(tokens, asBlock)
	local tokensPurged = {}
	local count        = 0

	do
		-- Dummy start token.
		local token = tokens[1]
		count       = count + 1

		tokensPurged[count] = {
			type           = "start",
			value          = "",
			representation = "",

			sourceString   = token and token.sourceString or "",
			sourcePath     = token and token.sourcePath   or "?",

			lineStart      = 1,
			lineEnd        = 1,
			positionStart  = 1,
			positionEnd    = 1,
		}
	end

	-- Remove useless tokens.
	for tok = 1, #tokens do
		local tokType = tokens[tok].type

		if not (tokType == "comment" or tokType == "whitespace") then
			count               = count + 1
			tokensPurged[count] = tokens[tok]
		end
	end

	do
		-- Dummy end token.
		local token = tokens[#tokens]
		local ln    = token and 1+countString(token.sourceString, "\n", true) or 0
		local pos   = token and #token.sourceString+1                         or 0
		count       = count + 1

		tokensPurged[count] = {
			type           = "end",
			value          = "",
			representation = "",

			sourceString   = token and token.sourceString or "",
			sourcePath     = token and token.sourcePath   or "?",

			lineStart      = ln,
			lineEnd        = ln,
			positionStart  = pos,
			positionEnd    = pos,
		}
	end

	statementErrorReported = false

	local ast, _, err
	if asBlock then
		ast, _, err = parseBlock(tokensPurged, 2, 2, false)
	else
		ast, _, err = parseExpressionInternal(tokensPurged, 2, 0)
	end
	if not ast then  return nil, err  end

	return ast
end

-- ast, error = parse( tokens )
-- ast, error = parse( luaString [, pathForErrorMessages="?" ] )
local function parse(luaOrTokens, path)
	assertArg2("parse", 1, luaOrTokens, "string","table")

	-- ast, error = parse( tokens )
	if type(luaOrTokens) == "table" then
		assertArg1("parse", 2, path, "nil")

		return tokensToAst(luaOrTokens, true)

	-- ast, error = parse( luaString, pathForErrorMessages )
	else
		if path == nil then
			path = "?"
		else
			assertArg1("parse", 2, path, "string")
		end

		local tokens, err = tokenize(luaOrTokens, path)
		if not tokens then  return nil, err  end

		return tokensToAst(tokens, true)
	end
end

-- ast, error = parseExpression( tokens )
-- ast, error = parseExpression( luaString [, pathForErrorMessages="?" ] )
local function parseExpression(luaOrTokens, path)
	assertArg2("parseExpression", 1, luaOrTokens, "string","table")

	-- ast, error = parseExpression( tokens )
	if type(luaOrTokens) == "table" then
		assertArg1("parseExpression", 2, path, "nil")

		return tokensToAst(luaOrTokens, false)

	-- ast, error = parseExpression( luaString, pathForErrorMessages )
	else
		if path == nil then
			path = "?"
		else
			assertArg1("parseExpression", 2, path, "string")
		end

		local tokens, err = tokenize(luaOrTokens, path)
		if not tokens then  return nil, err  end

		return tokensToAst(tokens, false)
	end
end

-- ast, error = parseFile( path )
local function parseFile(path)
	assertArg1("parseFile", 1, path, "string")

	local tokens, err = tokenizeFile(path)
	if not tokens then  return nil, err  end

	return tokensToAst(tokens, true)
end



local nodeConstructors = {
	["vararg"]      = function()  return AstVararg     (nil)  end,
	["table"]       = function()  return AstTable      (nil)  end,
	["lookup"]      = function()  return AstLookup     (nil)  end,
	["call"]        = function()  return AstCall       (nil)  end,
	["function"]    = function()  return AstFunction   (nil)  end,
	["break"]       = function()  return AstBreak      (nil)  end,
	["return"]      = function()  return AstReturn     (nil)  end,
	["block"]       = function()  return AstBlock      (nil)  end,
	["declaration"] = function()  return AstDeclaration(nil)  end,
	["assignment"]  = function()  return AstAssignment (nil)  end,
	["if"]          = function()  return AstIf         (nil)  end,
	["while"]       = function()  return AstWhile      (nil)  end,
	["repeat"]      = function()  return AstRepeat     (nil)  end,

	["identifier"] = function(argCount, name, attribute)
		if argCount == 0 then
			errorf(3, "Missing name argument for identifier.")
		elseif type(name) ~= "string" then
			errorf(3, "Invalid name argument value type '%s'. (Expected string)", type(name))
		elseif not stringFind(name, "^[%a_][%w_]*$") or KEYWORDS[name] then
			errorf(3, "Invalid identifier name '%s'.", name)
		end

		if attribute == nil or attribute == "" then
			-- void
		elseif type(attribute) ~= "string" then
			errorf(3, "Invalid attribute argument value type '%s'. (Expected string)", type(attribute))
		elseif not (attribute == "close" or attribute == "const") then
			errorf(3, "Invalid attribute name '%s'. (Must be 'close' or 'const'.)", attribute)
		end

		local ident     = AstIdentifier(nil, name)
		ident.attribute = attribute or ""
		return ident
	end,

	["label"] = function(argCount, name)
		if argCount == 0 then
			errorf(3, "Missing name argument for label.")
		elseif type(name) ~= "string" then
			errorf(3, "Invalid name argument value type '%s'. (Expected string)", type(name))
		elseif not stringFind(name, "^[%a_][%w_]*$") or KEYWORDS[name] then
			errorf(3, "Invalid label name '%s'.", name)
		end

		return AstLabel(nil, name)
	end,

	["goto"] = function(argCount, name)
		if argCount == 0 then
			errorf(3, "Missing label name argument for goto.")
		elseif type(name) ~= "string" then
			errorf(3, "Invalid label name argument value type '%s'. (Expected string)", type(name))
		elseif not stringFind(name, "^[%a_][%w_]*$") or KEYWORDS[name] then
			errorf(3, "Invalid label name '%s'.", name)
		end

		return AstGoto(nil, name)
	end,

	["literal"] = function(argCount, value)
		if argCount == 0 then
			errorf(3, "Missing value argument for literal.")
		elseif not (type(value) == "number" or type(value) == "string" or type(value) == "boolean" or type(value) == "nil") then
			errorf(3, "Invalid literal value type '%s'. (Expected number, string, boolean or nil)", type(value))
		end

		return AstLiteral(nil, value)
	end,

	["unary"] = function(argCount, op)
		if argCount == 0 then
			errorf(3, "Missing operator argument for unary expression.")
		elseif not OPERATORS_UNARY[op] then
			errorf(3, "Invalid unary operator '%s'.", tostring(op))
		end

		return AstUnary(nil, op)
	end,

	["binary"] = function(argCount, op)
		if argCount == 0 then
			errorf(3, "Missing operator argument for binary expression.")
		elseif not OPERATORS_BINARY[op] then
			errorf(3, "Invalid binary operator '%s'.", tostring(op))
		end

		return AstBinary(nil, op)
	end,

	["for"] = function(argCount, kind)
		if argCount == 0 then
			errorf(3, "Missing kind argument for 'for' loop.")
		elseif not (kind == "numeric" or kind == "generic") then
			errorf(3, "Invalid for loop kind '%s'. (Must be 'numeric' or 'generic')", tostring(kind))
		end

		return AstFor(nil, kind)
	end,
}

local nodeConstructorsFast = {
	["vararg"]      = function()  return AstVararg     (nil)  end,
	["table"]       = function()  return AstTable      (nil)  end,
	["lookup"]      = function()  return AstLookup     (nil)  end,
	["call"]        = function()  return AstCall       (nil)  end,
	["function"]    = function()  return AstFunction   (nil)  end,
	["break"]       = function()  return AstBreak      (nil)  end,
	["return"]      = function()  return AstReturn     (nil)  end,
	["block"]       = function()  return AstBlock      (nil)  end,
	["declaration"] = function()  return AstDeclaration(nil)  end,
	["assignment"]  = function()  return AstAssignment (nil)  end,
	["if"]          = function()  return AstIf         (nil)  end,
	["while"]       = function()  return AstWhile      (nil)  end,
	["repeat"]      = function()  return AstRepeat     (nil)  end,

	["label"]       = function(name)   return AstLabel  (nil, name)   end,
	["goto"]        = function(name)   return AstGoto   (nil, name)   end,
	["literal"]     = function(value)  return AstLiteral(nil, value)  end,
	["unary"]       = function(op)     return AstUnary  (nil, op)     end,
	["binary"]      = function(op)     return AstBinary (nil, op)     end,
	["for"]         = function(kind)   return AstFor    (nil, kind)   end,

	["identifier"] = function(name, attribute)
		local ident     = AstIdentifier(nil, name)
		ident.attribute = attribute or ""
		return ident
	end,
}



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
	if nodeConstructors[nodeType] then
		return (nodeConstructors[nodeType](select("#", ...), ...))
	else
		errorf(2, "Invalid node type '%s'.", tostring(nodeType))
	end
end
local function newNodeFast(nodeType, ...)
	return nodeConstructorsFast[nodeType](...)
end

local cloneNodeArrayAndChildren

local function cloneNodeAndMaybeChildren(node, cloneChildren)
	local nodeType = node.type
	local clone

	if nodeType == "identifier" then
		clone           = AstIdentifier(nil, node.name)
		clone.attribute = node.attribute

	elseif nodeType == "vararg" then
		clone             = AstVararg(nil)
		clone.adjustToOne = node.adjustToOne

	elseif nodeType == "literal" then
		clone = AstLiteral(nil, node.value)

	elseif nodeType == "break" then
		clone = AstBreak(nil)

	elseif nodeType == "label" then
		clone = AstLabel(nil, node.name)

	elseif nodeType == "goto" then
		clone = AstGoto(nil, node.name)

	elseif nodeType == "lookup" then
		clone = AstLookup(nil)

		if cloneChildren then
			clone.object = node.object and cloneNodeAndMaybeChildren(node.object, true)
			clone.member = node.member and cloneNodeAndMaybeChildren(node.member, true)
		end

	elseif nodeType == "unary" then
		clone = AstUnary(nil, node.operator)

		if cloneChildren then
			clone.expression = node.expression and cloneNodeAndMaybeChildren(node.expression, true)
		end

	elseif nodeType == "binary" then
		clone = AstBinary(nil, node.operator)

		if cloneChildren then
			clone.left  = node.left  and cloneNodeAndMaybeChildren(node.left,  true)
			clone.right = node.right and cloneNodeAndMaybeChildren(node.right, true)
		end

	elseif nodeType == "call" then
		clone             = AstCall(nil)
		clone.method      = node.method
		clone.adjustToOne = node.adjustToOne

		if cloneChildren then
			clone.callee = node.callee and cloneNodeAndMaybeChildren(node.callee, true)
			cloneNodeArrayAndChildren(clone.arguments, node.arguments)
		end

	elseif nodeType == "function" then
		clone = AstFunction(nil)

		if cloneChildren then
			cloneNodeArrayAndChildren(clone.parameters, node.parameters)
			clone.body = node.body and cloneNodeAndMaybeChildren(node.body, true)
		end

	elseif nodeType == "return" then
		clone = AstReturn(nil)

		if cloneChildren then
			cloneNodeArrayAndChildren(clone.values, node.values)
		end

	elseif nodeType == "block" then
		clone = AstBlock(nil)

		if cloneChildren then
			cloneNodeArrayAndChildren(clone.statements, node.statements)
		end

	elseif nodeType == "declaration" then
		clone = AstDeclaration(nil)

		if cloneChildren then
			cloneNodeArrayAndChildren(clone.names,  node.names)
			cloneNodeArrayAndChildren(clone.values, node.values)
		end

	elseif nodeType == "assignment" then
		clone = AstAssignment(nil)

		if cloneChildren then
			cloneNodeArrayAndChildren(clone.targets, node.targets)
			cloneNodeArrayAndChildren(clone.values,  node.values)
		end

	elseif nodeType == "if" then
		clone = AstIf(nil)

		if cloneChildren then
			clone.condition = node.condition and cloneNodeAndMaybeChildren(node.condition, true)
			clone.bodyTrue  = node.bodyTrue  and cloneNodeAndMaybeChildren(node.bodyTrue,  true)
			clone.bodyFalse = node.bodyFalse and cloneNodeAndMaybeChildren(node.bodyFalse, true)
		end

	elseif nodeType == "while" then
		clone = AstWhile(nil)

		if cloneChildren then
			clone.condition = node.condition and cloneNodeAndMaybeChildren(node.condition, true)
			clone.body      = node.body      and cloneNodeAndMaybeChildren(node.body,      true)
		end

	elseif nodeType == "repeat" then
		clone = AstRepeat(nil)

		if cloneChildren then
			clone.body      = node.body      and cloneNodeAndMaybeChildren(node.body,      true)
			clone.condition = node.condition and cloneNodeAndMaybeChildren(node.condition, true)
		end

	elseif nodeType == "for" then
		clone = AstFor(nil, node.kind)

		if cloneChildren then
			cloneNodeArrayAndChildren(clone.names,  node.names)
			cloneNodeArrayAndChildren(clone.values, node.values)
			clone.body = node.body and cloneNodeAndMaybeChildren(node.body, true)
		end

	elseif nodeType == "table" then
		clone = AstTable(nil)

		if cloneChildren then
			for i, tableField in ipairs(node.fields) do
				clone.fields[i] = {
					key          = tableField.key   and cloneNodeAndMaybeChildren(tableField.key,   true),
					value        = tableField.value and cloneNodeAndMaybeChildren(tableField.value, true),
					generatedKey = tableField.generatedKey,
				}
			end
		end

	else
		errorf("Invalid node type '%s'.", tostring(nodeType))
	end

	clone.pretty = node.pretty
	clone.prefix = node.prefix
	clone.suffix = node.suffix
	-- Should we set node.token etc. too? @Incomplete

	return clone
end

function cloneNodeArrayAndChildren(cloneArray, sourceArray)
	for i, node in ipairs(sourceArray) do
		cloneArray[i] = cloneNodeAndMaybeChildren(node, true)
	end
end

local function cloneNode(node)
	return (cloneNodeAndMaybeChildren(node, false))
end

local function cloneTree(node)
	return (cloneNodeAndMaybeChildren(node, true))
end



local INVOLVED_NEVER  = newSet{ "function", "literal", "vararg" }
local INVOLVED_ALWAYS = newSet{ "break", "call", "goto", "label", "lookup", "return" }

local mayAnyNodeBeInvolvedInJump

local function mayNodeBeInvolvedInJump(node)
	if INVOLVED_NEVER[node.type] then
		return false

	elseif INVOLVED_ALWAYS[node.type] then
		return true

	elseif node.type == "identifier" then
		return (node.declaration == nil) -- Globals may invoke a metamethod on the environment.

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

--[[local]] function mayAnyNodeBeInvolvedInJump(nodes)
	for _, node in ipairs(nodes) do
		if mayNodeBeInvolvedInJump(node) then  return true  end
	end
	return false
end



local printNode, printTree
do
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

			if node.declaration then
				ioWrite(" (decl=", node.declaration.type)
				if parser.printIds then  ioWrite("#", node.declaration.id)  end
				ioWrite(")")
			end

		elseif nodeType == "literal" then
			if node.value == nil or node.value == true or node.value == false then
				ioWrite(" (", tostring(node.value), ")")
			elseif type(node.value) == "string" then
				ioWrite(' (string="', ensurePrintable(node.value), '")')
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
			if node.parameters[1] and node.parameters[#node.parameters].type == "vararg" then  ioWrite(" (vararg)")  end

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
			ioWrite(tostring(key), " ")
		end

		_printNode(node)

		local nodeType = node.type

		if nodeType == "table" then
			for i, tableField in ipairs(node.fields) do
				if tableField.key then
					_printTree(tableField.key, indent, i..(tableField.generatedKey and "KEYGEN" or "KEY   "))
				elseif tableField.generatedKey then
					for i = 1, indent do  ioWrite(parser.indentation)  end
					ioWrite(i, "KEYGEN -\n")
				end
				if tableField.value then  _printTree(tableField.value, indent, i.."VALUE ")  end
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
local function traverseTree(node, leavesFirst, cb, parent, container, k)
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
		if node.body and traverseTree(node.body, leavesFirst, cb, node, node, "body") then  return true  end

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
local function traverseTreeReverse(node, leavesFirst, cb, parent, container, k)
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
		if node.body and traverseTreeReverse(node.body, leavesFirst, cb, node, node, "body") then  return true  end
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



-- declIdent | nil  = findIdentifierDeclaration( ident )
-- declIdent.parent = decl | func | forLoop
local function findIdentifierDeclaration(ident)
	local name   = ident.name
	local parent = ident

	while true do
		local lastChild = parent
		parent          = parent.parent

		if not parent then  return nil  end

		if parent.type == "declaration" then
			local decl = parent

			if lastChild.container == decl.names then
				assert(lastChild == ident)
				return ident -- ident is the declaration node.
			end

		elseif parent.type == "function" then
			local func = parent

			if lastChild.container == func.parameters then
				assert(lastChild == ident)
				return ident -- ident is the declaration node.
			else
				local func      = parent
				local declIdent = lastItemWith1(func.parameters, "name", name) -- Note: This will ignore any vararg parameter.
				if declIdent then  return declIdent  end
			end

		elseif parent.type == "for" then
			local forLoop = parent

			if lastChild.container == forLoop.names then
				assert(lastChild == ident)
				return ident -- ident is the declaration node.
			elseif lastChild.container ~= forLoop.values then
				local declIdent = lastItemWith1(forLoop.names, "name", name)
				if declIdent then  return declIdent  end
			end

		elseif parent.type == "block" then
			local block = parent

			-- Look through statements above lastChild.
			for i = lastChild.key-1, 1, -1 do
				local statement = block.statements[i]

				if statement.type == "declaration" then
					local decl      = statement
					local declIdent = lastItemWith1(decl.names, "name", name)
					if declIdent then  return declIdent  end
				end
			end

		elseif parent.type == "repeat" then
			local repeatLoop = parent

			-- Repeat loop conditions can see into the loop block.
			if lastChild == repeatLoop.condition then
				local block = repeatLoop.body

				for i = #block.statements, 1, -1 do
					local statement = block.statements[i]

					if statement.type == "declaration" then
						local decl      = statement
						local declIdent = lastItemWith1(decl.names, "name", name)
						if declIdent then  return declIdent  end
					end
				end
			end
		end
	end
end

-- declVararg|nil    = findVarargDeclaration( vararg )
-- declVararg.parent = func
local function findVarargDeclaration(vararg)
	local parent = vararg

	while true do
		parent = parent.parent
		if not parent then  return nil  end

		if parent.type == "function" then
			local lastParam = parent.parameters[#parent.parameters]

			if lastParam and lastParam.type == "vararg" then
				return lastParam
			else
				return nil
			end
		end
	end
end

local function findLabel(gotoNode)
	local name   = gotoNode.name
	local parent = gotoNode

	while true do
		parent = parent.parent
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

local function updateReferences(node, updateTopNodePositionInfo)
	local topNodeParent    = nil
	local topNodeContainer = nil
	local topNodeKey       = nil

	if updateTopNodePositionInfo == false then
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
			ident.declaration = findIdentifierDeclaration(ident) -- We can call this because all parents and previous nodes already have their references updated at this point.

			--[[ DEBUG
			print(F(
				"%-10s  %-12s  %s",
				ident.name,
				(        ident.declaration and ident.declaration.type or ""),
				tostring(ident.declaration and ident.declaration.id   or "")
			))
			--]]

		elseif node.type == "vararg" then
			local vararg       = node
			vararg.declaration = findVarargDeclaration(vararg) -- We can call this because all relevant 'parent' references have been updated at this point.

			--[[ DEBUG
			print(F(
				"%-10s  %-12s  %s",
				"...",
				(        vararg.declaration and vararg.declaration.type or ""),
				tostring(vararg.declaration and vararg.declaration.id   or "")
			))
			--]]

		elseif node.type == "goto" then
			local gotoNode = node
			gotoNode.label = findLabel(gotoNode) -- We can call this because all relevant 'parent' references have been updated at this point.
		end
	end, topNodeParent, topNodeContainer, topNodeKey)
end



local function replace(node, replacement, parent, container, key, stats)
	tableInsert(stats.nodeReplacements, Location(container[key], "replacement", replacement))

	container[key] = replacement

	replacement.sourceString = node.sourceString
	replacement.sourcePath   = node.sourcePath

	replacement.token    = node.token
	replacement.line     = node.line
	replacement.position = node.position

	replacement.parent    = parent
	replacement.container = container
	replacement.key       = key
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
			return AstLiteral(unary.token, -expr.value)
		end
		return nil
	end,
	["not"] = function(unary, expr)
		-- @Incomplete: Fold 'not (expr1 ~= expr2)' into 'expr1 == expr2'.
		if expr.type == "literal" then
			return AstLiteral(unary.token, (not expr.value))
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
			return AstLiteral(unary.token, bitsToInt(bits1))
		end
		return nil
	end,
}

local binaryFolders = {
	["+"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueNumberLike(l.value) and isValueNumberLike(r.value) then
			return AstLiteral(binary.token, l.value+r.value)
		end
		return nil
	end,
	["-"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueNumberLike(l.value) and isValueNumberLike(r.value) then
			return AstLiteral(binary.token, l.value-r.value)
		end
		return nil
	end,
	["*"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueNumberLike(l.value) and isValueNumberLike(r.value) then
			return AstLiteral(binary.token, l.value*r.value)
		end
		return nil
	end,
	["/"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueNumberLike(l.value) and isValueNumberLike(r.value) then
			return AstLiteral(binary.token, l.value/r.value)
		end
		return nil
	end,
	["//"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueNumberLike(l.value) and isValueNumberLike(r.value) then
			return AstLiteral(binary.token, mathFloor(l.value/r.value))
		end
		return nil
	end,
	["^"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueNumberLike(l.value) and isValueNumberLike(r.value) then
			return AstLiteral(binary.token, l.value^r.value)
		end
		return nil
	end,
	["%"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueNumberLike(l.value) and isValueNumberLike(r.value) then
			return AstLiteral(binary.token, l.value%r.value)
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
			return AstLiteral(binary.token, bitsToInt(bits1))
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
			return AstLiteral(binary.token, bitsToInt(bits1))
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
			return AstLiteral(binary.token, bitsToInt(bits1))
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

			return AstLiteral(binary.token, bitsToInt(bits1))
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

			return AstLiteral(binary.token, bitsToInt(bits1))
		end

		return nil
	end,
	[".."] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and isValueNumberOrString(l.value) and isValueNumberOrString(r.value) then
			return AstLiteral(binary.token, l.value..r.value)
		end
		return nil
	end,
	["<"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and areValuesNumbersOrStringsAndOfSameType(l.value, r.value) then
			return AstLiteral(binary.token, (l.value < r.value))
		end
		return nil
	end,
	["<="] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and areValuesNumbersOrStringsAndOfSameType(l.value, r.value) then
			return AstLiteral(binary.token, (l.value <= r.value))
		end
		return nil
	end,
	[">"] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and areValuesNumbersOrStringsAndOfSameType(l.value, r.value) then
			return AstLiteral(binary.token, (l.value > r.value))
		end
		return nil
	end,
	[">="] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" and areValuesNumbersOrStringsAndOfSameType(l.value, r.value) then
			return AstLiteral(binary.token, (l.value >= r.value))
		end
		return nil
	end,
	["=="] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" then
			return AstLiteral(binary.token, (l.value == r.value))
		end
		return nil
	end,
	["~="] = function(binary, l, r)
		if l.type == "literal" and r.type == "literal" then
			return AstLiteral(binary.token, (l.value ~= r.value))
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

local statsForSimplify

local function simplifyNode(node, parent, container, key)
	if not parent then
		-- void

	elseif node.type == "unary" then
		-- Note: We don't fold e.g. '- - -expr' into '-expr' because metamethods may
		-- be called, and folding '- -expr' into 'expr' would remove what could be a
		-- runtime error if 'expr' didn't contain a number.
		local replacement = unaryFolders[node.operator](node, node.expression)
		if replacement then  replace(node, replacement, parent, container, key, statsForSimplify)  end

	elseif node.type == "binary" then
		-- @Incomplete: Fold 'expr - -n' into 'expr + n' etc.
		local replacement = binaryFolders[node.operator](node, node.left, node.right)
		if replacement then  replace(node, replacement, parent, container, key, statsForSimplify)  end

	elseif node.type == "if" then
		-- @Incomplete: Fold 'if not not expr' into 'if expr'. (Also for 'while' and 'repeat'.)
		local ifNode = node

		if ifNode.condition.type == "literal" then -- @Incomplete: There are more values that make simplification possible (e.g. functions, but who would put that here anyway). :SimplifyTruthfulValues
			local replacement = ifNode.condition.value and ifNode.bodyTrue or ifNode.bodyFalse

			if replacement and replacement.statements[1] then
				replace(ifNode, replacement, parent, container, key, statsForSimplify)
				return simplifyNode(replacement, parent, container, key)
			else
				tableRemove(container, key)
				tableInsert(statsForSimplify.nodeRemovals, Location(ifNode))
				statsForSimplify.nodeRemoveCount = statsForSimplify.nodeRemoveCount + 1
			end
		end

	elseif node.type == "while" then
		local whileLoop = node

		if whileLoop.condition.type == "literal" then -- :SimplifyTruthfulValues
			if whileLoop.condition.value then
				whileLoop.condition.value = true
			else
				tableRemove(container, key)
				tableInsert(statsForSimplify.nodeRemovals, Location(whileLoop))
				statsForSimplify.nodeRemoveCount = statsForSimplify.nodeRemoveCount + 1
			end
		end

	elseif node.type == "repeat" then
		local repeatLoop = node

		if repeatLoop.condition.type == "literal" then -- :SimplifyTruthfulValues
			if repeatLoop.condition.value then
				replace(repeatLoop, repeatLoop.body, parent, container, key, statsForSimplify)
				return simplifyNode(repeatLoop.body, parent, container, key)
			else
				repeatLoop.condition.value = false
			end
		end

	elseif node.type == "for" then
		local forLoop = node

		if
			forLoop.kind == "numeric"
			and forLoop.values[2]
			and not forLoop.values[4] -- If this value exists then there will be an error when Lua tries to run the file, but it's not our job to handle that here.
			and forLoop.values[1].type == "literal" and type(forLoop.values[1].value) == "number"
			and forLoop.values[2].type == "literal" and type(forLoop.values[2].value) == "number"
			and (not forLoop.values[3] or (forLoop.values[3].type == "literal" and type(forLoop.values[3].value) == "number"))
		then
			local from =                       forLoop.values[1].value
			local to   =                       forLoop.values[2].value
			local step = forLoop.values[3] and forLoop.values[3].value or 1

			if (step > 0 and from > to) or (step < 0 and from < to) then
				tableRemove(container, key)
				tableInsert(statsForSimplify.nodeRemovals, Location(forLoop))
				statsForSimplify.nodeRemoveCount = statsForSimplify.nodeRemoveCount + 1
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
				tableInsert(statsForSimplify.nodeReplacements, Location(block, "replacement", nil)) -- We replace the block with its contents.

				for i, statement in ipairs(block.statements) do
					tableInsert(parent.statements, key+i-1, statement)
				end
			end
		end

	elseif node.type == "literal" then
		local literal = node
		if literal.value == 0 then  literal.value = 0  end -- Normalize '-0'.
	end
end

local function _simplify(node, stats)
	statsForSimplify = stats
	traverseTreeReverse(node, true, simplifyNode)
end

local function simplify(node)
	local stats = Stats()
	_simplify(node, stats)
	return stats
end



local function isNodeDeclLike(node)
	return node.type == "declaration" or node.type == "function" or node.type == "for"
end

local function getNameArrayOfDeclLike(declLike)
	return declLike.names or declLike.parameters
end



-- (statement, block) | declIdent | repeatLoop | declLike = findParentStatementAndBlockOrNodeOfInterest( node, declIdent )
local function findParentStatementAndBlockOrNodeOfInterest(node, declIdent)
	while true do
		if node == declIdent then  return declIdent, nil  end

		local lastChild = node
		node            = node.parent

		if not node then  return nil  end

		if node.type == "block"                                                 then  return lastChild, node  end
		if node.type == "declaration" and lastChild.container ~= node.values    then  return node,      nil   end
		if node.type == "function"                                              then  return node,      nil   end
		if node.type == "for"         and lastChild.container ~= node.values    then  return node,      nil   end
		if node.type == "repeat"      and lastChild           == node.condition then  return node,      nil   end
	end
end

-- foundCurrentDeclIdent = lookForCurrentDeclIdentAndRegisterCurrentIdentAsWatcherInBlock( declIdentWatchers, currentIdentInfo, block, statementStartIndex )
local function lookForCurrentDeclIdentAndRegisterCurrentIdentAsWatcherInBlock(declIdentWatchers, identInfo, block, iStart)
	local statements           = block.statements
	local currentIdentOrVararg = identInfo.ident
	local currentDeclIdent     = currentIdentOrVararg.declaration

	for i = iStart, 1, -1 do
		local statement = statements[i]

		if statement.type == "declaration" then
			local decl = statement

			for _, declIdent in ipairsr(decl.names) do
				-- Note: Declaration identifiers also watch themselves. (Is this good?) :DeclarationIdentifiersWatchThemselves
				declIdentWatchers[declIdent] = declIdentWatchers[declIdent] or {}
				tableInsert(declIdentWatchers[declIdent], currentIdentOrVararg)

				if declIdent == currentDeclIdent then  return true  end
			end
		end
	end

	return false
end

local function getInformationAboutIdentifiersAndUpdateReferences(node)
	local identInfos        = {--[[ [ident1]=identInfo1, identInfo1, ... ]]} -- identInfo = {ident=identOrVararg, type=lrvalueType}
	local declIdentWatchers = {--[[ [declIdent1]={identOrVararg1,...}, ... ]]}

	traverseTree(node, function(node, parent, container, key)
		node.parent    = parent
		node.container = container
		node.key       = key

		if node.type == "identifier" or node.type == "vararg" then
			local currentIdentOrVararg       = node
			local findDecl                   = (currentIdentOrVararg.type == "identifier") and findIdentifierDeclaration or findVarargDeclaration
			local currentDeclIdent           = findDecl(currentIdentOrVararg) -- We can call this because all parents and previous nodes already have their references updated at this point.
			currentIdentOrVararg.declaration = currentDeclIdent

			local identType = (
				(parent and (
					(parent.type == "declaration" and (container == parent.names     )) or
					(parent.type == "assignment"  and (container == parent.targets   )) or
					(parent.type == "function"    and (container == parent.parameters)) or
					(parent.type == "for"         and (container == parent.names     ))
				))
				and "lvalue"
				or  "rvalue"
			)

			-- if currentDeclIdent then  print(F("%s:%d: %s '%s'", currentIdentOrVararg.sourcePath, currentIdentOrVararg.line, identType, (currentIdentOrVararg.name or "...")))  end -- DEBUG

			local identInfo = {ident=currentIdentOrVararg, type=identType}
			tableInsert(identInfos, identInfo)
			identInfos[currentIdentOrVararg] = identInfo

			-- Determine visible declarations for the identifier.
			local block = currentIdentOrVararg -- Start node for while loop.

			while true do
				local statementOrInterest
				statementOrInterest, block = findParentStatementAndBlockOrNodeOfInterest(block, currentDeclIdent)

				if not statementOrInterest then
					-- We should only get here for globals (i.e. there should be no declaration).
					assert(not currentDeclIdent)
					return
				end

				if block then
					local statement = statementOrInterest

					assert(type(statement.key) == "number")
					assert(statement.container == block.statements)

					if lookForCurrentDeclIdentAndRegisterCurrentIdentAsWatcherInBlock(declIdentWatchers, identInfo, block, statement.key-1) then
						return
					end

				-- We found the current declIdent - don't look for more other declIdents to watch!
				elseif statementOrInterest == currentDeclIdent then
					local declIdent = statementOrInterest

					-- :DeclarationIdentifiersWatchThemselves
					declIdentWatchers[declIdent] = declIdentWatchers[declIdent] or {}
					tableInsert(declIdentWatchers[declIdent], currentIdentOrVararg)

					return

				-- We can jump from repeat loop conditions into the loop's body.
				elseif statementOrInterest.type == "repeat" then
					local repeatLoop = statementOrInterest
					block            = repeatLoop.body -- Start node for while loop.

					if lookForCurrentDeclIdentAndRegisterCurrentIdentAsWatcherInBlock(declIdentWatchers, identInfo, block, #block.statements) then
						return
					end

				-- Declaration-likes have names we want to watch.
				-- Note that findParentStatementAndBlockOrNodeOfInterest() is smart and skipped declaration-likes that we should completely ignore.
				else
					local declLike = statementOrInterest
					block          = declLike -- Start node for while loop.

					for _, declIdent in ipairsr(getNameArrayOfDeclLike(declLike)) do
						-- :DeclarationIdentifiersWatchThemselves
						declIdentWatchers[declIdent] = declIdentWatchers[declIdent] or {}
						tableInsert(declIdentWatchers[declIdent], currentIdentOrVararg)

						if declIdent == currentDeclIdent then  return  end
					end
				end
			end

		elseif node.type == "goto" then
			local gotoNode = node
			gotoNode.label = findLabel(gotoNode) -- We can call this because all relevant 'parent' references have been updated at this point.
		end
	end)

	return identInfos, declIdentWatchers
end



local function unregisterWatchersBeforeNodeRemoval(identInfos, declIdentWatchers, theNode, declIdentReadCount, declIdentAssignmentCount, funcInfos, currentFuncInfo, stats, registerRemoval)
	-- ioWrite("unregister ") ; printNode(theNode) -- DEBUG

	if registerRemoval then
		tableInsert(stats.nodeRemovals, Location(theNode)) -- There's no need to register the location every child node.
		stats.nodeRemoveCount = stats.nodeRemoveCount - 1 -- To counteract the +1 below.
	end

	traverseTree(theNode, true, function(node) -- @Speed
		--[[ DEBUG
		unregistered = unregistered or {}
		if unregistered[node] then
			printNode(node)
			print(debug.traceback("NEW", 2))
			print(unregistered[node])
		end
		unregistered[node] = debug.traceback("OLD", 2)
		--]]

		stats.nodeRemoveCount = stats.nodeRemoveCount + 1

		if node.type == "identifier" or node.type == "vararg" then
			local currentIdentOrVararg = node
			local currentIdentInfo     = identInfos[currentIdentOrVararg]
			assert(currentIdentInfo)

			-- Remove identifier info.
			for i, identInfo in ipairs(identInfos) do
				if identInfo == currentIdentInfo then
					removeUnordered(identInfos, i)
					break
				end
			end
			identInfos[currentIdentOrVararg] = nil

			-- Remove as watcher.
			for _, watcherIdents in pairs(declIdentWatchers) do -- @Speed
				for i, watcherIdent in ipairs(watcherIdents) do
					if watcherIdent == currentIdentOrVararg then
						removeUnordered(watcherIdents, i)
						break
					end
				end
			end

			-- Remove watcher list.
			if declIdentWatchers[currentIdentOrVararg] then
				declIdentWatchers[currentIdentOrVararg] = nil
				removeItemUnordered(currentFuncInfo.declIdents, currentIdentOrVararg)
			end

			-- Update access count.
			if not currentIdentOrVararg.declaration then
				-- void
			elseif currentIdentInfo.type == "rvalue" then
				local declIdent                     = currentIdentOrVararg.declaration
				declIdentReadCount[declIdent]       = declIdentReadCount[declIdent] - 1 -- :AccessCount
				assert(declIdentReadCount[declIdent] >= 0)
			elseif --[[currentIdentInfo.type == "lvalue" and]] currentIdentInfo.ident.parent.type == "assignment" then
				local declIdent                     = currentIdentOrVararg.declaration
				declIdentAssignmentCount[declIdent] = declIdentAssignmentCount[declIdent] - 1 -- :AccessCount
				assert(declIdentAssignmentCount[declIdent] >= 0)
			end

			-- removeItemUnordered(currentFuncInfo.locals,   currentIdentOrVararg)
			-- removeItemUnordered(currentFuncInfo.upvalues, currentIdentOrVararg)
			-- removeItemUnordered(currentFuncInfo.globals,  currentIdentOrVararg)

		elseif node.type == "assignment" then
			removeItemUnordered(currentFuncInfo.assignments, node)

		elseif isNodeDeclLike(node) then
			removeItemUnordered(currentFuncInfo.declLikes, node)
		end

		if funcInfos[node] then
			for i, funcInfo in ipairs(funcInfos) do
				if funcInfo.node == node then
					removeUnordered(funcInfos, i)
					break
				end
			end
			funcInfos[node] = nil
		end
	end)
end

local function isNodeValueList(node)
	return (node.type == "call" or node.type == "vararg") and not node.adjustToOne
end

local function areAllLvaluesUnwantedAndAllValuesCalls(lvalues, values, wantToRemoveLvalue)
	if not values[1] then  return false  end -- Need at least one call.

	for slot = 1, #lvalues do
		if not wantToRemoveLvalue[slot] then  return false  end
	end

	for _, valueExpr in ipairs(values) do
		if valueExpr.type ~= "call" then  return false  end
	end

	return true
end

local function getInformationAboutFunctions(theNode)
	-- Gather functions.
	local funcInfos = {}

	do
		-- We assume theNode is a block, but it's fine if it isn't.
		local funcInfo = {node=theNode, declLikes={}, declIdents={}, assignments={}--[[, locals={}, upvalues={}, globals={}]]}
		tableInsert(funcInfos, funcInfo)
		funcInfos[theNode] = funcInfo
	end

	traverseTree(theNode, function(node)
		if node == theNode then  return  end

		if node.type == "function" then
			local funcInfo = {node=node, declLikes={node}, declIdents={}, assignments={}--[[, locals={}, upvalues={}, globals={}]]}
			tableInsert(funcInfos, funcInfo)
			funcInfos[node] = funcInfo
		end
	end)

	-- Gather relevant nodes.
	for _, funcInfo in ipairs(funcInfos) do
		traverseTree(funcInfo.node, function(node)
			if node      == funcInfo.node then  return                   end
			if node.type == "function"    then  return "ignorechildren"  end

			if node.type == "identifier" or node.type == "vararg" then
				local identOrVararg = node
				local declIdent     = identOrVararg.declaration

				if identOrVararg == declIdent then
					tableInsert(funcInfo.declIdents, identOrVararg)
				end

				--[[
				if declIdent then
					local declLike = declIdent.parent
					local isInFunc = true
					local parent   = identOrVararg.parent

					while parent do
						if parent == declLike then -- declLike may be a function itself.
							break
						elseif parent.type == "function" then
							isInFunc = false
							break
						end
						parent = parent.parent
					end

					tableInsert((isInFunc and funcInfo.locals or funcInfo.upvalues), identOrVararg)

				else
					tableInsert(funcInfo.globals, identOrVararg)
				end
				]]

			elseif node.type == "declaration" or node.type == "for" then
				tableInsert(funcInfo.declLikes, node) -- Note: Identifiers will be added to funcInfo.declIdents when we get to them.

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

	return funcInfos
end

local function getAccessesOfDeclaredNames(funcInfos, identInfos, declIdentWatchers) -- @Cleanup: Don't require the funcInfos argument (unless it maybe speeds up something somwhere).
	local declIdentReadCount       = {--[[ [declIdent1]=count, ... ]]}
	local declIdentAssignmentCount = {--[[ [declIdent1]=count, ... ]]}

	for _, funcInfo in ipairs(funcInfos) do
		for _, declIdent in ipairs(funcInfo.declIdents) do
			local readCount       = 0
			local assignmentCount = 0

			for _, watcherIdent in ipairs(declIdentWatchers[declIdent]) do
				if watcherIdent.declaration == declIdent then
					local identInfo = identInfos[watcherIdent]

					if identInfo.type == "rvalue" then
						readCount = readCount + 1 -- :AccessCount
					elseif --[[identInfo.type == "lvalue" and]] identInfo.ident.parent.type == "assignment" then
						assignmentCount = assignmentCount + 1 -- :AccessCount
					end
				end
			end

			declIdentReadCount      [declIdent] = readCount
			declIdentAssignmentCount[declIdent] = assignmentCount
		end
	end

	return declIdentReadCount, declIdentAssignmentCount
end

-- Note: References need to be updated after calling this!
local function _optimize(theNode, stats)
	_simplify(theNode, stats)

	local identInfos, declIdentWatchers                = getInformationAboutIdentifiersAndUpdateReferences(theNode)
	local funcInfos                                    = getInformationAboutFunctions(theNode)
	local declIdentReadCount, declIdentAssignmentCount = getAccessesOfDeclaredNames(funcInfos, identInfos, declIdentWatchers)

	--
	-- Replace variables that are effectively constants with literals.
	--
	local replacedConstants = false

	for _, funcInfo in ipairs(funcInfos) do
		for _, declLike in ipairs(funcInfo.declLikes) do
			if declLike.type == "declaration" then
				local decl = declLike

				for slot = 1, #decl.names do
					local declIdent = decl.names[slot]
					local valueExpr = decl.values[slot]

					if
						declIdentAssignmentCount[declIdent] == 0
						and declIdentReadCount[declIdent] > 0
						and (
							(not valueExpr and not (decl.values[1] and isNodeValueList(decl.values[#decl.values])))
							or (
								valueExpr
								and valueExpr.type == "literal"
								and not (
									(type(valueExpr.value) == "string" and #valueExpr.value > parser.constantNameReplacementStringMaxLength)
									-- or (valueExpr.value == 0 and not NORMALIZE_MINUS_ZERO and tostring(valueExpr.value) == "-0") -- No, bad rule!
								)
							)
						)
					then
						-- where(declIdent, "Constant declaration.") -- DEBUG

						local valueLiteral = valueExpr
						local valueIsZero  = (valueLiteral ~= nil and valueLiteral.value == 0)

						for _, watcherIdent in ipairsr(declIdentWatchers[declIdent]) do
							if watcherIdent.declaration == declIdent then -- Note: declIdent is never a vararg here (because we only process declarations).
								local identInfo = identInfos[watcherIdent]

								if
									identInfo.type == "rvalue"
									-- Avoid creating '-0' (or '- -0') here as that may mess up Lua in weird/surprising ways.
									and not (valueIsZero and not NORMALIZE_MINUS_ZERO and watcherIdent.parent.type == "unary" and watcherIdent.parent.operator == "-")
								then
									-- where(watcherIdent, "Constant value replacement.") -- DEBUG

									local v                  = valueLiteral and valueLiteral.value
									local replacementLiteral = AstLiteral(watcherIdent.token, v)

									unregisterWatchersBeforeNodeRemoval(identInfos, declIdentWatchers, watcherIdent, declIdentReadCount, declIdentAssignmentCount, funcInfos, funcInfo, stats, false)
									replace(watcherIdent, replacementLiteral, watcherIdent.parent, watcherIdent.container, watcherIdent.key, stats)

									replacedConstants = true
								end
							end
						end--for declIdentWatchers
					end
				end--for declIdents
			end
		end--for declLikes
	end--for funcInfos

	if replacedConstants then
		return _optimize(theNode, stats) -- @Speed
	end

	--
	-- Remove useless assignments and declaration-likes (in that order).
	--
	-- Note that we go in reverse order almost everywhere! We may remove later stuff when we reach earlier stuff.
	--
	local function optimizeAssignmentOrDeclLike(statement, funcInfo)
		local isDecl       = (statement.type == "declaration")
		local isFunc       = (statement.type == "function")
		local isForLoop    = (statement.type == "for")
		local isAssignment = (statement.type == "assignment")

		if isDecl or isAssignment then
			local lvalues = statement.targets or statement.names
			local values  = statement.values

			-- Save some adjustment information.
			local madeToAdjusted = {}

			for slot = 1, #values-1 do -- Skip the last value.
				local valueExpr = values[slot]

				if isNodeValueList(valueExpr) then
					valueExpr.adjustToOne     = true
					madeToAdjusted[valueExpr] = true
				end
			end

			-- Remove useless extra values.
			for slot = #values, #lvalues+1, -1 do
				local valueExpr = values[slot]

				if not mayNodeBeInvolvedInJump(valueExpr) then
					unregisterWatchersBeforeNodeRemoval(identInfos, declIdentWatchers, valueExpr, declIdentReadCount, declIdentAssignmentCount, funcInfos, funcInfo, stats, true)
					tableRemove(values, slot)
				end
			end

			for slot = #lvalues+1, #values do
				values[slot].key = slot
			end

			-- Remove useless lvalues.
			local wantToRemoveLvalue        = {}
			local wantToRemoveValueIfExists = {}
			local mayRemoveValueIfExists    = {}

			for slot, lvalue in ipairsr(lvalues) do
				local declIdent = (lvalue.type == "identifier") and lvalue.declaration or nil

				if declIdent and declIdentReadCount[declIdent] == 0 then
					-- ioWrite("useless ") ; printNode(lvalue) -- DEBUG

					local valueExpr          = values[slot]
					local valueExprEffective = valueExpr

					if not valueExprEffective then
						valueExprEffective = values[#values]
						if valueExprEffective and not isNodeValueList(valueExprEffective) then  valueExprEffective = nil  end
					end

					wantToRemoveLvalue       [slot] = isAssignment or declIdentAssignmentCount[declIdent] == 0
					wantToRemoveValueIfExists[slot] = not (valueExpr and mayNodeBeInvolvedInJump(valueExpr))
					mayRemoveValueIfExists   [slot] = wantToRemoveValueIfExists[slot] and not (not valueExpr and lvalues[slot+1] and valueExprEffective)

					-- @Incomplete: Update funcInfo.locals and whatever else (if we end up using them at some point).
					-- @Incomplete: Replace 'unused,useless=func()' with 'useless=func()'.
					local canRemoveSlot = (wantToRemoveLvalue[slot] and mayRemoveValueIfExists[slot])

					-- Maybe remove lvalue.
					if canRemoveSlot and #lvalues > 1 then
						unregisterWatchersBeforeNodeRemoval(identInfos, declIdentWatchers, lvalue, declIdentReadCount, declIdentAssignmentCount, funcInfos, funcInfo, stats, true)
						tableRemove(lvalues, slot)
						for slot = slot, #lvalues do
							lvalues[slot].key = slot
						end
						wantToRemoveLvalue[slot] = wantToRemoveLvalue[slot+1] -- May become nil. We no longer care about the value of slot+1 and beyond after this point.
					end

					-- Maybe remove value.
					if wantToRemoveValueIfExists[slot] and valueExpr then
						if (canRemoveSlot or not values[slot+1]) and not (isAssignment and not values[2]) then
							unregisterWatchersBeforeNodeRemoval(identInfos, declIdentWatchers, valueExpr, declIdentReadCount, declIdentAssignmentCount, funcInfos, funcInfo, stats, true)
							tableRemove(values, slot)
							for slot = slot, #values do
								values[slot].key = slot
							end
							wantToRemoveValueIfExists[slot] = wantToRemoveValueIfExists[slot+1] -- May become nil. We no longer care about the value of slot+1 and beyond after this point.
							mayRemoveValueIfExists   [slot] = mayRemoveValueIfExists   [slot+1] -- May become nil. We no longer care about the value of slot+1 and beyond after this point.

						elseif not (valueExpr.type == "literal" and valueExpr.value == nil) then
							unregisterWatchersBeforeNodeRemoval(identInfos, declIdentWatchers, valueExpr, declIdentReadCount, declIdentAssignmentCount, funcInfos, funcInfo, stats, false)
							replace(valueExpr, AstLiteral(valueExpr.token, nil), valueExpr.parent, valueExpr.container, valueExpr.key, stats)
						end
					end
				end--if lvalue is relevant
			end--for lvalues

			-- Maybe remove or replace the whole statement.
			local statementIsRemoved = false

			do
				-- Remove the whole statement.
				if
					wantToRemoveLvalue[1]
					and (
						mayRemoveValueIfExists[1]
						or not (isAssignment or values[1]) -- Declaration-likes may have no value - assignments must have at least one.
					)
					and not (lvalues[2] or values[2])
				then
					unregisterWatchersBeforeNodeRemoval(identInfos, declIdentWatchers, statement, declIdentReadCount, declIdentAssignmentCount, funcInfos, funcInfo, stats, true)

					local block = statement.parent

					for i = statement.key, #block.statements do
						local statement     = block.statements[i+1] -- This be nil for the last 'i'.
						block.statements[i] = statement

						if statement then  statement.key = i  end
					end

					statementIsRemoved = true

				-- Replace 'unused=func()' with just 'func()'. This is a unique case as call expressions can also be statements.
				elseif areAllLvaluesUnwantedAndAllValuesCalls(lvalues, values, wantToRemoveLvalue) then
					for _, lvalue in ipairs(lvalues) do
						unregisterWatchersBeforeNodeRemoval(identInfos, declIdentWatchers, lvalue, declIdentReadCount, declIdentAssignmentCount, funcInfos, funcInfo, stats, true)
					end

					tableRemove(statement.container, statement.key) -- The parent ought to be a block!

					for slot, call in ipairs(values) do
						local i = statement.key + slot - 1
						tableInsert(statement.container, i, call)

						call.parent    = statement.parent
						call.container = statement.container
						call.key       = i
					end

					statementIsRemoved = true
				end
			end

			-- Restore or remove adjusted flags.
			-- @Speed: Don't do anything if statementIsRemoved is set (and we don't need to for some other reason).
			for i = 1, #values do
				local valueExpr = values[i]
				if (valueExpr.type == "call" or valueExpr.type == "vararg") then
					if statementIsRemoved or values[i+1] or not lvalues[i+1] or not madeToAdjusted[valueExpr] then
						valueExpr.adjustToOne = false
					end
				end
			end

		elseif isFunc or isForLoop then
			local declIdents = getNameArrayOfDeclLike(statement)

			for slot = #declIdents, (isForLoop and 2 or 1), -1 do
				local declIdent = declIdents[slot]
				if declIdentReadCount[declIdent] == 0 and declIdentAssignmentCount[declIdent] == 0 then
					unregisterWatchersBeforeNodeRemoval(identInfos, declIdentWatchers, declIdent, declIdentReadCount, declIdentAssignmentCount, funcInfos, funcInfo, stats, true)
					tableRemove(declIdents)
				else
					break
				end
			end

			for slot, declIdent in ipairs(declIdents) do
				declIdent.key = slot
			end

		else
			error(statement.type)
		end
	end

	for _, funcInfo in ipairsr(funcInfos) do
		for _, assignment in ipairsr(funcInfo.assignments) do
			optimizeAssignmentOrDeclLike(assignment, funcInfo)
		end
	end
	for _, funcInfo in ipairsr(funcInfos) do
		for _, declLike in ipairsr(funcInfo.declLikes) do
			optimizeAssignmentOrDeclLike(declLike, funcInfo)
		end
	end

	-- @Incomplete: Remove useless return statements etc.

	_simplify(theNode, stats) -- Not sure if needed. Or maybe we need to iterate?
end

-- Note: References need to be updated after calling this!
local function optimize(theNode)
	local stats = Stats()
	_optimize(theNode, stats)
	return stats
end



local generateName
do
	local BANK_LETTERS  = "etaoinshrdlcumwfgypbvkxjqzETAOINSHRDLCUMWFGYPBVKXJQZ" -- http://en.wikipedia.org/wiki/Letter_frequencies
	local BANK_ALPHANUM = "etaoinshrdlcumwfgypbvkxjqzETAOINSHRDLCUMWFGYPBVKXJQZ0123456789"

	local ILLEGAL_NAMES = {}
	for name in pairs(KEYWORDS) do  ILLEGAL_NAMES[name] = true  end
	if HANDLE_ENV             then  ILLEGAL_NAMES._ENV  = true  end

	local cache = {}

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

			local name = stringChar(tableUnpack(charBytes))
			if ILLEGAL_NAMES[name] then
				-- We will probably realistically never get here, partially because of the limited amount of locals and upvalues Lua allows.
				name = name.."_"
			end
			cache[nameGeneration] = name
		end

		return cache[nameGeneration]
	end

	-- for nameGeneration = 1, 3500 do  print(generateName(nameGeneration))  end ; error("TEST")
	-- for pow = 0, 32 do  print(generateName(2^pow))  end ; error("TEST")
end

-- stats = minify( node [, optimize=false ] )
local function minify(node, doOptimize)
	local stats = Stats()

	if doOptimize then
		_optimize(node, stats)
	end

	-- @Cleanup: Use findShadows()?
	local identInfos, declIdentWatchers                   = getInformationAboutIdentifiersAndUpdateReferences(node)
	-- local funcInfos                                    = getInformationAboutFunctions(node)
	-- local declIdentReadCount, declIdentAssignmentCount = getAccessesOfDeclaredNames(funcInfos, identInfos, declIdentWatchers)

	local allDeclIdents = {}

	for _, identInfo in ipairs(identInfos) do
		local identOrVararg = identInfo.ident
		local declIdent     = (identOrVararg.type == "identifier") and identOrVararg.declaration or nil

		if declIdent and not allDeclIdents[declIdent] then
			tableInsert(allDeclIdents, declIdent)
			allDeclIdents[declIdent] = true
		end
	end

	--
	-- Make sure frequencies affect who gets shorter names first.
	-- (This doesn't seem that useful at the moment. 2021-06-16)
	--
	--[[ :SortBeforeRename
	tableSort(allDeclIdents, function(a, b)
		if a.type == "vararg" or b.type == "vararg" then
			return a.id < b.id -- We don't care about varargs.
		end

		local aWatchers = declIdentWatchers[a.declaration]
		local bWatchers = declIdentWatchers[b.declaration]

		if not (aWatchers and bWatchers) then
			return a.id < a.id -- We don't care about globals.
		end

		if #aWatchers ~= #bWatchers then
			return #aWatchers < #bWatchers
		end

		return a.id < b.id
	end)
	--]]

	--
	-- Rename locals!
	--
	local renamed           = {--[[ [declIdent1]=true, ... ]]}
	local maxNameGeneration = 0

	--[[ :SortBeforeRename
	local remoteWatchers = {}
	--]]

	-- Assign generated names to declarations.
	for _, declIdent in ipairs(allDeclIdents) do
		local newName

		if declIdent.name == "_ENV" and HANDLE_ENV then
			-- There are probably some cases where we can safely rename _ENV,
			-- but it's likely not worth the effort to detect that.
			newName = "_ENV"

		else
			for nameGeneration = 1, 1/0 do
				newName         = generateName(nameGeneration)
				local collision = false

				for _, watcherIdent in ipairs(declIdentWatchers[declIdent]) do
					local watcherDeclIdent = watcherIdent.declaration

					-- Local watcher.
					if watcherDeclIdent then
						if renamed[watcherDeclIdent] and watcherDeclIdent.name == newName then
							collision = true
							break
						end

					-- Global watcher.
					elseif watcherIdent.name == newName then
						collision = true
						break
					end
				end--for declIdentWatchers

				--[[ :SortBeforeRename
				if not collision and remoteWatchers[declIdent] then
					for _, watcherDeclIdent in ipairs(remoteWatchers[declIdent]) do
						if watcherDeclIdent.name == newName then
							collision = true
							break
						end
					end
				end
				--]]

				if not collision then
					maxNameGeneration = mathMax(maxNameGeneration, nameGeneration)
					break
				end
			end--for nameGeneration
		end

		--[[ :SortBeforeRename
		for _, watcherIdent in ipairs(declIdentWatchers[declIdent]) do
			local watcherDeclIdent = watcherIdent.declaration

			if watcherDeclIdent and watcherDeclIdent ~= declIdent then
				remoteWatchers[watcherDeclIdent] = remoteWatchers[watcherDeclIdent] or {}

				if not remoteWatchers[watcherDeclIdent][declIdent] then
					tableInsert(remoteWatchers[watcherDeclIdent], declIdent)
					remoteWatchers[watcherDeclIdent][declIdent] = true
				end
			end
		end
		--]]

		if declIdent.name ~= newName then
			declIdent.name    = newName
			stats.renameCount = stats.renameCount + 1
		end
		renamed[declIdent] = true
	end--for allDeclIdents

	stats.generatedNameCount = maxNameGeneration
	-- print("maxNameGeneration", maxNameGeneration) -- DEBUG

	-- Rename all remaining identifiers.
	for _, identInfo in ipairs(identInfos) do
		local ident     = identInfo.ident -- Could be a vararg.
		local declIdent = (ident.type == "identifier") and ident.declaration or nil

		if declIdent and ident.name ~= declIdent.name then
			ident.name        = declIdent.name
			stats.renameCount = stats.renameCount + 1
		end
	end

	return stats
end



local function printTokens(tokens)
	local printLocs = parser.printLocations

	for i, token in ipairs(tokens) do
		local v = ensurePrintable(tostring(token.value))
		if #v > 200 then  v = stringSub(v, 1, 200-3).."..."  end

		if printLocs then  ioWrite(token.sourcePath, ":", token.lineStart, ": ")  end
		ioWrite(i, ". ", F("%-11s", token.type), " '", v, "'\n")
	end
end



local toLua
do
	local writeNode
	local writeStatements

	-- lastOutput = "" | "alphanum" | "number" | "-" | "."

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

	local function choosePretty(node, prettyFallback)
		if node.pretty ~= nil then  return node.pretty  end
		return prettyFallback
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
		local nStr = formatNumber(n)
		if (lastOutput == "-" and stringByte(nStr, 1) == 45--[[ "-" ]]) or lastOutput == "." then
			lastOutput = writeLua(buffer, " ", "")
		else
			ensureSpaceIfNotPretty(buffer, pretty, lastOutput, "alphanum","number")
		end
		lastOutput = writeLua(buffer, nStr, "number")
		return "number"
	end

	-- Returns nil and a message or error.
	local function writeCommaSeparatedList(buffer, pretty, indent, lastOutput, expressions, writeAttributes, nodeCb)
		for i, expr in ipairs(expressions) do
			if i > 1 then
				lastOutput = writeLua(buffer, ",", "")
				if pretty then  lastOutput = writeLua(buffer, " ", "")  end
			end

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, expr, true, nodeCb)
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
	local function writeFunctionParametersAndBody(buffer, pretty, indent, lastOutput, func, explicitParams, selfParam, nodeCb)
		lastOutput = writeLua(buffer, "(", "")

		if selfParam then
			if nodeCb then  nodeCb(selfParam, buffer)  end
			tableInsert(buffer, selfParam.prefix)
			tableInsert(buffer, selfParam.suffix)
		end

		local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, explicitParams, false, nodeCb)
		if not ok then  return nil, lastOutput  end

		lastOutput = writeLua(buffer, ")", "")
		if nodeCb then  nodeCb(func.body, buffer)  end
		pretty = choosePretty(func.body, pretty)
		tableInsert(buffer, func.body.prefix)
		if pretty then  lastOutput = writeLua(buffer, "\n", "")  end

		local ok;ok, lastOutput = writeStatements(buffer, pretty, indent+1, lastOutput, func.body.statements, nodeCb)
		if not ok then  return nil, lastOutput  end

		lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
		tableInsert(buffer, func.body.suffix)
		lastOutput = writeAlphanum(buffer, pretty, "end", lastOutput)

		return true, lastOutput
	end

	-- Returns nil and a message or error.
	function writeStatements(buffer, pretty, indent, lastOutput, statements, nodeCb)
		local skipNext = false

		for i, statement in ipairs(statements) do
			if skipNext then
				skipNext = false

			else
				local statementNext = statements[i+1]
				lastOutput          = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)

				if statementNext and isStatementFunctionDeclaration(statement, statementNext) then
					local decl       = statement
					local assignment = statementNext
					local func       = assignment.values[1]
					local pretty     = choosePretty(assignment, pretty)

					if nodeCb then
						nodeCb(decl,       buffer)
						nodeCb(assignment, buffer)
						nodeCb(func,       buffer)
					end

					tableInsert(buffer, decl.prefix)
					tableInsert(buffer, assignment.prefix)
					tableInsert(buffer, func.prefix)

					lastOutput = writeAlphanum(buffer, pretty, "local function", lastOutput)
					lastOutput = writeLua(buffer, " ", "")

					if nodeCb then  nodeCb(decl.names[1], buffer)  end

					tableInsert(buffer, decl.names[1].prefix)
					local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, assignment.targets[1], true, nodeCb)
					if not ok then  return nil, lastOutput  end
					tableInsert(buffer, decl.names[1].suffix)

					local ok;ok, lastOutput = writeFunctionParametersAndBody(buffer, choosePretty(func, pretty), indent, lastOutput, func, func.parameters, nil, nodeCb)
					if not ok then  return nil, lastOutput  end

					tableInsert(buffer, func.suffix)
					tableInsert(buffer, assignment.suffix)
					tableInsert(buffer, decl.suffix)

					skipNext = true

				else
					local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, statement, true, nodeCb)
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

	local function doesExpressionNeedParenthesisIfOnTheLeftSide(expr)
		local nodeType = expr.type
		-- Some things, like "binary" or "vararg", are not here because those expressions add their own parentheses.
		return nodeType == "literal" or nodeType == "table" or nodeType == "function"
	end

	-- Returns nil and a message or error.
	local function writeLookup(buffer, pretty, indent, lastOutput, lookup, forMethodCall, nodeCb)
		local objNeedParens = doesExpressionNeedParenthesisIfOnTheLeftSide(lookup.object)
		if objNeedParens then  lastOutput = writeLua(buffer, "(", "")  end

		local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, lookup.object, false, nodeCb)
		if not ok then  return nil, lastOutput  end

		if objNeedParens then  lastOutput = writeLua(buffer, ")", "")  end

		if canNodeBeName(lookup.member) then
			lastOutput = writeLua(buffer, (forMethodCall and ":" or "."), "")
			if nodeCb then  nodeCb(lookup.member, buffer)  end
			tableInsert(buffer, lookup.member.prefix)
			lastOutput = writeAlphanum(buffer, pretty, lookup.member.value, lastOutput)
			tableInsert(buffer, lookup.member.suffix)

		elseif forMethodCall then
			return nil, "Error: AST: Callee for method call is not a lookup."

		else
			lastOutput = writeLua(buffer, "[", "")

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, lookup.member, true, nodeCb)
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
	local function writeBinaryOperatorChain(buffer, pretty, indent, lastOutput, binary, nodeCb)
		local l = binary.left
		local r = binary.right

		if l.type == "binary" and l.operator == binary.operator then
			if nodeCb then  nodeCb(l, buffer)  end
			tableInsert(buffer, l.prefix)
			local ok;ok, lastOutput = writeBinaryOperatorChain(buffer, choosePretty(l, pretty), indent, lastOutput, l, nodeCb)
			if not ok then  return nil, lastOutput  end
			tableInsert(buffer, l.suffix)
		else
			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, l, false, nodeCb)
			if not ok then  return nil, lastOutput  end
		end

		if pretty then  lastOutput = writeLua(buffer, " ", "")  end

		if binary.operator == ".." then  ensureSpaceIfNotPretty(buffer, pretty, lastOutput, "number")  end

		local nextOutput = (
			(binary.operator == "-"            and "-"       ) or
			(binary.operator == ".."           and "."       ) or
			(stringFind(binary.operator, "%w") and "alphanum") or
			""
		)
		if nextOutput ~= "" then  ensureSpaceIfNotPretty(buffer, pretty, lastOutput, nextOutput)  end
		lastOutput = writeLua(buffer, binary.operator, nextOutput)

		if pretty then  lastOutput = writeLua(buffer, " ", "")  end

		if r.type == "binary" and r.operator == binary.operator then
			if nodeCb then  nodeCb(r, buffer)  end
			tableInsert(buffer, r.prefix)
			local ok;ok, lastOutput = writeBinaryOperatorChain(buffer, choosePretty(r, pretty), indent, lastOutput, r, nodeCb)
			if not ok then  return nil, lastOutput  end
			tableInsert(buffer, r.suffix)
		else
			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, r, false, nodeCb)
			if not ok then  return nil, lastOutput  end
		end

		return true, lastOutput
	end

	-- success, lastOutput = writeNode( buffer, pretty, indent, lastOutput, node, maySafelyOmitParens, nodeCallback )
	-- Returns nil and a message or error.
	function writeNode(buffer, pretty, indent, lastOutput, node, maySafelyOmitParens, nodeCb)
		if nodeCb then  nodeCb(node, buffer)  end
		pretty = choosePretty(node, pretty)

		tableInsert(buffer, node.prefix)

		local nodeType = node.type

		-- Expressions:

		if nodeType == "identifier" then
			local ident = node
			lastOutput = writeAlphanum(buffer, pretty, ident.name, lastOutput)

		elseif nodeType == "vararg" then
			local vararg = node
			if vararg.adjustToOne then  lastOutput = writeLua(buffer, "(", "")   end
			if lastOutput == "."  then  lastOutput = writeLua(buffer, " ", ".")  end
			lastOutput = writeLua(buffer, "...", ".")
			if vararg.adjustToOne then  lastOutput = writeLua(buffer, ")", "")  end

		elseif nodeType == "literal" then
			local literal = node

			if node.value == 0 and NORMALIZE_MINUS_ZERO then
				lastOutput = writeNumber(buffer, pretty, 0, lastOutput) -- Avoid writing '-0' sometimes.

			elseif literal.value == 1/0 then
				lastOutput = writeAlphanum(buffer, pretty, "(1/0)", lastOutput) -- Note: We parse this as one literal.

			elseif literal.value == -1/0 then
				lastOutput = writeLua(buffer, "(-1/0)", "") -- Note: We parse this as one literal.

			elseif literal.value ~= literal.value then
				lastOutput = writeLua(buffer, "(0/0)", "") -- Note: We parse this as one literal.

			elseif literal.value == nil or type(literal.value) == "boolean" then
				lastOutput = writeAlphanum(buffer, pretty, tostring(literal.value), lastOutput)

			elseif type(literal.value) == "number" or (jit and type(literal.value) == "cdata" and tonumber(literal.value)) then
				lastOutput = writeNumber(buffer, pretty, literal.value, lastOutput)

			elseif type(literal.value) == "string" then
				-- @Speed: Cache!
				local R           = isNumberInRange
				local s           = literal.value
				local doubleCount = countString(s, '"', true)
				local singleCount = countString(s, "'", true)
				local quote       = singleCount < doubleCount and "'" or '"'
				local quoteByte   = stringByte(quote)
				local pos         = 1

				lastOutput = writeLua(buffer, quote, "")

				while pos <= #s do
					local b1, b2, b3, b4 = stringByte(s, pos, pos+3)

					-- Printable ASCII.
					if R(b1,32,126) then
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
				return nil, F("Error: Failed outputting '%s' value '%s'.", type(literal.value), tostring(literal.value))
			end

		elseif nodeType == "table" then
			local tableNode = node
			lastOutput      = writeLua(buffer, "{", "")

			for i, tableField in ipairs(tableNode.fields) do
				if i > 1 then
					lastOutput = writeLua(buffer, ",", "")
					if pretty then  lastOutput = writeLua(buffer, " ", "")  end
				end

				if tableField.generatedKey then
					if tableField.key then
						if nodeCb then  nodeCb(tableField.key, buffer)  end
						tableInsert(buffer, tableField.key.prefix)
						tableInsert(buffer, tableField.key.suffix)
					end

				else
					if canNodeBeName(tableField.key) then
						if nodeCb then  nodeCb(tableField.key, buffer)  end
						tableInsert(buffer, tableField.key.prefix)
						lastOutput = writeLua(buffer, tableField.key.value, "alphanum")
						tableInsert(buffer, tableField.key.suffix)

					else
						lastOutput = writeLua(buffer, "[", "")

						local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, tableField.key, true, nodeCb)
						if not ok then  return nil, lastOutput  end

						lastOutput = writeLua(buffer, "]", "")
					end

					lastOutput = writeLua(buffer, "=", "")
				end

				local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, tableField.value, (not pretty), nodeCb)
				if not ok then  return nil, lastOutput  end
			end

			lastOutput = writeLua(buffer, "}", "")

		elseif nodeType == "lookup" then
			local lookup            = node
			local ok;ok, lastOutput = writeLookup(buffer, pretty, indent, lastOutput, lookup, false, nodeCb)
			if not ok then  return nil, lastOutput  end

		elseif nodeType == "unary" then
			local unary             = node
			local operatorOutput    = (unary.operator == "-" and "-") or (stringFind(unary.operator, "%w") and "alphanum") or ("")
			local prettyAndAlphanum = pretty and operatorOutput == "alphanum"

			if prettyAndAlphanum and not maySafelyOmitParens then  lastOutput = writeLua(buffer, "(", "")  end -- @Polish: Only output parentheses around child unaries/binaries if associativity requires it.

			if lastOutput == "-" and operatorOutput == "-" then  lastOutput = writeLua(buffer, " ", "")
			elseif                   operatorOutput ~= ""  then  ensureSpaceIfNotPretty(buffer, pretty, lastOutput, operatorOutput)
			end
			lastOutput = writeLua(buffer, unary.operator, operatorOutput)

			if prettyAndAlphanum then  lastOutput = writeLua(buffer, " ", "")  end

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, unary.expression, false, nodeCb)
			if not ok then  return nil, lastOutput  end

			if prettyAndAlphanum and not maySafelyOmitParens then  lastOutput = writeLua(buffer, ")", "")  end

		elseif nodeType == "binary" then
			local binary = node
			local op     = binary.operator

			if not maySafelyOmitParens then  lastOutput = writeLua(buffer, "(", "")  end -- @Polish: Only output parentheses around child unaries/binaries if associativity requires it.

			if op == ".." or op == "and" or op == "or" or op == "+" or op == "*" or op == "&" or op == "|" then
				local ok;ok, lastOutput = writeBinaryOperatorChain(buffer, pretty, indent, lastOutput, binary, nodeCb)
				if not ok then  return nil, lastOutput  end

			else
				local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, binary.left, false, nodeCb)
				if not ok then  return nil, lastOutput  end

				local operatorOutput = ((op == "-" and "-") or (stringFind(op, "%w") and "alphanum") or (""))

				if pretty then  lastOutput = writeLua(buffer, " ", "")  end

				if operatorOutput ~= "" then  ensureSpaceIfNotPretty(buffer, pretty, lastOutput, operatorOutput)  end
				lastOutput = writeLua(buffer, op, operatorOutput)

				if pretty then  lastOutput = writeLua(buffer, " ", "")  end

				local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, binary.right, false, nodeCb)
				if not ok then  return nil, lastOutput  end
			end

			if not maySafelyOmitParens then  lastOutput = writeLua(buffer, ")", "")  end

		elseif nodeType == "call" then -- Can be statement too.
			local call = node

			if call.adjustToOne then  lastOutput = writeLua(buffer, "(", "")  end

			if call.method then
				local lookup = call.callee

				if lookup.type ~= "lookup" then
					return nil, "Error: AST: Callee for method call is not a lookup."
				end

				if nodeCb then  nodeCb(lookup, buffer)  end

				tableInsert(buffer, lookup.prefix)
				local ok;ok, lastOutput = writeLookup(buffer, choosePretty(lookup, pretty), indent, lastOutput, lookup, true, nodeCb)
				if not ok then  return nil, lastOutput  end
				tableInsert(buffer, lookup.suffix)

			else
				local needParens = doesExpressionNeedParenthesisIfOnTheLeftSide(call.callee)
				if needParens then  lastOutput = writeLua(buffer, "(", "")  end

				local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, call.callee, false, nodeCb)
				if not ok then  return nil, lastOutput  end

				if needParens then  lastOutput = writeLua(buffer, ")", "")  end
			end

			lastOutput = writeLua(buffer, "(", "")

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, call.arguments, false, nodeCb)
			if not ok then  return nil, lastOutput  end

			lastOutput = writeLua(buffer, ")", "")
			if call.adjustToOne then  lastOutput = writeLua(buffer, ")", "")  end

		elseif nodeType == "function" then
			local func = node
			lastOutput = writeAlphanum(buffer, pretty, "function", lastOutput)

			local ok;ok, lastOutput = writeFunctionParametersAndBody(buffer, pretty, indent, lastOutput, func, func.parameters, nil, nodeCb)
			if not ok then  return nil, lastOutput  end

		-- Statements:

		elseif nodeType == "break" then
			lastOutput = writeAlphanum(buffer, pretty, "break", lastOutput)
			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "return" then
			local returnNode = node
			lastOutput       = writeAlphanum(buffer, pretty, "return", lastOutput)

			if returnNode.values[1] then
				if pretty then  lastOutput = writeLua(buffer, " ", "")  end

				local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, returnNode.values, false, nodeCb)
				if not ok then  return nil, lastOutput  end
			end

			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "label" then
			local label = node
			local name  = label.name
			if not (stringFind(name, "^[%a_][%w_]*$") and not KEYWORDS[name]) then
				return nil, F("Error: AST: Invalid label '%s'.", name)
			end
			lastOutput = writeLua(buffer, "::", "")
			lastOutput = writeAlphanum(buffer, pretty, name, lastOutput)
			lastOutput = writeLua(buffer, "::", "")
			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "goto" then
			local gotoNode = node
			local name     = gotoNode.name
			if not (stringFind(name, "^[%a_][%w_]*$") and not KEYWORDS[name]) then
				return nil, F("Error: AST: Invalid label '%s'.", name)
			end
			lastOutput = writeAlphanum(buffer, pretty, "goto", lastOutput)
			lastOutput = writeLua(buffer, " ", "")
			lastOutput = writeAlphanum(buffer, pretty, name,   lastOutput)
			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "block" then
			local block = node
			lastOutput  = writeAlphanum(buffer, pretty, "do", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, "\n", "")  end

			local ok;ok, lastOutput = writeStatements(buffer, pretty, indent+1, lastOutput, block.statements, nodeCb)
			if not ok then  return nil, lastOutput  end

			lastOutput = writeIndentationIfPretty(buffer, pretty, indent, lastOutput)
			lastOutput = writeAlphanum(buffer, pretty, "end", lastOutput)

		elseif nodeType == "declaration" then
			local decl = node
			lastOutput = writeAlphanum(buffer, pretty, "local", lastOutput)
			lastOutput = writeLua(buffer, " ", "")

			if not decl.names[1] then  return nil, "Error: AST: Missing name(s) for declaration."  end

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, decl.names, true, nodeCb)
			if not ok then  return nil, lastOutput  end

			if decl.values[1] then
				if pretty then  lastOutput = writeLua(buffer, " ", "")  end
				lastOutput = writeLua(buffer, "=", "")
				if pretty then  lastOutput = writeLua(buffer, " ", "")  end

				local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, decl.values, false, nodeCb)
				if not ok then  return nil, lastOutput  end
			end

			lastOutput = writeLua(buffer, ";", "")

		elseif nodeType == "assignment" then
			local assignment = node
			if not assignment.targets[1] then  return nil, "Error: AST: Missing target expression(s) for assignment."  end
			if not assignment.values[1]  then  return nil, "Error: AST: Missing value(s) for assignment."  end

			if isAssignmentFunctionAssignment(assignment) then
				local func = assignment.values[1]
				if nodeCb then  nodeCb(func, buffer)  end

				tableInsert(buffer, func.prefix)

				lastOutput = writeAlphanum(buffer, pretty, "function", lastOutput)
				lastOutput = writeLua(buffer, " ", "")

				local implicitSelfParam = (
					func.parameters[1] ~= nil
					and func.parameters[1].name == "self"
					and assignment.targets[1].type == "lookup"
					and canNodeBeName(assignment.targets[1].member)
				)

				if implicitSelfParam then
					if nodeCb then  nodeCb(assignment.targets[1], buffer)  end
					tableInsert(buffer, assignment.targets[1].prefix)
					local ok;ok, lastOutput = writeLookup(buffer, pretty, indent, lastOutput, assignment.targets[1], true, nodeCb)
					if not ok then  return nil, lastOutput  end
					tableInsert(buffer, assignment.targets[1].suffix)
				else
					local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, assignment.targets[1], false, nodeCb)
					if not ok then  return nil, lastOutput  end
				end

				local selfParam      = nil
				local explicitParams = func.parameters

				if implicitSelfParam then
					selfParam      = explicitParams[1]
					explicitParams = {tableUnpack(explicitParams, 2)}
				end

				local ok;ok, lastOutput = writeFunctionParametersAndBody(buffer, pretty, indent, lastOutput, func, explicitParams, selfParam, nodeCb)
				if not ok then  return nil, lastOutput  end

				tableInsert(buffer, func.suffix)

			else
				local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, assignment.targets, false, nodeCb)
				if not ok then  return nil, lastOutput  end

				if pretty then  lastOutput = writeLua(buffer, " ", "")  end
				lastOutput = writeLua(buffer, "=", "")
				if pretty then  lastOutput = writeLua(buffer, " ", "")  end

				local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, assignment.values, false, nodeCb)
				if not ok then  return nil, lastOutput  end

				lastOutput = writeLua(buffer, ";", "")
			end

		elseif nodeType == "if" then
			local ifNode = node
			lastOutput   = writeAlphanum(buffer, pretty, "if", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, " ", "")  end

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, ifNode.condition, true, nodeCb)
			if not ok then  return nil, lastOutput  end

			if pretty then  lastOutput = writeLua(buffer, " ", "")  end
			lastOutput = writeAlphanum(buffer, pretty, "then", lastOutput)
			if nodeCb then  nodeCb(ifNode.bodyTrue, buffer)  end
			local prettyBody = choosePretty(ifNode.bodyTrue, pretty)
			tableInsert(buffer, ifNode.bodyTrue.prefix)
			if prettyBody then  lastOutput = writeLua(buffer, "\n", "")  end

			local ok;ok, lastOutput = writeStatements(buffer, prettyBody, indent+1, lastOutput, ifNode.bodyTrue.statements, nodeCb)
			if not ok then  return nil, lastOutput  end

			local lastTrueBody              = ifNode.bodyTrue
			local suffixesForTrailingBodies = {}

			while ifNode.bodyFalse do
				lastOutput = writeIndentationIfPretty(buffer, prettyBody, indent, lastOutput)

				-- Automatically detect what looks like 'elseif'.
				if #ifNode.bodyFalse.statements == 1 and ifNode.bodyFalse.statements[1].type == "if" then
					tableInsert(buffer, lastTrueBody.suffix)
					local body = ifNode.bodyFalse

					if nodeCb then  nodeCb(body, buffer)  end
					tableInsert(suffixesForTrailingBodies, body.suffix)
					ifNode = body.statements[1]
					if nodeCb then  nodeCb(ifNode, buffer)  end
					pretty = choosePretty(ifNode, pretty)

					tableInsert(buffer, body.prefix)
					tableInsert(buffer, ifNode.prefix)
					lastOutput = writeAlphanum(buffer, prettyBody, "elseif", lastOutput)
					if pretty then  lastOutput = writeLua(buffer, " ", "")  end

					local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, ifNode.condition, true, nodeCb)
					if not ok then  return nil, lastOutput  end

					if pretty then  lastOutput = writeLua(buffer, " ", "")  end
					lastOutput = writeAlphanum(buffer, pretty, "then", lastOutput)
					if nodeCb then  nodeCb(ifNode.bodyTrue, buffer)  end
					prettyBody = choosePretty(ifNode.bodyTrue, pretty)
					if prettyBody then  lastOutput = writeLua(buffer, "\n", "")  end

					tableInsert(buffer, ifNode.bodyTrue.prefix)
					local ok;ok, lastOutput = writeStatements(buffer, prettyBody, indent+1, lastOutput, ifNode.bodyTrue.statements, nodeCb)
					if not ok then  return nil, lastOutput  end
					tableInsert(buffer, ifNode.bodyTrue.suffix)

					lastTrueBody = ifNode

				else
					lastOutput = writeAlphanum(buffer, prettyBody, "else", lastOutput)
					if nodeCb then  nodeCb(ifNode.bodyFalse, buffer)  end
					prettyBody = choosePretty(ifNode.bodyFalse, pretty)
					tableInsert(buffer, ifNode.bodyFalse.prefix)
					if prettyBody then  lastOutput = writeLua(buffer, "\n", "")  end

					local ok;ok, lastOutput = writeStatements(buffer, prettyBody, indent+1, lastOutput, ifNode.bodyFalse.statements, nodeCb)
					if not ok then  return nil, lastOutput  end

					tableInsert(suffixesForTrailingBodies, ifNode.bodyFalse.suffix)
					break
				end
			end

			lastOutput = writeIndentationIfPretty(buffer, prettyBody, indent, lastOutput)
			tableInsert(buffer, lastTrueBody.suffix)
			for i = #suffixesForTrailingBodies, 1, -1 do
				tableInsert(buffer, suffixesForTrailingBodies[i])
			end
			lastOutput = writeAlphanum(buffer, prettyBody, "end", lastOutput)

		elseif nodeType == "while" then
			local whileLoop = node
			lastOutput      = writeAlphanum(buffer, pretty, "while", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, " ", "")  end

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, whileLoop.condition, true, nodeCb)
			if not ok then  return nil, lastOutput  end

			if pretty then  lastOutput = writeLua(buffer, " ", "")  end
			lastOutput = writeAlphanum(buffer, pretty, "do", lastOutput)

			if nodeCb then  nodeCb(whileLoop.body, buffer)  end
			local prettyBody = choosePretty(whileLoop.body, pretty)
			tableInsert(buffer, whileLoop.body.prefix)
			if prettyBody then  lastOutput = writeLua(buffer, "\n", "")  end

			local ok;ok, lastOutput = writeStatements(buffer, prettyBody, indent+1, lastOutput, whileLoop.body.statements, nodeCb)
			if not ok then  return nil, lastOutput  end

			lastOutput = writeIndentationIfPretty(buffer, prettyBody, indent, lastOutput)
			tableInsert(buffer, whileLoop.body.suffix)
			lastOutput = writeAlphanum(buffer, prettyBody, "end", lastOutput)

		elseif nodeType == "repeat" then
			local repeatLoop = node
			lastOutput       = writeAlphanum(buffer, pretty, "repeat", lastOutput)
			if nodeCb then  nodeCb(repeatLoop.body, buffer)  end
			local prettyBody = choosePretty(repeatLoop.body, pretty)
			tableInsert(buffer, repeatLoop.body.prefix)
			if prettyBody then  lastOutput = writeLua(buffer, "\n", "")  end

			local ok;ok, lastOutput = writeStatements(buffer, prettyBody, indent+1, lastOutput, repeatLoop.body.statements, nodeCb)
			if not ok then  return nil, lastOutput  end

			lastOutput = writeIndentationIfPretty(buffer, prettyBody, indent, lastOutput)
			tableInsert(buffer, repeatLoop.body.suffix)
			lastOutput = writeAlphanum(buffer, prettyBody, "until", lastOutput)
			if pretty then  lastOutput = writeLua(buffer, " ", "")  end

			local ok;ok, lastOutput = writeNode(buffer, pretty, indent, lastOutput, repeatLoop.condition, true, nodeCb)
			if not ok then  return nil, lastOutput  end

		elseif nodeType == "for" then
			local forLoop = node
			if not forLoop.names[1]  then  return nil, "Error: AST: Missing name(s) for 'for' loop."   end
			if not forLoop.values[1] then  return nil, "Error: AST: Missing value(s) for 'for' loop."  end

			lastOutput = writeAlphanum(buffer, pretty, "for", lastOutput)
			lastOutput = writeLua(buffer, " ", "")

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, forLoop.names, false, nodeCb)
			if not ok then  return nil, lastOutput  end

			if pretty then  lastOutput = writeLua(buffer, " ", "")  end

			if forLoop.kind == "numeric" then
				lastOutput = writeLua(buffer, "=", "")
			elseif forLoop.kind == "generic" then
				lastOutput = writeAlphanum(buffer, pretty, "in", lastOutput)
			else
				return nil, F("Error: Unknown 'for' loop kind '%s'.", forLoop.kind)
			end

			if pretty then  lastOutput = writeLua(buffer, " ", "")  end

			local ok;ok, lastOutput = writeCommaSeparatedList(buffer, pretty, indent, lastOutput, forLoop.values, false, nodeCb)
			if not ok then  return nil, lastOutput  end

			if pretty then  lastOutput = writeLua(buffer, " ", "")  end
			lastOutput = writeAlphanum(buffer, pretty, "do", lastOutput)
			if nodeCb then  nodeCb(forLoop.body, buffer)  end
			local prettyBody = choosePretty(forLoop.body, pretty)
			tableInsert(buffer, forLoop.body.prefix)
			if prettyBody then  lastOutput = writeLua(buffer, "\n", "")  end

			local ok;ok, lastOutput = writeStatements(buffer, prettyBody, indent+1, lastOutput, forLoop.body.statements, nodeCb)
			if not ok then  return nil, lastOutput  end

			lastOutput = writeIndentationIfPretty(buffer, prettyBody, indent, lastOutput)
			tableInsert(buffer, forLoop.body.suffix)
			lastOutput = writeAlphanum(buffer, prettyBody, "end", lastOutput)

		else
			return false, F("Error: Unknown node type '%s'.", tostring(nodeType))
		end

		tableInsert(buffer, node.suffix)

		return true, lastOutput
	end

	-- luaString    = toLua( astNode [, prettyOuput=false, nodeCallback ] )
	-- nodeCallback = function( node, outputBuffer )
	-- Returns nil and a message on error.
	function toLua(node, pretty, nodeCb)
		assertArg1("toLua", 1, node, "table")

		local buffer = {}

		local ok, err
		if node.type == "block" then -- @Robustness: This exception isn't great. Should there be a file scope node?
			if nodeCb then  nodeCb(node, buffer)  end
			tableInsert(buffer, node.prefix)
			ok, err = writeStatements(buffer, choosePretty(node, pretty), 0, "", node.statements, nodeCb)
			tableInsert(buffer, node.suffix)
		else
			ok, err = writeNode(buffer, pretty, 0, "", node, true, nodeCb)
		end

		if ok then
			return tableConcat(buffer)
		else
			return nil, err
		end
	end
end



-- node = getChild( node, fieldName )
-- node = getChild( node, fieldName, index )                -- If the node field is an array.
-- node = getChild( node, fieldName, index, tableFieldKey ) -- If the node field is a table field array.
local function getChild(node, fieldName, i, tableFieldKey)
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
local function setChild(node, fieldName, i, tableFieldKey, childNode)
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
local function addChild(node, fieldName, i, childNode, extraChildNode)
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
local function removeChild(node, fieldName, i)
	assertArg1("removeChild", 1, node,      "table")
	assertArg1("removeChild", 2, fieldName, "string")
	assertArg2("removeChild", 3, i,         "number","nil")

	local nodeType       = node.type
	local childFields    = CHILD_FIELDS[nodeType] or errorf(2, "Unknown node type '%s'.", tostring(nodeType))
	local childFieldType = childFields[fieldName] or errorf(2, "Unknown node field '%s.%s'.", nodeType, tostring(fieldName))

	if childFieldType == "nodearray" or childFieldType == "tablefields" then
		tableRemove(node[fieldName], i) -- This also works if i is nil.
	else
		errorf(2, "Node field '%s.%s' is not an array.", nodeType, tostring(fieldName))
	end
end



local validateTree
do
	local function addValidationError(path, errors, s, ...)
		tableInsert(errors, F("%s: "..s, tableConcat(path, " > "), ...))
	end

	local function validateNode(node, path, errors, prefix)
		local nodeType = node.type

		tableInsert(path,
			(prefix and prefix.."."..nodeType or nodeType)
			.. (parser.printIds and "#"..node.id or "")
		)

		if nodeType == "identifier" then
			local ident = node
			if not stringFind(ident.name, "^[%a_][%w_]*$") then
				addValidationError(path, errors, "Invalid identifier name: Bad format: %s", ident.name)
			elseif KEYWORDS[ident.name] then
				addValidationError(path, errors, "Invalid identifier name: Name is a keyword: %s", ident.name)
			end
			if not (ident.attribute == "" or ident.attribute == "close" or ident.attribute == "const") then
				addValidationError(path, errors, "Invalid identifier attribute '%s'.", ident.attribute)
			end

		elseif nodeType == "vararg" then
			-- void

		elseif nodeType == "literal" then
			local literal = node
			local vType   = type(literal.value)
			if not (vType == "number" or vType == "string" or vType == "boolean" or vType == "nil" or (jit and vType == "cdata" and tonumber(literal.value))) then
				addValidationError(path, errors, "Invalid literal value type '%s'.", vType)
			end

		elseif nodeType == "break" then
			-- void

		elseif nodeType == "label" then
			local label = node
			if not stringFind(label.name, "^[%a_][%w_]*$") then
				addValidationError(path, errors, "Invalid label name: Bad format: %s", label.name)
			elseif KEYWORDS[label.name] then
				addValidationError(path, errors, "Invalid label name: Name is a keyword: %s", label.name)
			end

		elseif nodeType == "goto" then
			local gotoNode = node
			if not stringFind(gotoNode.name, "^[%a_][%w_]*$") then
				addValidationError(path, errors, "Invalid label name: Bad format: %s", gotoNode.name)
			elseif KEYWORDS[gotoNode.name] then
				addValidationError(path, errors, "Invalid label name: Name is a keyword: %s", gotoNode.name)
			end

		elseif nodeType == "lookup" then
			-- @Incomplete: Should we detect nil literal objects? :DetectRuntimeErrors
			local lookup = node
			if not lookup.object then
				addValidationError(path, errors, "Missing 'object' field.")
			elseif not EXPRESSION_NODES[lookup.object.type] then
				addValidationError(path, errors, "The object is not an expression. (It is '%s'.)", lookup.object.type)
			else
				validateNode(lookup.object, path, errors, "object")
			end
			if not lookup.member then
				addValidationError(path, errors, "Missing 'member' field.")
			elseif not EXPRESSION_NODES[lookup.member.type] then
				addValidationError(path, errors, "The member is not an expression. (It is '%s'.)", lookup.member.type)
			else
				validateNode(lookup.member, path, errors, "member")
			end

		elseif nodeType == "unary" then
			local unary = node
			if not OPERATORS_UNARY[unary.operator] then
				addValidationError(path, errors, "Invalid unary operator '%s'.", unary.operator)
			end
			if not unary.expression then
				addValidationError(path, errors, "Missing 'expression' field.")
			elseif not EXPRESSION_NODES[unary.expression.type] then
				addValidationError(path, errors, "The 'expression' field does not contain an expression. (It is '%s'.)", unary.expression.type)
			else
				validateNode(unary.expression, path, errors, nil)
			end

		elseif nodeType == "binary" then
			local binary = node
			if not OPERATORS_BINARY[binary.operator] then
				addValidationError(path, errors, "Invalid binary operator '%s'.", binary.operator)
			end
			if not binary.left then
				addValidationError(path, errors, "Missing 'left' field.")
			elseif not EXPRESSION_NODES[binary.left.type] then
				addValidationError(path, errors, "The left side is not an expression. (It is '%s'.)", binary.left.type)
			else
				validateNode(binary.left, path, errors, "left")
			end
			if not binary.right then
				addValidationError(path, errors, "Missing 'right' field.")
			elseif not EXPRESSION_NODES[binary.right.type] then
				addValidationError(path, errors, "The right side is not an expression. (It is '%s'.)", binary.right.type)
			else
				validateNode(binary.right, path, errors, "right")
			end

		elseif nodeType == "call" then
			local call = node
			if not call.callee then
				addValidationError(path, errors, "Missing 'callee' field.")
			elseif not EXPRESSION_NODES[call.callee.type] then
				addValidationError(path, errors, "Callee is not an expression. (It is '%s'.)", call.callee.type)
			-- elseif call.callee.type == "literal" or call.callee.type == "table" then -- @Incomplete: Do this kind of check? Or maybe we should stick to strictly validating the AST even if the resulting Lua code would raise a runtime error. :DetectRuntimeErrors
			-- 	addValidationError(path, errors, "Callee is uncallable.")
			elseif call.method and not (
				call.callee.type == "lookup"
				and call.callee.member
				and call.callee.member.type == "literal"
				and type(call.callee.member.value) == "string"
				and stringFind(call.callee.member.value, "^[%a_][%w_]*$")
				and not KEYWORDS[call.callee.member.value]
			) then
				addValidationError(path, errors, "Callee is unsuitable for method call.")
			else
				validateNode(call.callee, path, errors, "callee")
			end
			for i, expr in ipairs(call.arguments) do
				if not EXPRESSION_NODES[expr.type] then
					addValidationError(path, errors, "Argument %d is not an expression. (It is '%s')", i, expr.type)
				else
					validateNode(expr, path, errors, "arg"..i)
				end
			end

		elseif nodeType == "function" then
			local func = node
			for i, ident in ipairs(func.parameters) do
				if not (ident.type == "identifier" or (ident.type == "vararg" and i == #func.parameters)) then
					addValidationError(path, errors, "Parameter %d is not an identifier%s. (It is '%s')", i, (i == #func.parameters and " or vararg" or ""), ident.type)
				else
					validateNode(ident, path, errors, "param"..i)
				end
			end
			if not func.body then
				addValidationError(path, errors, "Missing 'body' field.")
			elseif func.body.type ~= "block" then
				addValidationError(path, errors, "Body is not a block.")
			else
				validateNode(func.body, path, errors, "body")
			end

		elseif nodeType == "return" then
			local returnNode = node
			for i, expr in ipairs(returnNode.values) do
				if not EXPRESSION_NODES[expr.type] then
					addValidationError(path, errors, "Value %d is not an expression. (It is '%s')", i, expr.type)
				else
					validateNode(expr, path, errors, i)
				end
			end

		elseif nodeType == "block" then
			local block = node
			for i, statement in ipairs(block.statements) do
				if not STATEMENT_NODES[statement.type] then
					addValidationError(path, errors, "Child node %d is not a statement. (It is '%s'.)", i, statement.type)
				else
					validateNode(statement, path, errors, i)
				end
			end

		elseif nodeType == "declaration" then
			local decl = node
			if not decl.names[1] then
				addValidationError(path, errors, "Missing name(s).")
			end
			for i, ident in ipairs(decl.names) do
				if ident.type ~= "identifier" then
					addValidationError(path, errors, "Name %d is not an identifier. (It is '%s')", i, ident.type)
				else
					validateNode(ident, path, errors, "name"..i)
				end
			end
			for i, expr in ipairs(decl.values) do
				if not EXPRESSION_NODES[expr.type] then
					addValidationError(path, errors, "Value %d is not an expression. (It is '%s')", i, expr.type)
				else
					validateNode(expr, path, errors, "value"..i)
				end
			end

		elseif nodeType == "assignment" then
			local assignment = node
			if not assignment.targets[1] then
				addValidationError(path, errors, "Missing target expression(s).")
			end
			for i, expr in ipairs(assignment.targets) do
				if not (expr.type == "identifier" or expr.type == "lookup") then
					addValidationError(path, errors, "Target %d is not an identifier or lookup. (It is '%s')", i, expr.type)
				else
					validateNode(expr, path, errors, "target"..i)
				end
			end
			if not assignment.values[1] then
				addValidationError(path, errors, "Missing value(s).")
			end
			for i, expr in ipairs(assignment.values) do
				if not EXPRESSION_NODES[expr.type] then
					addValidationError(path, errors, "Value %d is not an expression. (It is '%s')", i, expr.type)
				else
					validateNode(expr, path, errors, "value"..i)
				end
			end

		elseif nodeType == "if" then
			local ifNode = node
			if not ifNode.condition then
				addValidationError(path, errors, "Missing 'condition' field.")
			elseif not EXPRESSION_NODES[ifNode.condition.type] then
				addValidationError(path, errors, "The condition is not an expression. (It is '%s'.)", ifNode.condition.type)
			else
				validateNode(ifNode.condition, path, errors, "condition")
			end
			if not ifNode.bodyTrue then
				addValidationError(path, errors, "Missing 'bodyTrue' field.")
			elseif ifNode.bodyTrue.type ~= "block" then
				addValidationError(path, errors, "Body for true branch is not a block.")
			else
				validateNode(ifNode.bodyTrue, path, errors, "true")
			end
			if not ifNode.bodyFalse then
				-- void
			elseif ifNode.bodyFalse.type ~= "block" then
				addValidationError(path, errors, "Body for false branch is not a block.")
			else
				validateNode(ifNode.bodyFalse, path, errors, "false")
			end

		elseif nodeType == "while" then
			local whileLoop = node
			if not whileLoop.condition then
				addValidationError(path, errors, "Missing 'condition' field.")
			elseif not EXPRESSION_NODES[whileLoop.condition.type] then
				addValidationError(path, errors, "The condition is not an expression. (It is '%s'.)", whileLoop.condition.type)
			else
				validateNode(whileLoop.condition, path, errors, "condition")
			end
			if not whileLoop.body then
				addValidationError(path, errors, "Missing 'body' field.")
			elseif whileLoop.body.type ~= "block" then
				addValidationError(path, errors, "Body is not a block.")
			else
				validateNode(whileLoop.body, path, errors, "true")
			end

		elseif nodeType == "repeat" then
			local repeatLoop = node
			if not repeatLoop.body then
				addValidationError(path, errors, "Missing 'body' field.")
			elseif repeatLoop.body.type ~= "block" then
				addValidationError(path, errors, "Body is not a block.")
			else
				validateNode(repeatLoop.body, path, errors, "true")
			end
			if not repeatLoop.condition then
				addValidationError(path, errors, "Missing 'condition' field.")
			elseif not EXPRESSION_NODES[repeatLoop.condition.type] then
				addValidationError(path, errors, "The condition is not an expression. (It is '%s'.)", repeatLoop.condition.type)
			else
				validateNode(repeatLoop.condition, path, errors, "condition")
			end

		elseif nodeType == "for" then
			local forLoop = node
			if not (forLoop.kind == "numeric" or forLoop.kind == "generic") then
				addValidationError(path, errors, "Invalid for loop kind '%s'.", forLoop.kind)
			end
			if not forLoop.names[1] then
				addValidationError(path, errors, "Missing name(s).")
			elseif forLoop.kind == "numeric" and forLoop.names[2] then
				addValidationError(path, errors, "Too many names for numeric loop. (Got %d)", #forLoop.names)
			end
			for i, ident in ipairs(forLoop.names) do
				if ident.type ~= "identifier" then
					addValidationError(path, errors, "Name %d is not an identifier. (It is '%s')", i, ident.type)
				else
					validateNode(ident, path, errors, "name"..i)
				end
			end
			if not forLoop.values[1] then
				addValidationError(path, errors, "Missing value(s).")
			elseif forLoop.kind == "numeric" and not forLoop.values[2] then
				addValidationError(path, errors, "Too few values for numeric loop. (Got %d)", #forLoop.values)
			elseif forLoop.kind == "numeric" and forLoop.values[4] then
				addValidationError(path, errors, "Too many values for numeric loop. (Got %d)", #forLoop.values)
			end
			for i, expr in ipairs(forLoop.values) do
				if not EXPRESSION_NODES[expr.type] then
					addValidationError(path, errors, "Value %d is not an expression. (It is '%s')", i, expr.type)
				else
					validateNode(expr, path, errors, "value"..i)
				end
			end
			if not forLoop.body then
				addValidationError(path, errors, "Missing 'body' field.")
			elseif forLoop.body.type ~= "block" then
				addValidationError(path, errors, "Body is not a block.")
			else
				validateNode(forLoop.body, path, errors, "body")
			end

		elseif nodeType == "table" then
			local tableNode = node
			for i, tableField in ipairs(tableNode.fields) do
				-- @Incomplete: Should we detect nil literal keys? :DetectRuntimeErrors
				if not tableField.key then
					if not tableField.generatedKey then
						addValidationError(path, errors, "Missing 'key' field for table field %d.", i)
					end
				elseif not EXPRESSION_NODES[tableField.key.type] then
					addValidationError(path, errors, "The key for table field %d is not an expression. (It is '%s'.)", i, tableField.key.type)
				elseif tableField.generatedKey and tableField.key.type ~= "literal" then
					addValidationError(path, errors, "The generated key for table field %d is not a numeral. (It is '%s'.)", i, tableField.key.type)
				elseif tableField.generatedKey and type(tableField.key.value) ~= "number" then
					addValidationError(path, errors, "The generated key for table field %d is not a number. (It's a '%s' literal.)", i, type(tableField.key.value))
				else
					validateNode(tableField.key, path, errors, "key")
				end
				if not tableField.value then
					addValidationError(path, errors, "Missing 'value' field for table field %d.", i)
				elseif not EXPRESSION_NODES[tableField.value.type] then
					addValidationError(path, errors, "The value for table field %d is not an expression. (It is '%s'.)", i, tableField.value.type)
				else
					validateNode(tableField.value, path, errors, "value")
				end
			end

		else
			errorf("Invalid node type '%s'.", tostring(nodeType)) -- We don't call addValidationError() for this - it's just an assertion.
		end

		path[#path] = nil
	end

	-- isValid, errors = validateTree( astNode )
	function validateTree(node)
		local path   = {}
		local errors = {}

		validateNode(node, path, errors, nil)

		if errors[1] then
			return false, tableConcat(errors, "\n")
		else
			return true
		end
	end
end



local EXPRESSION_TYPES = newSet{"binary","call","function","identifier","literal","lookup","table","unary","vararg"}

local function isExpression(node)
	return EXPRESSION_TYPES[node.type] == true
end

local function isStatement(node)
	return EXPRESSION_TYPES[node.type] == nil or node.type == "call"
end



local function resetNextId()
	nextSerialNumber = 1
end



-- astNode = valueToAst( value [, sortTableKeys=false ] )
local function valueToAst(v, sortTableKeys)
	local vType = type(v)

	if vType == "number" or vType == "string" or vType == "boolean" or vType == "nil" then
		return AstLiteral(nil, v)

	elseif vType == "table" then
		local t         = v
		local tableNode = AstTable(nil)
		local keys      = {}
		local indices   = {}

		for k in pairs(t) do
			tableInsert(keys, k)
		end

		if sortTableKeys then
			local keyStringRepresentations = {}

			for _, k in ipairs(keys) do
				keyStringRepresentations[k] = keyStringRepresentations[k] or tostring(k)
			end

			tableSort(keys, function(a, b)
				return keyStringRepresentations[a] < keyStringRepresentations[b]
			end)
		end

		for i = 1, #t do
			indices[i] = true
		end

		for _, k in ipairs(keys) do
			if not indices[k] then
				local tableField = {key=valueToAst(k,sortTableKeys), value=valueToAst(t[k],sortTableKeys), generatedKey=false}
				tableInsert(tableNode.fields, tableField)
			end
		end

		for i = 1, #t do
			local tableField = {key=valueToAst(i,sortTableKeys), value=valueToAst(t[i],sortTableKeys), generatedKey=true}
			tableInsert(tableNode.fields, tableField)
		end

		return tableNode

	else
		error("Invalid value type '"..vType.."'.", 2)
	end
end



-- identifiers = findGlobalReferences( astNode )
local function findGlobalReferences(theNode)
	local idents = {}

	traverseTree(theNode, function(node)
		if node.type == "identifier" and not node.declaration then
			tableInsert(idents, node)
		end
	end)

	return idents
end



-- identifiers = findDeclaredNames( astNode )
local function findDeclaredNames(theNode)
	local declIdents = {}

	traverseTree(theNode, function(node)
		-- Note: We don't now, but if we would require updateReferences() to be called first
		-- we could just check the type and if node.declaration==node. Decisions...

		if node.type == "declaration" or node.type == "for" then
			for _, declIdent in ipairs(node.names) do
				tableInsert(declIdents, declIdent)
			end

		elseif node.type == "function" then
			for _, declIdent in ipairs(node.parameters) do
				if declIdent.type == "identifier" then -- There may be a vararg at the end.
					tableInsert(declIdents, declIdent)
				end
			end
		end
	end)

	return declIdents
end



-- shadows, foundPrevious = maybeRegisterShadow( shadowSequences, shadowSequenceByIdent, shadows|nil, currentDeclIdent, declIdent )
local function maybeRegisterShadow(shadowSequences, shadowSequenceByIdent, shadows, currentDeclIdent, declIdent)
	if declIdent.name ~= currentDeclIdent.name then
		return shadows, false
	end

	if not shadows then
		shadows = {currentDeclIdent}
		shadowSequenceByIdent[currentDeclIdent] = shadows
		tableInsert(shadowSequences, shadows)
	end

	if shadowSequenceByIdent[declIdent] then
		-- Shortcut! declIdent is shadowing others, so just copy the existing data.
		for _, prevDeclIdent in ipairs(shadowSequenceByIdent[declIdent]) do
			tableInsert(shadows, prevDeclIdent)
		end
		return shadows, true

	else
		tableInsert(shadows, declIdent)
		return shadows, false
	end
end

-- shadowSequences = findShadows( astNode )
-- shadowSequences = { shadowSequence1, ... }
-- shadowSequence  = { shadowingIdentifier, shadowedIdentifier1, ... }
local function findShadows(theNode)
	local shadowSequences       = {}
	local shadowSequenceByIdent = {}

	for _, currentDeclIdent in ipairs(findDeclaredNames(theNode)) do
		local shadows       = nil
		local child         = currentDeclIdent
		local foundPrevious = false

		while child.parent do
			local parent = child.parent

			if isNodeDeclLike(parent) then
				local declIdents = getNameArrayOfDeclLike(parent)
				local childIndex = indexOf(declIdents, child) or #declIdents+1

				for i = childIndex-1, 1, -1 do
					shadows, foundPrevious = maybeRegisterShadow(shadowSequences, shadowSequenceByIdent, shadows, currentDeclIdent, declIdents[i])
					if foundPrevious then  break  end
				end
				if foundPrevious then  break  end

			elseif parent.type == "block" then
				local statements = parent.statements

				for i = child.key-1, 1, -1 do
					if statements[i].type == "declaration" then
						for _, declIdent in ipairs(statements[i].names) do
							shadows, foundPrevious = maybeRegisterShadow(shadowSequences, shadowSequenceByIdent, shadows, currentDeclIdent, declIdent)
							if foundPrevious then  break  end
						end
						if foundPrevious then  break  end
					end
				end
				if foundPrevious then  break  end
			end

			child = parent
			if child == theNode then  break  end -- Stay within theNode (in case theNode has a parent).
		end
	end

	return shadowSequences
end



parser = {
	--
	-- Constants.
	--
	VERSION = PARSER_VERSION,

	INT_SIZE = INT_SIZE,
	MAX_INT  = MAX_INT,
	MIN_INT  = MIN_INT,

	--
	-- Functions.
	--

	-- Tokenizing.
	tokenize     = tokenize,
	tokenizeFile = tokenizeFile,

	-- Token actions.
	newToken     = newToken,
	updateToken  = updateToken,
	cloneToken   = cloneToken,
	concatTokens = concatTokens,

	-- AST parsing.
	parse           = parse,
	parseExpression = parseExpression,
	parseFile       = parseFile,

	-- AST manipulation.
	newNode     = newNode,
	newNodeFast = newNodeFast,
	valueToAst  = valueToAst,
	cloneNode   = cloneNode,
	cloneTree   = cloneTree,
	getChild    = getChild,
	setChild    = setChild,
	addChild    = addChild,
	removeChild = removeChild,

	-- AST checking.
	isExpression = isExpression,
	isStatement  = isStatement,
	validateTree = validateTree,

	-- AST traversal.
	traverseTree        = traverseTree,
	traverseTreeReverse = traverseTreeReverse,
	updateReferences    = updateReferences,

	-- Big AST operations.
	simplify = simplify,
	optimize = optimize,
	minify   = minify,

	-- Conversion.
	toLua = toLua,

	-- Printing.
	printTokens   = printTokens,
	printNode     = printNode,
	printTree     = printTree,
	formatMessage = formatMessage,

	-- Utilities.
	findDeclaredNames    = findDeclaredNames,
	findGlobalReferences = findGlobalReferences,
	findShadows          = findShadows,

	-- Misc.
	resetNextId = resetNextId, -- @Undocumented

	--
	-- Settings.
	--
	printIds       = false,
	printLocations = false,
	indentation    = "    ",

	constantNameReplacementStringMaxLength = 200, -- @Cleanup: Maybe use a better name.
}

return parser



--[[!===========================================================

Copyright © 2020-2022 Marcus 'ReFreezed' Thunström

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

==============================================================]]
