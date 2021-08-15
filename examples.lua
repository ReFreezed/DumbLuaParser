--[[============================================================
--=
--=  Examples for Dumb Lua Parser
--=
--=    1 - Find globals
--=    2 - Replace calls to a function
--=    3 - Construct an AST (the painful way)
--=
--=-------------------------------------------------------------
--=
--=  Dumb Lua Parser - Lua parsing library
--=  by Marcus 'ReFreezed' Thunström
--=
--============================================================]]

local parser = require("dumbParser")

io.stdout:setvbuf("no")
io.stderr:setvbuf("no")



--
-- 1 - Find globals
--

local ast = parser.parse(
	[[
		local x = math.floor(1.5) -- 'math' is a global.
		y       = "foo"           -- 'y' is a global.
	]],
	"abc.lua"
)

parser.updateReferences(ast)

parser.traverseTree(ast, function(node)
	if node.type == "identifier" and not node.declaration then
		print(parser.formatMessage(node, "", "Found global '%s'.", node.name))
	end
end)

print("\n\n")



--
-- 2 - Replace calls to a function
--

--
-- Here we change calls to assert() into 'if' statements:
--
--     -- From this.
--     assert(foo)
--
--     -- Into this.
--     if not (foo) then
--         error("Assertion failed: foo")
--     end
--

local ast = parser.parse[[
	local function calculate(x)
		assert(type(x) == "number")
		-- ...
	end
	local function doThing(foo)
		assert(foo ~= "bar")
		-- ...
	end
]]

parser.traverseTree(ast, function(node, parent, container, key)
	if node.type == "call" and node.callee.type == "identifier" and node.callee.name == "assert" then
		-- We assume the parent is a block, i.e. that the call is a complete statement.
		local oldCall      = node
		local oldCondition = oldCall.arguments[1]

		-- Create the 'if' statement and the new condition.
		local ifNode    = parser.newNode("if")
		ifNode.bodyTrue = parser.newNode("block")

		local condition      = parser.newNode("unary", "not")
		condition.expression = oldCondition
		ifNode.condition     = condition

		-- Create the error() call.
		local message = "Assertion failed: " .. parser.toLua(oldCondition)

		local errorCall               = parser.newNode("call")
		errorCall.callee              = parser.newNode("identifier", "error")
		errorCall.arguments[1]        = parser.newNode("literal", message)
		ifNode.bodyTrue.statements[1] = errorCall

		-- Replace the node.
		container[key] = ifNode

		-- Prevent traversal down through the old call.
		return "ignorechildren"
	end
end)

print(parser.toLua(ast, true))
print("\n\n")



--
-- 3 - Construct an AST (the painful way)
--

--[[ This is the program we will create:
function _G.double(n)
	return 2 * n
end
local x = double(5)
]]

local new = parser.newNode

local block = new("block")

local assignment             = new("assignment")
assignment.targets[1]        = new("lookup")
assignment.targets[1].object = new("identifier", "_G")
assignment.targets[1].member = new("literal", "double")
table.insert(block.statements, assignment)

local func           = new("function")
func.parameters[1]   = new("identifier", "n")
func.body            = new("block")
assignment.values[1] = func

local ret           = new("return")
ret.values[1]       = new("binary", "*")
ret.values[1].left  = new("literal", 2)
ret.values[1].right = new("identifier", "n")
table.insert(func.body.statements, ret)

local decl                  = new("declaration")
decl.names[1]               = new("identifier", "x")
decl.values[1]              = new("call")
decl.values[1].callee       = new("identifier", "double")
decl.values[1].arguments[1] = new("literal", 5)
table.insert(block.statements, decl)

print(parser.toLua(block, true))
print("\n\n")


