--[[============================================================
--=
--=  Examples for Dumb Lua Parser
--=
--=    1 - Find globals
--=    2 - Replace calls to a function
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

local ast = parser.parse[[
local x = math.floor(1.5) -- 'math' is a global.
y       = "foo"           -- 'y' is a global.
]]

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


