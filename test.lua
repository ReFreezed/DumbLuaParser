local a
local b, c = true, "f\0o"

-- Comment
--[[ Long ]] --[==[ comment. ]==]

do
	local x = 1
	local y = 1.
	local z = .1
	local n = 0x0.8p-1

	local y = (a:b[[]])
	local s = "\0\1\2\3\4\5\6\7\8\9\10\11\12\13\\\01499\15\16\17\18\19\"\20\21\22\23¤\24\25\26\27\28\29\30\31~"

	do foo() ; o:m() end

	while false or true do
		foo()
	end
	repeat
		local x = x + foo()
	until x - -7 == 8

	for i = 1, 9 do  foo(i)  end
	for i, v in ipairs(t) do  bar(i, v)  end

	local function foo() end
	local function foo(a, ...) return end
	function a:b(x, ...) return ... end
	a[x] = function(i, j) if i then return end return 5 end
	g = a[x]()

	local a = 1 + 2 + 3
	local b = "1".."2".."3"
	local c = 1 + 2^3^4 * 5

	local s = a   .. b
	local s = 1.  .. x
	local s = 1.1 .. x

	local x = a and b and c or q and w or 5

	foo() ; (f1 or f2).k, (f1 or f2).k = x, y

	if x or y then foo() end

	local t = {x=5, y=(a or b), f=function(...)return"bark"end}

	local m = -8
	local n = not false or false
	local l = #t + 1
	local b = not not not true

	;;;;

	-- Special number notations.
	local n = - -5
	local n = -1/0
	local n = - - 1 / 0
	local n = - (-1/0)
	local n = 5 - 1/0
	local n = - - (-0)

	--[[ Lua 5.2+
	goto label
	::label::

	local a = x & y
	local b = x ~ y
	local c = x | y
	local d = x << y
	local e = x >> y
	local f = x // y
	local g = ~x

	local foo = ((y & 0x7) << 4) | 0xFF00
	--]]
end

--
-- Minify tests
--
do
	local longNameA = global    -- Watchers: 3
	local longNameB = longNameA -- Watchers: 5
	local global    = global    -- Watchers: 4
	local longNameC = longNameB -- Watchers: 3
	local getGlobal = e         -- Watchers: 2
	local longNameD = longNameB -- Watchers: 1
	local longNameE = longNameA -- Watchers: 0

	function globalFunc(x) local x = x end

	local upvalue = 0

	local function f(arg1, arg2)
		local assign1, assign2 = 0, 0
		local decl, func

		decl = 0
		func = function()end

		for loop1, loop2 in ipairs(t) do
			func(upvalue, arg1,arg2, assign1,assign2, decl, loop1,loop2)
		end
	end
end

-- Folding.
do
	local n = 1 << 8     -- 256
	local n = 2^99999    -- huge
	local n = 5 - - - -5 -- 10

	local b = "yes" == "no"
	local b = 5     ~= nil
	local b = 80.6  >= 34

	local s = "one" .. 2 .. "three"

	local v = true  and always1()
	local v = true  or  never1()
	local v = 0     and always2()
	local v = 0     or  never2()
	local v = false and never3()
	local v = false or  always3()
	local v = nil   and never4()
	local v = nil   or  always4()
end

-- Removal of nodes.
do
	do
		local noButKeep, yesKeep, noRemove = globalFunc()
		globalFunc(yesKeep)
		noButKeep, yesKeep, noRemove = globalFunc()
	end

	for noButKeep, yesKeep, noRemove in globalFunc() do
		globalFunc(yesKeep)
	end

	do
		local noButKeep, yesKeep, noRemove = globalFunc()
		local function called()
			yesKeep = global1
		end
		local function notCalled()
			yesKeep = global2
			return global3
		end
		noButKeep, yesKeep, noRemove = globalFunc()
		called()
		print(yesKeep)
	end

	do
		globalFunc()
	end

	do
		local forward

		local function localFunc()
			forward()
		end

		function globalFunc()
			localFunc()
		end

		do
			local n = 0
			function forward()
				n = n + 1
				return n
			end
		end
	end

	-- This should be removed completely.
	do
		local n = 1 + 2

		local function f()
			local g = global
		end
		f = nil
	end
end
