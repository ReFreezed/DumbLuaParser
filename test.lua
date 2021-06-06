-- [===[
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

-- Simplifying/folding.
do
	local n1 = 1 << 8     -- 256
	local n1 = 2^99999    -- huge
	local n1 = 5 - - - -5 -- 10

	local n2 = 2779          -- 2779
	local n2 = ~2779         -- -2780
	local n2 = 2779 & 0x1011 -- 17
	local n2 = 2779 ~ 0x1011 -- 6858
	local n2 = 2779 | 0x1011 -- 6875

	local n3 =  2       -- 2
	local n3 =  2 >>  1 -- 1
	local n3 =  2 <<  1 -- 4
	local n3 = -2       -- -2
	local n3 = -2 >>  1 -- 32bit:2147483647, 64bit:9223372036854775807
	local n3 = -2 <<  1 -- -4
	local n3 =  2 >>  0 -- 2
	local n3 =  2 <<  0 -- 2
	local n3 = -2 << -1 -- 32bit:2147483647, 64bit:9223372036854775807
	local n3 = -2 >> -1 -- -4

	local n4 = 1/0 & 2 -- Should not fold.

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

	yes()
	if 9 > 1 then
		ifYes()
		if 1 == "foo" then  ifNo()  end
	end
	while 9 > 1 do
		whileYes()
		while 1 == "foo" do  whileNo()  end
	end
	repeat
		repeatYes()
		repeat  repeatOnce()  until 1 ~= "foo"
	until 9 <= 1
	yes()
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
		local function localFunc()  forward()  end
		function globalFunc()  localFunc()  end
		do
			local n = 0
			function forward()  n = n + 1 ; return n  end
		end
	end

	-- This block should be removed completely.
	do
		local n = 1 + 2

		local function f()
			local g = global
		end
		f = nil
	end

	-- Complex removals.
	do
		_"Remove everything."
		local a       = 1
		local a       = 1, 2
		local a, b    = 1
		local a, b    = 1, 2
		_"Keep calls."
		local a       = globalFunc1()
		local a       = globalFunc1(), 2
		local a       = 1, globalFunc2()
		local a       = 1, globalFunc2(), 3
		local a       = 1, 2, globalFunc3()
		_"Keep all calls."
		local a       = globalFunc1(), globalFunc2()
		_"Keep call."
		local a, b, c = 1, globalFunc2(), 3, 4 -- @Incomplete: Improve the result of this.
		c             = 1
		_"Keep call, c must be 3."
		local a, b, c = 1, globalFunc2(), 3, 4
		global1       = c
		_"C must be third value returned from call."
		local a, b, c = globalFunc123()
		global2       = c
		_"Keep call, c must be nil."
		local a, b, c = globalFunc1(), 2
		global3       = c
	end
end
--]===]
