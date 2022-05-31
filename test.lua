-- [===[
local a
local b, c = true, "f\0o"

local _ = 3, 345, 0xff, 0xBEBADA
local _ = 3.0, 3.1416, 314.16e-2, 0.31416E1, 34e1
local _ =  .0,  .1416,    .16e-2,  .31416E1, 34e1
local _ = 0x0.1E, 0xA23p-4, 0X1.921FB54442D18P+1
local _ = "'\'", [[\]]

-- Comment
--[[ Long ]] --[==[
comment.
]==]

do
	local x = 1
	local y = 1.
	local z = .1
	local n = 0x0.8p-1

	local y = (a:b[[]])
	local s = "\0\1\2\3\4\5\6\7\8\9\10\11\12\13\\\01499\15\16\17\18\19\"\20\21\22\23¤\24\25\26\27\28\29\30\31~ëÓ§\255`\0011\1\z

	\xff\u{b4}"

	local x = 1 + 2 + 3
	local y = (1 - 2) - 3
	local z = 1 - (2 - 3)

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
	local c = 1 + 4^3^2 * 5

	local s = a   .. b
	local s = 1.  .. x
	local s = 1.1 .. x

	local x = a and b and c or q and w or 5

	t.k1.k2       = x
	;(t.k1).k2    = x
	;(t1 or t2).k = x

	;(t1 or t2).k, (t3 or t4).k = x, y

	if x or y then foo() end

	if x then
		foo()
	elseif y then
		bar()
	else
		baz()
	end

	local t = {x=5, ["y"]=(a or b), f=function(...)return"bark"end}

	local m = -8
	local n = not false or false
	local l = #t + 1
	local b = not not not true

	local s1 = "\"''a''b''c''\""
	local s2 = '\'""a""b""c""\''

	print(a .. ... .. 3.75 .. 400)

	local s = (""):rep(9)
	local n = ({9})[1]
	local v = (...)[1]
	local b = (function(n) return n>1 end)(9)

	;;;;

	-- Special number notations.
	local n = - -5
	local n = -1/0
	local n = - -1 / 0
	local n = - (-1/0)
	local n = 5 - 1/0
	local n = - - (-0)

	-- [[ Lua 5.2+
	goto foo
	do goto foo end
	::foo::

	local a = x & y
	local b = x ~ y
	local c = x | y
	local d = x << y
	local e = x >> y
	local f = x // y
	local g = ~x

	local foo = ((y & 0x7) << 4) | 0xFF00

	while 1 do
		break
		unreachable()
	end
	--]]

	-- [[ Lua 5.4+
	local x<close>
	local y<close>, z<const> = 1, 2
	--]]
end

-- Minify tests.
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

	-- [[ Lua 5.2+
	local _ENV = _ENV
	local function getEnv()
		return _ENV
	end
	--]]
end
--]===]

return
