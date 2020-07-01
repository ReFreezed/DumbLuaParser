local a
local b, c = true, "f\0o"

local x = 1
local y = 1.
local z = .1

local y = (a:b[[]])
local s = "\0\1\2\3\4\5\6\7\8\9\10\11\12\13\\\01499\15\16\17\18\19\"\20\21\22\23¤\24\25\26\27\28\29\30\31~"

do foo() ; o:m() end

while false or true do  foo()  end
repeat foo() until x - -7 == 8

for i = 1, 9 do  foo(i)  end
for i, v in ipairs(t) do  bar(i, v)  end

local function foo() end
function a:b(x) return end
a[x] = function() return x end

local a = 1 + 2 + 3
local b = "1".."2".."3"
local c = 1 + 2^3^4 * 5

local s = a   .. b
local s = 1.  .. x
local s = 1.1 .. x

local x = a and b and c or q and w or 5

;(f1 or f2).k, (f1 or f2).k = x, y

if x or y then foo() end
