# Dumb Lua Parser

*Dumb Lua Parser* is a library for tokenizing Lua code or creating ASTs (Abstract Syntax Trees) and converting the data back to Lua.
It can also optimize and minify code.

The library is a [single Lua file](dumbParser.lua) with no dependencies other than Lua itself.
It works with Lua 5.1, 5.2, 5.3 and 5.4. LuaJIT should work too.
MIT license.

Download the latest stable release [here](https://github.com/ReFreezed/DumbLuaParser/releases/latest)
or, if you're cloning the repository, checkout the last commit with a version tag.
Also available on [LuaRocks](https://luarocks.org/modules/refreezed/dumbluaparser).


## Basic usage

```lua
local parser = require("dumbParser")

local tokens = parser.tokenizeFile("cool.lua")
local ast    = parser.parse(tokens)

parser.simplify(ast)
parser.printTree(ast)

local lua = parser.toLua(ast, true)
print(lua)
```

Check out [examples.lua](examples.lua) for some examples.


## Documentation

See the [website](http://refreezed.com/luaparser/docs/) or the top of the [source file](dumbParser.lua).

