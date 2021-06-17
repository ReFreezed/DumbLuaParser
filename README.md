# Dumb Lua Parser

*Dumb Lua Parser* is a library for tokenizing Lua code or creating ASTs (Abstract Syntax Trees) and converting the data back to Lua.

The library is a [single Lua file](dumbParser.lua) with no dependencies other than Lua itself.
It works with Lua 5.1, 5.2, 5.3 and 5.4. Should work with LuaJIT too.
MIT license.

Download the latest stable release [here](https://github.com/ReFreezed/DumbLuaParser/releases/latest)
or, if you're cloning the repository, checkout the last commit with a version tag.


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


## Documentation

See the [source file](dumbParser.lua) for the full documentation.

