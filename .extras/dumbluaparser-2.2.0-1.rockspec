rockspec_format = "3.0"

package = "DumbLuaParser"
version = "2.2.0-1"
source  = {url="git-ssh://git@github.com:ReFreezed/DumbLuaParser.git", branch="master", tag="2.2.0"}

description = {
	summary  = "Lua parsing library capable of optimizing and minifying code.",
	detailed = [[
		Dumb Lua Parser is a library for tokenizing Lua code or creating ASTs
		(Abstract Syntax Trees) and converting the data back to Lua. It can
		also optimize and minify code.
	]],

	license    = "MIT",
	homepage   = "http://luaparser.refreezed.com/",
	issues_url = "https://github.com/ReFreezed/DumbLuaParser/issues",

	labels = {
		"ast",
		"minification",
		"optimization",
		"parsing",
		"serialization",
	},
}
dependencies = {
	"lua >= 5.1, < 5.5",
}
build = {
	type    = "builtin",
	modules = {
		dumbParser = "dumbParser.lua",
	},
}
