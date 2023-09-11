module CoreToLua where

import Core (Core (..))
import Data.Text (Text)
import Lua.AST qualified as Lua

coreToLua :: Core Text -> Lua.Program
coreToLua core = Lua.Program (coreToLuaDecls core) Nothing

coreToLuaDecls :: Core Text -> [Lua.Declaration]
coreToLuaDecls = _
