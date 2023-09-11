module CoreToLua where

import Core (Core (..))
import qualified Lua.AST as Lua
import Data.Text (Text)

coreToLua :: Core Text -> Lua.Program
coreToLua core = Lua.Program (coreToLuaDecls core) Nothing

coreToLuaDecls :: Core Text -> [Lua.Declaration]
coreToLuaDecls = _

