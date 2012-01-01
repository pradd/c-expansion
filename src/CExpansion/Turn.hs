module CExpansion.Turn ( turn ) where

import Config
import CExpansion.Galaxy

turn (Galaxy ss) = Galaxy (id ss)
