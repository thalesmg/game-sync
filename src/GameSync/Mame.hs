module GameSync.Mame where

import Development.Shake (Rules)
import GameSync.Utils (justCopyRules)

mame2003PlusRulesOPi5 :: FilePath -> FilePath -> Rules ()
mame2003PlusRulesOPi5 = justCopyRules "mame2003plus" "mame" "*.zip"

mame2003PlusRulesRPi4 :: FilePath -> FilePath -> Rules ()
mame2003PlusRulesRPi4 = justCopyRules "mame2003plus" "mame" "*.zip"
