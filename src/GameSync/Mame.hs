module GameSync.Mame where

import Development.Shake (Rules)
import GameSync.Utils (justCopyRules)

mame2003PlusRules :: FilePath -> FilePath -> Rules ()
mame2003PlusRules = justCopyRules "mame2003plus" "mame" "*.zip"
