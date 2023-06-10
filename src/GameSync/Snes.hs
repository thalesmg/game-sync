module GameSync.Snes where

import Development.Shake (Rules)
import GameSync.Utils (justCopyRules')

snesRules :: FilePath -> FilePath -> Rules ()
snesRules = justCopyRules' "snes" "*.zip"
