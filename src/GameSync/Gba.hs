module GameSync.Gba where

import Development.Shake (Rules)
import GameSync.Utils (justCopyRules')

gbaRules :: FilePath -> FilePath -> Rules ()
gbaRules = justCopyRules' "gba" "*.iso"
