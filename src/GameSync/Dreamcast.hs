module GameSync.Dreamcast where

import Development.Shake (Rules)
import GameSync.Utils (justCopyRules')

dreamcastRules :: FilePath -> FilePath -> Rules ()
dreamcastRules = justCopyRules' "dreamcast" "*.zip"
