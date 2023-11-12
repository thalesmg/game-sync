module GameSync.Gamecube where

import Development.Shake (Rules)
import GameSync.Utils (justCopyRules')

gamecubeRules :: FilePath -> FilePath -> Rules ()
gamecubeRules = justCopyRules' "gamecube" ["*.iso"]
