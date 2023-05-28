module GameSync.N64 where

import Development.Shake (Rules)
import GameSync.Utils (justCopyRules')

n64Rules :: FilePath -> FilePath -> Rules ()
n64Rules = justCopyRules' "n64" "*.zip"
