module GameSync where

import GameSync.MegaDrive (megaDriveRules)
import GameSync.N64 (n64Rules)
import GameSync.Snes (snesRules)
import GameSync.Ps1 (ps1Rules)
import GameSync.Mame (mame2003PlusRules)
import Development.Shake (Rules, phony, putInfo, removeFilesAfter)

allRules :: FilePath -> FilePath -> Rules ()
allRules inroot outroot = do
  phony "clean" $ do
    putInfo "cleaning..."
    removeFilesAfter outroot ["//"]

  megaDriveRules inroot outroot
  snesRules inroot outroot
  n64Rules inroot outroot
  ps1Rules inroot outroot
  mame2003PlusRules inroot outroot
