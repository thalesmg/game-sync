module GameSync where

import GameSync.Dreamcast (dreamcastRules)
import GameSync.Gamecube (gamecubeRules)
import GameSync.Gba (gbaRules)
import GameSync.MegaDrive (megaDriveRules)
import GameSync.N64 (n64Rules)
import GameSync.Snes (snesRules)
import GameSync.Psx (psxRules)
import GameSync.Mame (mame2003PlusRulesOPi5, mame2003PlusRulesRPi4)
import Development.Shake (Rules, phony, putInfo, removeFilesAfter)

allRulesOPi5 :: FilePath -> FilePath -> Rules ()
allRulesOPi5 inroot outroot = do
  -- phony "clean" $ do
  --   putInfo "cleaning..."
  --   removeFilesAfter outroot ["//"]

  megaDriveRules inroot outroot
  snesRules inroot outroot
  n64Rules inroot outroot
  psxRules inroot outroot
  mame2003PlusRulesOPi5 inroot outroot
  gamecubeRules inroot outroot
  gbaRules inroot outroot
  dreamcastRules inroot outroot

allRulesRPi4 :: FilePath -> FilePath -> Rules ()
allRulesRPi4 inroot outroot = do
  megaDriveRules inroot outroot
  snesRules inroot outroot
  n64Rules inroot outroot
  psxRules inroot outroot
  mame2003PlusRulesRPi4 inroot outroot
  gbaRules inroot outroot
