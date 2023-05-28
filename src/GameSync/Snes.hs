module GameSync.Snes where

import Development.Shake (Rules, want, (%>), copyFile')
import Development.Shake.FilePath ((</>), takeFileName)

snesRoms :: [FilePath]
snesRoms =
  [ "Disney's Goof Troop.zip"
  , "Donkey Kong Country 2 - Diddy's Kong Quest.zip"
  , "Donkey Kong Country 3 - Dixie Kong's Double Trouble!.zip"
  , "Donkey Kong Country.zip"
  , "Great Circus Mystery Starring Mickey & Minnie, The.zip"
  , "Joe & Mac 2 - Lost in the Tropics.zip"
  , "Joe & Mac.zip"
  , "Maui Mallard in Cold Shadow.zip"
  , "Mega Man 7.zip"
  , "Mega Man X.zip"
  , "Mega Man X2.zip"
  , "Mega Man X3.zip"
  , "Super Mario All-Stars.zip"
  , "Super Mario Kart.zip"
  , "Super Mario RPG - Legend of the Seven Stars.zip"
  , "Super Mario World 2 - Yoshi's Island.zip"
  , "Super Mario World.zip"
  , "Super Metroid.zip"
  , "Tetris Attack.zip"
  ]

snesRules :: FilePath -> FilePath -> Rules ()
snesRules inroot outroot = do
  want (map ((outroot </>) . ("snes" </>)) snesRoms)

  outroot </> "snes" </> "*.zip" %> \out -> do
    let src = inroot </> "snes" </> takeFileName out
    copyFile' src out
