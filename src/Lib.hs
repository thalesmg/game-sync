module Lib where

import Control.Arrow (first)
import Control.Monad (when)
import Development.Shake (FilePattern, Rules, Action, (%>), want, cmd_, need, phony, putInfo, removeFilesAfter, liftIO, getDirectoryFiles, getDirectoryFilesIO, copyFile', Stdout(..), cmd, doesFileExist, action, (|%>))
import Development.Shake.FilePath ((</>), (-<.>), (<.>), takeFileName, dropExtension)
import Data.Foldable (traverse_)
import Data.List (isSuffixOf, isInfixOf, stripPrefix, sortOn)
import Data.Ord (Down(..))
import qualified Data.Map as M
import GHC.Stack (HasCallStack)

mapFst :: Functor f => (a -> b) -> f (a, c) -> f (b, c)
mapFst f = fmap (first f)

debugR :: Show x => x -> Rules ()
debugR = action . putInfo . show

megaDriveRoms =
  M.fromList $
  -- mapFst ((<.> "zip") . ("megadrive" </>))
  [ ("Aero the Acro-Bat 2", "Aero the Acro-Bat 2 (U) [!].gen")
  , ("Aero the Acro-Bat", "Aero the Acro-Bat (U) [c][!].gen")
  , ("Golden Axe II","Golden Axe II (W) [!].gen")
  , ("Golden Axe","Golden Axe (W) (REV01) [!].gen")
  , ("Quack Shot Starring Donald Duck", "Quack Shot Starring Donald Duck (W) (REV01) [!].gen")
  , ("Streets of Rage 2", "Streets of Rage 2 (U) [!].gen")
  , ("Streets of Rage", "Streets of Rage (W) (REV01) [!].gen")
  , ("Taz-Mania", "Taz-Mania (W) [!].gen")
  , ("Toejam & Earl in Panic on Funkotron", "Toejam & Earl in Panic on Funkotron (U) [!].gen")
  ]

megaDriveRules :: FilePath -> FilePath -> Rules ()
megaDriveRules inroot outroot = do
  files <- liftIO $ getDirectoryFilesIO (inroot </> "megadrive") ["/*.7z"]
  -- debugR files
  want (map ((outroot </>) . ("megadrive" </>) . (-<.> "zip")) files)

  phony "clean" $ do
    putInfo "cleaning..."
    removeFilesAfter outroot ["//"]

  outroot </> "megadrive" </> "*.zip" %> \out -> do
    let src = inroot </> "megadrive" </> takeFileName out -<.> "7z"
    candidates <- list7ZFiles src
    let -- inner = megaDriveRoms M.! dropExtension (takeFileName out)
        Found inner = megaDriveInferInner candidates
        outdir = outroot </> "megadrive"
        outInner = outdir </> inner
    need [src]
    cmd_ "7z" "x" "-aos" ["-o" <> outdir] ["-i!" <> inner] [src]
    cmd_ "zip" "-m" [out] [outInner]

justCopyRules :: FilePath -> FilePath -> FilePattern -> FilePath -> FilePath -> Rules ()
justCopyRules slugin slugout pat inroot outroot = do
  files <- liftIO $ getDirectoryFilesIO (inroot </> slugin) ["/" <> pat]
  let outFiles = map ((outroot </>) . (slugout </>) . takeFileName) files
  want outFiles

  outroot </> slugout </> pat %> \out -> do
    let src = inroot </> slugin </> takeFileName out
    copyFile' src out

justCopyRules' :: FilePath -> FilePattern -> FilePath -> FilePath -> Rules ()
justCopyRules' slug = justCopyRules slug slug

n64Rules :: FilePath -> FilePath -> Rules ()
n64Rules = justCopyRules' "n64" "*.zip"

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

mame2003PlusRules :: FilePath -> FilePath -> Rules ()
mame2003PlusRules = justCopyRules "mame2003plus" "mame" "*.zip"

ps1Rules :: FilePath -> FilePath -> Rules ()
ps1Rules inroot outroot = do
  zippedFiles <- liftIO $ getDirectoryFilesIO (inroot </> "ps1") ["/*.zip"]
  let chds = map ((inroot </>) . ("ps1" </>) . (-<.> "chd")) zippedFiles
  want chds

  inroot </> "ps1" </> "*.chd" %> \out -> do
    let src = out -<.> "zip"
    b <- doesFileExist src
    when b $ do
      need [src]
      Stdout rawList <- cmd "unzip" "-Z" "-1" [src]
      let contents = lines rawList
          [cueFile] = [f | f <- contents, ".cue" `isSuffixOf` f]
      cmd_ "unzip" "-d" [inroot </> "ps1"] [src]
      cmd_ "chdman" "createcd" "--input" [inroot </> "ps1" </> cueFile] "--output" [out]
      removeFilesAfter (inroot </> "ps1") (takeFileName src : contents)

  chdFiles <- liftIO $ getDirectoryFilesIO (inroot </> "ps1") ["/*.chd"]
  let chdsOut = map ((outroot </>) . ("ps1" </>)) chdFiles
  want chdsOut

  let bioss = [ "scph5501.bin"
              ]
      biossTargets = map ((outroot </>) . ("ps1" </>)) bioss
  want biossTargets
  biossTargets |%> \out -> do
    let src = inroot </> "ps1" </> takeFileName out
    copyFile' src out

  justCopyRules' "ps1" "*.chd" inroot outroot

list7ZFiles :: FilePath -> Action [FilePath]
list7ZFiles file7z = do
  Stdout info <- cmd "7z" "l" "-ba" "-slt" [file7z]
  pure [f | f' <- lines info, Just f <- [stripPrefix "Path = " f']]

data Sieve a = Fail
             | Continue [a]
             | Found a
  deriving (Show)

megaDriveInferInner :: HasCallStack => [FilePath] -> Sieve FilePath
megaDriveInferInner = go
  where
    go xs = Continue xs
            &. firstFilter [filterGood]
            &. firstFilter filterLang
            &. hasLeastTags
            &. greatestRevision

    infixl 1 &.
    (&.) (Continue xs) f = f xs
    (&.) Fail _ = error "eita"
    (&.) (Found x) _ = Found x

    firstFilter [] _ = Fail
    firstFilter (f:fs) xs =
      case filter f xs of
        [] -> firstFilter fs xs
        [x] -> Found x
        xs' -> case firstFilter fs xs of
                 Fail -> Continue xs'
                 Found x -> Found x
                 Continue _ -> Continue xs'

    filterGood = isInfixOf "[!]"

    filterLang =
      [ lang "U",
        lang "W",
        lang "E",
        lang "UE",
        const True
      ]
    lang c = isInfixOf ("(" <> c <> ")")

    hasLeastTags xs = let ts = map fst
                               -- . sortOn snd
                               . filter ((==) 0 . snd)
                               . map (\x -> (x, length . filter (/= "!") $ tags x))
                               $ xs
                      in case ts of
                           [] -> Continue xs
                           [x] -> Found x
                           _ -> Continue ts

    greatestRevision xs = let [x] = take 1 . sortOn Down $ xs in Found x

tags :: String -> [String]
tags = goTags []

goTags :: [String] -> String -> [String]
goTags acc "" = acc
goTags acc (c:cs)
  | c == '[' = let (tag, _:rest) = span (/= ']') cs in goTags (tag : acc) rest
  | otherwise = goTags acc cs
