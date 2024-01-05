module GameSync.MegaDrive where

import GameSync.Utils (list7ZFiles, justCopyRules')

import qualified Data.Map as M
import Control.Monad (when)
import Development.Shake (Rules, liftIO, getDirectoryFilesIO, want, (%>), need, cmd_, phony, doesFileExist)
import Development.Shake.FilePath ((</>), (-<.>), takeFileName)
import GHC.Stack (HasCallStack)
import Data.List (isInfixOf, sortOn)
import Data.Ord (Down(..))

megaDriveRoms :: M.Map String String
megaDriveRoms =
  M.fromList
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
  files <- liftIO $ getDirectoryFilesIO (inroot </> "megadrive-selected") ["/*.7z"]
  -- debugR files
  let preparedFiles = map ((inroot </>) . ("megadrive" </>) . (-<.> "zip")) files
  want preparedFiles

  inroot </> "megadrive" </> "*.zip" %> \out -> do
    b <- doesFileExist out
    when (not b) $ do
      let src = inroot </> "megadrive-selected" </> takeFileName out -<.> "7z"
      candidates <- list7ZFiles src
      let -- inner = megaDriveRoms M.! dropExtension (takeFileName out)
	  Found inner = megaDriveInferInner candidates
	  outdir = inroot </> "megadrive"
	  outInner = outdir </> inner
      need [src]
      cmd_ "7z" "x" "-aos" ["-o" <> outdir] ["-i!" <> inner] [src]
      cmd_ "zip" "-m" [out] [outInner]

  phony "megadrive-pre" $ do
    need preparedFiles

  justCopyRules' "megadrive" ["*.zip"] inroot outroot

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
