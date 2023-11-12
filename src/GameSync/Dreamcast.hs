module GameSync.Dreamcast where

import Development.Shake (Rules, Stdout (..), liftIO, getDirectoryFilesIO, want, (%>), doesFileExist, need, cmd, cmd_, removeFilesAfter, phony, withTempDirWithin)
import Development.Shake.FilePath ((</>), (-<.>), takeFileName)
import Control.Monad (when)
import Data.List (isSuffixOf)
import GameSync.Utils (justCopyRules'')

systemSlug :: FilePath
systemSlug = "dreamcast"

dreamcastRules :: FilePath -> FilePath -> Rules ()
dreamcastRules inroot outroot = do
  zippedFiles <- liftIO $ getDirectoryFilesIO (inroot </> systemSlug) ["/*.zip"]
  let chds = map ((inroot </>) . (systemSlug </>) . (-<.> "chd")) zippedFiles
  want chds

  inroot </> systemSlug </> "*.chd" %> \out -> do
    let src = out -<.> "zip"
    b <- doesFileExist src
    when b $ do
      need [src]
      Stdout rawList <- cmd "unzip" "-Z" "-1" [src]
      let contents = lines rawList
          [gdiFile] = [f | f <- contents, ".gdi" `isSuffixOf` f]
      withTempDirWithin (inroot </> systemSlug) $ \tmpdir -> do
        cmd_ "unzip" "-d" [tmpdir] [src]
        cmd_ "chdman" "createcd" "--input" [tmpdir </> gdiFile] "--output" [out]
      removeFilesAfter (inroot </> systemSlug) [takeFileName src]

  phony "dreamcast-pre" $ do
    need chds

  chdFiles <- liftIO $ getDirectoryFilesIO (inroot </> systemSlug) ["/*.chd"]
  let chdsOut = map ((outroot </>) . (systemSlug </>)) chdFiles
  want chdsOut

  justCopyRules'' "dreamcast" (need chds) "*.chd" inroot outroot
