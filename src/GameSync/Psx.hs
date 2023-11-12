module GameSync.Psx where

import Development.Shake (Rules, Stdout (..), liftIO, getDirectoryFilesIO, want, (%>), doesFileExist, need, cmd, cmd_, removeFilesAfter, (|%>), copyFile')
import Development.Shake.FilePath ((</>), (-<.>), takeFileName)
import Control.Monad (when)
import Data.List (isSuffixOf)
import GameSync.Utils (justCopyRules')

psxRules :: FilePath -> FilePath -> Rules ()
psxRules inroot outroot = do
  zippedFiles <- liftIO $ getDirectoryFilesIO (inroot </> "psx") ["/*.zip"]
  let chds = map ((inroot </>) . ("psx" </>) . (-<.> "chd")) zippedFiles
  want chds

  inroot </> "psx" </> "*.chd" %> \out -> do
    let src = out -<.> "zip"
    b <- doesFileExist src
    when b $ do
      need [src]
      Stdout rawList <- cmd "unzip" "-Z" "-1" [src]
      let contents = lines rawList
          [cueFile] = [f | f <- contents, ".cue" `isSuffixOf` f]
      cmd_ "unzip" "-d" [inroot </> "psx"] [src]
      cmd_ "chdman" "createcd" "--input" [inroot </> "psx" </> cueFile] "--output" [out]
      removeFilesAfter (inroot </> "psx") (takeFileName src : contents)

  chdFiles <- liftIO $ getDirectoryFilesIO (inroot </> "psx") ["/*.chd"]
  let chdsOut = map ((outroot </>) . ("psx" </>)) chdFiles
  want chdsOut

  let bioss = [ "scph5501.bin"
              , "scph5502.bin"
              ]
      biossTargets = map ((outroot </>) . ("psx" </>)) bioss
  want biossTargets
  biossTargets |%> \out -> do
    let src = inroot </> "psx" </> takeFileName out
    copyFile' src out

  justCopyRules' "psx" ["*.chd", "*.pbp"] inroot outroot
