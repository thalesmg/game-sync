module GameSync.Ps1 where

import Development.Shake (Rules, Stdout (..), liftIO, getDirectoryFilesIO, want, (%>), doesFileExist, need, cmd, cmd_, removeFilesAfter, (|%>), copyFile')
import Development.Shake.FilePath ((</>), (-<.>), takeFileName)
import Control.Monad (when)
import Data.List (isSuffixOf)
import GameSync.Utils (justCopyRules')

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
