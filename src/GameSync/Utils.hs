module GameSync.Utils where

import Control.Arrow (first)
import Development.Shake (Action, Stdout(..), cmd, FilePattern, Rules, liftIO, getDirectoryFilesIO, want, (%>), copyFile', action, putInfo)
import Development.Shake.FilePath ((</>), takeFileName)
import Data.List (stripPrefix)

list7ZFiles :: FilePath -> Action [FilePath]
list7ZFiles file7z = do
  Stdout info <- cmd "7z" "l" "-ba" "-slt" [file7z]
  pure [f | f' <- lines info, Just f <- [stripPrefix "Path = " f']]

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

mapFst :: Functor f => (a -> b) -> f (a, c) -> f (b, c)
mapFst f = fmap (first f)

debugR :: Show x => x -> Rules ()
debugR = action . putInfo . show
