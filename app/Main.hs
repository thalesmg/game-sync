{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Lib
import Development.Shake (shakeArgs, shakeArgsWith, shakeOptions, withoutActions, want)
-- import Options.Applicative (Parser, execParser, strOption, long, short, metavar, help, info, fullDesc)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..))

-- data Options = MkOptions { inroot :: FilePath
--                          , outroot :: FilePath
--                          }

-- optionsParser :: Parser Options
-- optionsParser = MkOptions
--   <$> strOption
--       ( long "inroot"
--         <> short 'i'
--         <> metavar "INROOT"
--         <> help "Root directory containing system directories to read from"
--       )
--   <*> strOption
--       ( long "outroot"
--         <> short 'o'
--         <> metavar "OUTROOT"
--         <> help "Root directory containing system directories to write to"
--       )

flags :: [OptDescr (Either String FilePath)]
flags = [ Option "i" ["inroot"] (ReqArg Right "INROOT") "Root directory containing system directories to read from"
        , Option "o" ["outroot"] (ReqArg Right "OUTROOT") "Root directory containing system directories to write to"
        ]

main :: IO ()
main = do
  -- let opts = info optionsParser fullDesc
  -- MkOptions{inroot, outroot} <- execParser opts
  shakeArgsWith shakeOptions flags $ \opts targets -> do
    let [inroot, outroot] = opts
    let rules = do
          megaDriveRules inroot outroot
          n64Rules inroot outroot
          snesRules inroot outroot
          ps1Rules inroot outroot
    pure . Just $ if null targets then rules else want targets >> withoutActions rules
