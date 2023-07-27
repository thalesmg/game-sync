{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import GameSync (allRulesOPi5, allRulesRPi4)
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

data System = OPi5 | RPi4
data Opt = MkPath FilePath | MkSystem System

flags :: [OptDescr (Either String Opt)]
flags = [ Option "i" ["inroot"] (ReqArg (Right . MkPath) "INROOT") "Root directory containing system directories to read from"
        , Option "o" ["outroot"] (ReqArg (Right . MkPath) "OUTROOT") "Root directory containing system directories to write to"
        , Option "s" ["system"] (ReqArg (\case
                                            "opi5" -> Right . MkSystem $ OPi5
                                            "rpi4" -> Right . MkSystem $ RPi4
                                            _ -> Left "invalid system")
                                "SYSTEM") "Target system"
        ]

main :: IO ()
main = do
  -- let opts = info optionsParser fullDesc
  -- MkOptions{inroot, outroot} <- execParser opts
  shakeArgsWith shakeOptions flags $ \opts targets -> do
    let [MkPath inroot, MkPath outroot, MkSystem system] = opts
    let rules = case system of
                  OPi5 -> allRulesOPi5 inroot outroot
                  RPi4 -> allRulesRPi4 inroot outroot
    pure . Just $ if null targets then rules else want targets >> withoutActions rules
