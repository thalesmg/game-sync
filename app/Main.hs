module Main (main) where

import Lib
import Development.Shake (shakeArgs, shakeOptions)

main :: IO ()
main = shakeArgs shakeOptions $ do
  let inroot = "/mnt/nixos/home/thales/Downloads/roms/"
      outroot = "/tmp/dist"
  -- megaDriveRules "/mnt/nixos/home/thales/Downloads/roms/"
  megaDriveRules inroot outroot
  n64Rules inroot outroot
  snesRules inroot outroot
  ps1Rules inroot outroot
