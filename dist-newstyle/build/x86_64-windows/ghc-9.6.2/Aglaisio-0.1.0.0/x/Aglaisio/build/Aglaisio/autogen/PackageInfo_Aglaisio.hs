{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_Aglaisio (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "Aglaisio"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A configurable app for validating directory and file structure."
copyright :: String
copyright = ""
homepage :: String
homepage = ""
