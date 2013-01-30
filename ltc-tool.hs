{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Version ( showVersion )
import System.Console.CmdArgs
import Paths_ltc ( version )
import Text.Printf ( printf )

data Modes = Fsck { dir :: FilePath }
           deriving ( Show, Data, Typeable )

ltcModes :: [Modes]
ltcModes =
    [ Fsck { dir = def &= typDir &= argPos 0 }
    ]
    &= program "ltc"
    &= summary (printf "ltc v%s - LTc utility" (showVersion version))

main :: IO ()
main = do
    opts <- cmdArgs $ modes ltcModes
    case opts of
        Fsck d -> do
            printf "Checking %s\n" d

