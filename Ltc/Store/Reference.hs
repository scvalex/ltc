{-# LANGUAGE TypeFamilies #-}

module Ltc.Store.Reference (
        Reference
    ) where

import Control.Monad ( when )
import Ltc.Store.Class ( Store(..) )
import System.Directory ( createDirectory, doesDirectoryExist )
import System.FilePath ( (</>) )

formatString :: String
formatString = "reference"

version :: Int
version = 1

data Reference = Reference
    { getLocation :: FilePath
    }

instance Store Reference where
    data ConnectParameters Reference = ConnectParameters
        { location :: FilePath
        }

    open params = doOpen params

    close handle = doClose handle

    get _handle _key _version = undefined
    getLatest _handle _key = undefined

    set _handle _key _value = undefined

    del _handle _key = undefined

doOpen :: ConnectParameters Reference -> IO Reference
doOpen params = do
    storeExists <- doesDirectoryExist (location params)
    when (not storeExists) (initStore (location params))
    return (Reference { getLocation = location params })

doClose :: Reference -> IO ()
doClose _handle = return ()

initStore :: FilePath -> IO ()
initStore loc = do
    createDirectory loc
    writeFile (loc </> "format") formatString
    writeFile (loc </> "version") (show version)
    createDirectory (loc </> "store")
