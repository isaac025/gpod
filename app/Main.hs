module Main where

import System.Gpod
import Control.Monad (when)

main :: IO ()
main = do
    mptr <- parseiTunesDB "/media/ipod"
    case mptr of
        Nothing -> error "parsing of ipod failed"
        Just ptr -> do
            let fs = getFilename ptr
            copied <- copyiTunesDB "/home/ih1d/gpod/Satellite.mp3" fs
            when copied $ do
                wrote <- writeToShuffle ptr
                if wrote
                    then do
                        synced <- iTunesStartSync ptr
                        if synced
                            then do
                                stopped <- iTunesStopSync ptr
                                if stopped
                                    then freeiTunesDB ptr
                                    else error "failed while stopping ipod sync"
                            else error "error while syncing ipod"
                    else error "could not write to ipod"
