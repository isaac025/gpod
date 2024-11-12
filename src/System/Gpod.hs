module System.Gpod where

import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import System.Glib.GError (GError)
import System.Gpod.Types
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "itdb_parse"
    itdb_parse :: CString -> Ptr (Ptr GError) -> IO (Ptr ItdbiTunesDB)

foreign import ccall unsafe "itdb_free"
    itdb_free :: Ptr ItdbiTunesDB -> IO ()

foreign import ccall unsafe "itdb_write"
    itdb_write :: Ptr ItdbiTunesDB -> Ptr (Ptr GError) -> IO Int

foreign import ccall unsafe "itdb_shuffle_write"
    itdb_shuffle_write :: Ptr ItdbiTunesDB -> Ptr (Ptr GError) -> IO Int

foreign import ccall unsafe "itdb_cp"
    itdb_cp :: CString -> CString -> IO Int

foreign import ccall unsafe "itdb_start_sync"
    itdb_start_sync :: Ptr ItdbiTunesDB -> IO Int

foreign import ccall unsafe "itdb_stop_sync"
    itdb_stop_sync :: Ptr ItdbiTunesDB -> IO Int

itdbParse :: String -> Ptr (Ptr GError) -> Ptr ItdbiTunesDB
itdbParse mp ptrErr = unsafePerformIO $
    withCString mp $ \path -> do
        itdb_parse path ptrErr

itdbFree :: Ptr ItdbiTunesDB -> ()
itdbFree itdb = unsafePerformIO $ itdb_free itdb

itdbpWrite :: Ptr ItdbiTunesDB -> Ptr (Ptr GError) -> Int
itdbpWrite itdb gerr = unsafePerformIO $ itdb_write itdb gerr

itdbShuffleWrite :: Ptr ItdbiTunesDB -> Ptr (Ptr GError) -> Int
itdbShuffleWrite itdb gerr = unsafePerformIO $ itdb_shuffle_write itdb gerr

itdbCp :: String -> String -> Int
itdbCp src dest = unsafePerformIO $
    withCString src $ \cSrc -> do
        withCString dest $ \cDest -> do
            itdb_cp cSrc cDest

itdbStartSync :: Ptr ItdbiTunesDB -> Int
itdbStartSync ptr = unsafePerformIO $ itdb_start_sync ptr

itdbStopSync :: Ptr ItdbiTunesDB -> Int
itdbStopSync ptr = unsafePerformIO $ itdb_stop_sync ptr

parseiTunesDB :: String -> IO (Maybe (Ptr ItdbiTunesDB))
parseiTunesDB mp = do
    ptrErr <- malloc :: IO (Ptr (Ptr GError))
    poke ptrErr nullPtr
    let itdb = itdbParse mp ptrErr
    err <- peek ptrErr
    if err /= nullPtr
        then free ptrErr >> pure Nothing
        else pure $ Just itdb

freeiTunesDB :: Ptr ItdbiTunesDB -> IO ()
freeiTunesDB = pure . itdbFree

copyiTunesDB :: String -> String -> IO Bool
copyiTunesDB src dest = do
    let i = itdbCp src dest
    if i == 0 then pure False else pure True

writeToShuffle :: Ptr ItdbiTunesDB -> IO Bool
writeToShuffle ptr = do
    ptrErr <- malloc :: IO (Ptr (Ptr GError))
    poke ptrErr nullPtr
    let wrote = itdbShuffleWrite ptr ptrErr
    err <- peek ptrErr
    if (err /= nullPtr) || (wrote == 0)
        then pure False
        else pure True

iTunesStartSync :: Ptr ItdbiTunesDB -> IO Bool
iTunesStartSync ptr = do
    let synced = itdbStartSync ptr
    if synced == 0 then pure False else pure True

iTunesStopSync :: Ptr ItdbiTunesDB -> IO Bool
iTunesStopSync ptr = do
    let stopped = itdbStopSync ptr
    if stopped == 0 then pure False else pure True

getFilename :: Ptr ItdbiTunesDB -> String
getFilename ptr = unsafePerformIO ((peekCString . iTunesDB_filename) =<< peek ptr)
