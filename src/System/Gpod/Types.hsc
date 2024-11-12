{-# LANGUAGE RecordWildCards #-}

module System.Gpod.Types where

import System.Glib.GList (GList)
import Foreign.Storable (Storable(..)) 
import Foreign.Ptr (Ptr)
import Foreign.C.Types
import Foreign.C.String (CString)

#include <gpod/itdb.h>

data ItdbDevice
data ItdbiTunesDBPrivate
data ItdbTrackPrivate
data ItdbChapterdata 

type GPointer = Ptr ()
type ItdbUserDataDuplicateFunc = GPointer
type ItdbUserDataDestroyFunc = GPointer


data ItdbiTunesDB = ItdbiTunesDB 
    { iTunesDB_tracks :: Ptr GList
    , iTunesDB_playlists :: Ptr GList
    , iTunesDB_filename :: CString
    , iTunesDB_device :: Ptr ItdbDevice
    , iTunesDB_version :: CUInt
    , iTunesDB_id :: CULong
    , iTunesDB_tzoffset :: CInt
    , iTunesDB_reserved_int2 :: CInt
    , iTunesDB_priv :: Ptr ItdbiTunesDBPrivate 
    , iTunesDB_usertype :: CULong
    , iTunesDB_userdata :: GPointer
    , iTunesDB_userdata_duplicate :: ItdbUserDataDuplicateFunc 
    , iTunesDB_userdata_destroy :: ItdbUserDataDestroyFunc 
    }

instance Storable ItdbiTunesDB where
    sizeOf _ = (#size Itdb_iTunesDB)
    alignment _ = 8
    peek ptr = do
        t <- (#peek Itdb_iTunesDB, tracks) ptr
        p <- (#peek Itdb_iTunesDB, playlists) ptr
        f <- (#peek Itdb_iTunesDB, filename) ptr
        d <- (#peek Itdb_iTunesDB, device) ptr
        v <- (#peek Itdb_iTunesDB, version) ptr
        i <- (#peek Itdb_iTunesDB, id) ptr
        z <- (#peek Itdb_iTunesDB, tzoffset) ptr
        r <- (#peek Itdb_iTunesDB, reserved_int2) ptr
        pv <- (#peek Itdb_iTunesDB, priv) ptr
        ut <- (#peek Itdb_iTunesDB, usertype) ptr
        ud <- (#peek Itdb_iTunesDB, userdata) ptr
        udd <- (#peek Itdb_iTunesDB, userdata_duplicate) ptr
        uddy <- (#peek Itdb_iTunesDB, userdata_destroy) ptr
        pure $ ItdbiTunesDB t p f d v i z r pv ut ud udd uddy
    poke ptr (ItdbiTunesDB{..}) = do
        (#poke Itdb_iTunesDB, tracks) ptr iTunesDB_tracks
        (#poke Itdb_iTunesDB, playlists) ptr iTunesDB_playlists
        (#poke Itdb_iTunesDB, filename) ptr iTunesDB_filename
        (#poke Itdb_iTunesDB, device) ptr iTunesDB_device
        (#poke Itdb_iTunesDB, version) ptr iTunesDB_version
        (#poke Itdb_iTunesDB, id) ptr iTunesDB_id
        (#poke Itdb_iTunesDB, tzoffset) ptr iTunesDB_tzoffset
        (#poke Itdb_iTunesDB, reserved_int2) ptr iTunesDB_reserved_int2
        (#poke Itdb_iTunesDB, priv) ptr iTunesDB_priv
        (#poke Itdb_iTunesDB, usertype) ptr iTunesDB_usertype
        (#poke Itdb_iTunesDB, userdata) ptr iTunesDB_userdata
        (#poke Itdb_iTunesDB, userdata_duplicate) ptr iTunesDB_userdata_duplicate
        (#poke Itdb_iTunesDB, userdata_destroy) ptr iTunesDB_userdata_destroy


data ItdbTrack = ItdbTrack
    { track_itdb :: Ptr ItdbiTunesDB
    , track_title :: CString
    , track_ipod_path :: CString
    , track_album :: CString
    , track_artist :: CString
    , track_genre :: CString
    , track_filetype :: CString
    , track_comment :: CString
    , track_category :: CString   
    , track_composer :: CString
    , track_grouping :: CString
    , track_description :: CString
    , track_podcasturl :: CString
    , track_podcastrss :: CString
    , track_chapterdata :: Ptr ItdbChapterdata 
    , track_subtitle :: CString
    , track_tvshow :: CString
    , track_tvepisode :: CString
    , track_tvnetwork :: CString
    , track_albumartist :: CString
    , track_keywords :: CString
    , track_sort_artist :: CString
    , track_sort_title :: CString
    , track_sort_album :: CString
    , track_sort_albumartist :: CString
    , track_sort_composer :: CString
    , track_sort_tvshow :: CString
    , track_track_id :: CUInt 
    , track_size :: CUInt  
    , track_tracklen :: CInt  
    , track_cd_nr :: CInt  
    , track_cds :: CInt  
    , track_track_nr :: CInt  
    , track_tracks :: CInt  
    , track_bitrate :: CInt  
    , track_samplerate :: CUShort 
    , track_samplerate_low :: CUShort 
    , track_year :: CInt  
    , track_volume :: CInt  
    , track_soundcheck :: CUInt 
    , track_time_added :: CTime  
    , track_time_modified :: CTime  
    , track_time_played :: CTime  
    , track_bookmark_time :: CUInt 
    , track_rating :: CUInt 
    , track_playcount :: CUInt 
    , track_playcount2 :: CUInt 
    , track_recent_playcount :: CUInt 
    , track_transferred :: CInt 
    , track_track_BPM :: CShort  
    , track_app_rating :: CUChar  
    , track_type1 :: CUChar  
    , track_type2 :: CUChar  
    , track_compilation :: CUChar  
    , track_starttime :: CUInt 
    , track_stoptime :: CUInt 
    , track_checked :: CUChar  
    , track_dbid :: CULong 
    , track_drm_userid :: CUInt 
    , track_visible :: CUInt 
    , track_filetype_marker :: CUInt 
    , track_artwork_count :: CUShort 
    , track_artwork_size :: CUInt 
    , track_samplerate2 :: CFloat 
    , track_unk126 :: CUShort 
    , track_unk132 :: CUInt 
    , track_time_released :: CTime  
    , track_unk144 :: CUShort 
    , track_explicit_flag :: CUShort 
    , track_unk148 :: CUInt 
    , track_unk152 :: CUInt 
    , track_skipcount :: CUInt 
    , track_recent_skipcount :: CUInt 
    , track_last_skipped :: CUInt 
    , track_has_artwork :: CUChar 
    , track_skip_when_shuffling :: CUChar 
    , track_remember_playback_position :: CUChar 
    , track_flag4 :: CUChar 
    , track_dbid2 :: CULong 
    , track_lyrics_flag :: CUChar 
    , track_movie_flag :: CUChar 
    , track_mark_unplayed :: CUChar 
    , track_unk179 :: CUChar 
    , track_unk180 :: CUInt 
    , track_pregap :: CUInt 
    , track_samplecount :: CULong 
    , track_unk196 :: CUInt 
    , track_postgap :: CUInt 
    , track_unk204 :: CUInt 
    , track_mediatype :: CUInt 
    , track_season_nr :: CUInt 
    , track_episode_nr :: CUInt 
    , track_unk220 :: CUInt 
    , track_unk224 :: CUInt 
    , track_unk228 :: CUInt
    , track_unk232 :: CUInt
    , track_unk236 :: CUInt
    , track_unk240 :: CUInt
    , track_unk244 :: CUInt 
    , track_gapless_data :: CUInt 
    , track_unk252 :: CUInt 
    , track_gapless_track_flag :: CUShort 
    , track_gapless_album_flag :: CUShort 
    , track_obsolete :: CUShort 
    , track_reserved_int1 :: CInt 
    , track_reserved_int2 :: CInt 
    , track_reserved_int3 :: CInt 
    , track_reserved_int4 :: CInt 
    , track_reserved_int5 :: CInt 
    , track_reserved_int6 :: CInt 
    , track_priv :: Ptr ItdbTrackPrivate 
    , track_reserved2 :: GPointer 
    , track_reserved3 :: GPointer 
    , track_reserved4 :: GPointer 
    , track_reserved5 :: GPointer 
    , track_reserved6 :: GPointer 
    , track_usertype :: CULong
    , track_userdata :: GPointer
    , track_userdata_duplicate :: ItdbUserDataDuplicateFunc
    , track_userdata_destroy :: ItdbUserDataDestroyFunc
    }

