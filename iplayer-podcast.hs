module Main where

import System.IO (readFile, writeFile, IOMode (ReadMode), withFile, hFileSize)
import System.Directory (getDirectoryContents, getModificationTime)
import Control.Monad (filterM, mapM)
import Data.List (isSuffixOf, sortBy)
import Data.Ord (compare)
import Data.List.Split (wordsBy)
import Data.DateTime
import Text.XML.Light.Types (Content (Elem, Text), Element (..), Attr (..), QName (..), CData (..), CDataKind (CDataText, CDataVerbatim))
import Text.XML.Light.Output (ppElement)
import Network.HTTP (simpleHTTP, postRequestWithBody)

type PID = String
data Episode = Episode FilePath Integer DateTime deriving Show

-- configuration

feedURL       = "http://static.joshuagoodw.in/bucket/podcast/"
mediaURL      = "http://static.joshuagoodw.in/bucket/"
mediaPath     = "/usr/share/nginx/html/joshuagoodw.in/bucket/"
historyPath   = "/home/josh/.get_iplayer/download_history"
outputPath    = "/usr/share/nginx/html/joshuagoodw.in/bucket/podcast/i.xml"
stylesheetURL = "i.xsl"
hubURL        = "http://pubsubhubbub.appspot.com/"


isMediaFile :: FilePath -> Bool
isMediaFile ('.':_) = False
isMediaFile l = isSuffixOf ".m4a" l

-- chokes on non-get_iplayer filenames
getPID :: FilePath -> PID
getPID e = parts !! index
    where parts = wordsBy (== '_') e
          index = (length parts) - 2

toEpisode :: FilePath -> IO Episode
toEpisode name = do
    length <- withFile path ReadMode hFileSize
    time <- getModificationTime path
    return (Episode name length time)
    where path = fillPath name

fillPath :: FilePath -> FilePath
fillPath p = mediaPath ++ "/" ++ p

toURL :: FilePath -> String
toURL p = mediaURL ++ p

latestTimestamp :: [Episode] -> DateTime
latestTimestamp es = maximum $ timestamps
    where timestamps = map timestampOf es
          timestampOf (Episode _ _ t) = t

rfcFormatDateTime :: DateTime -> String
rfcFormatDateTime = formatDateTime "%a, %d %b %Y %H:%M:%S %z"

-- like the ppTopElement included in Text.XML.Light.Output,
-- but with an <?xml-stylesheet?> bit
ppTopElement :: Element -> String
ppTopElement e = "<?xml version='1.0'?>\n"
    ++ "<?xml-stylesheet type='text/xsl' href='"
    ++ stylesheetURL
    ++ "'?>\n"
    ++ ppElement e

simpleAttr :: String -> String -> Attr
simpleAttr key = Attr (QName key Nothing Nothing)

simpleElement :: String -> String -> Content
simpleElement key value =
    Elem (Element
        (QName key Nothing Nothing)
        []
        [Text (CData CDataText value Nothing)]
        Nothing
        )

item :: Episode -> Content
item (Episode name length time) =
    Elem (Element
        (QName "item" Nothing Nothing)
        []
        [ simpleElement "title"   (name)
        , simpleElement "pubDate" (rfcFormatDateTime $ time)
        , Elem (Element
            (QName "description" Nothing Nothing)
            []
            [Text (CData CDataVerbatim ("<p>" ++ description ++ "</p>") Nothing)]
            Nothing
            )
        , simpleElement "link"            url
        , simpleElement "guid"            url
        , Elem (Element
            (QName "enclosure" Nothing Nothing)
            [ simpleAttr "url"    url
            , simpleAttr "type"   "audio/m4a"
            , simpleAttr "length" (show length)
            ]
            []
            Nothing
            )
        ]
        Nothing)
    where url = toURL name
          description = ""

feed :: [Episode] -> DateTime -> Element
feed history currentTime =
    Element
    (QName "rss" Nothing Nothing)
    [ simpleAttr "version"      "2.0"
    , simpleAttr "xmlns:atom"   "http://www.w3.org/2005/Atom"
    , simpleAttr "xmlns:itunes" "http://www.itunes.com/dtds/podcast-1.0.dtd"
    , simpleAttr "xml:lang"     "en-GB"
    ]
    [ Elem (Element
        (QName "channel" Nothing Nothing)
        []
        (
            [ simpleElement "title"        "iPlayer Recordings"
            , simpleElement "description"  "Mostly weak drama"
            , simpleElement "link"          feedURL
            , simpleElement "pubDate"       (rfcFormatDateTime pubDate)
            , simpleElement "lastBuildDate" (rfcFormatDateTime currentTime)
            , Elem (Element
                (QName "link" Nothing (Just "atom"))
                [ simpleAttr "rel"  "hub"
                , simpleAttr "href" hubURL
                ]
                []
                Nothing
                )
            , Elem (Element
                (QName "link" Nothing (Just "atom"))
                [ simpleAttr "rel"  "self"
                , simpleAttr "href" feedURL
                ]
                []
                Nothing
                )
            ]
            ++
            map item history
        )
        Nothing)
    ]
    Nothing
    where pubDate = latestTimestamp history

shouldAnnounce :: DateTime -> DateTime -> Bool
shouldAnnounce now pubDate = diffMinutes' now pubDate < 5

maybeAnnounce :: DateTime -> DateTime -> IO ()
maybeAnnounce now pubDate | shouldAnnounce now pubDate = pubsubhubbub
                          | otherwise = return ()

compareEpisodes :: Episode -> Episode -> Ordering
compareEpisodes (Episode _ _ t1) (Episode _ _ t2) = compare t2 t1

pubsubhubbub :: IO ()
pubsubhubbub = do
    r <- simpleHTTP (postRequestWithBody hubURL "application/x-www-form-urlencoded" ("hub.mode=publishhub&url=" ++ feedURL))
    return ()

main :: IO ()
main = do
    files <- getDirectoryContents mediaPath
    episodes <- mapM toEpisode $ filter isMediaFile files
    currentTime <- getCurrentTime
    let xml = ppTopElement $ feed (sortBy compareEpisodes episodes) currentTime
    writeFile outputPath xml
    -- if the latest epsiode was downloaded less than 5 minutes ago,
    -- contact the pubsubhubbub hub
    maybeAnnounce currentTime $ latestTimestamp episodes
