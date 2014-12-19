module Main where

import System.IO (readFile, writeFile, IOMode (ReadMode), withFile, hFileSize)
import System.Directory (doesFileExist)
import Control.Monad (filterM, mapM)
import Data.List (isPrefixOf, isSuffixOf, stripPrefix)
--import Data.Ord (compare)
import Data.List.Utils (split, replace)
import Data.Maybe (fromJust)
import Data.DateTime
import Text.XML.Light.Types (Content (Elem, Text), Element (..), Attr (..), QName (..), CData (..), CDataKind (CDataText, CDataVerbatim))
import Text.XML.Light.Output (ppElement)
import Network.HTTP (simpleHTTP, postRequestWithBody)



type Field = String
type Episode = [Field]
type History = [Episode]
type EpisodeWithLength  = (Episode, Integer)
type HistoryWithLengths = [EpisodeWithLength]

-- configuration

feedURL       = "http://static.joshuagoodw.in/bucket/podcast/"
mediaURL      = "http://static.joshuagoodw.in/bucket/"
mediaPath     = "/usr/share/nginx/html/joshuagoodw.in/bucket/"
historyPath   = "/home/josh/.get_iplayer/download_history"
outputPath    = "/usr/share/nginx/html/joshuagoodw.in/bucket/podcast/i.xml"
stylesheetURL = "i.xsl"
hubURL        = "http://pubsubhubbub.appspot.com/"


cauterise :: String -> History
cauterise s = map toFields episodes
    where episodes = lines s
          toFields = split "|"

hasEpisodePathCorrectPrefix :: Episode -> Bool
hasEpisodePathCorrectPrefix e = mediaPath `isPrefixOf` (e !! 6)

doesEpisodeFileExist :: Episode -> IO Bool
doesEpisodeFileExist e = doesFileExist (e !! 6)

addLength :: Episode -> IO EpisodeWithLength
addLength e = do
    length <- withFile (e !! 6) ReadMode hFileSize
    return (e, length)

anyAreSuffixesOf :: Eq a => [[a]] -> [a] -> Bool
anyAreSuffixesOf [n] h = isSuffixOf n h
anyAreSuffixesOf (n:ns) h | isSuffixOf n h = True
                          | otherwise = anyAreSuffixesOf ns h

isMediaFile :: FilePath -> Bool
isMediaFile ('.':_) = False
isMediaFile f = anyAreSuffixesOf ["m4a", "mp4", "m4v"] f

toMediaFileURL :: FilePath -> String
toMediaFileURL p = mediaURL ++ fromJust (stripPrefix mediaPath p)

latestTimestamp :: HistoryWithLengths -> DateTime
latestTimestamp h = fromSeconds $ read $ maximum timestamps
    where timestamps = map timestamp h
          timestamp (x, _) = x !! 4

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

item :: EpisodeWithLength -> Content
item (_:name:episode:_:timestamp:_:filename:_:duration:description:_:_:image:_:link:_, length) =
    Elem (Element
        (QName "item" Nothing Nothing)
        []
        [ simpleElement "title"   (name ++ " - " ++ episode)
        , simpleElement "pubDate" (rfcFormatDateTime $ fromSeconds $ read timestamp)
        , Elem (Element
            (QName "description" Nothing Nothing)
            []
            [Text (CData CDataVerbatim ("<p>" ++ (replace "  " "</p><p>" description) ++ "</p>") Nothing)]
            Nothing
            )
        , simpleElement "link" link
        , simpleElement "guid" mediaFileURL
        , Elem (Element
            (QName "enclosure" Nothing Nothing)
            [ simpleAttr "url"    mediaFileURL
            , simpleAttr "type"   "audio/m4a"
            , simpleAttr "length" (show length)
            ]
            []
            Nothing
            )
        , simpleElement "itunes:duration" duration
        , simpleElement "itunes:image"    (replace "_360" "_640" image)
        ]
        Nothing)
    where mediaFileURL = toMediaFileURL filename

feed :: HistoryWithLengths -> DateTime -> Element
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
            [ simpleElement "title"         "iPlayer Recordings"
            , simpleElement "description"   "Mostly weak drama"
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
                          | otherwise                  = return ()

--compareEpisodes :: Episode -> Episode -> Ordering
--compareEpisodes (Episode _ _ t1) (Episode _ _ t2) = compare t2 t1

pubsubhubbub :: IO ()
pubsubhubbub = do
    r <- simpleHTTP (postRequestWithBody hubURL "application/x-www-form-urlencoded" ("hub.mode=publishhub&url=" ++ feedURL))
    return ()

main :: IO ()
main = do
    history <- readFile historyPath
    let episodes = reverse $ filter hasEpisodePathCorrectPrefix $ cauterise history
    episodes <- filterM doesEpisodeFileExist episodes
    episodesWithLengths <- mapM addLength episodes
    currentTime <- getCurrentTime
    let xml = ppTopElement $ feed episodesWithLengths currentTime
    writeFile outputPath xml
    -- if the latest epsiode was downloaded less than 5 minutes ago,
    -- contact the pubsubhubbub hub
    maybeAnnounce currentTime $ latestTimestamp episodesWithLengths
