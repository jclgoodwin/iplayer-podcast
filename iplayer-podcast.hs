module Main where

import System.IO (readFile, writeFile, IOMode (ReadMode), withFile, hFileSize)
import System.Directory (doesFileExist)
import Control.Monad (filterM, mapM)
import Data.List (isPrefixOf, stripPrefix)
import Data.List.Split (wordsBy)
import Data.Maybe (fromJust)
import Data.DateTime
import Text.XML.Light.Types (Content (Elem, Text), Element (..), Attr (..), QName (..), CData (..), CDataKind (CDataText, CDataVerbatim))
import Text.XML.Light.Output (ppElement)
import Network.Curl (withCurlDo, curlPost)

type Field   = String
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
hubURL        = "https://pubsubhubbub.appspot.com/"


cauterise :: String -> History
cauterise s = map toFields episodes
    where episodes = lines s
          toFields = wordsBy (== '|')

hasEpisodePathCorrectPrefix :: Episode -> Bool
hasEpisodePathCorrectPrefix e = mediaPath `isPrefixOf` (e !! 6)

doesEpisodeFileExist :: Episode -> IO Bool
doesEpisodeFileExist e = doesFileExist (e !! 6)

addLength :: Episode -> IO EpisodeWithLength
addLength e = do
    length <- withFile (e !! 6) ReadMode hFileSize
    return (e, length)

toURL :: String -> String
toURL p = mediaURL ++ fromJust (stripPrefix mediaPath p)

-- like the ppTopElement included in Text.XML.Light.Output, but with an <?xml-stylesheet?> bit
ppTopElement :: Element -> String
ppTopElement e = "<?xml version='1.0'?>\n"
                ++ "<?xml-stylesheet type='text/xsl' href='"
                ++ stylesheetURL
                ++ "'?>\n"
                ++ ppElement e

rfcFormatDateTime :: DateTime -> String
rfcFormatDateTime = formatDateTime "%a, %d %b %Y %H:%M:%S %z"

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
item (_:name:episode:_:timestamp:_:filename:_:duration:description:_:_:_:link:_, length) =
    Elem (Element
        (QName "item" Nothing Nothing)
        []
        [ simpleElement "title"           (name ++ " - " ++ episode)
        , simpleElement "pubDate"         (rfcFormatDateTime $ fromSeconds $ read timestamp)
        , Elem (Element
            (QName "description" Nothing Nothing)
            []
            [Text (CData CDataVerbatim ("<p>" ++ description ++ "</p>") Nothing)]
            Nothing
            )
        , simpleElement "link"            link
        , simpleElement "guid"            (toURL filename)
        , simpleElement "itunes:duration" duration
        , Elem (Element
            (QName "enclosure" Nothing Nothing)
            [ simpleAttr "url"    (toURL filename)
            , simpleAttr "type"   "audio/m4a"
            , simpleAttr "length" (show length)
            ]
            []
            Nothing
            )
        ]
        Nothing)

feed :: HistoryWithLengths -> DateTime -> Element
feed history time =
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
            , simpleElement "pubDate"       (rfcFormatDateTime $ fromSeconds $ read latestTimestamp)
            , simpleElement "lastBuildDate" (rfcFormatDateTime time)
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
    where latestTimestamp = maximum timestamps
          timestamps = map timestamp history
          timestamp (x, _) = x !! 4

shouldAnnounce :: DateTime -> DateTime -> Bool
shouldAnnounce now latestTimestamp = diffMinutes' now latestTimestamp < 5

maybeAnnounce :: DateTime -> DateTime -> IO ()
maybeAnnounce now latestTimestamp | shouldAnnounce now latestTimestamp = pubsubhubbub
                                  | otherwise = return ()

pubsubhubbub :: IO ()
pubsubhubbub = withCurlDo $ do
    curlPost hubURL ["hub.mode=publish", "hub.url=" ++ feedURL]
    return ()

main :: IO ()
main = do
    text <- readFile historyPath
    let episodes = reverse $ filter hasEpisodePathCorrectPrefix $ cauterise text
    episodes <- filterM doesEpisodeFileExist episodes
    episodesWithLengths <- mapM addLength episodes
    time <- getCurrentTime
    let xml = ppTopElement $ feed episodesWithLengths time
    putStrLn xml
    writeFile outputPath xml
    -- contact the pubsubhubbub hub if the latest epsiode was downloaded less than 5 minutes ago
    let latestTimestamp = fromSeconds $ read $ maximum $ map (!! 4) episodes
    maybeAnnounce time latestTimestamp
