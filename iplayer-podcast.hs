module Main where

import System.IO (readFile, writeFile)
import System.Directory (doesFileExist)
import System.Posix (getFileStatus, fileSize, FileOffset)
import Control.Monad (filterM)
import Data.List (isPrefixOf, stripPrefix)
import Data.List.Split (wordsBy)
import Data.Maybe (fromJust)
import Data.DateTime
import Text.XML.Light.Types
import Text.XML.Light.Output (ppTopElement)

type Field   = String
type Episode = [Field]
type History = [Episode]


-- configuration

feedURL     = "http://static.joshuagoodw.in/bucket/podcast/"
mediaURL    = "http://static.joshuagoodw.in/bucket/"
mediaPath   = "/usr/share/nginx/html/joshuagoodw.in/bucket/"
historyPath = "/home/josh/.get_iplayer/download_history"
outputPath  = "/usr/share/nginx/html/joshuagoodw.in/bucket/podcast/i.xml"


cauterise :: String -> History
cauterise text = map toFields episodes
    where episodes = lines text
          toFields episode = wordsBy (== '|') episode

hasEpisodePathCorrectPrefix :: Episode -> Bool
hasEpisodePathCorrectPrefix e = isPrefixOf mediaPath (e !! 6)

doesEpisodeFileExist :: Episode -> IO Bool
doesEpisodeFileExist e = do
    existence <- doesFileExist (e !! 6)
    return existence

toURL :: String -> String
toURL p = mediaURL ++ fromJust (stripPrefix mediaPath p)

rfcFormatDateTime :: DateTime -> String
rfcFormatDateTime = formatDateTime "%a, %d %b %Y %H:%M:%S %z"

simpleAttr :: String -> String -> Attr
simpleAttr key value = Attr (QName key Nothing Nothing) value

simpleElement :: String -> String -> Content
simpleElement key value = 
    (Elem (Element
        (QName key Nothing Nothing)
        []
        [(Text (CData CDataText value Nothing))]
        Nothing
    ))

item :: Episode -> Content
item (_:name:episode:_:timestamp:_:filename:_:duration:description:_:_:_:link:_) =
    Elem (Element
        (QName "item" Nothing Nothing)
        []
        [ simpleElement "title"           (name ++ " - " ++ episode)
        , simpleElement "pubDate"         (rfcFormatDateTime $ fromSeconds $ read timestamp)
        , simpleElement "description"     description
        , simpleElement "link"            link
        , simpleElement "guid"            (toURL filename)
        , simpleElement "itunes:duration" duration
        , Elem
                (Element
                    (QName "enclosure" Nothing Nothing)
                    [ simpleAttr "url"    (toURL filename)
                    , simpleAttr "type"   "audio/m4a"
                    ]
                    []
                    Nothing)
        ]
        Nothing)

feed :: History -> DateTime -> Element
feed history time = (Element
    (QName "rss" Nothing Nothing)
    [ simpleAttr "version"      "2.0"
    , simpleAttr "xmlns:itunes" "http://www.itunes.com/dtds/podcast-1.0.dtd"
    , simpleAttr "xml:lang"     "en-GB"
    ]
    (
        [
        Elem
            (Element
            (QName "channel" Nothing Nothing)
            []
            (
                [ simpleElement "title"        "iPlayer Recordings"
                , simpleElement "description"  "Mostly weak drama"
                , simpleElement "link"          feedURL
                , simpleElement "pubDate"       (rfcFormatDateTime $ fromSeconds $ read latestTimestamp)
                , simpleElement "lastBuildDate" (rfcFormatDateTime time)
                ]
                ++
                (map item history)
            )
            Nothing)
        ]
    )
    Nothing)
    where latestTimestamp = maximum timestamps
          timestamps = map timestamp history
          timestamp x = x !! 4
  

main :: IO ()
main = do
    text <- readFile historyPath
    let episodes = filter hasEpisodePathCorrectPrefix $ cauterise text
    episodes <- filterM doesEpisodeFileExist episodes
    time <- getCurrentTime
    let xml = ppTopElement $ feed episodes time
    putStrLn xml
    writeFile outputPath xml
