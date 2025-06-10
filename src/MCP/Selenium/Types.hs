{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Selenium.Types
  ( Browser(..)
  , BrowserOptions(..)
  , LocatorStrategy(..)
  , ElementLocator(..)
  , SessionId(..)
  , ElementId(..)
  , SeleniumError(..)
  , SeleniumState(..)
  , ActionResult(..)
  , ConsoleLogLevel(..)
  , ConsoleLogEntry(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, (.=), (.:))
import Data.Text (Text)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Test.WebDriver (Element)
import qualified Test.WebDriver as WD

newtype SessionId = SessionId Text
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SessionId where
  toJSON (SessionId sid) = String sid

instance FromJSON SessionId where
  parseJSON (String s) = pure (SessionId s)
  parseJSON _ = fail "SessionId must be a string"

newtype ElementId = ElementId Text
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ElementId where
  toJSON (ElementId eid) = String eid

instance FromJSON ElementId where
  parseJSON (String s) = pure (ElementId s)
  parseJSON _ = fail "ElementId must be a string"

data Browser = Chrome | Firefox
  deriving (Eq, Show, Generic)

instance ToJSON Browser where
  toJSON Chrome = String "chrome"
  toJSON Firefox = String "firefox"

instance FromJSON Browser where
  parseJSON (String "chrome") = pure Chrome
  parseJSON (String "firefox") = pure Firefox
  parseJSON _ = fail "Browser must be 'chrome' or 'firefox'"

data BrowserOptions = BrowserOptions
  { headless :: Maybe Bool
  , arguments :: Maybe [Text]
  } deriving (Eq, Show, Generic)

instance ToJSON BrowserOptions where
  toJSON (BrowserOptions h args) = object
    [ "headless" .= h
    , "arguments" .= args
    ]

instance FromJSON BrowserOptions where
  parseJSON = parseJSON

data LocatorStrategy
  = ById
  | ByCss
  | ByXPath
  | ByName
  | ByTagName
  | ByClassName
  deriving (Eq, Show, Generic)

instance ToJSON LocatorStrategy where
  toJSON ById = String "id"
  toJSON ByCss = String "css"
  toJSON ByXPath = String "xpath"
  toJSON ByName = String "name"
  toJSON ByTagName = String "tag"
  toJSON ByClassName = String "class"

instance FromJSON LocatorStrategy where
  parseJSON (String "id") = pure ById
  parseJSON (String "css") = pure ByCss
  parseJSON (String "xpath") = pure ByXPath
  parseJSON (String "name") = pure ByName
  parseJSON (String "tag") = pure ByTagName
  parseJSON (String "class") = pure ByClassName
  parseJSON _ = fail "Invalid locator strategy"

data ElementLocator = ElementLocator
  { strategy :: LocatorStrategy
  , value :: Text
  , timeout :: Maybe Int
  } deriving (Eq, Show, Generic)

instance ToJSON ElementLocator where
  toJSON (ElementLocator s v t) = object
    [ "by" .= s
    , "value" .= v
    , "timeout" .= t
    ]

instance FromJSON ElementLocator where
  parseJSON = parseJSON

data ConsoleLogLevel
  = LogAll
  | LogSevere
  | LogWarning
  | LogInfo
  | LogDebug
  deriving (Eq, Show, Generic)

instance ToJSON ConsoleLogLevel where
  toJSON LogAll = String "ALL"
  toJSON LogSevere = String "SEVERE"
  toJSON LogWarning = String "WARNING"
  toJSON LogInfo = String "INFO"
  toJSON LogDebug = String "DEBUG"

instance FromJSON ConsoleLogLevel where
  parseJSON (String "ALL") = pure LogAll
  parseJSON (String "SEVERE") = pure LogSevere
  parseJSON (String "WARNING") = pure LogWarning
  parseJSON (String "INFO") = pure LogInfo
  parseJSON (String "DEBUG") = pure LogDebug
  parseJSON _ = fail "Invalid log level"

data ConsoleLogEntry = ConsoleLogEntry
  { logLevel :: ConsoleLogLevel
  , message :: Text
  , timestamp :: Int
  , source :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON ConsoleLogEntry where
  toJSON (ConsoleLogEntry level msg ts src) = object
    [ "level" .= level
    , "message" .= msg
    , "timestamp" .= ts
    , "source" .= src
    ]

instance FromJSON ConsoleLogEntry where
  parseJSON = parseJSON

data SeleniumError
  = SessionNotFound
  | ElementNotFound Text
  | BrowserError Text
  | InvalidLocator Text
  | NavigationError Text
  | ActionError Text
  | UnsupportedBrowser Text
  | TimeoutError Text
  deriving (Eq, Show, Generic)

instance ToJSON SeleniumError where
  toJSON SessionNotFound = String "No active browser session"
  toJSON (ElementNotFound msg) = String ("Element not found: " <> msg)
  toJSON (BrowserError msg) = String ("Browser error: " <> msg)
  toJSON (InvalidLocator msg) = String ("Invalid locator: " <> msg)
  toJSON (NavigationError msg) = String ("Navigation error: " <> msg)
  toJSON (ActionError msg) = String ("Action error: " <> msg)
  toJSON (UnsupportedBrowser msg) = String ("Unsupported browser: " <> msg)
  toJSON (TimeoutError msg) = String ("Timeout error: " <> msg)

data SeleniumState = SeleniumState
  { drivers :: Map SessionId WD.WD
  , currentSession :: Maybe SessionId
  } deriving (Generic)

data ActionResult = ActionResult
  { success :: Bool
  , sessionId :: Maybe SessionId
  , elementId :: Maybe ElementId
  , text :: Maybe Text
  , screenshot :: Maybe Text
  , error :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON ActionResult where
  toJSON (ActionResult s sid eid t ss err) = object
    [ "success" .= s
    , "sessionId" .= sid
    , "elementId" .= eid
    , "text" .= t
    , "screenshot" .= ss
    , "error" .= err
    ]
