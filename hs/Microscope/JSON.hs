{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Reference parser for the JSON Microscope files.
module Microscope.JSON where
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import qualified Data.Text as T
import Microscope.Types

-- | Try to get an element, fail with a useful message if we can't.
get :: FromJSON a => Object -> T.Text -> Parser a
get o s =
  o .: s `mplus` fail ("Required key not found or has wrong type: " ++ show s)

instance FromJSON World where
  parseJSON (Object o) =
    World <$> (get o "periods" `mplus` return [])
          <*> (get o "entities" `mplus` return [])
          <*> (get o "trivia" `mplus` return [])
          <*> get o "palette"
  parseJSON _ =
    mzero

instance FromJSON Palette where
  parseJSON (Object o) =
    Palette <$> (get o "yes" `mplus` return [])
            <*> (get o "no" `mplus` return [])
            <*> get o "theme"
  parseJSON _ =
    mzero

instance FromJSON Period where
  parseJSON (Object o) =
    Period <$> get o "tagline"
           <*> get o "description"
           <*> get o "mood"
           <*> (get o "events" `mplus` return [])
  parseJSON _ =
    mzero

instance FromJSON Entity where
  parseJSON (Object o) = do
    Entity <$> get o "name"
           <*> (get o "tags" `mplus` ((:[]) <$> get o "tags"))
           <*> get o "description"
           <*> (get o "subentities" `mplus` return [])
  parseJSON _ =
    mzero

instance FromJSON Event where
  parseJSON (Object o) =
    Event <$> get o "tagline"
          <*> get o "description"
          <*> get o "mood"
          <*> (get o "scenes" `mplus` return [])
  parseJSON _ =
    mzero

instance FromJSON Scene where
  parseJSON (Object o) =
    Scene <$> get o "question"
          <*> get o "answer"
          <*> get o "description"
  parseJSON _ =
    mzero

instance FromJSON RefString where
  parseJSON x = RefString <$> parseJSON x

instance FromJSON EntityTag where
  parseJSON x = EntityTag <$> parseJSON x

instance FromJSON EntityName where
  parseJSON x = EntityName <$> parseJSON x

instance FromJSON Mood where
  parseJSON x = do
    m <- parseJSON x
    case map toLower m of
      "light" -> return Light
      "dark"  -> return Dark
      _       -> mzero
