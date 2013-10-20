module Microscope.Types where
import Data.String

instance IsString RefString where
  fromString = RefString

-- | A string with references to entities.
newtype RefString = RefString String
  deriving (Ord, Eq)

-- | A Microscope world.
data World = World {
    worldPeriods  :: [Period],
    worldEntities :: [Entity],
    worldTrivia   :: [Trivia],
    worldPalette  :: Palette
  }

-- | A palette - includes, excludes and a theme description.
data Palette = Palette {
    paletteYes   :: [String],
    paletteNo    :: [String],
    paletteTheme :: String
  }

-- | A time period, or an era.
data Period = Period {
    periodTagline     :: RefString,
    periodDescription :: RefString,
    periodMood        :: Mood,
    periodEvents      :: [Event]
  }

-- | An entity - something that may be referred to using a name and that
--   presumably plays some kind of role in the narrative.
data Entity = Entity {
    entityName        :: EntityName,
    entityTags        :: [EntityTag],
    entityDescription :: RefString,
    entitySubentities :: [Entity]
  }

-- | An event - a happening, consisting of smaller scenes.
data Event = Event {
    eventTagline     :: RefString,
    eventDescription :: RefString,
    eventMood        :: Mood,
    eventScenes      :: [Scene]
  }

-- | A scene - the smallest unit of narration.
data Scene = Scene {
    sceneQuestion    :: RefString,
    sceneAnswer      :: RefString,
    sceneDescription :: RefString
  }

-- | Trivia; a random fact of the world.
type Trivia = RefString

-- | Something that may be used to tag entitites.
newtype EntityTag = EntityTag String
  deriving (Eq, Ord)

instance IsString EntityTag where
  fromString = EntityTag

instance Show EntityTag where
  show (EntityTag n) = n

newtype EntityName = EntityName String
  deriving (Eq, Ord)

instance IsString EntityName where
  fromString = EntityName

instance Show EntityName where
  show (EntityName n) = n

-- | Mood for a scene or period.
data Mood = Light | Dark deriving (Show, Eq)

-- | Any type that contains a narrative.
data SomeNarrative
  = PeriodNarrative Period
  | EventNarrative  Event
  | SceneNarrative  Scene
  | EntityNarrative Entity
