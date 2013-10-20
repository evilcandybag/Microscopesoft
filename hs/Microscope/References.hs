-- | Working with references in Microscope files.
module Microscope.References where
import Microscope.Types

-- | Any object that may contain references to things.
class HasRefs a where
  -- | Find all references in a given object.
  refsIn :: a -> [EntityName]

instance HasRefs RefString where
  refsIn (RefString s) =
      refs s
    where
      refs s =
        case span (/= ']') . drop 1 $ dropWhile (/= '[') s of
          ([], _)     -> []
          (ref, rest) -> EntityName ref : refs (drop 1 rest)

instance HasRefs a => HasRefs [a] where
  refsIn = concatMap refsIn

instance HasRefs World where
  refsIn w = concat [refsIn $ worldPeriods w,
                     refsIn $ worldEntities w,
                     refsIn $ worldTrivia w]

instance HasRefs Period where
  refsIn p = concat [refsIn $ periodTagline p,
                     refsIn $ periodDescription p,
                     refsIn $ periodEvents p]

instance HasRefs Entity where
  refsIn e = concat [refsIn $ entityDescription e,
                     refsIn $ entitySubentities e]

instance HasRefs Event where
  refsIn e = concat [refsIn $ eventTagline e,
                     refsIn $ eventDescription e,
                     refsIn $ eventScenes e]

instance HasRefs Scene where
  refsIn s = concatMap refsIn [sceneQuestion s,
                               sceneAnswer s,
                               sceneDescription s]

instance HasRefs SomeNarrative where
  refsIn (PeriodNarrative p) = refsIn p
  refsIn (EventNarrative e) = refsIn e
  refsIn (SceneNarrative s) = refsIn s
  refsIn (EntityNarrative e) = refsIn e
