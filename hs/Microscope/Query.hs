-- | Perform queries over a Microscope world.
module Microscope.Query where
import Control.Monad
import Data.Char (toLower)
import Microscope.Types

-- | A list of all entities with the given tag.
tagged :: World -> EntityTag -> [Entity]
tagged w t =
    go (worldEntities w)
  where
    t' = lowerTag t
    go (e:es)
      | t' `elem` map lowerTag (entityTags e) =
        e : go es ++ allSubentities e
      | otherwise = 
        go es ++ go (entitySubentities e)
    go _ = []
    
    allSubentities e =
      entitySubentities e ++ concatMap allSubentities (entitySubentities e)

-- | The entity with the given name, if one exists.
named :: World -> EntityName -> Maybe Entity
named w n =
    msum [named' e | e <- worldEntities w]
  where
    n' = lowerName n
    named' e
      | lowerName (entityName e) == n' =
        return e
      | otherwise =
        msum [named' e' | e' <- entitySubentities e]

-- | Make an entity name lowercase.
lowerName :: EntityName -> EntityName
lowerName (EntityName n) = EntityName $ map toLower n

-- | Make an entity tag lowercase.
lowerTag :: EntityTag -> EntityTag
lowerTag (EntityTag n) = EntityTag $ map toLower n
