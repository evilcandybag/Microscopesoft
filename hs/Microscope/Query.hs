-- | Perform queries over a Microscope world.
module Microscope.Query where
import Control.Monad
import Microscope.Types

-- | A list of all entities with the given tag.
tagged :: World -> EntityTag -> [Entity]
tagged w t = 
    go (worldEntities w)
  where
    go (e:es)
      | t `elem` entityTags e =
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
    named' e | entityName e == n = return e
             | otherwise         = msum [named' e | e' <- entitySubentities e]
