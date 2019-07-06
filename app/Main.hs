module Main where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

data Character = Soldier
               | Barbarian
               | Knight
               | Ranger
               | Thief
               | Monk
               | Musketeer
               | Wanderer
               | Mage
               | Cleric
               | Druid
               | Sorcerer
               deriving (Show, Ord, Eq, Enum)

data Item = Sword
          | Axe
          | Knife
          | Mace
          | Spear
          | Bow
          | Staff
          | HeavyArmor
          | MediumArmor
          | LightArmor
          | HeavyHelmet
          | MediumHelmet
          | LightHelmet
          | HeavyBracers
          | LightBracers
          | HeavyShoes
          | LightShoes
          | Herb
          | Potion
          | Scroll
          | Shield
          | Ring
          | Amulet
          deriving (Show, Ord, Eq, Enum)

data Furniture = Table
               | Mannequin
               | VerticalRack
               | Shelves
               deriving (Show, Ord, Eq, Enum)

items :: Set.Set Item
items = Set.fromList [Sword .. Amulet]

characters :: Set.Set Character
characters = Set.fromList [Soldier .. Sorcerer]

furnitures :: Set.Set Furniture
furnitures = Set.fromList [Table .. Shelves]

canUse :: Map.Map Character (Set.Set Item)
canUse = Map.fromList [(Soldier, Set.fromList [Sword, Mace, Knife, HeavyArmor, HeavyBracers, HeavyShoes, Potion, Shield]),
                       (Barbarian, Set.fromList [Sword, Axe, Spear, Mace, HeavyHelmet, HeavyBracers, HeavyShoes, Potion, Herb]),
                       (Knight, Set.fromList [Spear, Axe, HeavyArmor, HeavyBracers, HeavyShoes, Shield]),
                       (Ranger, Set.fromList [Bow, HeavyArmor, HeavyBracers, HeavyHelmet, Potion, Ring]),
                       (Thief, Set.fromList [Knife, Bow, MediumArmor, MediumHelmet, LightBracers, Amulet, Ring]),
                       (Monk, Set.fromList [LightArmor, Amulet, Ring]),
                       (Musketeer, Set.fromList [Sword, MediumArmor, LightBracers, HeavyShoes, Potion, Amulet]),
                       (Wanderer, Set.fromList [Knife, Axe, MediumArmor, MediumHelmet, LightShoes, Herb, Amulet]),
                       (Mage, Set.fromList [Staff, LightArmor, LightHelmet, LightShoes, Scroll, Ring]),
                       (Cleric, Set.fromList [Mace, Spear, LightArmor, LightHelmet, LightShoes, Scroll, Shield]),
                       (Druid, Set.fromList [Staff, Bow, LightArmor, Herb, Amulet, Scroll]),
                       (Sorcerer, Set.fromList [Staff, Knife, LightArmor, LightBracers, LightShoes, Scroll])]

canStore :: Map.Map Furniture (Set.Set Item)
canStore = Map.fromList [(Table, Set.fromList [Knife, Bow, Shield]),
                         (Mannequin, Set.fromList [HeavyArmor, MediumArmor, LightArmor, HeavyHelmet, MediumHelmet, LightHelmet, HeavyBracers, LightBracers, HeavyShoes, LightShoes]),
                         (VerticalRack, Set.fromList [Sword, Axe, Mace, Spear, Staff]),
                         (Shelves, Set.fromList [Herb, Potion, Scroll, Ring, Amulet])]

(|>) :: a -> (a -> c) -> c
(|>) a b = b a

main :: IO ()
main = do
  let usedBy :: Item -> Set.Set Character
        = let m = items
                  |> Set.toList
                  |> map (\item -> (item, canUse
                                          |> Map.toList
                                          |> filter (\(_c, is) -> Set.member item is)
                                          |> map fst
                                          |> Set.fromList))
                  |> Map.fromList
          in \i -> Maybe.fromMaybe (Set.fromList []) (Map.lookup i m)
  let reqfur :: Set.Set Item -> Set.Set Furniture
        = let m = items
                  |> Set.toList
                  |> map (\item -> (item, canStore
                                          |> Map.toList
                                          |> filter (\(_c, is) -> Set.member item is)
                                          |> map fst
                                          |> Set.fromList))
                  |> Map.fromList
          in \is -> is
                    |> Set.map (\i -> Maybe.fromMaybe (Set.fromList []) (Map.lookup i m))
                    |> Set.unions
  let optimalMannequin :: (Set.Set Item -> Bool)
      optimalMannequin is
        = not (Set.member Mannequin $ reqfur is)
        || let check ls = 1 == Set.size (Set.intersection is $ Set.fromList ls)
           in (check [HeavyArmor, MediumArmor, LightArmor]
            && check [HeavyBracers, LightBracers, HeavyShoes, LightShoes]
            && check [HeavyHelmet, MediumHelmet, LightHelmet])
  let canEquip :: (Set.Set Item -> Set.Set Character)
      canEquip is
        = canUse
          |> Map.toList
          |> filter (\(_c, cis) -> cis `Set.isSubsetOf` is)
          |> map fst
          |> Set.fromList

  let itemSets :: [Set.Set Item]
        = items
          |> Set.powerSet
          |> Set.toList
          |> filter (\is -> characters == (is |> Set.map usedBy |> Set.unions))
          |> filter (\is -> 1 <= Set.size (canEquip is))
          |> List.sortOn (\is -> (Set.size is, Set.size (reqfur is)))
          |> filter optimalMannequin

  putStrLn $ itemSets
             |> take 10
             |> map (\is -> (Set.toList is, Set.toList $ reqfur is, Set.toList $ canEquip is))
             |> map show
             |> unlines
