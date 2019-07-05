module Main where

import qualified Data.Map.Strict as Map
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
  print $ items
          |> Set.powerSet
          |> Set.take 5

  print "by"
  print "hello"
