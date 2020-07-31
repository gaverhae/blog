{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
(
  Inputs (Inputs),
  Outputs (Outputs),
  Code (Code),
  execIntcode,
  (|>),
  comma_separated_ints
) where

import qualified Data.Map.Lazy as Map
import qualified Data.List.Split as Split

(|>) :: a -> (a -> b) -> b
(|>) a f = f a

comma_separated_ints :: String -> [Int]
comma_separated_ints s = s |> Split.splitOn "," |> map read

newtype Memory = Memory (Map.Map Int Int)

init_mem :: [Int] -> Memory
init_mem ls = Memory $ Map.fromList $ zip [0..] ls

get :: Memory -> Int -> Int
get (Memory m) k = case Map.lookup k m of
  Just v -> v
  Nothing -> 0

set :: Memory -> Int -> Int -> Memory
set (Memory m) k v = Memory (Map.insert k v m)


data MachineState = MachineState {
                      ip :: Int,
                      mem :: Memory,
                      future_inputs :: [Int],
                      past_outputs :: [Int],
                      rel :: Int
                    }

data ParameterMode = Immediate | Position | Relative

data Effect = Input { store_at :: (ParameterMode, Int) }
            | Output { read_from :: (ParameterMode, Int) }
            | ComputeAndStore { store_at :: (ParameterMode, Int),
                                x :: (ParameterMode, Int),
                                y :: (ParameterMode, Int),
                                f :: (Int -> Int -> Int)
                              }
            | JumpIf { p :: (Int -> Bool),
                       val :: (ParameterMode, Int),
                       to :: (ParameterMode, Int)
                     }
            | ChangeRel { diff :: (ParameterMode, Int) }
            | Exit

parse_opcode :: Int -> (Int, [ParameterMode])
parse_opcode i = 
  i
  |> show
  |> reverse
  |> (++ repeat '0')
  |> splitAt 2
  |> (\(op, flags) -> (op |> reverse |> read,
                       map (\case { '0' -> Position; '1' -> Immediate; '2' -> Relative}) flags))

read_instruction :: MachineState -> Effect
read_instruction MachineState{ip, mem} =
  case parse_opcode (get mem ip) of
    (1, f1:f2:f3:_) -> ComputeAndStore { store_at = (f3, get mem (ip + 3)),
                                         x = (f1, get mem (ip + 1)),
                                         y = (f2, get mem (ip + 2)),
                                         f = (+)
                                       }
    (2, f1:f2:f3:_) -> ComputeAndStore { store_at = (f3, get mem (ip + 3)),
                                         x = (f1, get mem (ip + 1)),
                                         y = (f2, get mem (ip + 2)),
                                         f = (*)
                                       }
    (3, flag:_) -> Input { store_at = (flag, get mem (ip + 1)) }
    (4, flag:_) -> Output { read_from = (flag, (get mem (ip + 1))) }
    (5, f1:f2:_) -> JumpIf { p = (/= 0),
                             val = (f1, get mem (ip + 1)),
                             to = (f2, get mem (ip + 2))
                           }
    (6, f1:f2:_) -> JumpIf { p = (== 0),
                             val = (f1, get mem (ip + 1)),
                             to = (f2, get mem (ip + 2))
                           }
    (7, f1:f2:f3:_) -> ComputeAndStore { store_at = (f3, get mem (ip + 3)),
                                         x = (f1, get mem (ip + 1)),
                                         y = (f2, get mem (ip + 2)),
                                         f = (\a b -> if a < b then 1 else 0)
                                       }
    (8, f1:f2:f3:_) -> ComputeAndStore { store_at = (f3, get mem (ip + 3)),
                                         x = (f1, get mem (ip + 1)),
                                         y = (f2, get mem (ip + 2)),
                                         f = (\a b -> if a == b then 1 else 0)
                                       }
    (9, f1:_) -> ChangeRel { diff = (f1, get mem (ip + 1)) }
    (99, _) -> Exit
    _ -> undefined

apply :: MachineState -> Effect -> Either [Int] MachineState
apply MachineState{past_outputs, mem, ip, future_inputs, rel} e = case e of
  Input{store_at} ->
    Right MachineState { ip = ip + 2,
                         mem = write store_at $ head future_inputs,
                         future_inputs = tail future_inputs,
                         past_outputs = past_outputs,
                         rel = rel
                       }
  Output{read_from} ->
    Right MachineState { ip = ip + 2,
                         mem = mem,
                         future_inputs = future_inputs,
                         past_outputs = (read_param read_from) : past_outputs,
                         rel = rel
                       }
  ComputeAndStore{store_at, x, y, f} ->
    Right MachineState { ip = ip + 4,
                         mem = write store_at $ f (read_param x) (read_param y),
                         future_inputs = future_inputs,
                         past_outputs = past_outputs,
                         rel = rel
                       }
  JumpIf{p, val, to} ->
    Right MachineState { ip = if p (read_param val) then read_param to else ip + 3,
                         mem = mem,
                         future_inputs = future_inputs,
                         past_outputs = past_outputs,
                         rel = rel
                       }
  ChangeRel{diff} ->
    Right MachineState { ip = ip + 2,
                         mem = mem,
                         future_inputs = future_inputs,
                         past_outputs = past_outputs,
                         rel = rel + read_param diff
                       }
  Exit -> Left past_outputs
  where read_param (p, i) = case p of
          Immediate -> i
          Position -> get mem i
          Relative -> get mem (i + rel)
        write (p, i) v = case p of
          Immediate -> undefined
          Position -> set mem i v
          Relative -> set mem (i + rel) v

step :: MachineState -> Either [Int] MachineState
step ms = apply ms eff
  where eff = read_instruction ms

newtype Code = Code [Int]
newtype Inputs = Inputs [Int]
newtype Outputs = Outputs [Int]

execIntcode :: Inputs -> Code -> Outputs
execIntcode (Inputs inputs) (Code code) = run init_state
  where init_state = MachineState { ip = 0,
                                    future_inputs = inputs,
                                    past_outputs = [],
                                    mem = init_mem code,
                                    rel = 0
                                  }
        run ms = case step ms of
          Right ms -> run ms
          Left outputs -> Outputs $ reverse outputs
