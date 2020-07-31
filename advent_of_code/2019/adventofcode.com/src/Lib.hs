{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
(
  execIntcode
, ExitStatus(Success, Fail)
, (|>)
, comma_separated_ints
) where

import qualified Data.Array as Array
import qualified Data.List.Split as Split

(|>) :: a -> (a -> b) -> b
(|>) a f = f a

comma_separated_ints :: String -> [Int]
comma_separated_ints s = s |> Split.splitOn "," |> map read

data Memory = Memory (Array.Array Int Int)

data MachineState = MachineState {
                      ip :: Int,
                      mem :: Memory,
                      future_inputs :: [Int],
                      past_outputs :: [Int],
                      last_effect :: Maybe Effect
                    }

data ExitStatus = Success Int | Fail [Int]

data ParameterMode = Immediate | Position

data Effect = Input { store_at :: Int }
            | Output { read_from :: (ParameterMode, Int) }
            | ComputeAndStore { store_at :: Int,
                                x :: (ParameterMode, Int),
                                y :: (ParameterMode, Int),
                                f :: (Int -> Int -> Int)
                              }
            | NormalExit

parse_opcode :: Int -> (Int, [ParameterMode])
parse_opcode i = 
  i
  |> show
  |> reverse
  |> (++ repeat '0')
  |> take 5
  |> splitAt 2
  |> (\(op, flags) -> (op |> reverse |> read,
                       map (\case { '0' -> Position; '1' -> Immediate }) flags))

init_mem :: [Int] -> Memory
init_mem ls = Memory $ Array.listArray (0, length ls) ls

get :: Memory -> Int -> Int
get (Memory arr) i = arr Array.! i

set :: Memory -> Int -> Int -> Memory
set (Memory arr) idx val = Memory (arr Array.// [(idx, val)])

read_instruction :: MachineState -> Effect
read_instruction MachineState{ip, mem} =
  case parse_opcode (get mem ip) of
    (1, f1:f2:_) -> ComputeAndStore { store_at = (get mem (ip + 3)),
                                      x = (f1, get mem (ip + 1)),
                                      y = (f2, get mem (ip + 2)),
                                      f = (+)
                                    }
    (2, f1:f2:_) -> ComputeAndStore { store_at = (get mem (ip + 3)),
                                      x = (f1, get mem (ip + 1)),
                                      y = (f2, get mem (ip + 2)),
                                      f = (*)
                                    }
    (3, flags) -> Input { store_at = (get mem (ip + 1)) }
    (4, flag:_) -> Output { read_from = (flag, (get mem (ip + 1))) }
    (99, _) -> NormalExit
    _ -> undefined

apply :: MachineState -> Effect -> Either ExitStatus MachineState
apply MachineState{last_effect, past_outputs, mem, ip, future_inputs} e = case e of
  Input{store_at} ->
    Right MachineState { ip = ip + 2,
                         mem = set mem store_at $ head future_inputs,
                         future_inputs = tail future_inputs,
                         past_outputs = past_outputs,
                         last_effect = Just e
                       }
  Output{read_from} ->
    Right MachineState { ip = ip + 2,
                         mem = mem,
                         future_inputs = future_inputs,
                         past_outputs = (read_param read_from) : past_outputs,
                         last_effect = Just e
                       }
  ComputeAndStore{store_at, x, y, f} ->
    Right MachineState { ip = ip + 4,
                         mem = set mem store_at $ f (read_param x) (read_param y),
                         future_inputs = future_inputs,
                         past_outputs = past_outputs,
                         last_effect = Just e
                       }
  NormalExit ->
    case last_effect of
      Just Output{} -> Left $ if past_outputs |> tail |> all (== 0)
                              then Success $ head past_outputs
                              else Fail past_outputs
      _ -> Left $ Fail past_outputs
  where read_param (p, i) = case p of
          Immediate -> i
          Position -> get mem i


step :: MachineState -> Either ExitStatus MachineState
step ms = apply ms eff
  where eff = read_instruction ms

execIntcode :: [Int] -> [Int] -> ExitStatus
execIntcode inputs code = run init_state
  where init_state = MachineState { ip = 0,
                                    future_inputs = inputs,
                                    past_outputs = [],
                                    mem = init_mem code,
                                    last_effect = Nothing
                                  }
        run ms = case step ms of
          Right ms -> run ms
          Left es -> es
