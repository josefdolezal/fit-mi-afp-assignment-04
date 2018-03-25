module StackMachine where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Stack as Stack

import Control.Program

-- | Input is sequence of values (type Value is defined in Control.Program)
type Input = Seq.Seq Value
-- | Output is sequence of values (type Value is defined in Control.Program)
type Output = Seq.Seq Value

-- | Memory of computer stores on address of type Address a value of type Value (see Control.Program)
type Memory = Map.Map Address Value
-- | Lookup directory of subprograms (labels are marking starts of parts of a program where you can jump with JU or JZ)
type SubprogramDir = Map.Map Label Program
-- | Computer stack can store addresses and values
type ComputerStack = Stack.Stack (Either Address Value)


-- | Run program with given input (memory and stack should be empty at start)
-- | If there is a problem, error is raised ("Empty stack", "Not value", "Not address", "No input", "Unknown label", "Division by 0", "Uninitialized memory"), see tests
-- TODO: implement running the program
runProgram :: Program -> Input -> Output
copyValueToMemory :: ComputerStack -> Memory -> Memory
copyValueToMemory s mem = Map.insert address value mem
    where address = stackAddress $ Stack.pop s
          value   = stackValue s

replaceAddressByValue :: ComputerStack -> Memory -> ComputerStack
replaceAddressByValue s m = Stack.push value $ Stack.pop s
    where value = (Right $ valueAtAddress (stackAddress s) m s)

stackValue :: ComputerStack -> Value
stackValue s = case (Stack.top s) of
    Right v -> v
    _       -> error notValue

stackAddress :: ComputerStack -> Address
stackAddress s = case (Stack.top s) of
    Left a -> a
    _      -> error notAddress

valueAtAddress :: Address -> Memory -> ComputerStack -> Value
valueAtAddress a m s = case (Map.lookup (stackAddress s) m) of
    (Just v) -> v
    _        -> error notValue

nextInputValue :: Input -> Value
nextInputValue i = case (Seq.viewl i) of
    (v Seq.:< b) -> v
    _            -> error notValue

pop2 :: Stack.Stack a -> Stack.Stack a
pop2 = Stack.pop . Stack.pop

notValue = "Not value"
notAddress = "Not address"
