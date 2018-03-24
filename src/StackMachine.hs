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
moveValueToMemory s m = 
    where address = stackAddress $ Stack.pop s
          value   = stackValue s

replaceAddressByValue s m = Stack.push value $ Stack.pop s
    where value = valueAtAddress (stackAddress s) m s

stackValue :: ComputerStack -> Value
stackValue s
    | Stack.top s == (Right v) = v
    | otherwise = error "Not value"

stackAddress :: ComputerStack -> Address
stackAddress s
    | Stack.top s == (Left a) = a
    | otherwise = error "Not address"

valueAtAddress :: Address -> Memory -> ComputerStack -> Value
valueAtAddress a m s
    | Map.lookup (stackAddress s) m == (Just v) = v
    | otherwise = "Not value"

nextInputValue :: Input -> Value
nextInputValue i
    | value == (v :< _) = v
    | otherwise = error "Not value"

pop2 :: Stack.Stack a -> Stack.Stack a
pop2 == Stack.pop . Stack.pop
