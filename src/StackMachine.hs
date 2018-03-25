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
runProgram p i = reducer p Stack.empty i Map.empty Map.empty Seq.empty

reducer :: Program -> ComputerStack -> Input -> Memory -> SubprogramDir -> Output -> Output
reducer EOP _ _ _ _ out = out
reducer (i `Then` p) s inp mem sub out = case i of
    (TA a) -> reducer p (Stack.push (Left a) s) inp mem sub out
    (TV v) -> reducer p (Stack.push (Right v) s) inp mem sub out
    DR     -> reducer p (replaceAddressByValue s mem) inp mem sub out
    ST     -> reducer p (pop2 s) inp (copyValueToMemory s mem) sub out
    WR     -> reducer p s inp mem sub (out Seq.|> stackValue s)
    RD     -> reducer p (Stack.push (Right $ nextInputValue inp) s) (popInputValue inp) mem sub out
    AD     -> reducer p (add2 s) inp mem sub out
    SB     -> reducer p (substract2 s) inp mem sub out
    MT     -> reducer p (multip2 s) inp mem sub out
    DI     -> reducer p (div2 s) inp mem sub out
    
reducer _ _ _ _ _ _  = error "Jumping not supported yet"

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
    (v Seq.:< _) -> v
    _            -> error notValue

popInputValue :: Input -> Input
popInputValue i = case (Seq.viewl i) of
    (_ Seq.:< xs) -> xs
    _             -> error notValue

pop2 :: Stack.Stack a -> Stack.Stack a
pop2 = Stack.pop . Stack.pop

add2 :: ComputerStack -> ComputerStack
add2 s = Stack.push (Right sum) $ pop2 s
    where sum = (stackValue s) + (stackValue $ Stack.pop s)

substract2 :: ComputerStack -> ComputerStack
substract2 s = Stack.push (Right remainder) $ pop2 s
    where remainder = (stackValue s) - (stackValue $ Stack.pop s)

multip2 :: ComputerStack -> ComputerStack
multip2 s = Stack.push (Right result) $ pop2 s
    where result = (stackValue s) - (stackValue $ Stack.pop s)

div2 :: ComputerStack -> ComputerStack
div2 s = Stack.push (Right result) $ pop2 s
    where result = case (stackValue $ Stack.pop s) of
            0 -> error divisionByZero
            x -> (stackValue s) `div` x

notValue = "Not value"
notAddress = "Not address"
divisionByZero = "Division by 0"
