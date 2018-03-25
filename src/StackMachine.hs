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
runProgram p i = reducer p Stack.empty i Map.empty (scanLabels p Map.empty) Seq.empty

reducer :: Program -> ComputerStack -> Input -> Memory -> SubprogramDir -> Output -> Output
reducer EOP _ _ _ _ out = out
reducer (l `Marks` p) s inp mem sub out = reducer p s inp mem (Map.insert l p sub) out
reducer (i `Then` p) s inp mem sub out = case i of
    (TA a) -> reducer p (Stack.push (Left a) s) inp mem sub out
    (TV v) -> reducer p (Stack.push (Right v) s) inp mem sub out
    DR     -> case (replaceAddressByValue s mem) of
                (Left s') -> reducer p s' inp mem sub out
                (Right e) -> error e
    ST     -> case (copyValueToMemory s mem, pop2 s) of
                (Left mem', Left s') -> reducer p s' inp mem' sub out
                (Right e, _)          -> error e
                (_, Right e)          -> error e
    WR     -> case (stackValue s) of
                (Left v)  -> reducer p (Stack.pop s) inp mem sub (out Seq.|> v)
                (Right e) -> error e
    RD     -> case (nextInputValue inp, popInputValue inp) of
                ((Left v), (Left inp')) -> reducer p (Stack.push (Right v) s) inp' mem sub out
                ((Right e), _)          -> error e
                (_, (Right e))          -> error e
    AD     -> case (add2 s) of
                (Left s') -> reducer p s' inp mem sub out
                (Right e) -> error e 
    SB     -> case (substract2 s) of
                (Left s') -> reducer p s' inp mem sub out
                (Right e) -> error e
    MT     -> case (multip2 s) of
                (Left s') -> reducer p s' inp mem sub out
                (Right e) -> error e
    DI     -> case (div2 s) of
                (Left s') -> reducer p s' inp mem sub out
                (Right e) -> error e
    (JU l) -> case (subprogram l sub) of
                (Left p') -> reducer p' s inp mem sub out
                (Right e) -> error e
    (JZ l) -> case (stackValue s, subprogram l sub) of
                (Left 0, Left p') -> reducer p' (Stack.pop s) inp mem sub out
                (Left _, Left _)       -> reducer p (Stack.pop s) inp mem sub out
                (Right e, _)      -> error e
                (_, Right e)      -> error e

subprogram :: Label -> Map.Map Label Program -> Either Program String
subprogram l m = case (Map.lookup l m) of
    (Just p) -> Left p
    _        -> Right unknownLabel

scanLabels :: Program -> Map.Map Label Program -> Map.Map Label Program
scanLabels (l `Marks` p) m = scanLabels p $ Map.insert l p m
scanLabels (_ `Then` p) m  = scanLabels p m
scanLabels EOP m           = m

copyValueToMemory :: ComputerStack -> Memory -> (Either Memory String)
copyValueToMemory s mem = case (stackValue s, stackAddress $ Stack.pop s) of
    ((Left v), (Left a)) -> Left $ Map.insert a v mem
    ((Right e), _)       -> Right e
    (_, (Right e))       -> Right e

replaceAddressByValue :: ComputerStack -> Memory -> (Either ComputerStack String)
replaceAddressByValue s m = case (stackAddress s) of
    (Left a)  -> case (valueAtAddress a m) of
        (Left v)  -> Left (Stack.push (Right v) $ Stack.pop s)
        (Right e) -> Right e
    (Right e) -> Right e

stackValue :: ComputerStack -> (Either Value String)
stackValue s = case (Stack.top s) of
    Right v -> Left v
    _       -> Right notValue

stackAddress :: ComputerStack -> (Either Address String)
stackAddress s = case (Stack.top s) of
    Left a -> Left a
    _      -> Right notAddress

valueAtAddress :: Address -> Memory -> (Either Value String)
valueAtAddress a m =  case (Map.lookup a m) of
    (Just v) -> Left v
    _        -> Right uninitializedMemory

nextInputValue :: Input -> (Either Value String)
nextInputValue i = case (Seq.viewl i) of
    (v Seq.:< _) -> Left v
    _            -> Right noInput

popInputValue :: Input -> (Either Input String)
popInputValue i = case (Seq.viewl i) of
    (_ Seq.:< xs) -> Left xs
    _             -> Right noInput

pop2 :: Stack.Stack a -> (Either (Stack.Stack a) String)
pop2 s = case (stackTop2 s) of
    (Left (_, _, s)) -> Left s
    (Right e)    -> Right e

add2 :: ComputerStack -> (Either ComputerStack String)
add2 s = case (stackTop2Values s) of
    (Left (l, r, s')) -> Left $ Stack.push (Right $ l + r) s'
    (Right e)         -> Right e

substract2 :: ComputerStack -> (Either ComputerStack String)
substract2 s = case (stackTop2Values s) of
    (Left (l, r, s')) -> Left $ Stack.push (Right $ l - r) s'
    (Right e)         -> Right e

multip2 :: ComputerStack -> (Either ComputerStack String)
multip2 s = case (stackTop2Values s) of
    (Left (l, r, s')) -> Left $ Stack.push (Right $ l * r) s'
    (Right e)         -> Right e

div2 :: ComputerStack -> (Either ComputerStack String)
div2 s = case (stackTop2Values s) of
    (Left (l, 0, s')) -> Right divisionByZero
    (Left (l, r, s')) -> Left $ Stack.push (Right $ l `div` r) s'
    (Right e)     -> Right e

stackTop2 :: Stack.Stack a -> (Either (a, a, Stack.Stack a) String)
stackTop2 s = case (Stack.topSafe s) of
    (Just l) -> case (Stack.topSafe $ Stack.pop s) of
                    (Just r) -> Left (l, r, Stack.pop . Stack.pop $ s)
                    _        -> (Right emptyStack)
    _        -> (Right emptyStack)

stackTop2Values :: ComputerStack -> (Either (Value, Value, ComputerStack) String)
stackTop2Values s = case (stackTop2 s ) of
    (Left (Right l, Right r, s')) -> Left (l, r, s')
    (Right e)                    -> Right e
    _                            -> Right notValue

emptyStack = "Empty stack"
notValue = "Not value"
notAddress = "Not address"
divisionByZero = "Division by 0"
noInput = "No input"
uninitializedMemory = "Uninitialized memory"
unknownLabel = "Unknown label"