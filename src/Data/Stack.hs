module Data.Stack where

-- https://en.wikipedia.org/wiki/Stack_(abstract_data_type)
data Stack a = Empty | NonEmpty a (Stack a)
             deriving (Show, Read, Eq)

empty :: Stack a
empty = Empty

-- Get element from top of stack
-- If stack is empty error with message "Empty stack" will be raised
-- TODO: implement top
top :: Stack a -> a
top = undefined

-- Get element from top of stack (if there is some, otherwise return Nothing)
-- TODO: implement safe top
topSafe :: Stack a -> Maybe a
topSafe = undefined

-- Pop element from top of stack
-- If stack is empty error with message "Empty stack" will be raised
-- TODO: implement pop
pop :: Stack a -> Stack a
pop = undefined

-- Pop element from top of stack (if there is some, otherwise return Nothing)
-- TODO: implement safe pop
popSafe :: Stack a -> Maybe (Stack a)
popSafe = undefined

-- Push element to top of stack
-- TODO: implement push
push :: a -> Stack a -> Stack a
push = undefined

-- Get number of elements in stack
-- TODO: implement size
size :: Num n => Stack a -> n
size = undefined

-- Check if stack is empty
-- Note: is more effective than checking if size is zero
-- TODO: implement null (not by using size!)
null :: Stack a -> Bool
null = undefined
