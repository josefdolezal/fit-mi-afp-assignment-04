import Test.Hspec

import qualified ListsSpec
import qualified StackSpec
import qualified StackMachineSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Lists"        ListsSpec.spec
  describe "Data.Stack"   StackSpec.spec
  describe "StackMachine" StackMachineSpec.spec
