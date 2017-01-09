#define TESTS
#include <haskell>
import qualified Data.Tes3.Parser.Spec
import qualified Data.Tes3.Utils.Spec

main :: IO ()
main = void $ runTestTT tests

tests :: Test
tests = TestList
  [ Data.Tes3.Parser.Spec.tests
  , Data.Tes3.Utils.Spec.tests
  ]
