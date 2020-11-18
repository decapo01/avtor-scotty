
import Test.Hspec
import qualified Unit.One as ONE
import EndToEnd.Registration (registrationFailTest)

main :: IO ()
main = do
  ONE.test
  registrationFailTest