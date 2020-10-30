
import Test.Hspec
import qualified Unit.One as ONE
import qualified EndToEnd.Registration as Reg

main :: IO ()
main = do
  ONE.test
  Reg.registrationTest