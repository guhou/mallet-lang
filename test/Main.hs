module Main
    ( main
    )
where

import qualified MalletTest
import           Test.Hspec.Formatters
import           Test.Hspec.Runner

main :: IO ()
main = hspecWith config MalletTest.spec
    where config = defaultConfig { configFormatter = Just failed_examples }
