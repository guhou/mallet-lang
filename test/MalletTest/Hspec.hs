module MalletTest.Hspec
    ( hspecLaws
    )
where

import           Test.Hspec
import           Test.QuickCheck.Classes

hspecLaws :: Laws -> Spec
hspecLaws laws = describe
    (lawsTypeclass laws)
    (mapM_ (parallel . uncurry it) (lawsProperties laws))
