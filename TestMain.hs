{-# OPTIONS_HADDOCK ignore-exports #-}


import Test.QuickCheck
import Test.Vector


deepCheck :: Testable prop => prop -> IO ()
deepCheck = quickCheckWith (stdArgs { maxSuccess = 1000})


main :: IO ()
main = do
  putStrLn "testing inRange:"
  deepCheck inRangeProp1
  deepCheck inRangeProp2
  deepCheck inRangeProp3
  deepCheck inRangeProp4
  deepCheck inRangeProp5
  putStrLn "testing onPT:"
  deepCheck onPTProp1
  deepCheck onPTProp2
  putStrLn "testing getAngle:"
  deepCheck getAngleProp1
  deepCheck getAngleProp2
  deepCheck getAngleProp3
  deepCheck getAngleProp4
  deepCheck getAngleProp5
  deepCheck getAngleProp6
  putStrLn "testing scalarProd:"
  deepCheck scalarProdProp1
  deepCheck scalarProdProp2
  deepCheck scalarProdProp3
