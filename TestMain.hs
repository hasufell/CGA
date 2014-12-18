{-# OPTIONS_HADDOCK ignore-exports #-}


import Test.QuickCheck
import Test.MyPrelude
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
  deepCheck inRangeProp6
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
  deepCheck getAngleProp7
  putStrLn "testing scalarProd:"
  deepCheck scalarProdProp1
  deepCheck scalarProdProp2
  deepCheck scalarProdProp3
  deepCheck scalarProdProp4
  deepCheck scalarProdProp5
  putStrLn "testing dimToSquare:"
  deepCheck dimToSquareProp1
  putStrLn "testing vecLength:"
  deepCheck vecLengthProp1
  putStrLn "testing pt2Vec:"
  deepCheck pt2VecProp1
  deepCheck pt2VecProp2
  putStrLn "testing vec2Pt:"
  deepCheck vec2PtProp1
  deepCheck vec2PtProp2
  putStrLn "testing vp2:"
  deepCheck vp2Prop1
  deepCheck vp2Prop2
  putStrLn "testing det:"
  deepCheck detProp1
  deepCheck detProp2
  putStrLn "testing splitBy"
  deepCheck splitByProp1
  deepCheck splitByProp2
  deepCheck splitByProp3
