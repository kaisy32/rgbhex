module RgbHex
    ( 
        hexrgb,
        rgbhex
    ) where
import Numeric
import Control.Monad

hexrgb :: String -> [Int]
hexrgb xs = do
    map fst $ join $ map readHex [x1,x2,x3] where
        x1 = ((xs!!0):"") ++ ((xs!!1):"")
        x2 = ((xs!!1):"") ++ ((xs!!2):"")
        x3 = ((xs!!3):"") ++ ((xs!!4):"")

rgbhex :: (Int,Int,Int) -> String
rgbhex (x,y,z) = do
    let xs = properFraction $ (fromIntegral x)/16
    let ys = properFraction $ (fromIntegral y)/16
    let zs = properFraction $ (fromIntegral z)/16
    let a = unzip [xs,ys,zs]
    let a2 = map (\ks -> floor $ ks*16) $ snd a  
    let end = map (\xk -> showHex xk "" ) $ fst a
    let end1 = map (\xk -> showHex xk "" ) a2

    join $ (end!!0):(end1!!0):(end!!1):(end1!!1):(end!!2):(end1!!2):[]
