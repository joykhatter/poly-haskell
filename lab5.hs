module lab5 where

 first x y = do
  let q = length x
  if y<=0 then head x
  else if y>q then take 1 (drop q-1 x) 
  else take 1 (drop y-1 x)
  
 data poly = polygons Float Float
           deriving (Show)
		   
 
 instance Eq poly where
  (polygons x1 y1) == (polygons x2 y2 ) = (x1==x2 && y1==y2) 
  area (polygons x y) = (x * x * y)/(4*tan(180/y))
  (polygons x1 y1) areacheck (polygons x2 y2) = ((x1 * x1 * y1)/(4*tan(180/y1)) == (x2 * x2 * y2)/(4*tan(180/y2)))
  
  instance Show poly where
   show (polygons x y ) =
    "< regular polygons length: " ++ (show x) ++ ", regular polygons sides: " ++ (show y) ++ ">"
 