entrelazar :: [a] -> [a] -> [a]
entrelazar = foldr (\a r -> (\x -> case x of
									[] -> []
									y:ys -> a:y: (r ys)
								    
									)
						) (\x -> []) 

curry2 :: ((a,b) -> c) -> a -> b -> c
curry2  f a b =  f (a,b)


uncurry2 :: (a -> b -> c) -> ((a,b) -> c) 
uncurry2  f (a,b) =  f a b

pitagoricas :: [(Integer, Integer, Integer)]
pitagoricas = [(a,b,c) | a <- [0..], b <- [0..a], c <- [0..a],  b*b + c*c == a*a ]


primos :: [Integer]
primos = [x | x <- [1..], length (divisores x) == 2]
           where divisores x = [d | d <- [1..x], mod x d == 0]


partir :: [a] -> [([a], [a])]
partir a = [ p | i <- [0.. length a], p <- [splitAt i a] ]


-- listasQueSuman :: Int -> [[Int]]
-- listasQueSuman 0 = []
-- listasQueSuman n = 

data Map a b = Nil b | Def a b (Map a b)

definir :: Eq a => a -> b -> Map a b -> Map a b
definir clave valor (Nil b) = Def clave valor (Nil b)
definir clave valor (Def a b map) = if clave == a then
										Def a valor map
									else
										Def a b (definir clave valor map)


convertir :: Eq a => b -> [(a,b)] -> Map a b
