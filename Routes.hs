module Routes where

import Data.List
import Data.Maybe

data PathPattern = Literal String | Capture String deriving (Eq, Show)

data Routes f = Route [PathPattern] f | Scope [PathPattern] (Routes f) | Many [Routes f] deriving Show

-- Ejercicio 1: Dado un elemento separador y una lista, se deber a partir la lista en sublistas de acuerdo a la aparicíon del separador (sin incluirlo).


split :: Eq a => a -> [a] -> [[a]]
split e = foldr (\x r -> if (x == e) then
   				 			[]:r
   				 		else
   				 			if null r then [x]:r
   				 			else
   				 			(x:(head r)):(tail r)
   			 	) [[]]
 

-- Ejercicio 2: A partir de una cadena que denota un patrón de URL se deberá construir la secuencia de literales y capturas correspondiente.
pattern :: String -> [PathPattern]
pattern "" = [Literal ""]
pattern a = map convertirAPathPattern $ filter (\e -> e /= "") $ split '/' a

convertirAPathPattern :: String -> PathPattern
convertirAPathPattern a = if head a == ':' then Capture (tail a) else Literal a

-- Ejercicio 3: Obtiene el valor registrado en una captura determinada. Se puede suponer que la captura está definida en el contexto.
type PathContext = [(String, String)]

get :: String -> PathContext -> String
get s = foldr (\x r -> if fst x == s  then snd x else r) ""

-- Ejercicio 4: Dadas una ruta particionada y un patrón de URL, trata de aplicar el patrón a la ruta y devuelve, en caso de que
--              la ruta sea un prefijo válido para el patrón, el resto de la ruta que no se haya llegado a consumir y el contexto capturado hasta el punto alcanzado.
-- Se puede usar recursión explícita.

-- Just (["tpf"],[("nombreMateria","plp")]) ~=? matches (splitSlash "materias/plp/tpf") (pattern "materias/:nombreMateria")
--Just (["tpf"],[("nombreMateria","plp")]) ~=? ["materias","plp","alu", "007"] [Literal "materias",Capture "nombreMateria"]

-- ["alu", "007", "materias", "plp"] [Literal "materias",Capture "nombreMateria"]
--Just ([""],[("nombreMateria","poo")]) ~=?["materias","poo","hola","yerba","taragui"] [Literal "materias", Capture "nombreMateria", Literal "yerba", Capture "mate"]
-- [],[()]
matcheaLiteral e (Literal s) (Capture _ ) = (e == s) 
matcheaLiteral e (Capture s) _ = False

captureName (Capture x) = x

matches :: [String] -> [PathPattern] -> Maybe ( [String], PathContext )
matches [] ps = Nothing
matches ss [] = Just (ss, [])
matches (s1:s2:ss) (p1:p2:ps) = if matcheaLiteral s1 p1 p2 
							then Just ( (fst ( resMatches ss ps )), ((captureName p2) ,s2):(snd (resMatches ss ps))) 
							else Just ( (fst ( resMatches (s1:s2:ss) ps )), (snd (resMatches ss (p2:ps))))
							where resMatches ss ps = case matches ss ps of
												Just (s, p) -> (s, p)
												Nothing -> ([],[])


-- DSL para rutas
route :: String -> a -> Routes a
route s f = Route (pattern s) f

scope :: String -> Routes a -> Routes a
scope s r = Scope (pattern s) r

many :: [Routes a] -> Routes a
many l = Many l

-- Ejercicio 5: Definir el fold para el tipo Routes f y su tipo. Se puede usar recursión explícita.
foldRoutes :: ( [PathPattern] -> f -> b ) -> ( [PathPattern] -> b -> b ) -> ( [b]  -> b ) -> Routes f -> b
foldRoutes f1 f2 f3 = g
						  where
						    g (Route xs f) = f1 xs f
						    g (Scope xs r) = f2 xs $ foldRoutes f1 f2 f3 r
						    g (Many xs) =  f3 $ map (foldRoutes f1 f2 f3) xs

-- Auxiliar para mostrar patrones. Es la inversa de pattern.
patternShow :: [PathPattern] -> String
patternShow ps = concat $ intersperse "/" ((map (\p -> case p of
  Literal s -> s
  Capture s -> (':':s)
  )) ps)

-- Ejercicio 6: Genera todos los posibles paths para una ruta definida.

paths :: Routes a -> [String]
paths = foldRoutes (\ps f -> rutasString ps )
				   (\ps res -> concat $ map (\unaRuta ->  adjuntarRutasProcesadas unaRuta res ) (rutasString ps)  )
				   (\res -> concat res )
				   where adjuntarRutasProcesadas unaRuta pathsRes =  map (\unPath -> unaRuta ++ ('/':unPath) ) pathsRes

rutasString = map pathPatternToString
pathPatternToString (Capture x) = ':':x
pathPatternToString (Literal x) = x


-- Ejercicio 7: Evalúa un path con una definición de ruta y, en caso de haber coincidencia, obtiene el handler correspondiente 
--              y el contexto de capturas obtenido.
{-
Nota: la siguiente función viene definida en el módulo Data.Maybe.
 (=<<) :: (a->Maybe b)->Maybe a->Maybe b
 f =<< m = case m of Nothing -> Nothing; Just x -> f x
-}
eval :: Routes a -> String -> Maybe (a, PathContext)
eval r string =  map (\unPath -> matches [string] (pattern unPath)) (paths r)


-- Ejercicio 8: Similar a eval, pero aquí se espera que el handler sea una función que recibe como entrada el contexto 
--              con las capturas, por lo que se devolverá el resultado de su aplicación, en caso de haber coincidencia.
exec :: Routes (PathContext -> a) -> String -> Maybe a
exec routes path = undefined

-- Ejercicio 9: Permite aplicar una funci ́on sobre el handler de una ruta. Esto, por ejemplo, podría permitir la ejecución 
--              concatenada de dos o más handlers.
wrap :: (a -> b) -> Routes a -> Routes b

wrap f = foldRoutes 
-- Si es el caso base de las rutas, entonces es construir una nueva ruta en la que al handler previamente se le aplica f
		(\paths h -> (Route paths $ f h)) 
-- En el caso de que sea un scope, el scope se compone de una lista de Paths y una Routes f... entonces es tomar los mismos paths y armar un scope pero con el resultado parcial que vino calculado en la recursión del fold
		(\paths resultadoParcial -> Scope paths resultadoParcial) 

-- En el caso de que sea un Many, alcanza con armar un many con las rutas que le vienen.. se asume que esas rutas ya vienen procesadas con el handler "cambiado".
		(\resultadoDeLasRutas -> many resultadoDeLasRutas)

-- Ejercicio 10: Genera un Routes que captura todas las rutas, de cualquier longitud. A todos los patrones devuelven el mismo valor. 
--               Las capturas usadas en los patrones se deberán llamar p0, p1, etc. 
--               En este punto se permite recursión explícita.
catch_all :: a -> Routes a
catch_all h = undefined
