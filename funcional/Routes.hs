module Routes where

import Data.List
import Data.Maybe

data PathPattern = Literal String | Capture String deriving (Eq, Show)

data Routes f = Route [PathPattern] f | Scope [PathPattern] (Routes f) | Many [Routes f] deriving Show




-- Ejercicio 1: Dado un elemento separador y una lista, se deber a partir la lista en sublistas de acuerdo a la aparicíon del separador (sin incluirlo).
{-

"Iteramos" sobre la lista tomando decisiones

Si encontramos un elemento de split, por ejemplo el '/' entonces tenemos que agregar una lista vacía a la lista resultado para que los elementos que encontremos después los metamos en esa nueva lista.

De esa forma, por cada '/' deberíamos tener una lista nueva



-}
split :: Eq a => a -> [a] -> [[a]]
split e = foldr (\x r -> if (x == e) then

						-- Encontramos elemento de corte, arrancamos una lista nueva...
   				 			[]:r
   				 		else
						-- Si no es elemento de corte, entonces lo metemos al comienzo de la primer lista del resultado parcial..
   				 			meterXAlComienzoDeLaPrimerLista x r

   			 	) [[]]

meterXAlComienzoDeLaPrimerLista x r = (x:(head r)):(tail r)

---------------------------------------------------------------------------------------------------------------------------------------


-- Ejercicio 2: A partir de una cadena que denota un patrón de URL se deberá construir la secuencia de literales y capturas correspondiente.
{-
 Dada la url, primero le aplicamos el split para obtener sus "PathPatterns en formato string" y de ahí sacamos los vacíos.
 Luego mediante un map los convertimos a sus Pathpattern correspondiente.
-}


pattern :: String -> [PathPattern]
pattern "" = []
pattern a = map convertirAPathPattern $ filter (\e -> e /= "") $ split '/' a

convertirAPathPattern :: String -> PathPattern
convertirAPathPattern a = if head a == ':' then Capture (tail a) else Literal a


---------------------------------------------------------------------------------------------------------------------------------------------------

-- Ejercicio 3: Obtiene el valor registrado en una captura determinada. Se puede suponer que la captura está definida en el contexto.

{-

Es como buscar un valor en un diccionario..

Iteramos las claves, si la encontramos devolvemos su valor respectivo.. caso contrario seguimos buscando en el resto de la lista..

En caso de no encontrar nada devolvemos el string vacío

-}

type PathContext = [(String, String)]

get :: String -> PathContext -> String
get unNombre = foldr (\unaCaptura r -> if fst unaCaptura == unNombre  then snd unaCaptura else r) ""

---------------------------------------------------------------------------------------------------------------------------------------------------


-- Ejercicio 4: Dadas una ruta particionada y un patrón de URL, trata de aplicar el patrón a la ruta y devuelve, en caso de que
--              la ruta sea un prefijo válido para el patrón, el resto de la ruta que no se haya llegado a consumir y el contexto capturado hasta el punto alcanzado.
-- Se puede usar recursión explícita.

{-

Ejemplos

Just (["tpf"],[("nombreMateria","plp")]) ~=? matches (splitSlash "materias/plp/tpf") (pattern "materias/:nombreMateria")
Just (["tpf"],[("nombreMateria","plp")]) ~=? ["materias","plp","alu", "007"] [Literal "materias",Capture "nombreMateria"]

["alu", "007", "materias", "plp"] [Literal "materias",Capture "nombreMateria"]
Just ([""],[("nombreMateria","poo")]) ~=?["materias","poo","hola","yerba","taragui"] [Literal "materias", Capture "nombreMateria", Literal "yerba", Capture "mate"]
 [],[()]
-}


matches :: [String] -> [PathPattern] -> Maybe ( [String], PathContext )
matches [] ps = Nothing
matches ss [] = Just (ss, [])
matches (s:ss) (p:ps) = case p of
						 Capture pat  -> 	Just ( fst ( resMatches ss ps ) ,  (pat ,s):(snd (resMatches ss ps) ) )
						 Literal lit  -> 	if lit == s 
											then Just ( fst ( resMatches ss ps ) ,  snd (resMatches ss ps) )
											else Nothing
						where resMatches ss ps = case matches ss ps of
												Just (rs, rp) -> (rs, rp)
												Nothing -> ([],[])

----------------------------------------------------------------------------------------------------------------------

-- DSL para rutas
route :: String -> a -> Routes a
route s f = Route (pattern s) f

scope :: String -> Routes a -> Routes a
scope s r = Scope (pattern s) r

many :: [Routes a] -> Routes a
many l = Many l

----------------------------------------------------------------------------------------------------------------------

-- Ejercicio 5: Definir el fold para el tipo Routes f y su tipo. Se puede usar recursión explícita.
{-

Vamos a separar en casos para explicar:

1) Routes [PathPattern] f

Acá el valor Z que se suele explicitar en el foldr sobre listas es aplicarle a una función 'f1' los PathPattern que componen a la ruta y su handler.
Este sería nuestro caso base.


2) Scope [PathPattern] (Routes f) 

Acá lo que hacemos es aplicar 'f2' pasando como parámetro la lista de PathPattern que componen al scope y como segundo parámetro
es el llamado recursivo de foldRoutes sobre las rutas que componen al scope original

3) Many [Routes f]

En este caso es llamar a foldRoutes para cada ruta del many

-}

foldRoutes :: ( [PathPattern] -> f -> b ) -> ( [PathPattern] -> b -> b ) -> ( [b]  -> b ) -> Routes f -> b
foldRoutes f1 f2 f3 = g
						  where
						    g (Route xs f) = f1 xs f
						    g (Scope xs r) = f2 xs $ foldRoutes f1 f2 f3 r
						    g (Many xs) =  f3 $ map (foldRoutes f1 f2 f3) xs

--------------------------------------------------------------------------------------------------------------------

-- Auxiliar para mostrar patrones. Es la inversa de pattern.
patternShow :: [PathPattern] -> String
patternShow ps = concat $ intersperse "/" ((map (\p -> case p of
  Literal s -> s
  Capture s -> (':':s)
  )) ps)

--------------------------------------------------------------------------------------------------------------------

-- Ejercicio 6: Genera todos los posibles paths para una ruta definida.

{-

En este caso usamos el foldRoutes previamente definido.

1) Routes [PathPattern] f

Acá lo que queremos es tomar los PathPattern que componen a la ruta y convertirlos a string. Para este caso no deberíamos tener un resultado parcial al cual
concatenarle estos paths obtenidos así que el lamba sólo invoca a 'rutasString pathPatterns'


2) Scope [PathPattern] (Routes f) 

Acá vamos a tener un conjunto de pathPatterns que los pasamos a string (mediante 'rutasString ps' )
y luego a cada uno le adjuntamos los paths que salen de Routes f en el llamado recursivo.

3) Many [Routes f]

En este caso es llamar a foldRoutes para cada ruta del many

-}

paths :: Routes a -> [String]
paths = foldRoutes (\pathPatterns f -> (patternShow pathPatterns):[] )
				   (\pths res -> map (\aRes -> if null aRes then  patternShow pths
				   											else patternShow pths ++ "/" ++ aRes) res )
				   (\res -> concat res )
{-
formarRutaCompleta :: [PathPattern] -> String
formarRutaCompleta ps =  intercalate "/" $ rutasString ps

rutasString = map pathPatternToString

pathPatternToString (Capture x) = ':':x
pathPatternToString (Literal x) = x
-}
--------------------------------------------------------------------------------------------------------------------------------------

-- Ejercicio 7: Evalúa un path con una definición de ruta y, en caso de haber coincidencia, obtiene el handler correspondiente 
--              y el contexto de capturas obtenido.
{-
Nota: la siguiente función viene definida en el módulo Data.Maybe.
 (=<<) :: (a->Maybe b)->Maybe a->Maybe b
 f =<< m = case m of Nothing -> Nothing; Just x -> f x
-}
{--}
eval :: Routes a -> String -> Maybe (a, PathContext)
eval unaRuta url = eval2 unaRuta (split '/' url) 

eval2 :: Routes a -> [String] -> Maybe (a, PathContext)
eval2 =	foldRoutes (\pathPatterns handler -> (\url -> case matches url pathPatterns of 
											Nothing -> Nothing
											Just(noConsumido,pathContext) -> Just( handler, pathContext)
							  )
				   )
				   (\pathPatterns res -> (\url -> case matches url pathPatterns of 
				    						Nothing -> Nothing
				    						Just (noConsumido, pathContext) ->
				    							case  (res noConsumido) of
				    								Nothing -> Nothing
				    								Just (hand, pathC) -> Just (hand, pathContext ++ pathC) 
				    			)
				   )
				   (\res -> (\url -> devolverNotNothing (map (\r ->  (r url) ) res ) )  )

devolverNotNothing res = case filter notNothing res of
						[] -> Nothing
						(x:xs) -> x

notNothing :: Maybe (a,PathContext) -> Bool
notNothing (Just(a, pc)) = True
notNothing Nothing = False

------------------------------------------------------------------------------------------------------------------------------------------


-- Ejercicio 8: Similar a eval, pero aquí se espera que el handler sea una función que recibe como entrada el contexto 
--              con las capturas, por lo que se devolverá el resultado de su aplicación, en caso de haber coincidencia.

exec :: Routes (PathContext -> a) -> String -> Maybe a
exec routes path = case eval routes path of
					Nothing -> Nothing
					(Just(handler, pc)) -> Just(handler pc) 

------------------------------------------------------------------------------------------------------------------------------------------

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