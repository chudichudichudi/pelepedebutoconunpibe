import Routes
import Test.HUnit
import Data.List (sort)
import Data.Maybe (fromJust, isNothing	)

rutasFacultad = many [
  route ""             "ver inicio",
  route "ayuda"        "ver ayuda",
  scope "materia/:nombre/alu/:lu" $ many [
    route "inscribir"   "inscribe alumno",
    route "aprobar"     "aprueba alumno"
  ],
  route "alu/:lu/aprobadas"  "ver materias aprobadas por alumno"
  ]

rutasStringOps = route "concat/:a/:b" (\ctx -> (get "a" ctx) ++ (get "b" ctx))

-- evaluar t para correr todos los tests
t = runTestTT allTests

allTests = test [
	"patterns" ~: testsPattern,
	"matches" ~: testsMatches,
	"paths" ~: testsPaths,
	"eval" ~: testsEval,
	"evalWrap" ~: testsEvalWrap,
	"evalCtxt"~: testsEvalCtxt,
	"execEntity" ~: testsExecEntity,
	"pathContext" ~: testsPathContext
	]

splitSlash = split '/'


{- ---------------------------------------------------------------------------------------------
TEST DE PATTERN

-}	

testsPattern = test [
  splitSlash "" ~=? [""],
	splitSlash "/" ~=? ["",""],
	splitSlash "/foo" ~=? ["", "foo"],
	[Literal ""] ~=? pattern "", 
	[] ~=? pattern "/", 
	pattern "lit1/:cap1/:cap2/lit2/:cap3" ~=? [Literal "lit1", Capture "cap1", Capture "cap2", Literal "lit2", Capture "cap3"]
	]

	
{- ---------------------------------------------------------------------------------------------
TEST DE GET

-}	
	

testsPathContext = test [
	get "nombre" [("nombre","chimi")] ~=? "chimi",
	get "nombre" [("apellido","curry")] ~=? "",
	get "nombre" [("apellido","curry"), ("nombre","chimi")] ~=? "chimi",
	get "" [("apellido","curry"), ("nombre","chimi")] ~=? "",
	get "nombre" [("nombre","chimi"), ("nombre","curry")] ~=? "chimi"
	]


{- ---------------------------------------------------------------------------------------------
TEST DE MATCHES

-}	


testsMatches = test [

	Nothing ~=? matches [""] (pattern "lala"),
	Just (["tpf"],[("nombreMateria","plp")]) ~=? matches (splitSlash "materias/plp/tpf") (pattern "materias/:nombreMateria"),
	Just (["alu","007−1"] ,[("nombre","plp")]) ~=? matches ["materia","plp","alu","007−1"] [ Literal "materia",Capture "nombre"],
	Nothing ~=? matches ["otra","ruta"] [ Literal "ayuda"],
	Nothing ~=? matches [ ] [ Literal "algo"],
	Just([""],[]) ~=? matches (splitSlash "") [], 
	Nothing ~=? matches ["materia","plp","alu","007−1"] [ Literal "alu",Capture ":libreta"],
	Just(["tpf","fpt","jasquel"],[("nombreMateria","plp")]) ~=? matches (splitSlash "materias/plp/tpf/fpt/jasquel") (pattern "materias/:nombreMateria"),	
	Just(["materias","plp","tpf","fpt","jasquel"],[]) ~=? matches (splitSlash "materias/plp/tpf/fpt/jasquel") (pattern ""),
	Just(["jasquel"],[("nombreMateria","plp"),("fpt","fpt")]) ~=? matches (splitSlash "materias/plp/tpf/fpt/jasquel") (pattern "materias/:nombreMateria/tpf/:fpt"),	
	Just([],[("nombreMateria","plp"),("fpt","fpt")]) ~=? matches (splitSlash "materias/plp/tpf/fpt/jasquel") (pattern "materias/:nombreMateria/tpf/:fpt/jasquel"),	
	Just([],[("nombreMateria","plp"),("fpt","fpt"), ("poolog","prolog")]) ~=? matches (splitSlash "materias/plp/tpf/fpt/jasquel/prolog") (pattern "materias/:nombreMateria/tpf/:fpt/jasquel/:poolog")
	
	]


path0 = route "foo" 1
path1 = scope "foo" (route "bar" 2)
path2 = scope "foo" (route ":bar" 3)
path3 = scope "" $ scope "" $ many [ scope "" $ route "foo" 1]
path123 = scope "" $ route "foo" 123
path555 = route "" 1
path100 = scope "foo" $ many [ route "bar" 1, route ":pirulo" 1]
path101 = scope "foo1" $ many [ route "bar" 1, many [ route ":pirulo" 1, scope "foo2" $ many [route "foo3" 1, route ":foo4" 1]]]
path102 = many [ route "hola" 1 , scope "foo1" $ many [ route "bar" 1, many [ route ":pirulo" 1, scope "foo2" $ many [route "foo3" 1, route ":foo4" 1]]]]

path444 = many [
  (route "asd" 1),
  (route "pepe" 12)
  ]

path445 = many [
  (scope "folder" (many [
    (route "lorem" 4),
    (route "ipsum" 5)
    ])),
    (route "" 1)
  ]


path446 = many [(route "lorem" 4),(route "lorem" 5)]

path4 = many [
  (route "" 1),
  (route "lo/rem" 2),
  (route "ipsum" 3),
  (scope "folder" (many [
    (route "lorem" 4),
    (route "ipsum" 5)
    ]))
  ]

 
testsEval = test [
		Just ("ver materias aprobadas por alumno" ,[("lu","007-01")]) ~=? eval rutasFacultad "alu/007-01/aprobadas",
		Just ("aprueba alumno" ,[("nombre","plp"),("lu","007-01")]) ~=? eval rutasFacultad "materia/plp/alu/007-01/aprobar",
		Nothing ~=? eval rutasFacultad "alu/007-01",
		Nothing ~=?	eval (route "alu/:lu/aprobadas"  "ver materias aprobadas por alumno") "alu/007-01",
		Just(1,[]) ~=? eval (route "" 1) "",
		1 ~=? justEvalP4 "",
		4 ~=? justEvalP4 "folder/lorem",
		5 ~=? justEvalP4 "folder/ipsum"
		
		
	]
	where justEvalP4 s = fst (fromJust (eval path4 s))

testsEvalCtxt = test [
	Just (1, []) ~=? eval path0 "foo",
	Just (2, []) ~=? eval path1 "foo/bar",
	isNothing (eval path1 "foo/bla") ~? "",
	Just (3, [("bar", "bla")]) ~=? eval path2 "foo/bla",
	Just (1, []) ~=? eval path3 "foo",
	Just (123, []) ~=? eval path123 "/foo"	
	]	
	
	
	
	
	
{- ---------------------------------------------------------------------------------------------
TEST DE PATHS

-}	

testsPaths = test [
 	sort ["","post","post/:id","post/:id/create","post/:id/update","post/:id/delete","category","category/:id","category/:id/create","category/:id/update","category/:id/delete"] ~=? sort (paths path5),
	sort [""] ~=? sort (paths path555),
 	sort ["foo"] ~=? sort (paths path0),
 	sort ["foo/bar"] ~=? sort (paths path1),
 	sort ["foo/:bar"] ~=? sort (paths path2),
 	sort ["foo/bar", "foo/:pirulo"] ~=? sort (paths path100),
 	sort ["foo1/bar","foo1/:pirulo","foo1/foo2/foo3","foo1/foo2/:foo4"] ~=? sort (paths path101),
	sort ["hola","foo1/bar","foo1/:pirulo","foo1/foo2/foo3","foo1/foo2/:foo4"] ~=? sort (paths path102),
	["personas/santiago"] ~=? paths (route "personas/santiago" "aprobar")
	]	


{- ---------------------------------------------------------------------------------------------
TEST DE WRAP

-}	

path410 = wrap (+10) path4
path411 = wrap (++ " currificada") rutasFacultad
 
 
testsEvalWrap = test [
		14 ~=? justEvalP410 "folder/lorem",
		"ver ayuda currificada" ~=? justEvalP411 "ayuda",
		"inscribe alumno currificada" ~=? justEvalP411 "materia/plp/alu/123/inscribir",
		"ver inicio currificada" ~=? justEvalP411 ""
	]
	where justEvalP410 s = fst (fromJust (eval path410 s))
justEvalP411 s = fst (fromJust (eval path411 s))
	

	
-- ejempo donde el valor de cada ruta es una función que toma context como entrada.
-- para estos se puede usar en lugar además de eval, la función exec para devolver
-- la applicación de la función con sobre el contexto determinado por la ruta
rest nombre = many [
  (route nombre (const (nombre++"#index"))),
  (scope (nombre++"/:id") (many [
    (route "" (const (nombre++"#show"))),
    (route "create" (\ctx -> nombre ++ "#create of " ++ (get "id" ctx))),
    (route "update" (\ctx -> nombre ++ "#update of " ++ (get "id" ctx))),
    (route "delete" (\ctx -> nombre ++ "#delete of " ++ (get "id" ctx)))
    ]))
  ]


path5 = many [
  (route "" (const "root_url")),
  (rest "post"),
  (rest "category")
  ]	


{- ---------------------------------------------------------------------------------------------
TEST DE EXEC

-}	  
  

testsExecEntity = test [
	Just "root_url" ~=? exec path5 "",
	Just "post#index" ~=? exec path5 "post",
	Just "category#create of 7" ~=? exec path5 "category/7/create",
	Just "post#show" ~=? exec path5 "post/35/",
	Just "post#create of 35" ~=? exec path5 "post/35/create",
	Just "ruta" ~=? exec (route "" (const ("ruta"))) ""
	
	]

