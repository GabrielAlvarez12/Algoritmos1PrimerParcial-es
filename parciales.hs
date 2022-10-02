



-----------------------------------------PARCIAL 2021 TEMA A----------------------------------------------------------------
--1) 
data EmpresaTelefono = Claro | Personal | Movistar | Tuenti deriving Eq 
type Frase = String


fraseEmpresa :: EmpresaTelefono -> Frase
fraseEmpresa Claro = "Claro la red mas poderosa" 
fraseEmpresa Personal = "Personal es como vos"
fraseEmpresa Movistar = "Movistar compartida la vida es mas" 
fraseEmpresa Tuenti = "La mas economica"


--2) 
type NombrePersona = String

data MisEmpresas = Ninguna | AgregarEmpresa EmpresaTelefono NombrePersona MisEmpresas

tengoEmpresa :: MisEmpresas -> EmpresaTelefono ->NombrePersona->Bool

tengoEmpresa Ninguna _ _   = False
tengoEmpresa (AgregarEmpresa et np mi ) et1 np1 | et1 == et && np1 == np = True 
                                                | otherwise = tengoEmpresa (mi) et1 np1


--3)
data ListaAsoc a b  = Vacia | Nodo a b  ( ListaAsoc a b  ) deriving (Show)

type NroTel = Int 


agregaLa :: ListaAsoc EmpresaTelefono NroTel ->EmpresaTelefono->NroTel-> ListaAsoc EmpresaTelefono NroTel
agregaLa Vacia et nt = Nodo et nt Vacia 
agregaLa (Nodo a b r) et nt = Nodo a b (agregaLa r et nt )
--agregaLa la et nt =Nodo et nt
-- esta ultima definicion funciona mejor y no necesita caso base 

-----------------------------------------------parcial 2022 TEMA A -----------------------------------------------------------
--1) !
 -- a) !!
 
data Forma = Piedra | Papel |Tijera

le_gana :: Forma ->Forma -> Bool
le_gana Piedra Tijera = True 
le_gana Piedra Papel = False
le_gana Piedra Piedra = False 
le_gana Tijera Piedra = False 
le_gana Tijera Papel = True 
le_gana Tijera Tijera = False 
le_gana Papel Tijera = False   
le_gana Papel Piedra = True
le_gana Papel Papel = False  

--aux para ejercicio 2) 
sonIguales :: Forma->Forma ->Bool
sonIguales Piedra Piedra = True
sonIguales Piedra Tijera = False
sonIguales Piedra Papel = False
sonIguales Papel Papel = True
sonIguales Papel Tijera = False
sonIguales Papel Piedra = False
sonIguales Tijera Tijera = True
sonIguales Tijera Piedra = False
sonIguales Tijera Papel = False

-- b) !!
type Nombre = String 
data Jugador = Mano Nombre Forma

ganador :: Jugador->Jugador->Maybe Nombre
ganador (Mano x1 x2) (Mano y1 y2) | le_gana x2 y2 = Just x1
                                  | le_gana y2 x2 = Just y1
                                  | otherwise = Nothing
--2)
quien_jugo :: Forma -> [Jugador]->[Nombre]
quien_jugo _ [] = []
quien_jugo y ((Mano x1 y1):js) | sonIguales y1 y = x1 : (quien_jugo y js)
                               | otherwise = (quien_jugo y js)


--3) 
data NotaMusical = Do | Re |Mi |Fa |Sol |La |Si deriving Eq
data Figura = Negra | Corchea deriving Eq

data Melodia = Void | Entonar NotaMusical Figura Melodia deriving Eq

contar_tiempos :: Melodia -> Int
contar_tiempos Void = 0
contar_tiempos (Entonar _ Negra la)  = 2 + contar_tiempos(la)
contar_tiempos (Entonar _ Corchea la) = 1 + contar_tiempos(la)   
                                

-- proof=   contar_tiempos Entonar Re Negra (Entonar Mi Corchea (Entonar Fa Negra (Entonar Mi Negra Void)))
--4)

data Arbol a = Hoja | Rama ( Arbol a ) a ( Arbol a ) deriving (Show)


arbolSum :: Arbol Int -> Arbol Int ->Arbol Int
arbolSum Hoja bs = bs
arbolSum as Hoja = as
arbolSum (Rama l1 c1 r1) (Rama l2 c2 r2 ) = Rama (arbolSum l1 l2) (c1+c2) (arbolSum r1 r2) 

-- no anda por error de pattern creo que el algoritmos que estoy escrbiendo tiene sentido pero no lo estoy escrbiendo bien

--arbolSum (Rama x1 i x2) ((Hoja) j (Hoja)) = Rama (arbolSum x1 Hoja ) j+i (arbolSum x2 Hoja) 
--hace lo mismo que el segundo caso asique creo no hace falta 

---------------------------------------------Parcial 2022 TEMA D ---------------------------------------------------------
--1) 
data Color = Rojo |Negro  |Azul deriving (Show)
mismo_color :: Color->Color->Bool
mismo_color Rojo Rojo = True
mismo_color Rojo Azul = False
mismo_color Rojo Negro = False
mismo_color Azul Azul = True
mismo_color Azul Rojo = False
mismo_color Azul Negro = False
mismo_color Negro Negro = True
mismo_color Negro Rojo = False
mismo_color Negro Azul = False

data Tipo = Auto |Moto |Camion 
data Vehiculo = Const Tipo Color
pintar_auto :: Vehiculo ->Color ->Maybe Vehiculo
pintar_auto (Const Moto _) c = Nothing
pintar_auto (Const Camion _ ) c= Nothing
pintar_auto (Const Auto _ ) c= Just (Const Auto c)


-- 2 ) 
solo_de_color :: Color ->[Vehiculo]->[Tipo]
solo_de_color _ [] = []
solo_de_color c ((Const x y):vs)| mismo_color c y = x:(solo_de_color c vs)
                                | otherwise = solo_de_color c vs


-- 3) !!

type Precio = Int 
type Name = String
data Producto = Item Name Precio
data Compra = Nada | AgregaProd Producto Int Compra



costo :: Compra->Precio 
costo Nada = 0
costo (AgregaProd (Item _ p) y lc) = p *y + costo lc

--costo (AgregaProd x y lc) = (getPresio x *y )+ costo lc
getPresio :: Producto ->Precio
getPresio (Item _ b) = b

--proof : getPresio AgregaProd Item "jabon" 25 3 Nada
-- no matchea 
-- no se entrar a presio sin usar get presio 

--4) !!

--arbolBusca :: Arbol (String, Int)-> Int ->Maybe String

--arbolBusca Hoja _ = Nothing
--arbolBusca (Rama x1 (i1,i2) x2) k | k==i2  = Just i1
--                                  | k< i2 =  arbolBusca x1 k 
--                                 | otherwise = arbolBusca x2 k
-- esto solo funciona si el arbol esta ordenado

-- para un arbol desordenado hacemos esto
arbolBusca :: Arbol (Int, String) -> Int -> Maybe String
arbolBusca Hoja _ = Nothing
arbolBusca (Rama l (x, ws) r) k
    | x == k = Just ws
    | otherwise = case arbolBusca l k of
        Just ws' -> Just ws'
        Nothing -> arbolBusca r k









----------------------------------------------PARCIAL AÑO 2022 TEMA B ----------------------------------------------------------


--1) 
data Palo = Espada | Basto | Oro | Copa  


mismoPalo :: Palo->Palo->Bool
mismoPalo Espada Espada = True
mismoPalo Basto Basto = True
mismoPalo Oro Oro = True
mismoPalo Copa Copa = True
mismoPalo _ _ = False

data Fig = Uno|Dos|Tres|Cuatro|Cinco|Seis|Siete|Sota|Caballo|Rey


valorFigura :: Fig->Int
valorFigura Uno = 1
valorFigura Dos = 2
valorFigura Tres = 3
valorFigura Cuatro = 4
valorFigura Cinco = 5
valorFigura Seis = 6
valorFigura Siete = 7
valorFigura Sota = 8
valorFigura Caballo = 9
valorFigura Rey = 10



data Carta = Naipe Fig Palo

sumaCartas :: Carta -> Carta-> Maybe Int
sumaCartas (Naipe f1 p1) (Naipe f2 p2) | mismoPalo p1 p2 = Just (valorFigura f1 + valorFigura f2)
                                       | otherwise = Nothing


-- 2) !!

compatibles :: Carta ->[Carta]->[Fig]
compatibles _ [] = []
compatibles (Naipe f1 p1) (Naipe f2 p2 : cs) | mismoPalo p1 p2 = f2 : compatibles (Naipe f1 p2) cs
                                             | otherwise = compatibles (Naipe f1 p2) cs 



-- 3)

type Duracion = Int 
type Nome = String


data Cancion = Tema Nome Duracion

data Estado = Escuchado | Noescuchado
data Playlist = Empty | EnLista Cancion Estado Playlist

tiempoReproduccion :: Playlist ->Int
tiempoReproduccion Empty = 0 
tiempoReproduccion (EnLista (Tema _ d) Escuchado pl) = d + tiempoReproduccion pl
tiempoReproduccion (EnLista _ Noescuchado pl) =  tiempoReproduccion pl


-- 4)

a_podar :: Arbol a -> Arbol a
--a_podar Hoja = undefined --asi cubre todos los casos pero el ejercicio no lo pide xd 
a_podar (Rama Hoja c Hoja) = Hoja
a_podar (Rama l c r) = Rama (a_podar l) c (a_podar r)



-----------------------------------------------Tema C PARCIAL 2022-----------------------------------------------------------------------------
-- START : 15:21

data Numeracion= One | Two |Three |Four deriving Show

misma_numeracion :: Numeracion ->Numeracion -> Bool
misma_numeracion One One = True
misma_numeracion Two Two = True
misma_numeracion Three Three = True
misma_numeracion _ _ = False


data Domino = Ficha Numeracion Numeracion

encajar :: Domino ->Domino ->Maybe Numeracion
encajar (Ficha _ n2)(Ficha n3 n4) | misma_numeracion n2 n3 = Just n4
                                   | otherwise = Nothing

--2)
comp :: [Domino]-> Numeracion ->[Numeracion]
comp [] _ = []
comp (Ficha n1 n2 : fs) n | misma_numeracion n n1 = n2: comp fs n
                                 | otherwise = comp fs n 

-- 3)
type Evento = String
data Categoria = Cumple | Reunion |Otro
data Calendario = SinEventos | Agendar Evento Categoria Calendario

listar_reuniones :: Calendario -> [Evento]
listar_reuniones SinEventos = []
listar_reuniones (Agendar e Reunion cs ) = e: listar_reuniones cs
listar_reuniones (Agendar _ _ cs ) =  listar_reuniones cs

--4) 

--data Arbol a = Hoja | Rama ( Arbol a ) a ( Arbol a ) deriving (Show)
a_min :: Ord a => Arbol a -> a 
--a_min Hoja = undefined
a_min (Rama Hoja c Hoja) = c
a_min (Rama r c Hoja) = c `min` a_min r
a_min (Rama Hoja c l) = c `min` a_min l
a_min (Rama l c r) = c `min`  (a_min r) `min` (a_min l)
-- end 15:49

--timer without proof : 28 Mins
