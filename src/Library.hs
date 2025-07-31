module Library where
import PdePreludat


--PRACTICA HASKELL
--SAPHo
--Sistema de Administracion de la Propiedad Horizontal
-- ¡Las expensas se disparan!... Los consorcios de los edificios de Ciudad Batracia están 
-- teniendo complicaciones para entender el por qué de las cosas que suceden en sus aposentos 
-- y queremos ayudarlos a marearse un poco más.
-- De cada edificio se conoce la composición de pisos que tiene, y de cada uno de estos, a su vez, 
-- los departamentos que lo componen. Además, de un edificio, se conoce el valor base por metro 
-- cuadrado y un coeficiente de robustez que va de 0 a 1.
-- De los departamentos de cada piso se conoce su superficie en metros cuadrados y un porcentaje 
-- de habitabilidad, que va de 0 a 100.
--
-- Notas: 
-- Modelar lo consignado a continuación, maximizando el uso de orden superior, aplicación parcial y 
-- composición. 
-- Se prefieren soluciones sin recursividad. En caso de usarla, la resolución será tomada en cuenta 
-- pero el puntaje será parcial.
-- Tip: puede ser útil el uso de la función zip, que dadas dos listas retorna una lista de 
-- pares (tuplas) con ambos primeros elementos, ambos segundos, etc. La lista más corta determina 
-- la cantidad final de pares de la lista resultante.

cambiarElemento posicion elemento lista = 
    take (posicion - 1) lista ++ [ elemento ] ++ drop posicion lista

--1)Generar el modelo que represente los edificios y los departamentos. 
--  Si se encuentra alguna otra abstracción útil, incluirla.
data Edificio = Edificio {
    pisos :: [Piso],
    valorM2 :: Number,
    coefRob :: Number
} deriving (Show, Eq)

data Piso = Piso {
    departamentos :: [Departamento]
} deriving (Show, Eq)

data Departamento = Departamento {
    superficie :: Number,
    habitabilidad :: Number
} deriving (Show, Eq)

--2)
--   Conocimientos de edificios:
--a) Cheto: decimos que un edificio es cheto, cuando todos sus pisos tienen un único departamento.

esCheto :: Edificio -> Bool
esCheto (Edificio pisos _ _) = all ((==1).length.departamentos) pisos

--b) Pajarera: Cuando los pisos tienen al menos 6 departamentos cada uno.

esPajarera :: Edificio -> Bool
esPajarera (Edificio pisos _ _) = all ((>=6).length.departamentos) pisos

--c) Pirámide: Cuando cada piso tiene menos departamentos que el piso inmediato inferior.

esPiramide :: Edificio -> Bool
esPiramide (Edificio pisos _ _) = all (\(x,y) -> length (departamentos x) > length (departamentos y)) paresPisos
    where paresPisos = zip pisos (tail pisos)

tipoEdificio condicion (Edificio pisos _ _) = all (condicion.cantidadDepartamentos) pisos

--cantidadDepartamentos piso = length.departamentos $ piso
cantidadDepartamentos = length.departamentos

--esCheto2 edificio = tipoEdificio (==1) edificio
esCheto2  = tipoEdificio (==1) 

--esPajarera2 edificio = tipoEdificio (>=6) edificio
esPajarera2 = tipoEdificio (>=6)

esPiramide2 (Edificio pisos _ _) = all (\(x,y) -> cantidadDepartamentos x > cantidadDepartamentos y) paresPisos
    where paresPisos = zip pisos (tail pisos)

--3)
--  Conocer el precio del departamento más caro de un edificio, según su superficie y el valor base del 
--  metro cuadrado del edificio, multiplicado por el coeficiente de robustez del mismo.

--  El departamento mas caro sera el de mayor superficie, ya que para todos el valor base del metro cuadrado
--  y el coeficiente de robustez del edificio es el mismo

precioMasCaroDepto :: Edificio -> Number
precioMasCaroDepto (Edificio pisos valorM2 coefRob) = (coefRob*).(valorM2*).supDeptoMasGrande $ deptos
    where deptos = concat $ map departamentos pisos

--deptoMasGrande departamentos = maximum.map superficie $ departamentos
supDeptoMasGrande :: [Departamento] -> Number
supDeptoMasGrande = maximum.map superficie

--foldl1 compararSuperficie departamentos
--compararSuperficie dpto1 dpto2 
--    | superficie dpto1 > superficie dpto2 = dpto1
--    | otherwise = dpto2

--4)
--  Remodelaciones... como somos cool, las nombramos en inglés:
--  a) Merge: Dada una lista de departamentos, nos devuelve uno nuevo “unificado”, 
--     con la superficie total de los anteriores y la habitabilidad promedio.

merge :: [Departamento] -> Departamento
merge departamentos = Departamento {
    superficie = superficieTotal,
    habitabilidad = habitabilidadProm
}
    where 
        superficieTotal = sum . map superficie $ departamentos
        habitabilidadProm = (/length departamentos) . sum . map habitabilidad $ departamentos

--  b) Split: Dado una cantidad y un departamento, nos da una lista de departamentos resultantes
--     de dividir el anterior en esa cantidad, con la superficie homogénea y la misma habitabilidad.

split cantidad departamento = replicate cantidad $ departamento {superficie= superficie departamento / cantidad}

--5) Las catástrofes están a la orden día en Ciudad Batracia y afectan a los edificios,
--   por lo que no podemos omitir sus efectos en nuestro modelo:
type Catastrofe = Edificio -> Edificio

--a) Incendio: Se produce desde un piso en particular, afectando a este y todos los pisos superiores. 
--   Reduce la habitabilidad de los departamentos afectados en 30 puntos porcentuales y la robustez del edificio se reduce a la mitad.

incendio nroPisoAfectado edificio = edificio{
    coefRob = coefRob edificio / 2,
    pisos = pisosSinAfectar ++ pisosAfectados
}
    where 
        pisosAfectados = map afectarPiso $ drop (nroPisoAfectado - 1) (pisos edificio)
        pisosSinAfectar = take (nroPisoAfectado - 1) (pisos edificio)

afectarPiso piso = piso {
    departamentos = map afectarDepartamento $ departamentos piso
}

afectarDepartamento = reducirHab 30

reducirHab efecto departamento = departamento {
    habitabilidad= max 0 (habitabilidad departamento - efecto)
}


--b) Plaga: La plaga afecta a un piso del edificio dado por su número y reduce la habitabilidad 
--   de sus departamentos en una cantidad de puntos porcentuales variable.

plaga nroPisoAfectado redHabitabilidad edificio = edificio{
    pisos = pisosAbajo ++ [pisoAfectado] ++ pisosArriba
}
    where
        pisoAfectado = plagarPiso redHabitabilidad $ pisos edificio !! nroPisoAfectado
        pisosArriba = drop nroPisoAfectado $ pisos edificio
        pisosAbajo = take (nroPisoAfectado - 1) $ pisos edificio

plagarPiso efecto piso = piso {
    departamentos = map (reducirHab efecto) $ departamentos piso
}
--plaga usando cambiarElemento
plaga2 nroPisoAfectado redHabitabilidad edificio = edificio {
    pisos = cambiarElemento nroPisoAfectado pisoPlagado $ pisos edificio
}
    where
        pisoPlagado = plagarPiso redHabitabilidad $ pisos edificio !! nroPisoAfectado

--c) Terremoto: Reduce la robustez del edificio en un valor indicado.

terremoto redRobustez edificio = edificio {
    coefRob = max 0 (coefRob edificio - redRobustez)
}

--6)
--a) Ampliación: se realiza la construcción de un nuevo piso con una determinada cantidad de departamentos 
--   y de metros, que se reparten equitativamente entre los departamentos. El piso se agrega arriba, 
--   porque las máquinas para levantar los N pisos superiores de un edificio las tenemos descompuestas 
--   y no podemos meterlo en el medio. Al ser nuevo, su habitabilidad es de 100.

ampliacion cantDeptos metrosTotal edificio = edificio {
    pisos = pisos edificio ++ [nuevoPiso]
}
    where
        nuevoPiso = Piso {departamentos= split cantDeptos Departamento {superficie=metrosTotal, habitabilidad=100}}
        --nuevoPiso = generarPiso cantDeptos metrosTotal

generarPiso cantDeptos metrosTotal = Piso {
    departamentos = replicate cantDeptos $ Departamento {superficie= metrosTotal / cantDeptos, habitabilidad=100}
}


--b) Fumigación: A cada departamento que tiene habitabilidad menor a 60%, les sube en 20 puntos porcentuales.

fumigacion edificio = edificio {
    pisos = fumigarPisos $ pisos edificio
}

fumigarPisos pisos = map fumigarDeptos pisos

fumigarDeptos piso = piso {
    departamentos = map necesitaFumigacion $ departamentos piso
}

necesitaFumigacion departamento 
    | habitabilidad departamento < 60 = departamento {habitabilidad = habitabilidad departamento + 20}
    | otherwise = departamento

--c) MergeEdificio: Debe aplicar un merge sobre un número de piso dado de un edificio.

mergeEdificio nroPiso edificio = edificio {
    pisos = cambiarElemento nroPiso nuevoPiso $ pisos edificio 
}
    where
        pisoMergeado = merge $ departamentos $ pisos edificio !! (nroPiso - 1)
        nuevoPiso = Piso { departamentos = [pisoMergeado]}

--d) SplitEdificio: Recibe una cantidad de nuevos departamentos y el número de piso donde debe hacer 
--   un split sobre el último departamento. 

splitEdificio cantDeptos nroPiso edificio = edificio {
    pisos = cambiarElemento nroPiso nuevoPiso $ pisos edificio
 }
    where
        pisoAfectado = pisos edificio !! (nroPiso - 1)
        deptoAfectado = split cantDeptos . last $ departamentos pisoAfectado
        nuevoPiso = pisoAfectado {departamentos = init (departamentos pisoAfectado) ++ deptoAfectado}

--7
--Dada la siguiente función, determine y explique su tipo:
--funcionLoca a b c = 
--	all ((>c) . fst a) . foldl (\x y -> b y . snd a $ x) []

-- la funcion quiere determinar si todos los elementos de una lista cumplen con una condicion
-- en este caso se usa la funcion foldl para a partir de una lista dada generar un resultado acumulado
-- de la misma a partir de una funcion acumuladora
-- (\x y -> b y . snd a $ x) es una funcion anonima donde a x se le aplica la funcion compuesta b y .snd a
-- para ello pasa como parametro al elemento x a la funcion que es segundo elemento de la tupla a. A ese 
-- resultado lo compone con la funcion b y. 
-- sobre el resultado del foldl, verifica que todos sus elementos cumplan con una condicion: ((>c) . fst a)
-- La condicion implica que todos los elementos resultantes del foldl tienen que ser mayores que c 
-- despues de haberles aplicado la funcion que es primer elemento de la tupla a 

