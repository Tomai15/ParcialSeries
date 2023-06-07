-----------
--Punto 1--
-----------

data Serie = Serie{
    nombre :: String,
    actores :: [Actor],
    presupuestoAnual :: Int,
    temporadasEstimadas :: Int,
    ratingPromedio :: Int,
    cancelada :: Bool
}

data Actor = Actor{
    nombreActor :: String,
    sueldoPretendido :: Int,
    restricciones :: [Restriccion]
}

type Restriccion = Bool

serieEnRojo :: Serie -> Bool
serieEnRojo unaSerie =  presupuestoAnual unaSerie < sumarSueldos (actores unaSerie)

sumarSueldos :: [Actor] -> Int
sumarSueldos = sum.map sueldoPretendido

serieProblematica :: Serie -> Bool
serieProblematica unaSerie = (length.mapActoresProblematicos.actores) unaSerie > 3

mapActoresProblematicos :: [Actor] -> [Actor]
mapActoresProblematicos  = filter ((>1).length.restricciones)

-----------
--Punto 2--
-----------

type Productor = Serie -> Serie

conFavoritismo :: [Actor] -> Productor
conFavoritismo unosActores unaSerie = cambiarActores  unaSerie unosActores
    --unaSerie { actores = unosActores ++ drop 2 (actores unaSerie)}

timBurton :: Productor
timBurton unaSerie = cambiarActores unaSerie [johnnyDepp,helenaBonham]

johnnyDepp = Actor "johnny Depp" 20000000 []

helenaBonham = Actor "helena Bonham" 5000000 []

gatopardeitor :: Productor
gatopardeitor unaSerie = unaSerie

estireitor :: Productor
estireitor unaSerie = unaSerie {temporadasEstimadas = temporadasEstimadas unaSerie *2}

desespereitor :: [Serie -> Serie] -> Productor
desespereitor ideas unaSerie = foldr ($) unaSerie ideas 

canceleitor :: Int -> Productor
canceleitor cifraCancelacion unaSerie
    | serieEnRojo unaSerie || ratingPromedio unaSerie < cifraCancelacion = unaSerie {cancelada= True}
    | otherwise = unaSerie

cambiarActores :: Serie -> [Actor] -> Serie
cambiarActores unaSerie actoresAponer = unaSerie {actores = actoresAponer ++ drop 2 (actores unaSerie)}

-----------
--Punto 3--
-----------
{-
bienestarSerie :: Serie -> Int
bienestarSerie unaSerie
    | cancelada unaSerie = 0
    | otherwise = bienestarReparto unaSerie + bienestarLongitud unaSerie

bienestarLongitud :: Serie -> Int
bienestarLongitud unaSerie
    | temporadasEstimadas unaSerie > 4 = 5
    | otherwise = 10-(temporadasEstimadas unaSerie)*2

bienestarReparto :: Serie -> Int
bienestarReparto unaSerie
    | length (actores unaSerie) < 10 = 3
    | otherwise = (10-length (filter (length.restricciones.actores) unaSerie))
-}
-----------
--Punto 4--
-----------

--masBienestar :: [Productor] -> [Serie] -> [Serie]
--masBienestar productores series = map (mejorProductorParaSerie) series

mejorProductorParaSerie :: [Productor] -> Serie -> Serie
mejorProductorParaSerie productores unaSerie = serieConMejorBienestar (map ($unaSerie) productores)

serieConMejorBienestar :: [Serie] -> Serie
serieConMejorBienestar unasSeries = 


dfdsfdsfdsfsd
