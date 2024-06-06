module Library where
import PdePreludat


data Plomero = UnPlomero {
    nombre :: String,
    herramientas :: [Herramienta],
    reparaciones :: [Reparacion],
    dinero :: Number
} deriving (Show, Eq)

data Herramienta = UnaHerramienta {
    denominacion :: String,
    precio :: Number,
    empuñadura :: Material
} deriving (Show, Eq)

data Material = Hierro | Madera | Goma | Plastico deriving (Show, Eq)

data Reparacion = UnaReparacion {
    descripcion :: String,
    requerimiento :: Plomero -> Bool
} deriving (Show, Eq)

--Punto 1

mario :: Plomero
mario = UnPlomero "Mario" [llaveInglesa, martillo] [] 1200

wario :: Plomero
wario = UnPlomero "Wario" (repeat llaveInglesa) [] 0.50

llaveInglesa :: Herramienta
llaveInglesa = UnaHerramienta "Llave inglesa" 200 Hierro

martillo :: Herramienta
martillo = UnaHerramienta "Martillo" 20 Madera

--Punto 2

tieneHerramienta :: String -> Plomero -> Bool
tieneHerramienta unaDenominacion plomero = any (coincideDenominacion unaDenominacion) (herramientas plomero)

coincideDenominacion :: String -> Herramienta -> Bool
coincideDenominacion unaDenominacion herramienta = unaDenominacion == denominacion herramienta

esMalvado :: Plomero -> Bool
esMalvado = (== "Wa") . take 2 . nombre

puedeComprar :: Herramienta -> Plomero -> Bool
puedeComprar herramienta = (>= precio herramienta) . dinero

--Punto 3

esBuena :: Herramienta -> Bool
esBuena (UnaHerramienta _ precio Hierro) = precio > 10000
esBuena (UnaHerramienta "Martillo" _ empuñadura) = empuñadura `elem` [Madera, Goma]
esBuena herramienta = False

--Punto 4

comprarHerramienta :: Herramienta -> Plomero -> Plomero
comprarHerramienta herramienta plomero
    | puedeComprar herramienta plomero = (agregarHerramienta herramienta . modificarDinero ((-1) * precio herramienta)) plomero
    | otherwise = plomero

modificarDinero :: Number -> Plomero -> Plomero
modificarDinero precio plomero = plomero {dinero = dinero plomero + precio}

agregarHerramienta :: Herramienta -> Plomero -> Plomero
agregarHerramienta herramienta plomero = plomero {herramientas = herramienta : herramientas plomero}

--Punto 5

filtracionDeAgua :: Reparacion
filtracionDeAgua = UnaReparacion "Filtracion de agua" (tieneHerramienta "Llave inglesa")

esDificil :: Reparacion -> Bool
esDificil reparacion = tieneMuchosCaracteres (descripcion reparacion) && esUnGrito (descripcion reparacion)

esUnGrito :: String -> Bool
esUnGrito = all esMayuscula . filter (/= ' ')

esMayuscula :: Char -> Bool
esMayuscula letra = letra `elem` ['A'..'Z']

tieneMuchosCaracteres :: String -> Bool
tieneMuchosCaracteres = (>100) . length

calcularPresupuesto :: Reparacion -> Number
calcularPresupuesto (UnaReparacion descripcion _) = 3 * length descripcion

--Punto 6

realizarReparacion :: Reparacion -> Plomero -> Plomero
realizarReparacion reparacion plomero
    | puedeResolverlo reparacion plomero = (agregarReparacion reparacion . modificarDinero (calcularPresupuesto reparacion)) plomero
    | otherwise = cobrarVisita plomero

puedeResolverlo :: Reparacion -> Plomero -> Bool
puedeResolverlo _ plomero = esMalvado plomero && tieneHerramienta "Martillo" plomero

cambiarHerramientasSegun :: Plomero -> Reparacion -> Plomero
cambiarHerramientasSegun plomero reparacion
    | esMalvado plomero = agregarHerramienta (UnaHerramienta "Destornillador" 0 Plastico) plomero
    | esDificil reparacion = (quitarHerramientas plomero . herramientasBuenas) plomero
    | otherwise = quitarHerramientas plomero [head (herramientas plomero)]

quitarHerramientas :: Plomero -> [Herramienta] -> Plomero
quitarHerramientas plomero listaHerramientas = plomero {herramientas = filter (noEstanEnLaLista listaHerramientas) (herramientas plomero)}

noEstanEnLaLista :: [Herramienta] -> Herramienta -> Bool
noEstanEnLaLista listaHerramientas herramientaPlomero = notElem herramientaPlomero listaHerramientas

herramientasBuenas :: Plomero -> [Herramienta]
herramientasBuenas = filter esBuena . herramientas

agregarReparacion :: Reparacion -> Plomero -> Plomero
agregarReparacion reparacion plomero = plomero {reparaciones = reparacion : reparaciones plomero}

cobrarVisita :: Plomero -> Plomero
cobrarVisita = modificarDinero 100

--Punto 7

jornadaDeTrabajo :: Plomero -> [Reparacion] -> Plomero
jornadaDeTrabajo = foldl (flip realizarReparacion)

--Punto 8

elMasReparador :: [Plomero] -> [Reparacion] -> Plomero
elMasReparador = empleadoMas quienTieneMasReparaciones
quienTieneMasReparaciones :: Plomero -> Plomero -> Plomero
quienTieneMasReparaciones = quienTieneMas cantidadReparaciones

elMasAdinerado :: [Plomero] -> [Reparacion] -> Plomero
elMasAdinerado = empleadoMas quienTieneMasDinero
quienTieneMasDinero:: Plomero -> Plomero -> Plomero
quienTieneMasDinero = quienTieneMas dinero

elMasInvertidor :: [Plomero] -> [Reparacion] -> Plomero
elMasInvertidor = empleadoMas quienTieneMasInvertido
quienTieneMasInvertido:: Plomero -> Plomero -> Plomero
quienTieneMasInvertido = quienTieneMas totalInvertido

empleadoMas :: (Plomero -> Plomero -> Plomero) -> [Plomero] -> [Reparacion] -> Plomero
empleadoMas funcion plomeros reparaciones = foldl1 funcion (plomerosLuedoDeJornada plomeros reparaciones)

quienTieneMas :: (Plomero -> Number) -> Plomero -> Plomero -> Plomero
quienTieneMas funcion plomero1 plomero2
    | funcion plomero1 > funcion plomero2 = plomero1
    | otherwise = plomero2

cantidadReparaciones :: Plomero -> Number
cantidadReparaciones = length . herramientas

totalInvertido :: Plomero -> Number
totalInvertido = sum . map precio . herramientas

plomerosLuedoDeJornada :: [Plomero] -> [Reparacion] -> [Plomero]
plomerosLuedoDeJornada plomeros reparaciones= map (flip jornadaDeTrabajo reparaciones) plomeros