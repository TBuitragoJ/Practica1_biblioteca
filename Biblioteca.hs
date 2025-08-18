import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Maybe (mapMaybe)
import System.Directory (doesFileExist)

-- Definición del tipo de datos para libros disponibles en la biblioteca
data LibroDisponible = LibroDisponible {
    id_disp :: String,
    titulo :: String,
    genero :: String,
    descripcion :: String
} deriving (Show, Read)

-- Definición del tipo de datos para representar la información de un préstamo
data Libro = Libro {
    id_libro :: String,
    usuario :: String,  -- Nombre y apellido del usuario
    entrada :: UTCTime,
    fecha_limite :: UTCTime,  -- Fecha límite para devolver el libro
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el libro aún está en prestamo o ya esta libre
} deriving (Show, Read)

-- Banco de libros disponibles (50 libros, 10 por género)
bancoLibros :: [LibroDisponible]
bancoLibros = [
    -- Acción (10 libros)
    LibroDisponible "AC001" "El Gladiador de Roma" "accion" "Un guerrero luchando por su libertad en el Coliseo Romano.",
    LibroDisponible "AC002" "Operación Tormenta" "accion" "Misión secreta de élite para salvar al mundo de una amenaza nuclear.",
    LibroDisponible "AC003" "El Último Samurai" "accion" "Honor y sacrificio en el Japón feudal durante las guerras civiles.",
    LibroDisponible "AC004" "Furia en las Calles" "accion" "Un policía renegado persigue a la mafia en las calles de Nueva York.",
    LibroDisponible "AC005" "Comando Especial" "accion" "Soldados de élite en una misión imposible tras las líneas enemigas.",
    LibroDisponible "AC006" "El Cazador de Dragones" "accion" "Épica batalla contra criaturas míticas para salvar el reino.",
    LibroDisponible "AC007" "Velocidad Mortal" "accion" "Carreras clandestinas donde cada curva puede ser la última.",
    LibroDisponible "AC008" "El Vengador Silencioso" "accion" "Un asesino busca justicia contra quienes mataron a su familia.",
    LibroDisponible "AC009" "Guerra en el Espacio" "accion" "Batalla épica entre flotas espaciales por el destino de la galaxia.",
    LibroDisponible "AC010" "El Guerrero Perdido" "accion" "Un luchador amnésico descubre su pasado mientras lucha por sobrevivir.",
    
    -- Aventura (10 libros)
    LibroDisponible "AV001" "La Isla del Tesoro Perdido" "aventura" "Búsqueda de un tesoro legendario en una isla misteriosa.",
    LibroDisponible "AV002" "Expedición al Centro de la Tierra" "aventura" "Viaje épico hacia las profundidades del planeta.",
    LibroDisponible "AV003" "El Mapa de los Siete Mares" "aventura" "Piratas navegando en busca del mayor tesoro jamás perdido.",
    LibroDisponible "AV004" "La Selva Prohibida" "aventura" "Exploración de una selva llena de peligros y secretos ancestrales.",
    LibroDisponible "AV005" "El Vuelo del Águila Dorada" "aventura" "Aventura aérea en busca de una ciudad perdida en las montañas.",
    LibroDisponible "AV006" "Naufragios y Supervivencia" "aventura" "Lucha por la supervivencia en una isla desierta tras un naufragio.",
    LibroDisponible "AV007" "El Sendero del Explorador" "aventura" "Expedición a través de territorios inexplorados de África.",
    LibroDisponible "AV008" "La Cueva de los Cristales" "aventura" "Descubrimiento de una cueva mágica llena de cristales poderosos.",
    LibroDisponible "AV009" "Aventuras en el Amazonas" "aventura" "Exploración del río más misterioso del mundo.",
    LibroDisponible "AV010" "El Secreto de la Pirámide" "aventura" "Arqueólogos desentrañan los misterios de una pirámide egipcia.",
    
    -- Historia (10 libros)
    LibroDisponible "HI001" "Los Secretos de Cleopatra" "historia" "La vida y conspiraciones de la última reina de Egipto.",
    LibroDisponible "HI002" "El Imperio de Napoleón" "historia" "Ascenso y caída del emperador que conquistó Europa.",
    LibroDisponible "HI003" "Los Vikingos del Norte" "historia" "Sagas épicas de los guerreros nórdicos y sus conquistas.",
    LibroDisponible "HI004" "La Revolución Francesa" "historia" "Los eventos que cambiaron Francia y el mundo para siempre.",
    LibroDisponible "HI005" "El Imperio Romano" "historia" "Grandeza y decadencia del imperio más poderoso de la antigüedad.",
    LibroDisponible "HI006" "Los Caballeros Templarios" "historia" "Misterios y secretos de la orden religiosa más poderosa.",
    LibroDisponible "HI007" "La Guerra de los Cien Años" "historia" "Conflicto épico entre Francia e Inglaterra en la Edad Media.",
    LibroDisponible "HI008" "Los Aztecas de México" "historia" "Civilización precolombina y su encuentro con los conquistadores.",
    LibroDisponible "HI009" "La Ruta de la Seda" "historia" "Comercio y cultura a lo largo de la ruta comercial más famosa.",
    LibroDisponible "HI010" "Los Faraones de Egipto" "historia" "Dinastías y secretos de los gobernantes del antiguo Egipto.",
    
    -- Ciencia (10 libros)
    LibroDisponible "CI001" "El Universo Cuántico" "ciencia" "Introducción a los misterios de la física cuántica moderna.",
    LibroDisponible "CI002" "Darwin y la Evolución" "ciencia" "La teoría que revolucionó nuestra comprensión de la vida.",
    LibroDisponible "CI003" "El ADN y la Genética" "ciencia" "Descubrimientos sobre el código de la vida.",
    LibroDisponible "CI004" "Los Agujeros Negros" "ciencia" "Fenómenos más misteriosos del universo explicados.",
    LibroDisponible "CI005" "La Teoría de la Relatividad" "ciencia" "Einstein y su revolucionaria visión del tiempo y espacio.",
    LibroDisponible "CI006" "El Cambio Climático" "ciencia" "Causas, efectos y soluciones para el calentamiento global.",
    LibroDisponible "CI007" "Neurociencia del Cerebro" "ciencia" "Cómo funciona el órgano más complejo del cuerpo humano.",
    LibroDisponible "CI008" "La Química de la Vida" "ciencia" "Procesos químicos que hacen posible la existencia.",
    LibroDisponible "CI009" "Exploración de Marte" "ciencia" "Misiones espaciales y búsqueda de vida en el planeta rojo.",
    LibroDisponible "CI010" "El Origen del Universo" "ciencia" "Teorías sobre el Big Bang y la formación del cosmos.",
    
    -- Tecnología (10 libros)
    LibroDisponible "TE001" "Inteligencia Artificial" "tecnologia" "El futuro de las máquinas que piensan como humanos.",
    LibroDisponible "TE002" "La Era de Internet" "tecnologia" "Cómo la red mundial cambió la civilización humana.",
    LibroDisponible "TE003" "Robótica Avanzada" "tecnologia" "Robots que trabajan junto a los humanos del futuro.",
    LibroDisponible "TE004" "Realidad Virtual" "tecnologia" "Mundos digitales que parecen más reales que la realidad.",
    LibroDisponible "TE005" "Blockchain y Criptomonedas" "tecnologia" "La tecnología que está revolucionando las finanzas.",
    LibroDisponible "TE006" "Computación Cuántica" "tecnologia" "Ordenadores que utilizan las leyes de la física cuántica.",
    LibroDisponible "TE007" "Biotecnología Médica" "tecnologia" "Tecnología que está curando enfermedades incurables.",
    LibroDisponible "TE008" "Energías Renovables" "tecnologia" "Tecnologías limpias para un futuro sostenible.",
    LibroDisponible "TE009" "Nanotecnología" "tecnologia" "Ciencia a escala atómica para crear materiales impossibles.",
    LibroDisponible "TE010" "Ciudades Inteligentes" "tecnologia" "Urbes del futuro conectadas y automatizadas."
    ]

-- Función para registrar el prestamo de un Libro
registrarEntrada :: String -> String -> UTCTime -> UTCTime -> [Libro] -> [Libro]
registrarEntrada idLibro nombreUsuario tiempo fechaLimite prestamo =
    Libro idLibro nombreUsuario tiempo fechaLimite Nothing : prestamo

-- Función para registrar la devolucion de un Libro
registrarSalida :: String -> UTCTime -> [Libro] -> [Libro]
registrarSalida idLibro tiempo prestamo =
    map (\v -> if idLibro == id_libro v then v { salida = Just tiempo } else v) prestamo

-- Función para buscar un Libro por su nombre
buscarLibro :: String -> [Libro] -> Maybe Libro
buscarLibro idLibro prestamo =
    find (\v -> idLibro == id_libro v && isNothing (salida v)) prestamo
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un Libro permanecio en prestamo
tiempoEnprestamo:: Libro -> UTCTime -> NominalDiffTime
tiempoEnprestamo libro tiempoActual =
    case salida libro of
        Just tiempoSalida -> diffUTCTime tiempoSalida (entrada libro)
        Nothing           -> diffUTCTime tiempoActual (entrada libro)

-- Función para guardar la información de los Libros en un archivo de texto
guardarprestamo :: [Libro] -> IO ()
guardarprestamo prestamo = do
    resultado <- try (writeFile "Biblioteca.txt" (unlines (map mostrarLibro prestamo))) :: IO (Either IOException ())
    case resultado of
        Left ex -> putStrLn $ "Error guardando el prestamo: " ++ show ex
        Right _ -> putStrLn "Prestamo guardado en el archivo Biblioteca.txt."

-- Función para reintentar una operación en caso de error
reintentar :: Int -> IO a -> IO (Either IOException a)
reintentar 0 accion = catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
reintentar n accion = do
    resultado <- catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
    case resultado of
        Left ex -> do
            threadDelay 1000000  -- Esperar 1 segundo antes de reintentar
            reintentar (n - 1) accion
        Right val -> return (Right val)

-- Función para cargar la información de los prestamos desde un archivo de texto
cargarprestamo :: IO [Libro]
cargarprestamo = do
    existe <- doesFileExist "Biblioteca.txt"
    if not existe
        then return []
        else do
            resultado <- try (do
                contenido <- readFile "Biblioteca.txt"
                length contenido `seq` return contenido) :: IO (Either IOException String)
            case resultado of
                Left ex -> do
                    putStrLn $ "Error cargando el prestamo: " ++ show ex
                    return []
                Right contenido -> do
                    let lineas = filter (not . null) (lines contenido)
                    return (mapMaybe parsearLibroCSV lineas)
    where
        parsearLibroCSV :: String -> Maybe Libro
        parsearLibroCSV linea =
            let indices = findCommaIndices linea
            in case indices of
                [idx1, idx2, idx3, idx4] -> 
                    let idStr = take idx1 linea
                        usuarioStr = take (idx2 - idx1 - 1) (drop (idx1 + 1) linea)
                        entradaStr = take (idx3 - idx2 - 1) (drop (idx2 + 1) linea)
                        fechaLimiteStr = take (idx4 - idx3 - 1) (drop (idx3 + 1) linea)
                        salidaStr = drop (idx4 + 1) linea
                    in case (reads entradaStr, reads fechaLimiteStr) of
                        ([(entrada, "")], [(fechaLimite, "")]) -> Just $ Libro idStr usuarioStr entrada fechaLimite (parseSalida salidaStr)
                        _ -> Nothing
                _ -> Nothing
        
        findCommaIndices :: String -> [Int]
        findCommaIndices str = map fst $ filter ((== ',') . snd) $ zip [0..] str
        
        parseSalida :: String -> Maybe UTCTime
        parseSalida "Nothing" = Nothing
        parseSalida salidaStr = case reads salidaStr of
            [(salida, "")] -> Just salida
            _ -> Nothing
        
        readMaybe :: Read a => String -> Maybe a
        readMaybe s = case reads s of
            [(x, "")] -> Just x
            _ -> Nothing

-- Función para mostrar la información de un Libro como cadena de texto
mostrarLibro :: Libro -> String
mostrarLibro libro =
    id_libro libro ++ "," ++ usuario libro ++ "," ++ show (entrada libro) ++ "," ++ show (fecha_limite libro) ++ "," ++ show (salida libro)

-- Función para cargar la información de los Libros desde un archivo de texto (formato antiguo - no usar)
leerLibro:: IO [Libro]
leerLibro = do
    contenido <- readFile "Biblioteca.txt"
    let lineas = lines contenido
    return (mapMaybe parsearLibro lineas)
    where
        parsearLibro :: String -> Maybe Libro
        parsearLibro linea = case words linea of
            [id, usuario, entrada, fechaLimite, salida] -> 
                case (reads entrada, reads fechaLimite) of
                    ([(ent, "")], [(fechaLim, "")]) -> Just $ Libro id usuario ent fechaLim (readMaybeSalida salida)
                    _ -> Nothing
            _ -> Nothing

        readMaybeSalida :: String -> Maybe UTCTime
        readMaybeSalida "Nothing" = Nothing
        readMaybeSalida salidaStr = case reads salidaStr of
            [(salida, "")] -> Just salida
            _ -> Nothing




-- Función para mostrar libros por género
mostrarLibrosPorGenero :: String -> IO ()
mostrarLibrosPorGenero generoSeleccionado = do
    let librosDelGenero = filter (\libro -> genero libro == generoSeleccionado) bancoLibros
    putStrLn $ "\n=== LIBROS DE " ++ map (\c -> if c == 'a' then 'A' else c) generoSeleccionado ++ " ==="
    mapM_ (\libro -> do
        putStrLn $ "ID: " ++ id_disp libro
        putStrLn $ "Título: " ++ titulo libro
        putStrLn $ "Descripción: " ++ descripcion libro
        putStrLn "---"
        ) librosDelGenero
    putStrLn $ "Total: " ++ show (length librosDelGenero) ++ " libros disponibles"

-- Función para mostrar el menú de géneros
menuGeneros :: IO ()
menuGeneros = do
    putStrLn "\n=== CATÁLOGO DE LIBROS POR GÉNERO ==="
    putStrLn "1. Acción"
    putStrLn "2. Aventura" 
    putStrLn "3. Historia"
    putStrLn "4. Ciencia"
    putStrLn "5. Tecnología"
    putStrLn "6. Volver al menú principal"
    putStrLn "Seleccione un género:"
    opcion <- getLine
    case opcion of
        "1" -> mostrarLibrosPorGenero "accion"
        "2" -> mostrarLibrosPorGenero "aventura"
        "3" -> mostrarLibrosPorGenero "historia"
        "4" -> mostrarLibrosPorGenero "ciencia"
        "5" -> mostrarLibrosPorGenero "tecnologia"
        "6" -> return ()
        _ -> do
            putStrLn "Opción no válida."
            menuGeneros

-- Función para el ciclo principal del programa
cicloPrincipal :: [Libro] -> IO ()
cicloPrincipal prestamo = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Ver catálogo de libros por género"
    putStrLn "2. Registrar prestamo de un Libro"
    putStrLn "3. Registrar devolucion de un Libro"
    putStrLn "4. Buscar Libro por genero y id"
    putStrLn "5. Listar los Libros prestados"
    putStrLn "6. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            menuGeneros
            cicloPrincipal prestamo
            
        "2" -> do
            putStrLn "Ingrese su nombre y apellido:"
            nombreUsuario <- getLine
            putStrLn "Ingrese el ID del libro:"
            idLibro <- getLine
            putStrLn "Ingrese por cuántos días desea el préstamo:"
            diasStr <- getLine
            let dias = read diasStr :: Integer
            tiempoActual <- getCurrentTime
            let fechaLimite = addUTCTime (fromInteger (dias * 24 * 60 * 60)) tiempoActual
            let prestamosActualizados = registrarEntrada idLibro nombreUsuario tiempoActual fechaLimite prestamo
            putStrLn $ "Libro " ++ idLibro ++ " prestado a " ++ nombreUsuario ++ " por " ++ show dias ++ " días."
            putStrLn $ "Fecha límite de devolución: " ++ show fechaLimite
            guardarprestamo prestamosActualizados
            cicloPrincipal prestamosActualizados

        "3" -> do
            putStrLn "Ingrese el ID del libro a devolver:"
            idLibro <- getLine
            tiempoActual <- getCurrentTime
            let prestamosActualizados = registrarSalida idLibro tiempoActual prestamo
            putStrLn $ "Libro con ID " ++ idLibro ++ " devuelto."
            guardarprestamo prestamosActualizados
            cicloPrincipal prestamosActualizados

        "4" -> do
            putStrLn "Ingrese el ID del libro a buscar:"
            idLibro <- getLine
            case buscarLibro idLibro prestamo of
                Just libro -> do
                    tiempoActual <- getCurrentTime
                    let tiempoTotal = tiempoEnprestamo libro tiempoActual
                    let diasTranscurridos = floor (realToFrac tiempoTotal / (24 * 60 * 60)) :: Integer
                    let tiempoRestante = diffUTCTime (fecha_limite libro) tiempoActual
                    let diasRestantes = floor (realToFrac tiempoRestante / (24 * 60 * 60)) :: Integer
                    putStrLn $ "El libro con ID " ++ idLibro ++ " se encuentra en préstamo."
                    putStrLn $ "Usuario: " ++ usuario libro
                    putStrLn $ "Días transcurridos: " ++ show diasTranscurridos
                    putStrLn $ "Días restantes: " ++ show diasRestantes
                    putStrLn $ "Fecha límite: " ++ show (fecha_limite libro)
                Nothing -> putStrLn "El libro se encuentra disponible en este momento."
            cicloPrincipal prestamo
        "5" -> do
            putStrLn "Mostrando Lista de Libros en prestamo"
            contenido <- readFile "Biblioteca.txt"
            let lineas = filter (not . null) (lines contenido)
            mapM_ (\linea -> do
                let partes = splitCSV linea
                case partes of
                    [idStr, usuarioStr, entradaStr, fechaLimiteStr, salidaStr] ->
                        putStrLn $ "ID: " ++ idStr ++ ", Usuario: " ++ usuarioStr ++ ", Entrada: " ++ entradaStr ++ ", Fecha Límite: " ++ fechaLimiteStr ++ ", Salida: " ++ salidaStr
                    _ -> putStrLn $ "Error parseando: " ++ linea
                ) lineas
            putStrLn $ "Total de libros en préstamo: " ++ show (length lineas)
            cicloPrincipal prestamo




        "6" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal prestamo

-- Función simple para dividir CSV
splitCSV :: String -> [String]
splitCSV [] = []
splitCSV str = 
    let (before, remainder) = break (== ',') str
    in before : case remainder of
        [] -> []
        _:after -> splitCSV after

-- Función para parsear una línea CSV simple
parsearLineaCSV :: String -> Maybe Libro
parsearLineaCSV linea = 
    case splitCSV linea of
        [idStr, usuarioStr, entradaStr, fechaLimiteStr, salidaStr] ->
            case (reads entradaStr, reads fechaLimiteStr) of
                ([(entrada, "")], [(fechaLimite, "")]) -> Just $ Libro idStr usuarioStr entrada fechaLimite (parseSalidaCSV salidaStr)
                _ -> Nothing
        _ -> Nothing
    where
        parseSalidaCSV "Nothing" = Nothing
        parseSalidaCSV salidaStr = case reads salidaStr of
            [(salida, "")] -> Just salida
            _ -> Nothing

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar el parqueadero desde el archivo de texto
    prestamo <- cargarprestamo
    putStrLn "¡Bienvenido al Sistema de prestamos de la biblioteca!"

    -- Ciclo principal del programa
    cicloPrincipal prestamo

