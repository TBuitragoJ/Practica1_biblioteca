# 📚 Sistema de Gestión de Biblioteca en Haskell

Un sistema completo de gestión de préstamos de biblioteca desarrollado en Haskell funcional que permite administrar un catálogo de 50 libros organizados por géneros.

---

## 🤖 Notas de desarrollo y herramientas utilizadas

- El desarrollo del código fuente se realizó utilizando **IntelliJ IDEA** como entorno principal de programación.
- Se utilizó **Claude** (IA) para asistir en la subida del código al repositorio y en la creación de este README.

---

## 🚀 Características

- **Catálogo de 50 libros** distribuidos en 5 géneros
- **Navegación por géneros** con descripciones detalladas
- **Gestión de préstamos** con fechas límite en días
- **Control de usuarios** con nombre y apellido
- **Persistencia de datos** en archivo de texto
- **Interfaz de consola** intuitiva

## 📖 Géneros Disponibles

- **Acción** (10 libros): AC001-AC010
- **Aventura** (10 libros): AV001-AV010  
- **Historia** (10 libros): HI001-HI010
- **Ciencia** (10 libros): CI001-CI010
- **Tecnología** (10 libros): TE001-TE010

## 🛠️ Requisitos

- **GHC** (Glasgow Haskell Compiler) 8.0 o superior
- **runhaskell** para ejecución directa

## 🚀 Instalación y Ejecución

### 1. Clonar el repositorio
```bash
git clone https://github.com/TBuitragoJ/Practica1_biblioteca.git
cd Practica1_biblioteca
```

### 2. Ejecutar el programa
```bash
runhaskell Biblioteca.hs
```

### 3. O compilar y ejecutar
```bash
ghc Biblioteca.hs -o biblioteca
./biblioteca
```

## 📋 Guía de Uso Paso a Paso

### Menú Principal
Al ejecutar el programa verás estas opciones:

```
¡Bienvenido al Sistema de prestamos de la biblioteca!

Seleccione una opción:
1. Ver catálogo de libros por género
2. Registrar prestamo de un Libro
3. Registrar devolucion de un Libro
4. Buscar Libro por genero y id
5. Listar los Libros prestados
6. Salir
```

### 🔍 1. Ver Catálogo por Género

**Paso 1:** Selecciona `1` en el menú principal

**Paso 2:** Elige un género:
```
=== CATÁLOGO DE LIBROS POR GÉNERO ===
1. Acción
2. Aventura
3. Historia
4. Ciencia
5. Tecnología
6. Volver al menú principal
```

**Paso 3:** El sistema mostrará todos los libros del género seleccionado:
```
=== LIBROS DE ACCIÓN ===
ID: AC001
Título: El Gladiador de Roma
Descripción: Un guerrero luchando por su libertad en el Coliseo Romano.
---
```

### 📝 2. Registrar Préstamo

**Paso 1:** Selecciona `2` en el menú principal

**Paso 2:** Ingresa tu información personal:
```
Ingrese su nombre y apellido:
> Juan Pérez
```

**Paso 3:** Ingresa el ID del libro (consultado previamente en el catálogo):
```
Ingrese el ID del libro:
> AC001
```

**Paso 4:** Especifica la duración del préstamo:
```
Ingrese por cuántos días desea el préstamo:
> 15
```

**Resultado:** El sistema confirmará el préstamo y calculará la fecha límite automáticamente.

### 📤 3. Registrar Devolución

**Paso 1:** Selecciona `3` en el menú principal

**Paso 2:** Ingresa el ID del libro a devolver:
```
Ingrese el ID del libro a devolver:
> AC001
```

**Resultado:** El libro se marca como devuelto en el sistema.

### 🔎 4. Buscar Libro

**Paso 1:** Selecciona `4` en el menú principal

**Paso 2:** Ingresa el ID del libro:
```
Ingrese el ID del libro a buscar:
> AC001
```

**Resultado:** Si está prestado, mostrará:
- Usuario que lo tiene
- Días transcurridos desde el préstamo
- Días restantes hasta la fecha límite
- Fecha límite de devolución

### 📊 5. Listar Libros Prestados

**Paso 1:** Selecciona `5` en el menú principal

**Resultado:** Muestra todos los préstamos activos con:
- ID del libro
- Usuario
- Fecha de entrada
- Fecha límite
- Estado (prestado/devuelto)

## 💾 Persistencia de Datos

El sistema guarda automáticamente todos los préstamos en el archivo `Biblioteca.txt` en formato CSV:

```
AC001,Juan Pérez,2025-08-18 19:30:00.123456789 UTC,2025-09-02 19:30:00.123456789 UTC,Nothing
```

**Formato:** `ID,Usuario,FechaEntrada,FechaLímite,FechaSalida`

## 📁 Estructura del Proyecto

```
Practica1_biblioteca/
├── Biblioteca.hs          # Código fuente principal
├── Biblioteca.txt         # Base de datos de préstamos (se crea automáticamente)
└── README.md             # Este archivo
```

## 🏗️ Arquitectura del Código

### Tipos de Datos
- **`LibroDisponible`**: Representa libros en el catálogo con ID, título, género y descripción
- **`Libro`**: Representa un préstamo activo con usuario, fechas y estado

### Funciones Principales
- **`bancoLibros`**: Catálogo de 50 libros predefinidos
- **`registrarEntrada`**: Crear nuevo préstamo
- **`registrarSalida`**: Procesar devolución
- **`buscarLibro`**: Consultar estado de préstamo
- **`mostrarLibrosPorGenero`**: Filtrar y mostrar por género
- **`guardarprestamo`**: Persistir datos en archivo

## 🎯 Casos de Uso

### Para Bibliotecarios
1. Consultar catálogo completo organizizado por géneros
2. Registrar préstamos con control de fechas
3. Gestionar devoluciones
4. Monitorear préstamos vencidos
5. Generar reportes de préstamos activos

### Para Usuarios
1. Explorar catálogo de libros disponibles
2. Ver descripciones detalladas antes de solicitar
3. Conocer fechas límite de devolución
4. Consultar estado de sus préstamos

## 🔧 Personalización

### Agregar Nuevos Libros
Edita la lista `bancoLibros` en `Biblioteca.hs`:

```haskell
LibroDisponible "GE001" "Nuevo Título" "genero" "Descripción del libro."
```

### Modificar Géneros
Actualiza las funciones `menuGeneros` y `mostrarLibrosPorGenero` para incluir nuevas categorías.

## 🐛 Resolución de Problemas

### Error: "Could not find module"
- Instala GHC: `sudo apt install ghc` (Ubuntu/Debian)
- O usa Stack: `stack install ghc`

### Error: "No such file or directory"
- Asegúrate de estar en el directorio correcto
- Verifica que `Biblioteca.hs` existe

### Archivo Biblioteca.txt corrupto
- Elimina `Biblioteca.txt` para reset del sistema
- Se recreará automáticamente

## 👥 Contribuir

1. Fork el repositorio
2. Crea una rama para tu feature (`git checkout -b feature/nueva-funcionalidad`)
3. Commit tus cambios (`git commit -am 'Agrega nueva funcionalidad'`)
4. Push a la rama (`git push origin feature/nueva-funcionalidad`)
5. Abre un Pull Request

## 📄 Licencia

Este proyecto está bajo la Licencia MIT. Ver `LICENSE` para más detalles.

## 👨‍💻 Autor

**Tomás Buitrago** - [TBuitragoJ](https://github.com/TBuitragoJ)

**Emmanuel Cardona** - [EmmanuelCard](https://github.com/EmmanuelCard)

---

## 🎬 Video demostrativo

Puedes ver una demostración del sistema aquí: [https://youtu.be/1sOSAiWVBi4](https://youtu.be/1sOSAiWVBi4)

---

¿Encontraste útil este proyecto? ¡Dale una ⭐ en GitHub!