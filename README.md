# Web Services Project

This repository will serve as the code base for the Web Services Project.
With the purpose of bringing something different to the table, this repository will be focused on applying Functional Programming principles to real world web services (RWWS).

## Database

Relational Databases were used for this project. In this case, PostgreSQL, a very powerful RDBMS that can compete with other relational databases like Microsoft SQL Server, MySQL and SQLite

### Installation

- Install PostgreSQL

```
--MacOS users--

hombrew install postgresql

--GNU/Linux Users--
Depending on your distribution, most likely PostgreSQL is available as a package under your system's packet manager, you can head over and install it from there

--Docker/container Users--

WIP

```

- Create a new role designated for DB access (default one being used by this app is "postgre")
- Start your PostgreSQL server

## Backend

For the backend, Template Haskell (with Yesod) is being used to mantain a simple REST API that can handle Create Read Update Delete CRUD actions for the following entities:

- Users
- Hotels
- Reservations
- Rooms
- Transportation
- Transportation Types

Also, for the project, it was taken into account to use simple authentication and authorization with JSON Web Tokens (JWT)

### Installation

- Make sure to have Stack installed. You'll need it to build the project

```
--MacOS users--

hombrew install haskell-stack

--GNU/Linux Users--
Depending on your distribution, most likely Stack is available as a package under your system's packet manager, you can head over and install it from there
```

- Make sure to head over to the root of the backend directory (WebServices/HotelReservations/backend/virtualhotel)
- create a JWT SECRET

```

in your shell, issue the following command

export JWT_SECRET="your secret goes here"

make sure to change the value to something secure

```

- Build the project

```

stack build

```

- Run the project

```

stack exec yesod -- devel

```

Or

```

stack exec virtualhotel

```

The REST API is now running in your machine and you should be able to authenticate yourself with the following endpoints

http://localhost:3000/api/v1/auth/signup <- Register a new user
Or
http://localhost:3000/api/v1/auth/signin <- Login with an already existing user (EMAIL must exist and PASSWORD must match)

Afterwards, you should obtain a JWT. Make sure to set it as your Bearer Token in the headers, and you should be able to consume all the other endpoints in the API

**MAKE SURE YOUR DATABASE SERVER IS RUNNING PRIOR TO STARTING UP YOUR API**

## FrontEnd (WORK IN PROGRESS)

Still in early stages. The initial plan was to implement a simple front-end in React, along with State managers like Redux. Although, Elm is another attractive choice, given that it falls under the Functional Programming Paradigm

# Proyecto Servicios Web

En este repositorio se va a trabajar el proyecto de servicios web
Con el fin de traer algo distinto a la mesa, este repositorio va a estar enfocado en aplicar los principios de programacion funcional al mundo real de servicios web (MRSW)

## Base de datos

Se usaron bases de datos relacionales para este proyecto. En esta ocasion, PostgreSQL, un sistema gestor de bases de datos muy poderoso, capaz de competir con otras bases de datos relacionales como lo son Microsoft SQL Server, MySQL y SQLite

### Instalacion

- Instale PostgreSQL

```
-- Usuarios de MacOS--

hombrew install postgresql

--Usuarios de GNU/Linux--
Dependiendo de su distribucion, es probable que PostgreSQL este disponible como un paquete bajo el gestor de paquetes de su sistema, puede dirigirse a su gestor de paquetes e instalarlo desde ahi

--Docker/container Users--

WIP

```

- Cree un nuevo rol dedicado al acceso a la base de datos (por defecto el programa va a intentar acceder al rol "postgre" )
- Arranque el servidor PostgreSQL

## Backend

Para el backend, Se esta usando Template Haskell (Yesod) para el mantenimiento de un REST API sencillo que sea capaz de realizar acciones de Creacion Lectura Actualizacion y Borrado (CRUD) para las siguientes entidades:

- Usuarios
- Hoteles
- Reservaciones
- Habitaciones
- Transporte
- Tipos de Transporte

Adicionalmente, para este proyecto, se tomo en cuenta usar autenticacion y autorizacion simple con JSON Web Tokens (JWT)

### Instalacion

- Este seguro de tener Stack instalado. Lo necesitara para compilar el proyecto

````
--Usuarios de MacOS--

hombrew install haskell-stack

--Usuarios de GNU/Linux--
Dependiendo de su distribucion, es probable que Stack se encuentre disponible como un paquete bajo el gestor de paquetes de su sistema, puede dirigirse a su gestor de paquetes e instalarlo desde ahi```

--Docker/container Users--

WIP

```

- Este seguro de posicionarse en la raiz del directorio del backend (WebServices/HotelReservations/backend/virtualhotel)
- Cree un JWT SECRET

En su shell, ejecute el siguiente comando

```
export JWT_SECRET="your secret goes here"

Asegurese de cambiar el valor por algo mas seguro

```

- Compile el proyecto

```

stack build

```

- Corra el proyecto

```

stack exec yesod -- devel

```

O

```

stack exec virtualhotel

```

El API de REST ya deberia estar corriendo en su maquina y ya deberia de poder autenticarse con las siguientes rutas

http://localhost:3000/api/v1/auth/signup <- Registrarse
Or
http://localhost:3000/api/v1/auth/signin <- Ingresar (EMAIL debe existir y CONTRASEÑA debe coincidir)

Posteriormente, debe de obtener un JWT. Este seguro de establecerlo como un Bearer Token en sus headers, y deberia de poder consumir todas las rutas dentro del API

**ASEGURESE DE QUE SU SERVIDOR DE BASES DE DATOS SE ENCUENTRE CORRIENDO ANTES DE INICIAR SU API**

## FrontEnd (WORK IN PROGRESS)

Sigue en etapas tempranas. El plan inicial era implementar un front-end sencillo en React, junto a gestores de estado como Redux. Sin embargo, Elm es otra opcion atractiva, debido a que cae bajo el paradigma de programacion funcional
