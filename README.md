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

**MAKE SURE THAT YOUR DATABASE SERVER**

## FrontEnd (WORK IN PROGRESS)

Still in early stages. The initial plan was to implement a simple front-end in React, along with State managers like Redux. Although, Elm is another attractive choice, given that it falls under the Functional Programming Paradigm

# Proyecto Servicios Web

En este repositorio se va a trabajar el proyecto de servicios web
