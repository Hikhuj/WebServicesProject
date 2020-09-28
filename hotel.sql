CREATE DATABASE RESERVAS_SW
GO
USE RESERVAS_SW
GO

-- Create a new table called '[Usuario]' in schema '[dbo]'
-- Drop the table if it already exists
IF OBJECT_ID('[dbo].[Usuario]', 'U') IS NOT NULL
DROP TABLE [dbo].[Usuario]
GO
-- Create the table in the specified schema
CREATE TABLE [dbo].[Usuario]
(
    [USU_CODIGO] INT NOT NULL PRIMARY KEY IDENTITY (1,1) -- Primary Key column
    ,[USU_NOMBRE] VARCHAR(50) NOT NULL
    ,[USU_IDENTIFICACION] VARCHAR(20) NOT NULL
    ,[USU_PASSWORD] VARCHAR(10) NOT NULL
    ,[USU_EMAIL] VARCHAR(50) NOT NULL
    ,[USU_ESTADO] CHAR(1) NOT NULL
    ,[USU_FEC_NAC] DATETIME
    ,[USU_TELEFONO] VARCHAR(15) NOT NULL
    -- Specify more columns here
);
GO

-- Create a new table called '[Hotel]' in schema '[dbo]'
-- Drop the table if it already exists
IF OBJECT_ID('[dbo].[Hotel]', 'U') IS NOT NULL
DROP TABLE [dbo].[Hotel]
GO
-- Create the table in the specified schema
CREATE TABLE [dbo].[Hotel]
(
    [HOT_CODIGO] INT NOT NULL PRIMARY KEY IDENTITY (1,1) -- Primary Key column
    ,[HOT_NOMBRE] VARCHAR(50) NOT NULL
    ,[HOT_TELEFONO] VARCHAR(15) NOT NULL
    ,[HOT_EMAIL] VARCHAR(50) NOT NULL
    ,[HOT_DIRECCION] VARCHAR(100) NOT NULL
    ,[HOT_CATEGORIA] CHAR(1) NOT NULL
    -- Specify more columns here
);
GO
-- Create a new table called '[Habitacion]' in schema '[dbo]'
-- Drop the table if it already exists
IF OBJECT_ID('[dbo].[Habitacion]', 'U') IS NOT NULL
DROP TABLE [dbo].[Habitacion]
GO
-- Create the table in the specified schema
CREATE TABLE [dbo].[Habitacion]
(
    [HAB_CODIGO] INT NOT NULL PRIMARY KEY IDENTITY (1,1) -- Primary Key column
    ,[HOT_CODIGO] INT NOT NULL
    ,[HAB_NUMERO] INT NOT NULL
    ,[HAB_CAPACIDAD] INT NOT NULL
    ,[HAB_TIPO] VARCHAR(10) NOT NULL
    ,[HAB_DESCRIPCION] VARCHAR(100) NOT NULL
    ,[HAB_ESTADO] CHAR(1) NOT NULL
    ,[HAB_PRECIO] NUMERIC(18,2) NOT NULL
    -- Specify more columns here
);
GO

-- Create a new table called '[Reservacion]' in schema '[dbo]'
-- Drop the table if it already exists
IF OBJECT_ID('[dbo].[Reservacion]', 'U') IS NOT NULL
DROP TABLE [dbo].[Reservacion]
GO
-- Create the table in the specified schema
CREATE TABLE [dbo].[Reservacion]
(
    [RES_CODIGO] INT NOT NULL PRIMARY KEY IDENTITY (1,1) -- Primary Key column
    ,[USU_CODIGO] INT NOT NULL
    ,[HAB_CODIGO] INT NOT NULL
    ,[RES_FECHA_INGRESO] DATETIME NOT NULL
    ,[RES_FECHA_SALIDA] DATETIME NOT NULL
    -- Specify more columns here
);
GO

/*

For User Read options

SELECT USU_CODIGO, USU_IDENTIFICACION, USU_NOMBRE, USU_PASSWORD, USU_EMAIL, USU_ESTADO, USU_FEC_NAC, USU_TELEFONO
FROM   USUARIO
 */
