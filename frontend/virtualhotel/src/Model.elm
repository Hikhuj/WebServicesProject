module Model exposing (Room, Reservation, User, Hotel, Transport, TransportType)
import Time exposing (Posix)


type alias Room =
    { hab_capacidad : Int
    , hab_descripcion : String
    , hab_estado : String
    , fk_hot_codigo : Int
    , hab_numero : Int
    , id : Int
    , hab_tipo : String
    , hab_precio : Float
    }
-- Hotel Model

type alias Hotel = 
        { hot_nombre : String 
         , hot_telefono : String 
         , hot_email : String 
         , hot_direccion : String 
         , hot_categoria : String 
         , hot_estado : String 
         }

type alias User = 
        {  usu_nombre : String 
         , usu_identificacion : String 
         , usu_password : String 
         , usu_email : String 
         , usu_estado : String 
         , usu_fec_nac : Posix 
         , usu_telefono : String 
         }

type alias Reservation = 
        { fk_usu_codigo : Int
         , fk_hab_codigo : Int
         , fk_tra_codigo : Int
         , id : Int
         , res_fecha_ingreso : String 
         , res_fecha_salida : String 
         , res_estado : String 
        } 

type alias Transport = 
        { fk_tip_transporte : String
         , tra_descripcion : String 
         , tra_precio : Float
         , tra_estado : String 
         , tra_fecha_inicio : Posix 
         , tra_fecha_fin : Posix 
         }

type alias TransportType = 
        { tip_descripcion : String
         , tip_estado : String 
         , id : Int
         }

