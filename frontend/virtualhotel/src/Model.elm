module Model exposing (Room, Reservation, User, Hotel, Transport, TransportType, encodeTransportType )
import Time exposing (Posix)
import Json.Encode as Encode


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
         , id : Int 
         }

type alias User = 
        {  usu_nombre : String 
         , usu_identificacion : String 
         , usu_password : String 
         , usu_email : String 
         , usu_estado : String 
         , usu_fec_nac : String 
         , usu_telefono : String 
         , id : Int 
         }

type alias Reservation = 
        { fk_usu_codigo : Int
         , fk_hab_codigo : Int
         , fk_tra_codigo : Int
         , res_fecha_ingreso : String 
         , res_fecha_salida : String 
         , res_estado : String 
         , id : Int
        } 

type alias Transport = 
        { fk_tip_transporte : Int
         , tra_descripcion : String 
         , tra_precio : Float
         , tra_estado : String 
         , id : Int 
         }

type alias TransportType = 
        { tip_descripcion : String
         , tip_estado : String 
         , id : Int
         }

active : String
active = 
        "A"

encodeTransportType : String -> Encode.Value
encodeTransportType transportTypeDescription =
        Encode.object [

                ("tip_descripcion", Encode.string transportTypeDescription)
                ,("tip_estado", Encode.string active)
                ]
