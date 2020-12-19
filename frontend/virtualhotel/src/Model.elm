module Model exposing (Hotel, Reservation, Room, Transport, TransportType, User, encodeTransportType, encodeRoom, encodeUser, encodeHotel, encodeTransport, encodeReservation)

import Json.Encode as Encode
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


{--Hotel Encoder  --}


encodeRoom : Int -> String -> Int -> Int -> String -> Float -> Encode.Value
encodeRoom roomCapacity roomDescription hotelId roomNumber roomType roomPrice =
    Encode.object
        [
                ( "hab_capacidad" , Encode.int roomCapacity )
    , ( "hab_descripcion" , Encode.string roomDescription )
    , ( "hab_estado" , Encode.string active )
    , ( "fk_hot_codigo" , Encode.int hotelId )
    , ( "hab_numero" , Encode.int roomNumber )
    , ( "hab_tipo" , Encode.string roomType )
    , ( "hab_precio" , Encode.float roomPrice )
            ]


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

{--Hotel Encoder  --}


encodeHotel : String -> String -> String -> String -> String -> Encode.Value
encodeHotel hotelName hotelPhone hotelMail hotelAddress hotelCategory =
    Encode.object
        [ ( "hot_nombre", Encode.string hotelName )
        , ( "hot_telefono", Encode.string hotelPhone )
        , ( "hot_email", Encode.string hotelMail )
        , ( "hot_direccion", Encode.string hotelAddress )
        , ( "hot_categoria", Encode.string hotelCategory )
        , ( "hot_estado", Encode.string active )
        ]


type alias User =
    { usu_nombre : String
    , usu_identificacion : String
    , usu_password : String
    , usu_email : String
    , usu_estado : String
    , usu_fec_nac : String
    , usu_telefono : String
    , id : Int
    }





{--User Encoder  --}


encodeUser : String -> String -> String -> String -> String -> String -> Encode.Value
encodeUser userName userIdentification userPass userMail userBirthDate userPhone =
    Encode.object
        [ ( "usu_nombre", Encode.string userName )
        , ( "usu_identificacion", Encode.string userIdentification )
        , ( "usu_password", Encode.string userPass )
        , ( "usu_email", Encode.string userMail )
        , ( "usu_estado", Encode.string active )
        , ( "usu_fec_nac", Encode.string userBirthDate )
        , ( "usu_telefono", Encode.string userPhone )
        ]


type alias Reservation =
    { fk_usu_codigo : Int
    , fk_hab_codigo : Int
    , fk_tra_codigo : Int
    , res_fecha_ingreso : String
    , res_fecha_salida : String
    , res_estado : String
    , id : Int
    }



{--Reservation Encoder  --}


encodeReservation : Int -> Int -> Int -> String -> String -> Encode.Value
encodeReservation userId roomId transportId dateCheckIn dateCheckOut =
    Encode.object
        [ ( "fk_usu_codigo", Encode.int userId )
        , ( "fk_hab_codigo", Encode.int roomId )
        , ( "fk_tra_codigo", Encode.int transportId )
        , ( "res_fecha_ingreso", Encode.string dateCheckIn )
        , ( "res_fecha_salida", Encode.string dateCheckOut )
        , ( "res_estado", Encode.string active )
        ]


type alias Transport =
    { fk_tip_transporte : Int
    , tra_descripcion : String
    , tra_precio : Float
    , tra_estado : String
    , id : Int
    }



{--Transport Encoder  --}


encodeTransport : Int -> String -> Float -> Encode.Value
encodeTransport transportTypeId transportDescription transportPrice =
    Encode.object
        [ ( "fk_tip_transporte", Encode.int transportTypeId )
        , ( "tra_descripcion", Encode.string transportDescription )
        , ( "tra_precio", Encode.float transportPrice )
        , ( "tra_estado", Encode.string active )
        ]


type alias TransportType =
    { tip_descripcion : String
    , tip_estado : String
    , id : Int
    }



{--Transport Type Encoder  --}


encodeTransportType : String -> Encode.Value
encodeTransportType transportTypeDescription =
    Encode.object
        [ ( "tip_descripcion", Encode.string transportTypeDescription )
        , ( "tip_estado", Encode.string active )
        ]


active : String
active =
    "A"
