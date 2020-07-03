module Main exposing (..)


import Browser
import Html exposing (Html, h1, label, div, text, input, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Model = 
  { 
    estatura: String,
    peso: String,
    resultado: Float
  }

init : Model
init = 
  {estatura = ""
  , peso = ""
  , resultado = 0.0
  } 



-- UPDATE

type Msg 
  = Estatura String
  | Peso String
  | Resultado


update : Msg -> Model -> Model
update msg model = 
  case msg of
    Estatura newEstatura ->
      {model | estatura  =  newEstatura}

    Peso nuevoPeso ->
      {model | peso  =  nuevoPeso}

    Resultado->
      {model | resultado = Maybe.withDefault 0.0(String.toFloat model.peso) / (Maybe.withDefault 0.0(String.toFloat model.estatura) * Maybe.withDefault 0.0(String.toFloat model.estatura))}




-- VIEW


view : Model -> Html Msg
view model =
  div[]
  [ 
    div [] [
      h1 [] [text "Calculadora del índice de masa corportal (IMC)"]
    ]
    ,

    div[] [
    label [] [text "Introduzca su estatura (en metros): "]
    , input [type_ "number",  placeholder "Introduce tu estatura", value  model.estatura, onInput Estatura] []
    , text  model.estatura
    ]
    , 
    
    div[] [
      label [] [text "Introduzca su peso (en kgs): "]
      ,input [ type_ "number",  placeholder "Introduce tu peso", value  model.peso, onInput Peso] []
    , text  model.peso
    ]
    ,

    button [ onClick Resultado ] [ text "Calcule su IMC" ]
    , div[] [
    text  (String.fromFloat model.resultado)
    ]
    
  ]


