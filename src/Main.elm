module Main exposing (..)


import Browser
import Html exposing (Html, div, text, input, button)
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
    resultado: String
  }

init : Model
init = 
  {estatura = ""
  , peso = ""
  , resultado = ""
  } 




-- UPDATE

type Msg 
  = Estatura String
  | Peso String
  | Resultado String


update : Msg -> Model -> Model
update msg model = 
  case msg of
    Estatura newEstatura ->
      {model | estatura  =  newEstatura}

    Peso nuevoPeso ->
      {model | peso  =  nuevoPeso}

    Resultado res->
      {model | resultado = model.estatura}






-- VIEW


view : Model -> Html Msg
view model =
  div[]
  [ 

    div[] [
    input [ type_ "number",  placeholder "Introduce tu estatura", value  model.estatura, onInput Estatura] []
    , text  model.estatura
    ]
    , 
    
    div[] [
      input [ type_ "number",  placeholder "Introduce tu peso", value  model.peso, onInput Peso] []
    , text  model.peso
    ]
    
    
  ]

