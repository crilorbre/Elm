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
    resultado: String,
    interpretacion: String
  }

init : Model
init = 
  {estatura = ""
  , peso = ""
  , resultado = ""
  , interpretacion = ""
  } 




-- UPDATE

type Msg 
  = Estatura String
  | Peso String
  | Resultado
  | Interpretacion
  | Clear


update : Msg -> Model -> Model
update msg model = 
  case msg of
    Estatura newEstatura ->
      {model | estatura  =  newEstatura}

    Peso nuevoPeso ->
      {model | peso  =  nuevoPeso}

    Resultado ->
     {model | resultado = obtenerResultado model.estatura model.peso }

    Interpretacion ->
      if Maybe.withDefault 0.0 (String.toFloat (obtenerIMC model.estatura model.peso)) < 18.5 then
        update Resultado {model|interpretacion = "Bajo peso"}
      else if Maybe.withDefault 0.0 (String.toFloat (obtenerIMC model.estatura model.peso)) >= 18.5 && Maybe.withDefault 0.0 (String.toFloat (obtenerIMC model.estatura model.peso)) < 25 then
        update Resultado {model|interpretacion = "Normal"}
      else if Maybe.withDefault 0.0 (String.toFloat (obtenerIMC model.estatura model.peso)) >= 25 && Maybe.withDefault 0.0 (String.toFloat (obtenerIMC model.estatura model.peso)) < 30 then
        update Resultado {model|interpretacion = "Sobrepeso"}
      else 
        update Resultado {model|interpretacion = "Obesidad"}

    Clear ->
      init




-- AUXILIAR FUNCTIONS

-- Funcion para obtener el string que se muestra por pantalla visualizando los datos de
-- estatura, peso y imc
obtenerResultado: String -> String -> String
obtenerResultado estatura peso =
  "Para una estatura de " ++ estatura ++ " m y un peso de " ++
    peso ++ " kg, su IMC es de " ++ obtenerIMC estatura peso ++ "."

-- Funcion para obtener el IMC a partir de la estatura y el peso
-- y quedarme con 2 decimales
obtenerIMC: String -> String -> String
obtenerIMC estatura peso = 
  String.left 5 (String.fromFloat(Maybe.withDefault 0.0(String.toFloat peso) / (Maybe.withDefault 0.0(String.toFloat estatura) * Maybe.withDefault 0.0(String.toFloat estatura))))


-- VIEW


view : Model -> Html Msg
view model =
  div[style "display" "flex", style "flex-direction" "column", style "justify-content" "center", style "align-items" "center", style "min-height" "100vh"]  
  [ 
    div [style "background" "linear-gradient(to right, #44ddd0, #27d7a1)", style "border-radius" "5px", style "padding" "60px 40px", style "position" "relative"][

      div [] [
        h1 [style "text-align" "center"] [text "Calculadora del índice de masa corportal (IMC)"]
      ]
      ,
      div [][
        
        div[] [
        label [style "font-size" "20px"] [text "Introduzca su estatura (en metros): "]
        , input [style "width" "100%", style "margin" "15px 0", style "padding" "12px 0px", 
        type_ "number",  placeholder "Introduce tu estatura...", value  model.estatura, onInput Estatura] []
        ]
        , 
        
        div[] [
          label [style "font-size" "20px"] [text "Introduzca su peso (en kgs): "]
          ,input [style "width" "100%", style "margin" "15px 0", style "padding" "12px 0px", 
          type_ "number",  placeholder "Introduce tu peso...", value  model.peso, onInput Peso] []
        ]
      ]
      
      ,

      div[] [
      text  model.resultado
      ]
      ,
      viewValidation model,
      viewInterpretacion model
    ]
  ]


-- VALIDATION VIEW
viewValidation : Model -> Html Msg

viewValidation model =
  if model.estatura > "0"  && model.peso > "0" then
    div [style "position" "absolute", style "right" "25px", style "bottom" "25px", style "border-radius" "8px"] [
      button [ style "padding" "12px 30px", onClick Interpretacion ] [ text "Calcule su IMC" ]
      , 
      button [ style "padding" "12px 30px", onClick Clear ] [ text "Clear" ]
    ]
    
  else if model.estatura /= "" && model.estatura <= "0" then
    div [ style "color" "red" ] [ text "Por favor, introduce una estatura válida" ]
  else if model.peso /= "" && model.peso <= "0" then
    div [ style "color" "red" ] [ text "Por favor, introduce un peso válido" ]
  else
    div [][]

viewInterpretacion: Model -> Html Msg
viewInterpretacion model =
  if model.interpretacion /= "" then
    if model.interpretacion == "Bajo peso" then
      div [ style "color" "red" ] [ text "Bajo peso" ]
    else if model.interpretacion == "Normal" then
      div [ style "color" "red" ] [ text "Normal" ]
    else if model.interpretacion == "Sobrepeso" then
      div [ style "color" "red" ] [ text "Sobrepeso" ]
    else 
      div [ style "color" "red" ] [ text "Obesidad" ]
  else
    div [][]

    