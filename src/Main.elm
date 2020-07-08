module Main exposing (..)


import Browser
import Html exposing (Html, h1, label, div, text, input, button, table, th, td, tr)
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
  div[style "display" "flex", style "justify-content" "center", style "align-items" "center", style "min-height" "100vh"]  
  [ 
    
      div [style "height" "400px", style "width" "350px", style "padding" "42px", style "display" "flex", style "justify-content" "center", style "align-items" "center", style "background" "#55efc4", style "border-top-left-radius" "8px", style "border-bottom-left-radius" "8px", style "position" "relative"][
        
        div[] [
        label [style "font-size" "20px"] [text "Introduzca su estatura (en metros): "]
        , input [style "width" "100%", style "margin" "15px 0", style "padding" "12px 0px", 
        type_ "number",  placeholder "Introduce tu estatura...", value  model.estatura, onInput Estatura] []
        
        , label [style "font-size" "20px"] [text "Introduzca su peso (en kgs): "]
        ,input [style "width" "100%", style "margin" "15px 0", style "padding" "12px 0px", 
        type_ "number",  placeholder "Introduce tu peso...", value  model.peso, onInput Peso] []
        ],
        
        viewValidation model
      
      ]
      ,

      div[style "height" "400px", style "width" "350px", style "padding" "42px", style "display" "flex", style "flex-direction" "column", style "justify-content" "center", style "align-items" "center", style "background" "#55efc4", style "border-top-right-radius" "8px", style "border-bottom-right-radius" "8px"][
        div[style "height" "400px", style "width" "350px", style "margin" "10px", style "display" "flex", style "justify-content" "center", style "align-items" "center", style "background" "#f8f8f8", style "text-align" "center", style "font-size" "20px"][
          text  model.resultado
        ],
        div[style "height" "400px", style "width" "350px", style "margin" "10px", style "display" "flex", style "justify-content" "center", style "align-items" "center", style "background" "#f8f8f8"][
          viewInterpretacion model
        ]
      ]      
    
  ]


-- VALIDATION VIEW
viewValidation : Model -> Html Msg

viewValidation model =
  if model.estatura > "0"  && model.peso > "0" then
    div [style "position" "absolute", style "right" "25px", style "bottom" "25px"] [
      button [  style "padding" "12px 20px", style "border-radius" "16px", onClick Interpretacion ] [ text "Calcule su IMC" ]
      , 
      button [  style "padding" "12px 20px", style "border-radius" "16px", onClick Clear ] [ text "Clear" ]
    ]
    
  else if model.estatura /= "" && model.estatura <= "0" then
    div [ style "color" "#D8000C", style "background" "#FFD2D2", style "padding" "10px", style "border-radius" "16px",style "position" "absolute", style "bottom" "100px" ] [ text "Por favor, introduce una estatura válida" ]
  else if model.peso /= "" && model.peso <= "0" then
    div [ style "color" "#D8000C", style "background" "#FFD2D2", style "padding" "10px", style "border-radius" "16px",style "position" "absolute", style "bottom" "100px" ] [ text "Por favor, introduce un peso válido" ]
  else
    div [][]

viewInterpretacion: Model -> Html Msg
viewInterpretacion model =
  if model.interpretacion /= "" then
    table[][
      tr[][
        th[][text "Clasificacion"]
        , th [][text "IMC"]
      ],

      tr[if model.interpretacion == "Bajo peso" then style "background-color" "red" else style "" ""][
        td[][text "Bajo peso"],
        td[][text "<18,50"]
      ],

      tr[if model.interpretacion == "Normal" then style "background-color" "red" else style "" ""][
        td[][text "Normal"],
        td[][text "18,5 - 24,99"]
      ],

      tr[if model.interpretacion == "Sobrepeso" then style "background-color" "red" else style "" ""][
        td[][text "Sobrepeso"],
        td[][text "≥25,00"]
      ],

      tr[if model.interpretacion == "Obesidad" then style "background-color" "red" else style "" ""][
        td[][text "Obesidad"],
        td[][text "≥30,00"]
      ]

    ]
    
  else
    div [][]

