module Main exposing(main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Events exposing (onInput, onSubmit)
import Http
import List
import Regex
import Json.Encode as Encode exposing(string)
import Html.Events exposing (onSubmit)

main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- MODEL
type alias Model = { 
  taskTitle: String ,
  taskList: List String
  } 
init: () -> (Model, Cmd Msg)
init _ = ({
    taskTitle = "",
    taskList = []
  }, Cmd.none)
-- UPDATE
type Msg = Add_Task
         | ChangeInput String
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeInput title -> ({model | taskTitle = title}, Cmd.none)
    Add_Task -> ({taskList = model.taskTitle :: model.taskList ,
                  taskTitle = "" }, Cmd.none)
-- VIEW
view : Model -> Html Msg
view model = div [ style "font-family" "sans-serif" ] [
    Html.form [ onSubmit Add_Task, style "margin-bottom" "5px" ] [
        input [ onInput ChangeInput, value model.taskTitle ] [] ,
        button[ onClick Add_Task ] [text "submit"]
    ] ,
    div [] (List.map (\x -> div [] [text x]) model.taskList)
  ]