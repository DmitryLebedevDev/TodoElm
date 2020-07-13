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
import Date
import Task exposing (Task)
import List exposing (head)
import Http exposing (task)

main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- MODEL
type alias Task = {
  id: Int,
  title: String ,
  isCompite: Bool
  }
type alias Model = { 
  taskTitle: String ,
  taskList: List Task ,
  statusList: StateList
  } 
init: () -> (Model, Cmd Msg)
init _ = ({
    taskTitle = "",
    taskList = [],
    statusList = All
  }, Cmd.none)
-- UPDATE
type StateList = All
               | Active
               | Completed
type Msg = Add_Task
         | ChangeInput String
         | ChangeTaskCompliteStatus Int
         | SetStatusList StateList
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeInput title -> ({model | taskTitle = title}, Cmd.none)
    Add_Task -> if String.length model.taskTitle > 0 then
                  ({model | taskList = {
                              id = case head model.taskList of 
                                    Just task -> task.id+1 
                                    Nothing -> 0 , 
                              title = model.taskTitle ,
                              isCompite = False
                             } :: model.taskList,
                  taskTitle = "" }, Cmd.none)
                else (model, Cmd.none)
    ChangeTaskCompliteStatus id -> (
      {model | taskList = List.map (\task -> 
        if task.id == id 
          then {task | isCompite = not task.isCompite} 
          else task) 
        model.taskList} ,
      Cmd.none )
    SetStatusList status -> ({
      model | statusList = status
      }, Cmd.none)
-- VIEW
view : Model -> Html Msg
view model = div [
                   style "margin" "0px auto", 
                   style "font-family" "sans-serif", 
                   style "width" "320px"] [
    Html.form [ style "display" "flex",
                style "justify-content" "center", 
                onSubmit Add_Task, style "margin-bottom" "5px" ] [
        input [ style "width" "100%", onInput ChangeInput, value model.taskTitle ] [] ,
        button[ onClick Add_Task, type_ "button" ] [text "submit"]
    ] ,
    div [style "display" "flex", style "aligin-items" "center",
         style "flex-direction" "column",
         style "height" "300px", style "overflow" "auto" ] (List.map (\x -> div [] [
        label [] [
          input [ type_ "checkbox" , 
                checked x.isCompite ,
                onClick <| ChangeTaskCompliteStatus x.id ][],
          text x.title
        ]]
      ) <|  List.filter (\x -> case model.statusList of
                                All -> True
                                Active -> x.isCompite == False
                                Completed -> x.isCompite == True
                        )
    model.taskList) ,
    div [style "display" "flex", style "justify-content" "center"] [
      button [disabled <| model.statusList == All ,
              onClick  <| SetStatusList All
             ] [ text "All" ] ,
      button [disabled <| model.statusList == Active ,
              onClick  <| SetStatusList Active
             ] [ text "Active"] ,
      button [disabled <| model.statusList == Completed ,
              onClick  <| SetStatusList Completed
             ] [ text "Completed" ]
    ]
  ]