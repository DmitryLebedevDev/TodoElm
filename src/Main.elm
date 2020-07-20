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
import Time exposing (..)
import PortFunnel as LocalStorage

main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

monthToNumbers : Month -> String
monthToNumbers month = 
  case month of 
    Jan -> "01"
    Feb -> "02"
    Mar -> "03"
    Apr -> "04"
    May -> "05"
    Jun -> "06"
    Jul -> "07"
    Aug -> "08"
    Sep -> "09"
    Oct -> "10"
    Nov -> "11"
    Dec -> "12"
formtNumToDataNumber : Int -> String
formtNumToDataNumber num = 
  if num < 10 then
    "0" ++ (String.fromInt num)
  else
    String.fromInt num
timeToString : Time.Zone -> Time.Posix -> String
timeToString zone posix = 
  (formtNumToDataNumber <| Time.toHour zone posix)
  ++
  ":"
  ++
  (formtNumToDataNumber <| Time.toMinute zone posix)
  ++
  ":"
  ++
  (formtNumToDataNumber <| Time.toSecond zone posix)
type alias Time =
    Float
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick

-- MODEL
type alias Task = {
  id: Int,
  title: String ,
  isCompite: Bool ,
  timeStart: Time.Posix
  }
type alias Model = { 
  taskTitle: String ,
  taskList: List Task ,
  statusList: StateList ,
  time: Time.Posix ,
  timeZone: Time.Zone
  }
init: () -> (Model, Cmd Msg)
init _ = ({
    taskTitle = "",
    taskList = [],
    statusList = All,
    time = Time.millisToPosix 0,
    timeZone = Time.utc
  }, Task.perform SetTimeZone Time.here)
-- UPDATE
type StateList = All
               | Active
               | Completed
type Msg = Add_Task
         | Delete_Task Int
         | ChangeInput String
         | ChangeTaskCompliteStatus Int
         | SetStatusList StateList
         | Tick Time.Posix
         | SetTimeZone Time.Zone
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Delete_Task id -> ({model | taskList = List.filter (\x -> not <| x.id == id) model.taskList}, Cmd.none)
    ChangeInput title -> ({model | taskTitle = title}, Cmd.none)
    Add_Task -> if String.length model.taskTitle > 0 then
                  ({model | taskList = {
                              id = case head model.taskList of 
                                    Just task -> task.id+1 
                                    Nothing -> 0 , 
                              title = model.taskTitle ,
                              isCompite = False ,
                              timeStart = model.time
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
    Tick posix -> ( {model | time = posix}, Cmd.none)
    SetTimeZone zone -> ( { model | timeZone = zone }, Task.perform Tick Time.now)
-- VIEW
view : Model -> Html Msg
view model = div [
                   style "margin" "0px auto", 
                   style "font-family" "sans-serif", 
                   style "width" "320px"] [
    div [ style "text-align" "center" ] [
      text <| formtNumToDataNumber <| Time.toDay model.timeZone model.time ,
      text "." ,
      text <| monthToNumbers <| Time.toMonth model.timeZone model.time ,
      text "." ,
      text <| formtNumToDataNumber <| Time.toYear model.timeZone model.time
    ] ,
    div [ style "text-align" "center", style "margin-bottom" "5px" ] [
      text <| timeToString model.timeZone model.time
    ] ,
    Html.form [ style "display" "flex",
                style "justify-content" "center", 
                onSubmit Add_Task, style "margin-bottom" "5px" ] [
        input [ style "width" "100%", onInput ChangeInput, value model.taskTitle ] [] ,
        button[ style "margin-left" "5px", onClick Add_Task, type_ "button" ] [text "submit"]
    ] ,
    div [style "display" "flex", style "aligin-items" "center",
         style "flex-direction" "column",
         style "height" "300px", style "overflow" "auto" ] (List.map (\x -> div [] [
        label [ style "display" "flex", 
                style "justify-content" "space-between",
                style "margin-top" "5px",
                style "cursor" "pointer" 
              ] [
          div [] [
            input [ 
                style "cursor" "pointer" ,
                style "margin-right" "10px" ,
                type_ "checkbox" , 
                checked x.isCompite ,
                onClick <| ChangeTaskCompliteStatus x.id ][],
            text x.title
          ] ,
          --text <| timeToString model.timeZone x.timeStart ,
          button [ onClick <| Delete_Task x.id ] [ text "delete" ]
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