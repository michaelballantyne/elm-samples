import Html exposing (..)
import Random
import Array exposing (..)
import Html.Events exposing (..)
import Result

main = program { init = (initialModel, Cmd.none),
                 update = update,
                 view = view,
                 subscriptions = subscriptions }

subscriptions model = Sub.none

initialModel = { randomItems = [] }

choices = Array.fromList ["a", "b", "c"]

getChoice i = Maybe.withDefault "" (Array.get i choices)

randomChoice model =
    (model,
    Random.int 0 2
      |> Random.map getChoice
      |> Random.list 5
      |> Random.generate RandomValues)

update msg model =
    case msg of
        AddItems -> randomChoice model
        RandomValues newItems ->
            ({ model | randomItems = List.append newItems model.randomItems }, Cmd.none)

type Msg = AddItems | RandomValues (List String)

view model =
    div []
      (List.append
        [ button [ onClick AddItems ] [ text "add 5 items" ] ]
        ( renderItems model.randomItems ))

renderItems items = List.map (\i -> p [] [text i]) items
