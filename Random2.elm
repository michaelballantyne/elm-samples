import Html exposing (..)
import Random
import Array exposing (..)
import Html.Events exposing (..)
import Result

main = program { init = (initialModel, generateInitialSeed),
                 update = update,
                 view = view,
                 subscriptions = subscriptions }

generateInitialSeed = Random.generate InitialSeed (Random.int Random.minInt Random.maxInt)

subscriptions model = Sub.none

type alias Model = { randomItems : List String, seed : Random.Seed }
initialModel = { randomItems = [], seed = Random.initialSeed 0 }

choices = Array.fromList ["a", "b", "c"]

getChoice : Int -> String
getChoice i = Maybe.withDefault "" (Array.get i choices)

addNewItems : Int -> Model -> Model
addNewItems n model =
    case n of
        0 -> model
        _ -> let (randomValue, newSeed) = Random.step (Random.int 0 2) model.seed in
               addNewItems
                 (n - 1)
                 { model | seed = newSeed,
                           randomItems = (getChoice randomValue) :: model.randomItems }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        InitialSeed val -> ({ model | seed = Random.initialSeed val }, Cmd.none)
        AddItems -> (addNewItems 5 model, Cmd.none)

type Msg = InitialSeed Int | AddItems

view : Model -> Html Msg
view model =
    div []
      (List.append
        [ button [ onClick AddItems ] [ text "add 5 items" ] ]
        ( renderItems model.randomItems ))

renderItems items = List.map (\i -> p [] [text i]) items
