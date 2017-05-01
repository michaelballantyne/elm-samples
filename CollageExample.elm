import Html exposing (program)
import Keyboard exposing (..)
import AnimationFrame
import Time exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Color exposing (blue)

-- Need to use program rather than beginnerProgram for subscriptions.
main = program
 { init = (initialModel, Cmd.none),
   view = view,
   subscriptions = subscriptions,
   update = updateWithCommand }


type Msg = KeyMsg Key | Tick Time
type Key = KeyUp Int | KeyDown Int


initialModel = { x = 0, vx = 0,
                 y = 0, vy = 0,
                 groundAcceleration = 0 }


subscriptions model =
    Sub.batch
    [ Keyboard.downs (KeyDown >> KeyMsg)
    , Keyboard.ups (KeyUp >> KeyMsg)
    -- We want to do our drawing right before the browser does its next repaint.
    -- AnimationFrame schedules that for use. See https://developer.mozilla.org/en-US/docs/Web/API/window/requestAnimationFrame.
    , AnimationFrame.diffs Tick]


-- Not using any commands yet, so keep them out of the update functions.
updateWithCommand msg model =
    (update msg model, Cmd.none)

update msg model =
    case msg of
        Tick time -> tick model
        KeyMsg k -> key k model

horizontalVelocity = 3
jumpVelocity = 6

key msg model =
    model
        |> case msg of
            -- Up arrow
            KeyDown 38 -> jump

            -- Left arrow
            KeyDown 37 -> accelerateTo -horizontalVelocity
            KeyUp   37 -> stop (<)

            -- Right arrow
            KeyDown 39 -> accelerateTo horizontalVelocity
            KeyUp   39 -> stop (>)

            _ -> identity

accelerateTo velocity model =
    {model | groundAcceleration = velocity}

jump model =
    if model.vy == 0 then
        {model | vy = jumpVelocity}
    else
        model

stop compare model =
    if compare model.groundAcceleration 0 then
       {model | groundAcceleration = 0}
    else
        model

tick model =
    model
        |> traction
        |> gravity
        |> motion
        |> floor

traction model =
    if model.y == 0 then
       {model | vx = model.groundAcceleration}
    else
       model

gravityAcceleration = -0.3
gravity model = {model | vy = model.vy + gravityAcceleration}

motion model =
    {model | x = model.x + model.vx,
             y = model.y + model.vy}

floor model =
    if model.y < 0 then
       {model | y = 0, vy = 0}
    else
       model

view model =
    toHtml (collage 500 500
             [(filled blue (circle 5))])

