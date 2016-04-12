module Dog (..) where

import Graphics.Element exposing (..)
import Effects exposing (Effects, none)
import Signal exposing (message, Address)
import Time exposing (Time, fps)
import Html exposing (..)
import Html.Events exposing (on, targetValue)
import StartApp

type Action
    = Tick Time

-- Model for a sprite
type alias Sprite =
  { image: String
  , xDim: Int
  , yDim: Int
  , startingX: Int
  , startingY: Int
  , currentX: Int
  , currentY: Int
  , rows: Int
  , columns: Int
  , tileWidth: Int
  , tileHeight: Int}


-- Initial Sprite
dogSheet: Sprite
dogSheet =
  { image = "dogSheet.png"
    , xDim = 400
    , yDim = 120
    , startingX = 88
    , startingY = 9
    , currentX = 88
    , currentY = 7
    , rows = 3
    , columns = 10
    , tileWidth = 40
    , tileHeight = 40
  }

-- Go through the walking tiles
advance : (Sprite) -> Sprite
advance(sprite) =
  if sprite.currentX < sprite.xDim - 80
    then {sprite | currentX = sprite.currentX + 40 }
    else {sprite | currentX = sprite.startingX }

-- crops the correct piece of the image
returnEle : Sprite -> Element
returnEle(sprite) =
  croppedImage (sprite.currentX,sprite.currentY) sprite.tileHeight sprite.tileWidth sprite.image


-- View
view : Address Action -> Sprite -> Html
view address s =
    let
        onInput address contentToValue =
            on
                "input"
                targetValue
                (message address << contentToValue)
    in
        div
            []
            [ fromElement(returnEle(s)) ]


-- Update
update : Action -> Sprite -> ( Sprite, Effects Action )
update action s =
    let
        s' =
            case action of
                Tick _ ->
                    advance s
    in
        ( s', none )


app : StartApp.App (Sprite)
app =
    StartApp.start
        { view = view
        , update = update
        , init = ( dogSheet, none )
        , inputs = [ Signal.map Tick (fps 5) ]
        }

main : Signal Html
main =
    app.html
