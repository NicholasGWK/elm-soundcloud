import Soundcloud
import Html.App as App
import Html exposing (..)
import Task
import Http exposing (Error)

--- Model

type alias Model = { url : String
                   , oembedSrc : String
                   }

model : Model
model =
  { url = "https://soundcloud.com/thesleepyrapper/im-sleepy-ft-ugly-as-sin"
  , oembedSrc = ""
  }

type Msg = FetchSucceed String | FetchFailed Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- Should probably handle errors here :)
    FetchFailed err ->
      ( model, Cmd.none)
    FetchSucceed src ->
      ({ model | oembedSrc = src }, Cmd.none)

view : Model -> Html Msg
view model =
  case model.oembedSrc of
    "" ->
      div [] [text ("No oembedSrc")]
    _ ->
      Soundcloud.embed model.oembedSrc

fetch : String -> Cmd Msg
fetch url =
  Task.perform FetchFailed FetchSucceed (Soundcloud.fetchEmbedSrc url)

main = App.program
    { init = (model, fetch model.oembedSrc)
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }
