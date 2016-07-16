module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Json.Decode as Json
import Html.Events exposing (..)
import Http
import Task
import String exposing (..)
import Regex exposing (..)
-- Model

type alias Model =
    { url : String, src : String }

type Msg
    = GetEmbedCode | FetchSucceed String | FetchFail Http.Error

--Update


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
      GetEmbedCode ->
        (model, fetchEmbedCode model.url)

      FetchSucceed jsonData ->
        ( Model model.src (getSrcFromOEmbed jsonData), Cmd.none)

      FetchFail _ ->
        (model, Cmd.none)

fetchEmbedCode : String -> Cmd Msg
fetchEmbedCode url =
  let
    bodyData =
      Http.multipart [ Http.stringData "format" "json", Http.stringData "url" "https://soundcloud.com/thesleepyrapper/im-sleepy-ft-ugly-as-sin"]
    in
      Task.perform FetchFail FetchSucceed (Http.post decodeEmbedData url bodyData )

decodeEmbedData : Json.Decoder String
decodeEmbedData =
  Json.at ["html"] Json.string
--View

view : Model -> Html Msg
view model =
    div [] [ iframe [src model.src] [] ]

getSrcFromOEmbed : String -> String
getSrcFromOEmbed str =
  let
    matcher =
      regex "src=\"(.*)\""
    matches =
      List.head (find (AtMost 1) matcher str)
    in
     case matches of
       Just firstMatch ->
         getFirstSubMatch firstMatch
       Nothing ->
         "None"


getFirstSubMatch match =
  let
    firstMatch =
      List.head match.submatches
  in
    case firstMatch of
      Just src ->
        Maybe.withDefault "none" src
      Nothing ->
        "None"
-- Init

init : String -> (Model, Cmd Msg)
init url =
  ( Model url "" , fetchEmbedCode url)

-- Subs

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



main =
    Html.program { init = init "http://soundcloud.com/oembed", update = update, view = view, subscriptions = subscriptions }
