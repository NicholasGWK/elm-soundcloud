module Soundcloud exposing (fetchEmbedSrc, embed)

{-| This library just simplies using the Soundcloud API a bit so it's something more like JS SDK they provide. Limited use right now but
I needed it for a side project so thought I'd break it out into a library

# Functions

@docs fetchEmbedSrc
@docs embed
-}
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json
import Http exposing (post, Error)
import Task exposing (Task)
import String exposing (..)
import Regex exposing (..)
import Result exposing (..)


oembedUrl = "http://soundcloud.com/oembed"

{-|
  Given the url of a soundcloud track, will return a Task you may perform. The task will attempt to fetch the oembed data from the soundcloud api,
  and parse out specifically the iframe src so you can render it with the rest of your HTML. You must provide Msg's for success and failure cases

    fetchEmbedSrc : String -> Task Error String
-}
fetchEmbedSrc : String -> Task Error String
fetchEmbedSrc url =
  let
    bodyData =
      Http.multipart [ Http.stringData "format" "json", Http.stringData "url" url]
    in
    Http.post decodeEmbedData oembedUrl bodyData

decodeEmbedData : Json.Decoder String
decodeEmbedData =
  Json.customDecoder (Json.at ["html"] Json.string) getSrcFromOEmbed

{-|
  Shorthand for rending a div with an iframe in it using the iframe src that was fetched with fetchEmbedSrc. No styling etc, maybe should be configurable but is literally
  just div [] [ iframe [Html.Attributes.src url] [] ] so if you need more configurability just use the src however you want :).
-}
embed : String -> Html msg
embed url =
    div [] [ iframe [Html.Attributes.src url] [] ]

getSrcFromOEmbed : String -> Result String String
getSrcFromOEmbed str =
  let
    matcher =
      regex "src=\"(.*)\""
    matches =
      List.head (find (AtMost 1) matcher str)
    in
      Result.fromMaybe "No src found in response" matches `andThen` getFirstSubmatch `andThen` getSubmatchValue


getFirstSubmatch : Regex.Match -> Result String (Maybe String)
getFirstSubmatch match =
  fromMaybe "No submatches found" (List.head match.submatches)

getSubmatchValue : Maybe String -> Result String String
getSubmatchValue submatch =
  fromMaybe "Submatch src had no value" submatch
