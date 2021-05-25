module Pages.Ranking exposing (Model, Msg, page)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table
import Gen.Params.Ranking exposing (Params)
import Gen.Route as Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D
import Page
import Request
import Shared
import Time
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


serverURL =
    "https://elmshephy.pythonanywhere.com"



-- INIT


type Model
    = Error Http.Error
    | Data (List Record)
    | Loading


type alias Record =
    { player : String
    , time : Int
    , date : String
    }


init : ( Model, Cmd Msg )
init =
    ( Loading
    , Http.get { url = serverURL, expect = Http.expectJson GotRanking rankingDecoder }
    )


rankingDecoder : D.Decoder (List Record)
rankingDecoder =
    D.list
        (D.map3 Record
            (D.field "userName" D.string)
            (D.field "finishTime" D.int)
            (D.field "reportedDate" D.string)
        )



-- UPDATE


type Msg
    = GotRanking (Result Http.Error (List Record))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRanking result ->
            case result of
                Ok records ->
                    ( Data records, Cmd.none )

                Err errMsg ->
                    ( Error errMsg, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Ranking"
    , body = viewHtml model
    }


viewHtml : Model -> List (Html Msg)
viewHtml model =
    case model of
        Error errMsg ->
            case errMsg of
                Http.BadUrl message ->
                    [ text ("BadUrl" ++ message) ]

                Http.Timeout ->
                    [ text "Timeout" ]

                Http.NetworkError ->
                    [ text "NetworkError" ]

                Http.BadStatus code ->
                    [ text ("BadStatus" ++ String.fromInt code) ]

                Http.BadBody message ->
                    [ text ("BadBody" ++ message) ]

        Data records ->
            [ viewRanking records ]

        Loading ->
            [ text "Now Loading" ]


viewRanking : List Record -> Html Msg
viewRanking records =
    Grid.container []
        [ CDN.stylesheet
        , Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xs10 ]
                [ Table.table
                    { options = [ Table.striped ]
                    , thead =
                        Table.thead [ Table.inversedHead ]
                            [ Table.tr []
                                [ Table.th [] [ text "順位" ]
                                , Table.th [] [ text "名前" ]
                                , Table.th [] [ text "タイム" ]
                                , Table.th [] [ text "日付" ]
                                ]
                            ]
                    , tbody =
                        Table.tbody []
                            (List.indexedMap viewRankingRow records)
                    }
                ]
            ]
        , Grid.row [ Row.rightXs ]
            [ Grid.col [ Col.xs2 ]
                [ Button.linkButton
                    [ Button.outlinePrimary
                    , Button.attrs [ href (Route.toHref Route.Home_) ]
                    ]
                    [ text "ホームに戻る" ]
                ]
            ]
        ]


viewRankingRow : Int -> Record -> Table.Row Msg
viewRankingRow order record =
    Table.tr []
        [ Table.td [] [ text (String.fromInt (order + 1) ++ "位") ]
        , Table.td [] [ text record.player ]
        , Table.td [] [ text (millisToFancyTime record.time) ]
        , Table.td [] [ text record.date ]
        ]



-- Utility


millisToFancyTime : Int -> String
millisToFancyTime millis =
    let
        diffPosix =
            Time.millisToPosix millis

        hour =
            String.fromInt (Time.toHour Time.utc diffPosix) ++ "時間"

        minute =
            String.fromInt (Time.toMinute Time.utc diffPosix) ++ "分"

        second =
            String.fromInt (Time.toSecond Time.utc diffPosix) ++ "秒"
    in
    hour ++ minute ++ second
