module Pages.Home_ exposing (page)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page exposing (Page)
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page
page shared req =
    Page.static
        { view = view
        }




-- VIEW


view : View Never
view =
    { title = "Home"
    , body = htmlView
    }


htmlView : List (Html Never)
htmlView =
    [ Grid.container []
        [ CDN.stylesheet
        , Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xs6 ]
                [ h1 [] [ text "Shephy" ]
                ]
            , Grid.colBreak []
            , Grid.col [ Col.xs6 ]
                [ Button.linkButton
                    [ Button.outlinePrimary
                    , Button.attrs [ href (Route.toHref Route.Game)]
                    ]
                    [ text "ゲームを始める" ]
                ]
            , Grid.colBreak []
            , Grid.col [ Col.xs6 ]
                [ Button.linkButton
                    [ Button.outlinePrimary
                    , Button.attrs [ href (Route.toHref Route.Ranking)]
                    ]
                    [ text "ランキング" ]
                ]
            , Grid.colBreak []
            , Grid.col [ Col.xs6 ]
                [ Button.linkButton
                    [ Button.outlinePrimary
                    , Button.attrs [ href "https://boku-boardgame.net/shephy"]
                    ]
                    [ text "ルール（外部ページ）" ]
                ]
            ]
        ]
    ]
