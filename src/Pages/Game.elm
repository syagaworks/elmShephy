module Pages.Game exposing (Model, Msg, page)

import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Modal as Modal
import Browser
import Gen.Params.Game exposing (Params)
import Gen.Route as Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import List.Extra as List
import Page
import Random
import Random.List
import Request
import Shared
import Task
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



-- MODEL


type alias CardType =
    { title : String
    , titleEn : String
    , desc : String
    }


type alias SheepSlot =
    { sheep : Sheep
    , status : Status
    }


type alias HandSlot =
    { card : Card
    , status : Status
    }


type Status
    = Frozen
    | Selectable
    | Selected


type Sheep
    = S1000
    | S300
    | S100
    | S30
    | S10
    | S3
    | S1
    | S0


type alias Info =
    { info1 : String
    , info2 : String
    , info3 : String
    }


type alias Model =
    { deck : List Card
    , hand : List HandSlot
    , discarded : List Card
    , expelled : List Card
    , sheep : List SheepSlot
    , info : Info
    , phase : Phase
    , round : Int
    , focusedHand : Int
    , focusedSheep : Int
    , focusedMenu : ModalType
    , modalVisibility : Modal.Visibility
    , timeStart : Time.Posix
    , timeNow : Time.Posix
    , modalType : ModalType
    , showDecide : Bool
    , userName : String
    , registerStatus : RegisterStatus
    }


type RegisterStatus
    = Editing
    | Sending
    | Failed Http.Error
    | Succeeded String


type Page
    = Home
    | Game
    | Ranking


type ModalType
    = Deck
    | Discarded
    | Expelled
    | NoModal


type Phase
    = ChoosingHand
    | PlayingHand Card
    | Win
    | Lose
    | Title


initModel : Model
initModel =
    { deck = initDeck
    , hand = List.repeat 5 (HandSlot NoCard Selectable)
    , discarded = []
    , expelled = []
    , sheep = SheepSlot S1 Frozen :: List.repeat 6 (SheepSlot S0 Frozen)
    , info = Info "" "" "Shephy"
    , phase = ChoosingHand
    , round = 1
    , focusedHand = -1
    , focusedSheep = -1
    , focusedMenu = NoModal
    , modalVisibility = Modal.hidden
    , timeStart = Time.millisToPosix 0
    , timeNow = Time.millisToPosix 0
    , modalType = NoModal
    , showDecide = False
    , userName = ""
    , registerStatus = Editing
    }


init : ( Model, Cmd Msg )
init =
    gameInit


gameInit : ( Model, Cmd Msg )
gameInit =
    ( initModel
    , Cmd.batch
        [ Random.generate ShuffleCard (Random.List.shuffle initModel.deck)
        , Task.perform SetTimeStart Time.now
        ]
    )



-- UPDATE


type Msg
    = InitGame
    | ShuffleCard (List Card)
    | ClickHand Card Int
    | FocusHand Int
    | BlurHand
    | ClickSheep Int
    | FocusSheep Int
    | BlurSheep
    | ClickDecide
    | CloseModal
    | ClickInspiration Card
    | SetTimeStart Time.Posix
    | Tick Time.Posix
    | FocusMenu ModalType
    | BlurMenu
    | ClickMenu ModalType
    | InputName String
    | RegisterTime
    | GotResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitGame ->
            gameInit

        ShuffleCard deck ->
            ( { model | deck = deck } |> refillHand, Cmd.none )

        ClickHand card order ->
            updateClickHand card order model

        FocusHand order ->
            ( { model | focusedHand = order }, Cmd.none )

        BlurHand ->
            ( { model | focusedHand = -1 }, Cmd.none )

        FocusSheep order ->
            ( { model | focusedSheep = order }, Cmd.none )

        BlurSheep ->
            ( { model | focusedSheep = -1 }, Cmd.none )

        ClickSheep order ->
            case model.phase of
                PlayingHand card ->
                    case getSheepStatusAt model order of
                        Frozen ->
                            ( model, Cmd.none )

                        Selected ->
                            model
                                |> updateSheep
                                    (List.updateAt
                                        order
                                        (\s -> { s | status = Selectable })
                                        model.sheep
                                    )
                                |> noCmd

                        Selectable ->
                            updateClickSheep model order card

                _ ->
                    ( model, Cmd.none )

        ClickDecide ->
            case model.phase of
                PlayingHand card ->
                    updateClickDecide card model

                _ ->
                    ( model, Cmd.none )

        CloseModal ->
            model
                |> resetAllHand
                |> resetAllSheep
                |> updatePhase ChoosingHand
                |> updateModal Modal.hidden
                |> updateModalType NoModal
                |> (\m -> ( m, Random.generate ShuffleCard (Random.List.shuffle model.deck) ))

        ClickInspiration card ->
            model
                |> updateInfo "???????????????????????????????????????????????????"
                |> discardSelectedHand
                |> updateDeck (List.remove card model.deck)
                |> updateHandWithSlot (HandSlot card Selectable)
                |> updatePhase ChoosingHand
                |> updateModal Modal.hidden
                |> cleanUp

        SetTimeStart newTime ->
            ( { model | timeStart = newTime, timeNow = newTime }, Cmd.none )

        Tick newTime ->
            if model.phase == Win then
                ( model, Cmd.none )

            else
                ( { model | timeNow = newTime }, Cmd.none )

        FocusMenu modalType ->
            ( { model | focusedMenu = modalType }, Cmd.none )

        BlurMenu ->
            ( { model | focusedMenu = NoModal }, Cmd.none )

        ClickMenu modalType ->
            model
                |> updateModalType modalType
                |> updateModal Modal.shown
                |> noCmd

        InputName name ->
            ( { model | userName = name }, Cmd.none )

        RegisterTime ->
            let
                sendingText : String
                sendingText =
                    String.concat
                        [ "userName="
                        , model.userName
                        , "&finishTime="
                        , calculateDiffPosix model
                            |> Time.posixToMillis
                            |> String.fromInt
                        ]

                textDecoder : D.Decoder String
                textDecoder =
                    D.field "text" D.string
            in
            ( { model | registerStatus = Sending }
            , Http.post
                { url = serverURL
                , body =
                    Http.stringBody "application/x-www-form-urlencoded"
                        (String.concat
                            [ "userName="
                            , model.userName
                            , "&finishTime="
                            , calculateDiffPosix model
                                |> Time.posixToMillis
                                |> String.fromInt
                            ]
                        )
                , expect = Http.expectJson GotResponse textDecoder
                }
            )

        GotResponse result ->
            case result of
                Ok text ->
                    ( { model | registerStatus = Succeeded text }, Cmd.none )

                Err errText ->
                    ( { model | registerStatus = Failed errText }, Cmd.none )


updateClickHand : Card -> Int -> Model -> ( Model, Cmd Msg )
updateClickHand card order model =
    if isSelectedHand model order then
        model
            |> resetAllHand
            |> resetAllSheep
            |> updatePhase ChoosingHand
            |> noCmd

    else
        case model.phase of
            ChoosingHand ->
                model
                    |> updateHand
                        (List.updateAt
                            order
                            (\s -> { s | status = Selected })
                            model.hand
                        )
                    |> updatePlayCard card

            PlayingHand playedCard ->
                updateClickHandWhilePlaying card order playedCard model

            _ ->
                ( model, Cmd.none )


updateClickHandWhilePlaying : Card -> Int -> Card -> Model -> ( Model, Cmd Msg )
updateClickHandWhilePlaying clickCard clickIndex playCard model =
    case playCard of
        SheepDog ->
            model
                |> updateInfo "???????????????????????????????????????"
                |> discardHandAt clickIndex
                |> discardSelectedHand
                |> cleanUp

        PlanningSheep ->
            model
                |> updateInfo "??????????????????????????????????????????"
                |> expelHandAt clickIndex
                |> discardSelectedHand
                |> cleanUp

        AllPurposeSheep ->
            let
                allPurposeSheepIndex =
                    List.findIndex (\s -> s.card == AllPurposeSheep) model.hand
                        |> Maybe.withDefault -1

                newModel =
                    model
                        |> updateInfo (cardTitle clickCard ++ "???????????????????????????")
                        |> updateHand
                            (List.setAt
                                allPurposeSheepIndex
                                (HandSlot AllPurposeSheep Selectable)
                                model.hand
                            )
                        |> updatePhase ChoosingHand
            in
            updateClickHand clickCard allPurposeSheepIndex newModel

        _ ->
            ( model, Cmd.none )


updateClickDecide : Card -> Model -> ( Model, Cmd Msg )
updateClickDecide card model =
    case card of
        FillTheEarth ->
            model
                |> updateInfo "???????????????????????????{1}??????????????????"
                |> updateSheep
                    (List.setIf
                        (\s -> s.status == Selected)
                        (SheepSlot S1 Frozen)
                        model.sheep
                    )
                |> discardSelectedHand
                |> updateShowDecide
                |> cleanUp

        Dominion ->
            let
                sumOfSelected =
                    model.sheep
                        |> List.filter (\s -> s.status == Selected)
                        |> List.map sheepNum
                        |> List.sum

                newSheep =
                    List.find ((>=) sumOfSelected) sheepNumList
                        |> Maybe.withDefault 0

                modelCleared =
                    model
                        |> discardSelectedSheep
            in
            modelCleared
                |> updateInfo "????????????????????????????????????"
                |> updateSheep
                    (setSheep
                        modelCleared.sheep
                        (SheepSlot (numToSheep newSheep) Frozen)
                    )
                |> discardSelectedHand
                |> updateShowDecide
                |> cleanUp

        GoldenHooves ->
            model
                |> updateInfo "?????????????????????????????????????????????????????????"
                |> updateSheep
                    (List.updateIf
                        (\s -> s.status == Selected)
                        (\s -> rankUp s)
                        model.sheep
                    )
                |> discardSelectedHand
                |> updateShowDecide
                |> cleanUp

        _ ->
            ( model, Cmd.none )


updateClickSheep : Model -> Int -> Card -> ( Model, Cmd Msg )
updateClickSheep model order card =
    let
        slot =
            List.getAt order model.sheep
                |> Maybe.withDefault (SheepSlot S1000 Frozen)

        markSheep : Model -> Model
        markSheep mdl =
            mdl
                |> updateSheep
                    (List.updateAt
                        order
                        (\s -> { s | status = Selected })
                        mdl.sheep
                    )

        countSelectedSheep : Int
        countSelectedSheep =
            List.count (\s -> s.status == Selected) model.sheep

        countSheep : Int
        countSheep =
            List.count (\s -> s.sheep /= S0) model.sheep
    in
    case card of
        BeFruitful ->
            model
                |> updateInfo "????????????????????????????????????????????????"
                |> updateSheep (setSheep model.sheep slot)
                |> discardSelectedHand
                |> cleanUp

        Flourish ->
            let
                addSheep : Model -> Model
                addSheep mdl =
                    { mdl | sheep = setSheep mdl.sheep (rankDown slot) }
            in
            model
                |> updateInfo "?????????????????????1???????????????????????????3??????????????????"
                |> addSheep
                |> addSheep
                |> addSheep
                |> discardSelectedHand
                |> cleanUp

        FillTheEarth ->
            model
                |> markSheep
                |> noCmd

        Dominion ->
            model
                |> markSheep
                |> noCmd

        GoldenHooves ->
            model
                |> markSheep
                |> noCmd

        FallingRock ->
            model
                |> updateInfo "??????????????????????????????????????????"
                |> markSheep
                |> discardSelectedSheep
                |> discardSelectedHand
                |> cleanUp

        Storm ->
            if (countSelectedSheep + 1) >= 2 then
                model
                    |> updateInfo "??????????????????????????????????????????"
                    |> markSheep
                    |> discardSelectedSheep
                    |> discardSelectedHand
                    |> cleanUp

            else
                model
                    |> markSheep
                    |> noCmd

        Meteor ->
            if (countSelectedSheep + 1) >= 3 then
                model
                    |> updateInfo "??????????????????????????????????????????"
                    |> markSheep
                    |> discardSelectedSheep
                    |> expelSelectedHand
                    |> cleanUp

            else
                model
                    |> markSheep
                    |> noCmd

        Plague ->
            model
                |> updateInfo "?????????????????????????????????????????????????????????"
                |> updateSheep
                    (List.setIf
                        (\s -> s.sheep == slot.sheep)
                        (SheepSlot S0 Frozen)
                        model.sheep
                    )
                |> discardSelectedSheep
                |> discardSelectedHand
                |> cleanUp

        Crowding ->
            if countSheep - (countSelectedSheep + 1) <= 2 then
                model
                    |> updateInfo "??????????????????????????????????????????"
                    |> markSheep
                    |> discardSelectedSheep
                    |> discardSelectedHand
                    |> cleanUp

            else
                model
                    |> markSheep
                    |> noCmd

        Slump ->
            if (countSelectedSheep + 1) >= ceiling (toFloat countSheep / 2) then
                model
                    |> updateInfo "??????????????????????????????????????????"
                    |> markSheep
                    |> discardSelectedSheep
                    |> discardSelectedHand
                    |> cleanUp

            else
                model
                    |> markSheep
                    |> noCmd

        _ ->
            ( model, Cmd.none )


updatePlayCard : Card -> Model -> ( Model, Cmd Msg )
updatePlayCard card model =
    case card of
        BeFruitful ->
            if getFilledSheep model == 7 then
                model
                    |> updateInfo "?????????????????????????????????????????????????????????????????????"
                    |> discardSelectedHand
                    |> cleanUp

            else
                model
                    |> updateInfo "????????????????????????????????????"
                    |> turnOnFilledSheep
                    |> updatePhase (PlayingHand BeFruitful)
                    |> noCmd

        Multiply ->
            if getFilledSheep model == 7 then
                model
                    |> updateInfo "?????????????????????????????????????????????????????????????????????"
                    |> discardSelectedHand
                    |> cleanUp

            else
                model
                    |> updateInfo "{3}??????????????????"
                    |> updateSheep
                        (setSheep
                            model.sheep
                            (SheepSlot S3 Frozen)
                        )
                    |> discardSelectedHand
                    |> cleanUp

        Flourish ->
            if getFilledSheep model == 7 then
                model
                    |> updateInfo "?????????????????????????????????????????????????????????????????????"
                    |> discardSelectedHand
                    |> cleanUp

            else
                model
                    |> updateInfo "????????????????????????????????????"
                    |> turnOnFilledSheep
                    |> updatePhase (PlayingHand Flourish)
                    |> noCmd

        FillTheEarth ->
            if getFilledSheep model == 7 then
                model
                    |> updateInfo "?????????????????????????????????????????????????????????????????????"
                    |> discardSelectedHand
                    |> cleanUp

            else
                model
                    |> updateInfo "???????????????????????????????????????????????????????????????????????????????????????"
                    |> turnOnEmptySheep
                    |> updatePhase (PlayingHand FillTheEarth)
                    |> updateShowDecide
                    |> noCmd

        Dominion ->
            model
                |> updateInfo "?????????????????????????????????????????????????????????????????????????????????????????????"
                |> turnOnFilledSheep
                |> updatePhase (PlayingHand Dominion)
                |> updateShowDecide
                |> noCmd

        GoldenHooves ->
            let
                sheepSpecies =
                    model.sheep
                        |> List.map sheepNum
                        |> List.unique
                        |> List.remove 0
                        |> List.length
            in
            if sheepSpecies <= 1 then
                model
                    |> updateInfo "????????????1????????????????????????????????????????????????"
                    |> discardSelectedHand
                    |> cleanUp

            else
                model
                    |> updateInfo "??????????????????????????????????????????????????????????????????????????????????????????????????????"
                    |> turnOnNotLargestSheep
                    |> updatePhase (PlayingHand GoldenHooves)
                    |> updateShowDecide
                    |> noCmd

        FallingRock ->
            model
                |> updateInfo "?????????????????????1???????????????????????????"
                |> turnOnFilledSheep
                |> updatePhase (PlayingHand FallingRock)
                |> noCmd

        Storm ->
            if List.count (\s -> s.sheep /= S0) model.sheep <= 2 then
                model
                    |> updateInfo "??????????????????2??????????????????"
                    |> updateSheep (List.repeat 7 (SheepSlot S0 Frozen))
                    |> discardSelectedHand
                    |> cleanUp

            else
                model
                    |> updateInfo "?????????????????????2???????????????????????????"
                    |> turnOnFilledSheep
                    |> updatePhase (PlayingHand Storm)
                    |> noCmd

        Meteor ->
            if List.count (\s -> s.sheep /= S0) model.sheep <= 3 then
                model
                    |> updateInfo "??????????????????3??????????????????"
                    |> updateSheep (List.repeat 7 (SheepSlot S0 Frozen))
                    |> discardSelectedHand
                    |> cleanUp

            else
                model
                    |> updateInfo "?????????????????????3???????????????????????????"
                    |> turnOnFilledSheep
                    |> updatePhase (PlayingHand Meteor)
                    |> noCmd

        Shephion ->
            model
                |> updateInfo "??????????????????????????????????????????"
                |> updateSheep (List.repeat 7 (SheepSlot S0 Frozen))
                |> discardSelectedHand
                |> cleanUp

        Lightning ->
            model
                |> updateInfo "???????????????????????????1???????????????????????????"
                |> updateSheep
                    (List.setAt
                        (getMaxSheepIndex model)
                        (SheepSlot S0 Frozen)
                        model.sheep
                    )
                |> discardSelectedHand
                |> cleanUp

        Wolves ->
            let
                maxSheep =
                    List.maximumBy sheepNum model.sheep
                        |> Maybe.withDefault (SheepSlot S0 Frozen)
            in
            model
                |> updateInfo "???????????????????????????1???????????????????????????????????????"
                |> updateSheep
                    (List.setAt
                        (getMaxSheepIndex model)
                        (rankDown maxSheep)
                        model.sheep
                    )
                |> discardSelectedHand
                |> cleanUp

        Plague ->
            model
                |> updateInfo "?????????????????????1??????????????????????????????"
                |> turnOnFilledSheep
                |> updatePhase (PlayingHand Plague)
                |> noCmd

        Crowding ->
            if getFilledSheep model < 3 then
                model
                    |> updateInfo "??????????????????2??????????????????"
                    |> discardSelectedHand
                    |> cleanUp

            else
                model
                    |> updateInfo "?????????2???????????????????????????????????????????????????????????????"
                    |> turnOnFilledSheep
                    |> updatePhase (PlayingHand Crowding)
                    |> noCmd

        Slump ->
            if getFilledSheep model == 1 then
                model
                    |> updateInfo "????????????1?????????????????????????????????"
                    |> discardSelectedHand
                    |> cleanUp

            else
                model
                    |> updateInfo "?????????????????????????????????????????????????????????????????????????????????"
                    |> turnOnFilledSheep
                    |> updatePhase (PlayingHand Slump)
                    |> noCmd

        SheepDog ->
            if getFilledHand model == 1 then
                model
                    |> updateInfo "????????????????????????????????????"
                    |> discardSelectedHand
                    |> cleanUp

            else
                model
                    |> updateInfo "??????????????????????????????????????????"
                    |> updatePhase (PlayingHand SheepDog)
                    |> noCmd

        PlanningSheep ->
            if getFilledHand model == 1 then
                model
                    |> updateInfo "???????????????????????????????????????"
                    |> discardSelectedHand
                    |> cleanUp

            else
                model
                    |> updateInfo "?????????????????????????????????????????????"
                    |> updatePhase (PlayingHand PlanningSheep)
                    |> noCmd

        Inspiration ->
            if List.length model.deck == 0 then
                model
                    |> updateInfo "???????????????????????????"
                    |> discardSelectedHand
                    |> cleanUp

            else
                model
                    |> updateInfo "??????????????????????????????????????????????????????"
                    |> updatePhase (PlayingHand Inspiration)
                    |> updateModal Modal.shown
                    |> noCmd

        AllPurposeSheep ->
            model
                |> updateInfo "????????????????????????????????????????????????"
                |> updatePhase (PlayingHand AllPurposeSheep)
                |> noCmd

        NoCard ->
            ( model, Cmd.none )



-- Update helper


discardSelectedSheep : Model -> Model
discardSelectedSheep model =
    model
        |> updateSheep
            (List.setIf
                (\s -> s.status == Selected)
                (SheepSlot S0 Frozen)
                model.sheep
            )


discardSelectedHand : Model -> Model
discardSelectedHand model =
    let
        index =
            List.findIndex (\s -> s.status == Selected) model.hand
                |> Maybe.withDefault -1
    in
    discardHandAt index model


expelSelectedHand : Model -> Model
expelSelectedHand model =
    let
        index =
            List.findIndex (\s -> s.status == Selected) model.hand
                |> Maybe.withDefault -1
    in
    expelHandAt index model


discardHandAt : Int -> Model -> Model
discardHandAt order model =
    let
        newHand =
            List.setAt
                order
                (HandSlot NoCard Frozen)
                model.hand

        selectedCard =
            List.getAt order model.hand
                |> Maybe.withDefault (HandSlot NoCard Frozen)
                |> .card

        discardedCard =
            if selectedCard == NoCard then
                []

            else
                [ selectedCard ]

        newDiscarded =
            List.append discardedCard model.discarded
    in
    model
        |> updateHand newHand
        |> updateDiscarded newDiscarded


expelHandAt : Int -> Model -> Model
expelHandAt order model =
    let
        newHand =
            List.setAt
                order
                (HandSlot NoCard Frozen)
                model.hand

        selectedCard =
            List.getAt order model.hand
                |> Maybe.withDefault (HandSlot NoCard Frozen)
                |> .card

        expelledCard =
            if selectedCard == NoCard then
                []

            else
                [ selectedCard ]

        newExpelled =
            List.append expelledCard model.expelled
    in
    model
        |> updateHand newHand
        |> updateExpelled newExpelled


getSheepStatusAt : Model -> Int -> Status
getSheepStatusAt model order =
    let
        slot =
            List.getAt order model.sheep
                |> Maybe.withDefault (SheepSlot S1000 Frozen)
    in
    slot.status


getMaxSheepIndex : Model -> Int
getMaxSheepIndex model =
    let
        maxSheep =
            List.maximumBy sheepNum model.sheep
                |> Maybe.withDefault (SheepSlot S0 Frozen)
    in
    List.elemIndex maxSheep model.sheep
        |> Maybe.withDefault -1


turnOnSheep : SheepSlot -> SheepSlot
turnOnSheep slot =
    { slot | status = Selectable }


turnOnFilledSheep : Model -> Model
turnOnFilledSheep model =
    { model
        | sheep =
            List.updateIf (\s -> s.sheep /= S0) turnOnSheep model.sheep
    }


turnOnEmptySheep : Model -> Model
turnOnEmptySheep model =
    { model
        | sheep =
            List.updateIf (\s -> s.sheep == S0) turnOnSheep model.sheep
    }


getMaxSheep : Model -> Sheep
getMaxSheep model =
    List.maximumBy (\s -> sheepNum s) model.sheep
        |> Maybe.withDefault (SheepSlot S1000 Frozen)
        |> .sheep


turnOnNotLargestSheep : Model -> Model
turnOnNotLargestSheep model =
    { model
        | sheep =
            List.updateIf
                (\s -> (s.sheep /= getMaxSheep model) && s.sheep /= S0)
                turnOnSheep
                model.sheep
    }


updateInfo : String -> Model -> Model
updateInfo string model =
    { model | info = newInfo model.info string }


updatePhase : Phase -> Model -> Model
updatePhase phase model =
    { model | phase = phase }


updateDeck : List Card -> Model -> Model
updateDeck deck model =
    { model | deck = deck }


updateSheep : List SheepSlot -> Model -> Model
updateSheep slotList model =
    { model | sheep = slotList }


updateHand : List HandSlot -> Model -> Model
updateHand slotList model =
    { model | hand = slotList }


updateHandWithSlot : HandSlot -> Model -> Model
updateHandWithSlot slot model =
    { model | hand = setHand model.hand slot }


updateRound : Int -> Model -> Model
updateRound int model =
    { model | round = int }


updateDiscarded : List Card -> Model -> Model
updateDiscarded cardList model =
    { model | discarded = cardList }


updateExpelled : List Card -> Model -> Model
updateExpelled cardList model =
    { model | expelled = cardList }


updateModal : Modal.Visibility -> Model -> Model
updateModal visibility model =
    { model | modalVisibility = visibility }


updateModalType : ModalType -> Model -> Model
updateModalType modalType model =
    { model | modalType = modalType }


updateShowDecide : Model -> Model
updateShowDecide model =
    { model | showDecide = not model.showDecide }


noCmd : Model -> ( Model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )


withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd msg model =
    ( model, msg )


refillHand : Model -> Model
refillHand model =
    if List.any (\s -> s.card == NoCard) model.hand then
        case model.deck of
            [] ->
                model

            x :: xs ->
                let
                    newModel =
                        model
                            |> updateHandWithSlot (HandSlot x Selectable)
                            |> updateDeck xs
                in
                refillHand newModel

    else
        model


cleanUp : Model -> ( Model, Cmd Msg )
cleanUp modelOriginal =
    let
        thousandOrZero : Model -> Model
        thousandOrZero model =
            -- 1000???????????????????????????
            if List.any (\s -> s.sheep == S1000) model.sheep then
                model
                    |> updateInfo
                        "1000?????????????????????????????????????????????????????????????????????????????????????????????"
                    |> updateModal Modal.shown
                    |> updatePhase Win
                -- ??????????????????????????????

            else if getFilledSheep model == 0 then
                model
                    |> updateInfo "???????????????????????????????????????????????????????????????"
                    |> updateModal Modal.shown
                    |> updatePhase Lose

            else
                model

        newModel =
            thousandOrZero modelOriginal

        -- ???????????????????????????????????????Round3????????????????????????????????????????????????????????????????????????
        refillDeck : Model -> ( Model, Cmd Msg )
        refillDeck model =
            if List.all (\s -> s.card == NoCard) model.hand && model.deck == [] then
                if model.round == 3 then
                    model
                        |> updateInfo "??????1000?????????????????????????????????????????????????????????"
                        |> updateRound 4
                        |> updatePhase Lose
                        |> noCmd

                else
                    ( model
                        |> updateInfo "?????????????????????????????????????????????????????????????????????"
                        |> updateRound (model.round + 1)
                        |> updatePhase ChoosingHand
                        |> updateDiscarded []
                    , Random.generate
                        ShuffleCard
                        (Random.List.shuffle model.discarded)
                    )

            else
                ( { model | phase = ChoosingHand }, Cmd.none )
    in
    if newModel.phase == Win || newModel.phase == Lose then
        ( newModel, Cmd.none )

    else
        newModel
            |> refillHand
            |> resetAllHand
            |> resetAllSheep
            |> refillDeck


getFilledSheep : Model -> Int
getFilledSheep model =
    List.map .sheep model.sheep
        |> List.count ((/=) S0)


getFilledHand : Model -> Int
getFilledHand model =
    List.map .card model.hand
        |> List.count ((/=) NoCard)


resetAllSheep : Model -> Model
resetAllSheep model =
    { model | sheep = List.map (\s -> { s | status = Frozen }) model.sheep }


resetAllHand : Model -> Model
resetAllHand model =
    { model | hand = List.map (\h -> { h | status = Selectable }) model.hand }


isSelectedHand : Model -> Int -> Bool
isSelectedHand model order =
    case List.getAt order model.hand of
        Just { card, status } ->
            status == Selected

        _ ->
            False


setSheep : List SheepSlot -> SheepSlot -> List SheepSlot
setSheep sheepList sheepSlot =
    let
        emptySlot =
            List.findIndex (\n -> n.sheep == S0) sheepList
                |> Maybe.withDefault -1
    in
    List.setAt
        emptySlot
        sheepSlot
        sheepList


setHand : List HandSlot -> HandSlot -> List HandSlot
setHand handList handSlot =
    let
        emptySlot =
            List.findIndex (\n -> n.card == NoCard) handList
                |> Maybe.withDefault -1
    in
    List.setAt
        emptySlot
        handSlot
        handList


sheepRank =
    [ S1000, S300, S100, S30, S10, S3, S1, S0 ]


rankDown : SheepSlot -> SheepSlot
rankDown slot =
    let
        index =
            List.elemIndex slot.sheep sheepRank
                |> Maybe.withDefault -1

        newSheep =
            List.getAt (index + 1) sheepRank
                |> Maybe.withDefault S0
    in
    { slot | sheep = newSheep }


rankUp : SheepSlot -> SheepSlot
rankUp slot =
    let
        index =
            List.elemIndex slot.sheep sheepRank
                |> Maybe.withDefault -1

        newSheep =
            List.getAt (index - 1) sheepRank
                |> Maybe.withDefault S1000
    in
    { slot | sheep = newSheep }


newInfo : Info -> String -> Info
newInfo info string =
    Info info.info2 info.info3 string



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> View Msg
view model =
    { title = "Shephy"
    , body = htmlView model
    }


htmlView : Model -> List (Html Msg)
htmlView model =
    [ Grid.container []
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col [ Col.xs4 ]
                [ viewInfo model.info ]
            , viewMenu model
            ]
        , Grid.row []
            [ Grid.col [ Col.xs8 ]
                [ ButtonGroup.checkboxButtonGroup [ ButtonGroup.large ]
                    (List.indexedMap (viewSheep model) model.sheep)
                ]
            , Grid.col [ Col.xs2 ] (viewDecide model)
            ]
        , Grid.row []
            [ Grid.col [ Col.xs6 ]
                [ ListGroup.ul <|
                    List.indexedMap (viewHand model) model.hand
                ]
            , Grid.col [ Col.xs6, Col.middleXs ] (viewCardDesc model)
            ]
        , viewModal model
        ]
    ]


viewDecide : Model -> List (Html Msg)
viewDecide model =
    if model.showDecide then
        [ Button.button
            [ Button.outlinePrimary
            , Button.onClick ClickDecide
            ]
            [ text "??????" ]
        ]

    else
        []


viewMenu : Model -> Grid.Column Msg
viewMenu model =
    Grid.col [ Col.xs8 ]
        [ Grid.row []
            [ viewMenuItem "????????????" (String.fromInt model.round)
            , viewMenuItemWithModal model "??????" (String.fromInt (List.length model.deck) ++ "???") Deck
            , viewMenuItemWithModal model "??????" (String.fromInt (List.length model.discarded) ++ "???") Discarded
            , viewMenuItemWithModal model "??????" (String.fromInt (List.length model.expelled) ++ "???") Expelled
            , viewMenuItem "?????????" (calculateElapsedTime model)
            ]
        ]


viewMenuItem : String -> String -> Grid.Column Msg
viewMenuItem title body =
    Grid.col []
        [ Card.config []
            |> Card.block []
                [ Block.titleH6 [] [ text title ]
                , Block.text [] [ text body ]
                ]
            |> Card.view
        ]


viewMenuItemWithModal : Model -> String -> String -> ModalType -> Grid.Column Msg
viewMenuItemWithModal model title body modalType =
    Grid.col []
        [ Card.config
            (if model.focusedMenu == modalType then
                [ Card.success ]

             else
                []
            )
            |> Card.block
                [ Block.attrs
                    [ onClick (ClickMenu modalType)
                    , onMouseEnter (FocusMenu modalType)
                    , onMouseLeave BlurMenu
                    ]
                ]
                [ Block.titleH6 [] [ text title ]
                , Block.text [] [ text body ]
                ]
            |> Card.view
        ]


calculateElapsedTime : Model -> String
calculateElapsedTime model =
    let
        diffPosix =
            calculateDiffPosix model

        hour =
            String.fromInt (Time.toHour Time.utc diffPosix) ++ "??????"

        minute =
            String.fromInt (Time.toMinute Time.utc diffPosix) ++ "???"

        second =
            String.fromInt (Time.toSecond Time.utc diffPosix) ++ "???"
    in
    hour ++ minute ++ second


calculateDiffPosix : Model -> Time.Posix
calculateDiffPosix model =
    let
        startMillis =
            Time.posixToMillis model.timeStart

        nowMillis =
            Time.posixToMillis model.timeNow

        diffMillis =
            nowMillis - startMillis
    in
    Time.millisToPosix diffMillis


viewCardDesc : Model -> List (Html Msg)
viewCardDesc model =
    if model.focusedHand /= -1 then
        let
            focusedHand =
                List.getAt model.focusedHand model.hand
                    |> Maybe.withDefault
                        (HandSlot Shephion Frozen)
                    |> .card
        in
        [ viewCard focusedHand ]

    else
        []


viewCard : Card -> Html Msg
viewCard card =
    Card.config []
        |> Card.headerH3 []
            [ text <| cardTitle card ]
        |> Card.block []
            [ Block.text [] [ text <| cardDesc card ] ]
        |> Card.view


viewModal : Model -> Html Msg
viewModal model =
    case model.phase of
        Win ->
            viewModalWin model

        Lose ->
            viewModalLose model

        _ ->
            case model.modalType of
                Deck ->
                    viewModalCardList model "??????" model.deck

                Discarded ->
                    viewModalCardList model "??????" model.discarded

                Expelled ->
                    viewModalCardList model "??????" model.expelled

                NoModal ->
                    viewModalCardList model "?????????????????????????????????" model.deck


viewModalWin : Model -> Html Msg
viewModalWin model =
    Modal.config CloseModal
        |> Modal.h3 [] [ text "????????????????????????" ]
        |> Modal.body []
            [ p [] [ text ("????????????" ++ calculateElapsedTime model) ]
            , case model.registerStatus of
                Editing ->
                    Form.formInline []
                        [ Input.text
                            [ Input.attrs [ placeholder "?????????" ]
                            , Input.onInput InputName
                            ]
                        , Button.button
                            [ Button.primary
                            , Button.attrs [ class "ml-sm-2 my-2" ]
                            , Button.onClick RegisterTime
                            ]
                            [ text "??????" ]
                        ]

                Sending ->
                    text "Loading..."

                Failed errorHttp ->
                    case errorHttp of
                        Http.BadUrl message ->
                            text ("BadUrl" ++ message)

                        Http.Timeout ->
                            text "Timeout"

                        Http.NetworkError ->
                            text "NetworkError"

                        Http.BadStatus code ->
                            text ("BadStatus" ++ String.fromInt code)

                        Http.BadBody message ->
                            text ("BadBody" ++ message)

                Succeeded resText ->
                    text resText
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.onClick InitGame
                ]
                [ text "????????????" ]
            , Button.linkButton
                [ Button.outlinePrimary
                , Button.attrs [ href (Route.toHref Route.Home_) ]
                ]
                [ text "??????????????????" ]
            ]
        |> Modal.view model.modalVisibility


viewModalLose : Model -> Html Msg
viewModalLose model =
    Modal.config CloseModal
        |> Modal.h3 [] [ text "????????????????????????" ]
        |> Modal.body [] [ p [] [ text model.info.info3 ] ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.onClick InitGame
                ]
                [ text "????????????" ]
            , Button.linkButton
                [ Button.outlinePrimary
                , Button.attrs [ href (Route.toHref Route.Home_) ]
                ]
                [ text "??????????????????" ]
            ]
        |> Modal.view model.modalVisibility


viewModalCardList : Model -> String -> List Card -> Html Msg
viewModalCardList model title cardList =
    let
        viewCardInGrid : Card -> Grid.Column Msg
        viewCardInGrid card =
            Grid.col
                [ Col.sm12
                , Col.attrs <|
                    if model.modalType == NoModal then
                        [ onClick (ClickInspiration card) ]

                    else
                        []
                ]
                [ viewCard card ]
    in
    Modal.config CloseModal
        |> Modal.scrollableBody True
        |> Modal.large
        |> Modal.h3 [] [ text title ]
        |> Modal.body []
            [ Grid.containerFluid []
                [ Grid.row [] (List.map viewCardInGrid cardList) ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.attrs [ onClick CloseModal ]
                ]
                [ text "??????" ]
            ]
        |> Modal.view model.modalVisibility


viewInfo : Info -> Html Msg
viewInfo info =
    div []
        [ text info.info1
        , br [] []
        , text info.info2
        , br [] []
        , text info.info3
        ]


viewSheep : Model -> Int -> SheepSlot -> ButtonGroup.CheckboxButtonItem Msg
viewSheep model order slot =
    let
        mouseAction : Button.Option Msg
        mouseAction =
            Button.attrs
                [ onMouseEnter (FocusSheep order)
                , onMouseLeave BlurSheep
                ]
    in
    ButtonGroup.checkboxButton
        (slot.status == Selected)
        (case slot.status of
            Frozen ->
                [ Button.disabled True
                , Button.attrs <| [ onMouseLeave BlurSheep ]
                ]

            Selectable ->
                [ mouseAction
                , Button.onClick (ClickSheep order)
                , if order == model.focusedSheep then
                    Button.success

                  else
                    Button.info
                ]

            Selected ->
                [ mouseAction
                , Button.onClick (ClickSheep order)
                , Button.primary
                ]
        )
        [ text (String.fromInt (sheepNum slot)) ]


viewHand : Model -> Int -> HandSlot -> ListGroup.Item Msg
viewHand model order slot =
    let
        mouseAction : ListGroup.ItemOption Msg
        mouseAction =
            ListGroup.attrs
                [ onClick (ClickHand slot.card order)
                , onMouseEnter (FocusHand order)
                , onMouseLeave BlurHand
                ]

        handList : ListGroup.Item Msg
        handList =
            ListGroup.li
                (case slot.status of
                    Frozen ->
                        [ ListGroup.disabled ]

                    Selectable ->
                        [ mouseAction
                        , if order == model.focusedHand then
                            ListGroup.success

                          else
                            ListGroup.light
                        ]

                    Selected ->
                        [ mouseAction
                        , ListGroup.active
                        ]
                )
                [ text (cardTitle slot.card) ]
    in
    case slot.card of
        NoCard ->
            ListGroup.li [] [ text "???" ]

        _ ->
            handList



-- Data


type Card
    = BeFruitful
    | Multiply
    | Flourish
    | FillTheEarth
    | Dominion
    | GoldenHooves
    | FallingRock
    | Storm
    | Meteor
    | Shephion
    | Lightning
    | Wolves
    | Plague
    | Crowding
    | Slump
    | SheepDog
    | PlanningSheep
    | Inspiration
    | AllPurposeSheep
    | NoCard


sheepNumList =
    [ 1000, 300, 100, 30, 10, 3, 1, 0 ]


sheepNum : SheepSlot -> Int
sheepNum slot =
    let
        index =
            List.elemIndex slot.sheep sheepRank
                |> Maybe.withDefault -1
    in
    List.getAt index sheepNumList
        |> Maybe.withDefault 0


numToSheep : Int -> Sheep
numToSheep num =
    let
        index =
            List.elemIndex num sheepNumList
                |> Maybe.withDefault -1
    in
    List.getAt index sheepRank
        |> Maybe.withDefault S1000


cardDesc : Card -> String
cardDesc card =
    case card of
        BeFruitful ->
            "??????????????????1????????????????????????"

        Multiply ->
            "{3}????????????"

        Flourish ->
            "??????????????????1?????????????????????1???????????????????????????3????????????"

        FillTheEarth ->
            "{1}???????????????????????????"

        Dominion ->
            "??????????????????????????????????????????????????????1???????????????????????????????????????????????????1???????????????????????????"

        GoldenHooves ->
            "????????????????????????????????????????????????????????????????????????1???????????????????????????"

        FallingRock ->
            "??????????????????1??????????????????"

        Storm ->
            "??????????????????2??????????????????"

        Meteor ->
            "???????????????????????????????????????????????????3??????????????????"

        Shephion ->
            "??????????????????7??????????????????"

        Lightning ->
            "???????????????????????????1??????????????????"

        Wolves ->
            "???????????????????????????1???????????????1???????????????????????????{1}?????????????????????"

        Plague ->
            "??????????????????1??????????????????????????????"

        Crowding ->
            "2????????????????????????????????????????????????????????????"

        Slump ->
            "???????????????????????????????????????????????????????????????????????????????????????????????????????????????"

        SheepDog ->
            "??????1??????????????????"

        PlanningSheep ->
            "??????1?????????????????????"

        Inspiration ->
            "?????????????????????????????????1?????????????????????????????????????????????"

        AllPurposeSheep ->
            "??????1????????????????????????????????????????????????????????????????????????"

        NoCard ->
            ""


cardTitle : Card -> String
cardTitle card =
    case card of
        BeFruitful ->
            "?????????"

        Multiply ->
            "????????????"

        Flourish ->
            "??????"

        FillTheEarth ->
            "???????????????"

        Dominion ->
            "??????"

        GoldenHooves ->
            "????????????"

        FallingRock ->
            "??????"

        Storm ->
            "???"

        Meteor ->
            "?????????"

        Shephion ->
            "??????????????????"

        Lightning ->
            "??????"

        Wolves ->
            "???"

        Plague ->
            "??????"

        Crowding ->
            "??????"

        Slump ->
            "??????"

        SheepDog ->
            "?????????"

        PlanningSheep ->
            "???????????????"

        Inspiration ->
            "??????"

        AllPurposeSheep ->
            "???????????????"

        NoCard ->
            ""


initDeck : List Card
initDeck =
    [ BeFruitful
    , BeFruitful
    , BeFruitful
    , Multiply
    , Flourish
    , FillTheEarth
    , Dominion
    , Dominion
    , GoldenHooves
    , FallingRock
    , Storm
    , Meteor
    , Shephion
    , Lightning
    , Wolves
    , Plague
    , Crowding
    , Slump
    , SheepDog
    , PlanningSheep
    , Inspiration
    , AllPurposeSheep
    ]
