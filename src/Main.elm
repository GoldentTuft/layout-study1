module Main exposing (main)

import Browser
import Browser.Dom
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Task



-- model


type alias Model =
    { page : Page
    , menuOpen : Bool
    , dropDown1Config : DropDownConfig
    , dropDown2Open : Bool
    , menu2Open : Bool
    , rightSideBar : List String
    , searchBarOpen : Bool
    , searchText : String
    , hoverMenuOpen : Bool
    }


type alias DropDownConfig =
    { state : DropDownState
    , id : String
    }


type DropDownState
    = Open
    | Closed


type Page
    = Top
    | About


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = Top
      , menuOpen = False
      , dropDown1Config =
            { state = Closed
            , id = "dropDown1"
            }
      , dropDown2Open = False
      , menu2Open = False
      , rightSideBar = [ "item1", "longlonglonglong" ]
      , searchBarOpen = False
      , searchText = ""
      , hoverMenuOpen = False
      }
    , Cmd.none
    )



-- update


type Msg
    = NoOp
    | SelDropDownItem String
    | SetState DropDownState
    | OpenDropDown2
    | CloseDropDown2
    | OpenMenu
    | CloseMenu
    | ToAbout
    | ToTop
    | ToggleMenu2
    | ToggleSearchBar
    | InputSearchText String
    | OpenHover
    | CloseHover


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SelDropDownItem item ->
            { model
                | dropDown2Open = False
                , menuOpen = False
                , rightSideBar = item :: model.rightSideBar
                , hoverMenuOpen = False
            }
                |> update (SetState Closed)

        SetState state ->
            let
                config =
                    model.dropDown1Config

                newConfig =
                    { config | state = state }
            in
            ( { model | dropDown1Config = newConfig }, Cmd.none )

        OpenDropDown2 ->
            ( { model | dropDown2Open = True }, Cmd.none )

        CloseDropDown2 ->
            ( { model | dropDown2Open = False }, Cmd.none )

        OpenMenu ->
            ( { model | menuOpen = True }, Cmd.none )

        CloseMenu ->
            ( { model | menuOpen = False }, Cmd.none )

        ToggleMenu2 ->
            ( { model | menu2Open = not model.menu2Open }, Cmd.none )

        ToggleSearchBar ->
            ( { model | searchBarOpen = not model.searchBarOpen }, Cmd.none )

        ToAbout ->
            ( { model | page = About }, Cmd.none )

        ToTop ->
            ( { model | page = Top }, Cmd.none )

        InputSearchText str ->
            ( { model | searchText = str }, Cmd.none )

        OpenHover ->
            ( { model | hoverMenuOpen = True }, Cmd.none )

        CloseHover ->
            ( { model | hoverMenuOpen = False }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- view


view : Model -> Browser.Document Msg
view model =
    { title = "layout study1"
    , body =
        [ case model.page of
            Top ->
                mainLayout (top model) model

            About ->
                mainLayout about model
        ]
    }


modalStyle : Float -> List (Element.Attribute msg)
modalStyle alpha =
    [ Background.color <| rgba 0.0 0.0 0.0 alpha
    , htmlAttribute <| Html.Attributes.style "position" "fixed"
    , htmlAttribute <| Html.Attributes.style "z-index" "1000"
    , htmlAttribute <| Html.Attributes.style "top" "0"
    , htmlAttribute <| Html.Attributes.style "left" "0"
    , htmlAttribute <| Html.Attributes.style "width" "100%"
    , htmlAttribute <| Html.Attributes.style "height" "100%"
    , style "cursor" "default"
    ]


mainLayout : Element Msg -> Model -> Html Msg
mainLayout page model =
    layout
        [ Font.size rootFontSize ]
    <|
        column
            [ width fill, height fill ]
            [ nav model
            , page
            , footer
            ]


menu : Element Msg
menu =
    row (modalStyle 0.6)
        [ el [ width fill, height fill, Events.onClick CloseMenu ] <| none
        , column
            [ class "menu"
            , style "z-index" "2000"
            , paddingXY 0 10
            , alignRight
            , height fill
            ]
            [ menuItem "item1"
            , menuItem "itemitemitemitem2"
            , menuItem "item3"
            , menuItem "item4"
            , menuItem "item5"
            ]
        ]


menuItem : String -> Element Msg
menuItem label =
    el
        [ class "menu_item"
        , paddingXY 10 10
        , width fill
        , Events.onClick <| SelDropDownItem label
        ]
    <|
        text label


simpleDropDownItem : String -> Element Msg
simpleDropDownItem label =
    el
        [ class "dropDown_item"
        , paddingXY 10 10
        , width fill
        , Events.onClick <| SelDropDownItem label
        ]
    <|
        text label


dropDown2 : Element Msg
dropDown2 =
    column []
        [ el (modalStyle 0 ++ [ Events.onClick CloseDropDown2 ]) <| none
        , column
            [ class "dropDown"
            , style "z-index" "2000"
            , paddingXY 0 10
            ]
            [ simpleDropDownItem "item1"
            , simpleDropDownItem "itemitemitem2"
            , simpleDropDownItem "item3"
            ]
        ]


hoverMenu : Element Msg
hoverMenu =
    column
        [ class "dropDown"
        , style "z-index" "2000"
        , paddingXY 0 10
        ]
        [ simpleDropDownItem "item1"
        , simpleDropDownItem "itemitemitem2"
        , simpleDropDownItem "item3"
        ]


dropDown : Model -> Element Msg
dropDown model =
    column
        [ class "dropDown"
        , paddingXY 0 10
        , alignRight
        ]
        [ el
            [ tabindex 0
            , class "dropDown_item"
            , width fill
            , paddingXY 10 10
            , Events.onClick <| SelDropDownItem "item1"
            , Events.onFocus <| SetState Open
            , Events.onLoseFocus <| SetState Closed
            ]
          <|
            text "item1"
        , el
            [ tabindex 0
            , class "dropDown_item"
            , width fill
            , paddingXY 10 10
            , Events.onClick <| SelDropDownItem "itemitemitemitem2"
            , Events.onFocus <| SetState Open
            , Events.onLoseFocus <| SetState Closed
            ]
          <|
            text "itemitemitemitem2"
        , el
            [ tabindex 0
            , class "dropDown_item"
            , width fill
            , paddingXY 10 10
            , Events.onClick <| SelDropDownItem "item3"
            , Events.onFocus <| SetState Open
            , Events.onLoseFocus <| SetState Closed
            ]
          <|
            text "item3"
        , el
            -- 先の要素へ移らず閉じるため。しかしスペースが気になる。いらないかも。
            [ tabindex 0
            , Events.onFocus <| SetState Closed
            , Events.onLoseFocus <| SetState Closed
            ]
          <|
            none
        ]


nav : Model -> Element Msg
nav model =
    let
        navHeight =
            if model.menu2Open then
                px 210

            else
                px 70
    in
    column [ width fill ]
        [ el [ width fill, height navHeight ] <| none
        , column
            [ width fill
            , height navHeight
            , style "position" "fixed"
            , style "z-index" "100"
            , class "nav"
            , alignTop
            ]
            [ row
                [ width fill
                , spacing 10
                , padding 10
                ]
                [ el
                    [ class "nav_brand"
                    , Font.size (rem 2)
                    , alignLeft
                    ]
                    (text "Brand")
                , el
                    ([ style "user-select" "none"
                     , class "nav_item"
                     , pointer
                     ]
                        ++ (if model.dropDown2Open then
                                [ below dropDown2 ]

                            else
                                [ Events.onClick OpenDropDown2 ]
                           )
                    )
                  <|
                    text "drop2▼"
                , el [ class "nav_item", Events.onClick ToggleSearchBar ] <| text "🔍"
                , el [ class "nav_item", Events.onClick ToggleMenu2 ] <| text "menu2"
                , el
                    ([ class "nav_item"
                     , Events.onMouseEnter OpenHover
                     , Events.onMouseLeave CloseHover
                     ]
                        ++ (if model.hoverMenuOpen then
                                [ below hoverMenu ]

                            else
                                []
                           )
                    )
                  <|
                    text "hover"
                , el [ alignRight ] (text "Item1")
                , text "Item2"
                , text "Item3"
                , el [ class "nav_item", Events.onClick ToTop ] <| text "top"
                , el [ class "nav_item", Events.onClick ToAbout ] <| text "about"
                , el
                    ([ class "nav_item"
                     ]
                        ++ (if model.menuOpen then
                                [ below menu ]

                            else
                                [ Events.onClick OpenMenu ]
                           )
                    )
                  <|
                    text "menu"
                , el
                    ([ class "nav_item"
                     , id model.dropDown1Config.id
                     , tabindex -1
                     ]
                        ++ (case model.dropDown1Config.state of
                                Closed ->
                                    [ Events.onClick <| SetState Open
                                    ]

                                Open ->
                                    [ below <| dropDown model
                                    , Events.onClick <| SetState Closed
                                    , Events.onLoseFocus <| SetState Closed
                                    ]
                           )
                    )
                  <|
                    text "drop1▼"
                ]
            , if model.menu2Open then
                column
                    [ class "menu"
                    , width fill
                    , paddingXY 0 10
                    ]
                    [ menuItem "hoge"
                    , menuItem "piyo"
                    ]

              else
                none
            , if model.searchBarOpen then
                row [ width fill ]
                    [ el [ width <| fillPortion 15 ] <| none
                    , Input.search
                        [ width <| fillPortion 70
                        , Border.rounded 10
                        ]
                        { onChange = InputSearchText
                        , text = model.searchText
                        , placeholder = Just <| Input.placeholder [] <| text "search"
                        , label = Input.labelHidden "hoge"
                        }
                    , el [ width <| fillPortion 15 ] <| none
                    ]

              else
                none
            ]
        ]


footer : Element msg
footer =
    row
        [ class "footer"
        , width fill
        , padding 10

        -- ボーダーはelm-uiやcssファイルだと効きません
        , style "border-top" "1px solid rgb(0, 0, 0, 0.4)"

        -- フォントカラーはcssでは効きません
        , Font.color <| rgba 0 0 0 0.4
        ]
        [ el [ alignRight ] (text "footer") ]


box =
    el [ Background.color (rgb255 100 100 100), width (px 300), height (px 300) ] <| none


miniBox =
    el [ Background.color (rgb255 100 100 100), width (px 30), height (px 30) ] <| none


class : String -> Attribute msg
class =
    htmlAttribute << Html.Attributes.class


id : String -> Attribute msg
id =
    htmlAttribute << Html.Attributes.id


tabindex : Int -> Attribute msg
tabindex i =
    htmlAttribute <| Html.Attributes.tabindex i


style : String -> String -> Attribute msg
style a b =
    htmlAttribute (Html.Attributes.style a b)


rootFontSize =
    20


scaled =
    round << Element.modular rootFontSize 1.25


rem x =
    round <| rootFontSize * x


about : Element msg
about =
    column [ height fill ]
        [ paragraph [ width (fill |> maximum 400 |> minimum 100), padding 10 ]
            [ el [ alignLeft, padding 5, Font.size (rem 3) ] <| text "A"
            , text "elm-uiを使用してレイアウトしてみました。menuとdrop2は愚直に簡単に実装されています。drop1はあまり意味がなさそうですがフォーカスを制御した少し凝ったものです。"
            ]
        , paragraph [ width (fill |> maximum 400 |> minimum 100), padding 10 ]
            [ el [ alignLeft, padding 5, Font.size (rem 3) ] <| text "B"
            , text "elm-uiを使用すると、cssを利用せずにレイアウト面はできてしまいます。cssの苦痛から少し解放されます。"
            ]
        ]


top : Model -> Element msg
top model =
    row [ width fill ]
        [ el [ width (fillPortion 10) ] <| none
        , row [ explain Debug.todo, width (fillPortion 80), centerX ]
            [ column [ height fill ]
                [ el [ alignRight ] <| text "item1"
                , text "itemitem2"
                ]
            , column [ width fill, height fill, spacing 16, padding 16 ]
                ([ el [ alignLeft ] <| text "hello"
                 , text "world"
                 , text "🐐🐐🐐🐐🐐"
                 , el [ centerX ] <| text "hoge1"
                 , row [ width fill, spacing 4, explain Debug.todo ]
                    [ el [ width (fillPortion 15) ] <| none
                    , el [ Background.color (rgb255 100 200 100), width (fillPortion 70), height (px 300), centerX ] <| text "hoge2"
                    , el [ width (fillPortion 15) ] <| none
                    ]
                 , row [ width fill, spacing 4, explain Debug.todo ]
                    [ el [ width (px 100) ] <| none
                    , el [ Background.color (rgb255 100 200 100), width fill, height (px 300), centerX ] <| el [ centerX ] <| text "hoge3"
                    , el [ width (px 100) ] <| none
                    ]
                 , column []
                    [ row [ width fill ]
                        [ el [ alignLeft ] <| text "best: "
                        , el [ alignRight, Font.size (rem 3) ] <| text "1800.80"
                        ]
                    , row [ width fill ]
                        [ el [ alignLeft ] <| text "keys per minute: "
                        , el [ alignRight, Font.size (rem 3) ] <| text "0.00"
                        ]
                    ]
                 , wrappedRow [ spacing 5 ] <| List.repeat 50 <| miniBox
                 ]
                    ++ List.repeat 5 box
                )
            , column [ height fill, width shrink ] <|
                List.map (\x -> text x) model.rightSideBar
            ]
        , el [ width (fillPortion 10) ] <| none
        ]



-- main


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
