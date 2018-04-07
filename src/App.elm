module App exposing (Intent, Model, init, vigor)

import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick)
import Intl
import Vigor exposing (Recipe, Vigor)


type Intent
    = Increment


type alias Model =
    { counter : Int
    }


vigor : Recipe Model Intent ctx msg -> Vigor ctx msg
vigor =
    Vigor.summon
        { subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : Int -> Model
init initialCounter =
    { counter = initialCounter
    }


update : Intent -> Model -> ( Model, Cmd Intent )
update msg ({ counter } as model) =
    case msg of
        Increment ->
            ( { model | counter = counter + 1 }, Cmd.none )


view : Model -> Html Intent
view { counter } =
    Html.div []
        [ Html.button [ onClick Increment ] [ Html.text "+" ]
        , Html.span [] [ Html.text (toString counter) ]
        ]


someButton : Intl.Context -> List (Attribute Intl.Msg) -> Html Intl.Msg
someButton =
    Intl.translatable Html.button "Some Button" "some.button"


someLabel : Intl.Context -> List (Attribute Intl.Msg) -> Html Intl.Msg
someLabel =
    Intl.translatable Html.div "Hello, World!" "some.label"


--view : ctx -> Model -> List (Html Intent)
--view ctx { counter } =
--    [ someLabel ctx []
--    , someButton ctx
--        [ onClick Increment
--        ]
--    , Html.text (toString counter)
--    , someLabel ctx [ Attr.class "--different" ]
--    ]
