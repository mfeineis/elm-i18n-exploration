module Vigor exposing (Host, Recipe, Vigor, compose, summon)

import Html exposing (Html)


type alias Vigor ctx msg =
    { subscriptions : ctx -> Sub msg
    , update : msg -> ctx -> ( ctx, Cmd msg )
    , view : ctx -> Html msg
    }


type alias Recipe myModel myMsg ctx msg =
    { incoming : msg -> ctx -> Maybe myMsg
    , outgoing : myMsg -> msg
    , read : ctx -> myModel
    , store : ctx -> myModel -> ctx
    }


type alias Ingredients model msg =
    { subscriptions : model -> Sub msg
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Html msg
    }


type alias Host flags model msg =
    { init : flags -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Html msg
    }


summon : Ingredients myModel myMsg -> Recipe myModel myMsg ctx msg -> Vigor ctx msg
summon { subscriptions, update, view } recipe =
    { subscriptions = \ctx -> subscriptions (recipe.read ctx) |> Sub.map recipe.outgoing
    , update =
        \msg ctx ->
            case recipe.incoming msg ctx of
                Nothing ->
                    ( ctx, Cmd.none )

                Just myMsg ->
                    update myMsg (recipe.read ctx)
                        |> (\( model, cmd ) -> ( recipe.store ctx model, Cmd.map recipe.outgoing cmd ))
    , view = \ctx -> view (recipe.read ctx) |> Html.map recipe.outgoing
    }


compose : Host flags model msg -> List (Vigor model msg) -> Host flags model msg
compose program vigors =
    let
        init =
            program.init

        subscriptions =
            \model ->
                Sub.batch <|
                    program.subscriptions model
                        :: List.map (\vigor -> vigor.subscriptions model) vigors

        merge msg vigors ( model, cmd ) =
            case vigors of
                vigor :: rest ->
                    let
                        ( newModel, newCmd ) =
                            vigor msg model
                    in
                    merge msg rest ( newModel, Cmd.batch [ cmd, newCmd ] )

                [] ->
                    ( model, cmd )

        update =
            \msg model ->
                merge msg (program.update :: List.map .update vigors) ( model, Cmd.none )

        view =
            program.view
    in
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }
