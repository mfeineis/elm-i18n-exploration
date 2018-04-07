module History
    exposing
        ( History
        , append
        , canStepBack
        , canStepForward
        , init
        , present
        , stepBack
        , stepForward
        )


type alias History a =
    { future : List a
    , past : List a
    , present : a
    }


append : a -> History a -> History a
append it { future, past, present } =
    { future = []
    , past = present :: past
    , present = it
    }


canStepBack : History a -> Bool
canStepBack { past } =
    not (List.isEmpty past)


canStepForward : History a -> Bool
canStepForward { future } =
    not (List.isEmpty future)


init : a -> History a
init it =
    { future = []
    , past = []
    , present = it
    }


present : History a -> a
present { present } =
    present


stepBack : History a -> History a
stepBack ({ future, past, present } as unchanged) =
    case past of
        moment :: rest ->
            { future = present :: future
            , past = rest
            , present = moment
            }

        [] ->
            unchanged


stepForward : History a -> History a
stepForward ({ future, past, present } as unchanged) =
    case future of
        moment :: rest ->
            { future = rest
            , past = present :: past
            , present = moment
            }

        [] ->
            unchanged
