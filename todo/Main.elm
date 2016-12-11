module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


type alias Todo =
    { title : String
    , completed : Bool
    , editing : Bool
    , identifier : Int
    }


type FilterState
    = All
    | Active
    | Completed


type alias Model =
    { todos : List Todo
    , todo : Todo
    , filter : FilterState
    , nextIdentifier : Int
    }


type Msg
    = Add
    | Complete Todo
    | Uncomplete Todo
    | Delete Todo
    | Filter FilterState
    | UpdateField String
    | ClearCompleted
    | NoOp


onKeyUp : (Int -> Msg) -> Attribute Msg
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)


initialModel =
    { todos =
        [ { title = "the first todo"
          , completed = False
          , editing = False
          , identifier = 1
          }
        ]
    , todo = { newTodo | identifier = 2 }
    , filter = All
    , nextIdentifier = 3
    }


newTodo : Todo
newTodo =
    { title = ""
    , completed = False
    , editing = False
    , identifier = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add ->
            { model
                | todos = model.todo :: model.todos
                , todo = { newTodo | identifier = model.nextIdentifier }
                , nextIdentifier = model.nextIdentifier + 1
            }

        Complete todo ->
            let
                updateTodo thisTodo =
                    if thisTodo.identifier == todo.identifier then
                        { todo | completed = True }
                    else
                        thisTodo
            in
                { model
                    | todos = List.map updateTodo model.todos
                }

        ClearCompleted ->
            { model | todos = List.filter (\todo -> todo.completed == False) model.todos }

        Uncomplete todo ->
            let
                updateTodo thisTodo =
                    if thisTodo.identifier == todo.identifier then
                        { todo | completed = False }
                    else
                        thisTodo
            in
                { model
                    | todos = List.map updateTodo model.todos
                }

        Delete todo ->
            { model
                | todos = List.filter (\todo_ -> todo_.identifier /= todo.identifier) model.todos
            }

        Filter filterState ->
            { model | filter = filterState }

        UpdateField str ->
            let
                todo =
                    model.todo

                updatedTodo =
                    { todo | title = str }
            in
                { model | todo = updatedTodo }

        NoOp ->
            model


stylesheet =
    let
        tag =
            "link"

        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "todo.css"
            ]

        children =
            []
    in
        node tag attrs children


is13 : Int -> Msg
is13 x =
    case x of
        13 ->
            Add

        _ ->
            NoOp


todoView : Todo -> Html Msg
todoView todo =
    let
        handleComplete =
            case todo.completed of
                True ->
                    (\_ -> Uncomplete todo)

                False ->
                    (\_ -> Complete todo)
    in
        li [ classList [ ( "completed", todo.completed ) ] ]
            [ div [ class "view" ]
                [ input
                    [ class "toggle"
                    , type_ "checkbox"
                    , checked todo.completed
                    , onCheck handleComplete
                    ]
                    []
                , label [] [ text todo.title ]
                , button [ class "destroy", onClick (Delete todo) ] []
                ]
            ]


filterItemView : Model -> FilterState -> Html Msg
filterItemView model filterState =
    li []
        [ a
            [ classList [ ( "selected", (model.filter == filterState) ) ]
            , href "#"
            , onClick (Filter filterState)
            ]
            [ text (toString filterState) ]
        ]


filteredTodos : Model -> List Todo
filteredTodos model =
    let
        matchesFilter =
            case model.filter of
                All ->
                    always True

                Active ->
                    \todo -> todo.completed == False

                Completed ->
                    \todo -> todo.completed == True
    in
        List.filter matchesFilter model.todos


view : Model -> Html Msg
view model =
    div []
        [ stylesheet
        , section [ class "todoapp" ]
            [ header [ class "header" ]
                [ h1 [] [ text "Todos" ]
                , input
                    [ class "new-todo"
                    , placeholder "What needs to be done?"
                    , autofocus True
                    , onKeyUp is13
                    , onInput UpdateField
                    , value model.todo.title
                    ]
                    []
                ]
            , section [ class "main" ]
                [ ul [ class "todo-list" ]
                    (List.map todoView (filteredTodos model))
                ]
            , footer [ class "footer" ]
                [ span [ class "todo-count" ]
                    [ strong []
                        [ text
                            (toString
                                (List.length
                                    (List.filter (\todo -> todo.completed == False) model.todos)
                                )
                            )
                        ]
                    , text " items left"
                    ]
                , ul [ class "filters" ]
                    [ filterItemView model All
                    , filterItemView model Active
                    , filterItemView model Completed
                    ]
                , button [ class "clear-completed", onClick ClearCompleted ] [ text "Clear completed" ]
                ]
            ]
        ]


main =
    Html.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }
