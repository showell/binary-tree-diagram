module BinaryTreeDiagram exposing (BinaryTree(..), diagramView)

{-| Create a diagram of a binary tree.

@docs BinaryTree, diagramView

-}

import Html
    exposing
        ( Html
        )
import Svg
import Svg.Attributes


{-| Example tree:

```elem
Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)
```

The arguments are value, leftTree, rightTree.

-}
type BinaryTree v
    = Node v (BinaryTree v) (BinaryTree v)
    | Empty


type alias CoordNode v =
    { coord : ( Float, Float, Float )
    , data : v
    }


type alias Edge v =
    { parent : CoordNode v
    , child : CoordNode v
    }


type alias DiagramData v =
    { coordNodes : List (CoordNode v)
    , edges : List (Edge v)
    }


{-| Pass in functions to set the color and text.

    diagramView (\v -> v.color) (\v -> v.text) tree

    diagramView (\_ -> "blue") (\_ -> "") tree

-}
diagramView : (v -> String) -> (v -> String) -> BinaryTree v -> Html msg
diagramView getNodeColor getNodeText tree =
    let
        data : DiagramData v
        data =
            diagramData tree

        coordMaxY coordNode =
            let
                ( _, y, r ) =
                    coordNode.coord
            in
            y + r

        viewBoxHeight =
            data.coordNodes
                |> List.map coordMaxY
                |> List.maximum
                |> Maybe.withDefault 0
                |> String.fromFloat

        viewBox =
            "0 0 100 " ++ viewBoxHeight

        drawEdge : Edge v -> Html msg
        drawEdge edge =
            let
                ( x1, y1, r ) =
                    edge.parent.coord

                ( x2, y2, _ ) =
                    edge.child.coord

                stroke =
                    "gray"

                strokeWidth =
                    r / 30.0
            in
            Svg.line
                [ Svg.Attributes.x1 (String.fromFloat x1)
                , Svg.Attributes.y1 (String.fromFloat y1)
                , Svg.Attributes.x2 (String.fromFloat x2)
                , Svg.Attributes.y2 (String.fromFloat y2)
                , Svg.Attributes.stroke stroke
                , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
                ]
                []

        drawEdges : List (Html msg)
        drawEdges =
            data.edges
                |> List.map drawEdge

        drawCoordNode : CoordNode v -> Html msg
        drawCoordNode coordNode =
            let
                ( cx, cy, r ) =
                    coordNode.coord

                fontSize =
                    r * 0.7

                strokeWidth =
                    r / 30.0

                fill =
                    getNodeColor coordNode.data

                circle =
                    Svg.circle
                        [ Svg.Attributes.cx (String.fromFloat cx)
                        , Svg.Attributes.cy (String.fromFloat cy)
                        , Svg.Attributes.r (String.fromFloat r)
                        , Svg.Attributes.fill fill
                        ]
                        []

                text =
                    getNodeText coordNode.data

                textAnchor =
                    "middle"

                textFill =
                    "white"

                x =
                    cx

                y =
                    cy + (fontSize / 4)

                label =
                    Svg.text_
                        [ Svg.Attributes.x (String.fromFloat x)
                        , Svg.Attributes.y (String.fromFloat y)
                        , Svg.Attributes.fontSize (String.fromFloat fontSize)
                        , Svg.Attributes.fill textFill
                        , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
                        , Svg.Attributes.textAnchor textAnchor
                        ]
                        [ Svg.text text
                        ]
            in
            Svg.g [] [ circle, label ]

        drawCoordNodes : List (Html msg)
        drawCoordNodes =
            data.coordNodes
                |> List.map drawCoordNode
    in
    drawEdges
        ++ drawCoordNodes
        |> Svg.svg
            [ Svg.Attributes.width "100%"
            , Svg.Attributes.viewBox viewBox
            ]


diagramData : BinaryTree v -> DiagramData v
diagramData tree =
    let
        edges parentCoordNode childCoordNodeTree childDiagram =
            case childCoordNodeTree of
                Empty ->
                    []

                Node childCoordNode _ _ ->
                    let
                        newEdge =
                            { parent = parentCoordNode
                            , child = childCoordNode
                            }
                    in
                    newEdge :: childDiagram.edges

        diagram coordsTree =
            case coordsTree of
                Empty ->
                    { coordNodes = []
                    , edges = []
                    }

                Node topCoordNode leftCoordTree rightCoordTree ->
                    let
                        leftDiagram =
                            diagram leftCoordTree

                        rightDiagram =
                            diagram rightCoordTree

                        leftEdges =
                            edges topCoordNode leftCoordTree leftDiagram

                        rightEdges =
                            edges topCoordNode rightCoordTree rightDiagram

                        newEdges =
                            leftEdges ++ rightEdges

                        newCoordNodes =
                            topCoordNode
                                :: (leftDiagram.coordNodes ++ rightDiagram.coordNodes)
                    in
                    { coordNodes = newCoordNodes
                    , edges = newEdges
                    }
    in
    diagram (toCoordsTree tree)


toCoordsTree : BinaryTree v -> BinaryTree (CoordNode v)
toCoordsTree fullTree =
    let
        makeTree : Float -> Float -> Float -> Float -> BinaryTree v -> BinaryTree (CoordNode v)
        makeTree fullWidth ratioFactor xOffset yOffset tree =
            case tree of
                Node data left_ right_ ->
                    let
                        halfWidth =
                            fullWidth / 2

                        r =
                            ratioFactor * halfWidth

                        cx =
                            xOffset + halfWidth

                        cy =
                            yOffset + r

                        lxOffset =
                            cx - halfWidth

                        rxOffset =
                            cx

                        childYOffset =
                            yOffset + 2.2 * r

                        childRatioFactor =
                            min 0.9 (ratioFactor * 1.8)

                        coordNode =
                            { coord = ( cx, cy, r )
                            , data = data
                            }

                        left =
                            left_
                                |> makeTree halfWidth childRatioFactor lxOffset childYOffset

                        right =
                            right_
                                |> makeTree halfWidth childRatioFactor rxOffset childYOffset
                    in
                    Node coordNode left right

                Empty ->
                    Empty
    in
    makeTree 100 0.08 0 0 fullTree
