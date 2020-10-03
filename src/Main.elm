module Main exposing (..)

import Playground exposing (..)
import Set
import Array

maps : Array.Array Map
maps =  
    Array.fromList [ map1 , map2, map3 ]

type alias Game =
    { gameState : GameState
    , heroState : HeroState
    , lastDir : Dir
    , map : Map
    , currentMapIdx: Int 
    }


type alias Map =
    { circles : Set.Set ( Float, Float )
    , hero : ( Float, Float )
    }


type GameState
    = Running
    | Lost
    | Win


type HeroState
    = Standing
    | Jumping Dir ( Float, Float )


type Dir
    = Right
    | Left
    | Up
    | Down


main =
    game view update (Game Running Standing Right map1 0)


view computer { gameState, heroState, lastDir, map, currentMapIdx } =
    [words black "Turtle Jump" |> moveUp 300 |> scale 3] ++ mapPicture 30 map.circles ++ [ heroShape lastDir 30 map.hero ] ++
    (case gameState of
        Running ->
            []

        Win ->
            [ words black "You Win!", nextBtn ]

        Lost ->
            [ words black "You Lost!", restartBtn ])

update computer { gameState, heroState, lastDir, map, currentMapIdx } =
    case getGameState (Game gameState heroState lastDir map currentMapIdx) of
        Running ->
            case heroState of
                Standing ->
                    if computer.keyboard.right then
                        mmove Right (Game gameState heroState Right map currentMapIdx)

                    else if computer.keyboard.left then
                        mmove Left (Game gameState heroState Left map currentMapIdx)

                    else if computer.keyboard.up then
                        mmove Up (Game gameState heroState Up map currentMapIdx)

                    else if computer.keyboard.down then
                        mmove Down (Game gameState heroState Down map currentMapIdx)

                    else
                        Game gameState heroState lastDir map currentMapIdx

                Jumping dir target ->
                    mmoveHero dir target (Game gameState heroState lastDir map currentMapIdx)

        Win ->
            if computer.mouse.click && isMouseOn computer.mouse.x computer.mouse.y 60 20 0 -20 then
                case Array.get (currentMapIdx + 1) maps of
                    Just nextMap ->
                        Game Running Standing Right nextMap (currentMapIdx + 1)
                    Nothing ->
                        Game Running Standing Right map currentMapIdx
            else
                Game Win heroState lastDir map currentMapIdx

        Lost ->
            if computer.mouse.click && isMouseOn computer.mouse.x computer.mouse.y 70 20 0 -20 then
                case Array.get currentMapIdx maps of
                    Just currentMap -> 
                        Game Running Standing Right currentMap currentMapIdx
                    Nothing -> -- SHOULD NOT HAPPEN
                        Game Lost heroState lastDir map currentMapIdx
            else
                Game Lost heroState lastDir map currentMapIdx


getGameState : Game -> GameState
getGameState { gameState, heroState, map } =
    case heroState of
        Standing ->
            if Set.size map.circles == 1 then
                Win

            else if not <| canMove map then
                Lost

            else
                Running

        _ ->
            Running


mmoveHero : Dir -> ( Float, Float ) -> Game -> Game
mmoveHero dir target { gameState, heroState, lastDir, map, currentMapIdx } =
    if equalWithEpsilon map.hero target then
        Game gameState Standing lastDir (Map map.circles target) currentMapIdx

    else
        case dir of
            Right ->
                Game gameState heroState lastDir (Map map.circles (Tuple.mapFirst ((+) 0.1) map.hero)) currentMapIdx

            Left ->
                Game gameState heroState lastDir (Map map.circles (Tuple.mapFirst (\x -> x - 0.1) map.hero)) currentMapIdx

            Up ->
                Game gameState heroState lastDir (Map map.circles (Tuple.mapSecond ((+) 0.1) map.hero)) currentMapIdx

            Down ->
                Game gameState heroState lastDir (Map map.circles (Tuple.mapSecond (\y -> y - 0.1) map.hero)) currentMapIdx


mmove : Dir -> Game -> Game
mmove dir game =
    let
        circleFilter =
            case dir of
                Right ->
                    isCircleRightward

                Left ->
                    isCircleLeftward

                Up ->
                    isCircleUpward

                Down ->
                    isCircleDownward

        nextXY =
            heroNextXY circleFilter game.map
    in
    case nextXY of
        Just next ->
            Game game.gameState (Jumping dir next) game.lastDir (Map (Set.remove game.map.hero game.map.circles) game.map.hero) game.currentMapIdx

        Nothing ->
            game


equalWithEpsilon : ( Float, Float ) -> ( Float, Float ) -> Bool
equalWithEpsilon x y =
    distance x y < 0.01


tile : Number -> Shape
tile radius =
    group
        [ circle black radius
        , circle white (radius - 3)
        ]

restartBtn : Shape
restartBtn =
    group [ rectangle black 75 25
          , rectangle lightRed 70 20
          , words black "Restart"
          ] |> moveDown 20

nextBtn : Shape
nextBtn =
    group [ rectangle black 65 25
          , rectangle lightGreen 60 20
          , words black "Next"
          ] |> moveDown 20

mapPicture : Number -> Set.Set ( Float, Float ) -> List Shape
mapPicture radius circles =
    circles
        |> Set.toList
        |> List.map (\( x, y ) -> tile radius |> move (x * (radius * 2.5)) (y * (radius * 2.5)))


-- heroShape : Number -> ( Float, Float ) -> Shape
-- heroShape radius ( x, y ) =
--    circle red 10 |> move (x * (radius * 2.5)) (y * (radius * 2.5))

heroShape : Dir -> Number -> (Float, Float) -> Shape
heroShape dir radius (x, y) =
    image radius radius "https://elm-lang.org/images/turtle.gif" 
        |> move (x * (radius * 2.5)) (y * (radius * 2.5))
        |> rotate (dirToAngle dir)

dirToAngle : Dir -> Number
dirToAngle dir =
    case dir of
        Up -> 90
        Down -> -90
        Right -> 0
        Left -> 180

map1 : Map
map1 =
    Map (Set.fromList [ ( 1, 0 ), ( 1, 1 ), ( 0, 1 ), ( -1, 1 ), ( -1, 0 ), ( -1, -1 ) ]) ( -1, 0 )

map2 : Map
map2 =
    Map (Set.fromList [(0,2),(0,1),(0,0),(0,-1), (0,-2),(-2,0),(-2,-1), (2,0),(2,-1)]) (-2, 0)
    
map3 : Map
map3 =
    Map (Set.fromList [(-2,0),(-2,3),(-2,2), (0,2),(0,3), (2,3), (2,2), (2,0), (2,-2), (-2,-2), (0,-2), (0,-3)]) (-2,2)

heroMoving : Map -> Bool
heroMoving map =
    if not <| Set.member map.hero map.circles then
        True

    else
        False

heroNextXY : (( Float, Float ) -> ( Float, Float ) -> Bool) -> Map -> Maybe ( Float, Float )
heroNextXY circleFilter { circles, hero } =
    let
        ( heroX, heroY ) =
            hero

        nextCircles =
            Set.remove ( heroX, heroY ) circles

        circlesRightwards =
            Set.filter (circleFilter hero) nextCircles

        nearest =
            Set.foldl (nearestCircle hero) ( 100, 100 ) circlesRightwards
    in
    case Set.isEmpty circlesRightwards of
        True ->
            Nothing

        False ->
            Just <| nearest


moveHero : (( Float, Float ) -> ( Float, Float ) -> Bool) -> Map -> Maybe Map
moveHero circleFilter { circles, hero } =
    let
        ( heroX, heroY ) =
            hero

        nextCircles =
            Set.remove ( heroX, heroY ) circles

        circlesRightwards =
            Set.filter (circleFilter hero) nextCircles

        nearest =
            Set.foldl (nearestCircle hero) ( 100, 100 ) circlesRightwards
    in
    case Set.isEmpty circlesRightwards of
        True ->
            Nothing

        False ->
            Just <| Map nextCircles nearest


canMove : Map -> Bool
canMove map =
    moveHero isCircleRightward map
        |> tryAnother (moveHero isCircleLeftward map)
        |> tryAnother (moveHero isCircleUpward map)
        |> tryAnother (moveHero isCircleDownward map)
        |> Maybe.withDefault False


tryAnother : Maybe b -> Maybe a -> Maybe Bool
tryAnother maybe1 maybe2 =
    case maybe1 of
        Just value ->
            Just True

        Nothing ->
            Maybe.map (\_ -> True) maybe2


nearestCircle : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
nearestCircle hero circle nearest =
    if distance circle hero < distance nearest hero then
        circle

    else
        nearest


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance a1 a2 =
    Basics.sqrt <| distanceX a1 a2 ^ 2 + distanceY a1 a2 ^ 2


distanceX : ( Float, Float ) -> ( Float, Float ) -> Float
distanceX ( x1, _ ) ( x2, _ ) =
    x1 - x2


distanceY : ( Float, Float ) -> ( Float, Float ) -> Float
distanceY ( _, y1 ) ( _, y2 ) =
    y1 - y2


isCircleRightward : ( Float, Float ) -> ( Float, Float ) -> Bool
isCircleRightward hero circle =
    equalY circle hero && isXLarger circle hero


isCircleLeftward : ( Float, Float ) -> ( Float, Float ) -> Bool
isCircleLeftward hero circle =
    equalY circle hero && isXLarger hero circle


isCircleUpward : ( Float, Float ) -> ( Float, Float ) -> Bool
isCircleUpward hero circle =
    equalX circle hero && isYLarger circle hero


isCircleDownward : ( Float, Float ) -> ( Float, Float ) -> Bool
isCircleDownward hero circle =
    equalX circle hero && isYLarger hero circle


equalY : ( Float, Float ) -> ( Float, Float ) -> Bool
equalY ( _, y1 ) ( _, y2 ) =
    y1 == y2


isYLarger : ( Float, Float ) -> ( Float, Float ) -> Bool
isYLarger ( _, y1 ) ( _, y2 ) =
    y1 > y2


equalX : ( Float, Float ) -> ( Float, Float ) -> Bool
equalX ( x1, _ ) ( x2, _ ) =
    x1 == x2


isXLarger : ( Float, Float ) -> ( Float, Float ) -> Bool
isXLarger ( x1, _ ) ( x2, _ ) =
    x1 > x2
    
isMouseOn : Float -> Float -> Float -> Float -> Float -> Float -> Bool
isMouseOn mouseX mouseY width height moveX moveY =
    let
        maxX = (width/2) + moveX
        minX = -1 * (width/2) + moveX
        maxY = (height/2) + moveY
        minY = -1 * (height/2) + moveY
    in
        mouseX < maxX && mouseX > minX &&
        mouseY < maxY && mouseY > minY
