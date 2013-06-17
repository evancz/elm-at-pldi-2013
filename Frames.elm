module Frames where

import Keyboard
import Mouse
import Window

data Part
    = SuperTitle Text
    | Title Text
    | SubTitle Text
    | Bullet Text
    | SubBullet Text
    | Anything Element
    | Mouse (Int -> Int -> Element)

superTitle = SuperTitle . toText
title      = Title      . toText
subTitle   = SubTitle   . toText
bullet     = Bullet     . toText
subBullet  = SubBullet  . toText

data Position = Offset Int | Center

txt : Int -> Position -> Float -> Color -> Text -> Element
txt wid position hght colr content =
    let t = text . Text.height hght <| Text.color colr content
    in  case position of
          Center -> container wid (heightOf t) middle t
          Offset offset ->
              let t' = width (wid-offset) t
              in  flow down [ spacer wid (if offset < 50 then 30 else 6)
                            , spacer offset (heightOf t') `beside` t' ]

myBlue : Color
myBlue = rgb 96 181 204

myBlue' : Color
myBlue' = rgb 90 99 120

showPart : (Int,Int) -> Part -> Element
showPart (x,y) part =
    case part of
      SuperTitle str -> spacer 800 250 `above` txt 800 Center 3.2 myBlue str
      Title str -> spacer 800 40 `above` txt 800 Center 3.2 myBlue str
      SubTitle str -> spacer 800 10 `above` txt 800 Center 2 grey str
      Bullet str -> txt 800 (Offset 20) 2 grey str
      SubBullet str -> txt 800 (Offset 60) 1.4 grey str
      Anything elem -> elem
      Mouse f -> f x y
      _ -> asText 42

paperTitle = [markdown|
<span style="font-size:3.2em; color: rgb(96,181,204);">
Asynchronous<br>Functional Reactive Programming<br>
<span style="float:right;">for GUIs</span>
</span>
|]

titlePage : Element
titlePage = 
    let style hght = txt 400 Center hght grey . toText
        author name affl = flow down [ style 1.4 name, style 1 affl ]
    in flow down [ spacer 800 180,
                   container 800 (heightOf paperTitle) middle paperTitle,
                   spacer 800 100,
                   flow right [ author "Evan Czaplicki" "Prezi",
                                author "Stephen Chong" "Harvard" ] ]

mousePosition : Element
mousePosition = container 800 80 middle [markdown|```haskell
Mouse.position : Signal (Int,Int)
```
|]

myLift1 : Element
myLift1 = [markdown|```haskell

    lift  : (a -> b) -> Signal a -> Signal b
```
|]

myLift2 : Element
myLift2 = [markdown|```haskell
    lift2 : (a -> b -> c) ->
              Signal a -> Signal b -> Signal c
```
|]

shapesCode = [markdown|```haskell
main = lift scene Mouse.position

scene : (Int,Int) -> Element
scene (x,y) =
  collage 240 240
    [ ngon 5 60 |> filled (rgb 90 99 120)
                |> rotate (degrees (toFloat x))
                |> scale (toFloat y / 100) ]
```
|]

shapes x y =
    collage 800 240
    [ move (0-150,0) . scale 0.5 <| toForm shapesCode
    , ngon 5 60 |> filled myBlue'
                |> rotate (degrees (toFloat x))
                |> scale (toFloat y / 400)
                |> move (220,0)
    ]

myFoldp : Element
myFoldp = [markdown|```haskell
foldp : (a -> b -> b) -> b -> Signal a -> Signal b
```
|]

myCount : Element
myCount = [markdown|```haskell
        foldp (\\_ c -> c+1) 0 Mouse.clicks
```
|]

myAsync : Element
myAsync = [markdown|```haskell
           async : Signal a -> Signal a
```
|]

positionHelp = [markdown|```haskell
Mouse.position : Signal (Int,Int)

        asText : a -> Element
```
|]

translationHelp = [markdown|```haskell
   words : Signal String

toFrench : a -> Element
```
|]

helpAt pos = scale 0.6 . opacity 0.4 . move pos

syncHelp = [markdown|```haskell
   positions : Signal Element
translations : Signal (String,String)
     display : Element -> (String,String) -> Element
```
|]

frames : [[Part]]
frames =
    [ [ Anything titlePage ]
    , [ title "Functional Reactive Programming"
      , bullet "Allow interactivity in a purely functional setting"
      , subBullet "Promising approach for GUIs, games, and robotics"
      , bullet "FRP has struggled with semantics and performance. E.g:"
      , subBullet "Needless recomputation"
      , subBullet "Freezing on long computations"
      , subBullet "Bad interactions with lazy evaluation"
      ]
    , [ title "Key Contributions"
      , bullet "Simple and efficient semantics for FRP"
      , subBullet "Event-driven"
      , subBullet "Eager evaluation"
      , subBullet "Concurrent runtime / pipelining"
      , subBullet "Asynchrony"
      , bullet "Elm, a practical language for purely functional GUIs"
      , subBullet "Designed for FRP and graphics"
      , subBullet "This presentation is an Elm program!"
      ]
    , [ title "Elm"
      , subTitle "A practical language for purely functional GUIs"
      , bullet "Language features:"
      , subBullet "Functional Reactive Programming"
      , subBullet "Strongly statically typed with type inference"
      , subBullet "Extensible Records with structural typing"
      , subBullet "Module system and core libraries"
      , bullet "Social features:"
      , subBullet "Open-source project with lively community"
      , subBullet "Hundreds of interactive examples at elm-lang.org"
      , subBullet "A highly visible platform for advancing FRP"
      ]
    , [ superTitle "Functional Reactive Programming"
      , subTitle "pure and efficient interactions"
      ]

    , [ title "Signals"
      , bullet "Values that change over time"
      , subBullet "Mouse, Keyboard, Touch"
      , subBullet "Time, HTTP, File I/O"
      , bullet "For example,"
      , Anything mousePosition
      , SubBullet <| monospace (toText "Mouse.position") ++
                     toText " is the position of the mouse " ++
                     italic (toText "right now")
      , SubBullet <| toText "Anything that depends on " ++
                     monospace (toText "Mouse.position") ++
                     toText " is updated automatically"
      , let t clr = Text.color clr . toText
        in  Mouse (\x y -> container 800 140 middle . text . monospace . typeface "inconsolata" . Text.height 2 <| concat [ t myBlue  "("
                                                                                                                          , t myBlue' (show x)
                                                                                                                          , t myBlue  ","
                                                                                                                          , t myBlue' (show y)
                                                                                                                          , t myBlue  ")" ])
      ]

    , [ title "Signals"
      , bullet "Signals always have a current value"
      , bullet "Signals change discretely, only as needed"
      , bullet "Rules about the order of events:"
      , subBullet "Order is always maintained within a signal"
      , subBullet "Order does not need to be maintained between signals"
      ]

    , [ title "Transforming Signals"
      , Anything myLift1
      , Anything myLift2
      , Mouse shapes
      ]

    , [ title "Stateful Signals"
      , subTitle "Signals that depend on the past"
      , Anything myFoldp
      , Anything myCount
      ]
    , [ title "Signals so far"
      , Anything myLift1
      , Anything myFoldp
      , Anything (spacer 800 60)
      , subTitle "But we are missing something!"
      ]

    , [ title "Concurrency"
      , bullet "Graphical user interfaces are naturally concurrent"
      , subBullet "Long computations should not block unrelated computations"
      , bullet "We introduce concurrency without:"
      , subBullet "event-ordering problems"
      , subBullet "low-level abstractions"
      , subBullet "impurity"
      ]

    , [ title "Signal Graph"
      , subTitle "A graph interpretation for Elm&rsquo;s core FRP primitives"
      , Anything <| collage 800 400
                     [ scale 0.5 . toForm <| image 1161 473 "images/graphs.png" ]
      ]

    , [ title "Signal Graph"
      , Anything [markdown|```haskell
      positions = lift asText Mouse.position
```
|]
      , Anything <|
          collage 800 400
            [ move (0-200,0) . toForm <| image 212 323 "images/positions.png"
            , helpAt (150,0) . toForm <| positionHelp ]
      ]

    , [ title "Synchronization"
      , Anything [markdown|```haskell
     translations =
         lift2 (,) words (lift toFrench words)
```
|]
      , Anything <|
          collage 800 350
            [ scale 0.8 . move (0-200,0) . toForm <| image 309 385 "images/translations.png"
            , helpAt (140,0) . toForm <| translationHelp ]
      ]

    , [ title "Concurrency and Pipelining"
      , Anything [markdown|```haskell
       lift2 display positions translations
```
|]
      , Anything <|
          collage 800 400
            [ scale 0.8 . move (0-200,0) . toForm <| image 381 465 "images/sync.png"
            , helpAt (140,100) . toForm <| syncHelp ]
      ]

    , [ title "Asynchrony"
      , Anything myAsync
      , Anything <| [markdown|```haskell
       positions            async positions
```
|]
      , Anything <|
          collage 800 330
          [ move (0-200,0) . toForm <| image 212 323 "images/positions.png"
          , move (200,0) . toForm <| image 205 291 "images/asyncPositions.png"
          ]
      ]

    , [ title "Asynchrony"
      , Anything [markdown|```haskell
   lift2 display positions (async translations)
```
|]
      , Anything <| collage 800 400
                     [ scale 0.8 . toForm <| image 525 451 "images/async.png" ]
      ]

    , [ title "Signals"
      , Anything myLift1
      , Anything myFoldp
      , Anything [markdown|```haskell
           async : Signal a -> Signal a
```
|]
      ]

    , [ title "Relative Expressiveness"
      , bullet "Monadic FRP"
      , subBullet "Causes severe performance problems in pure languages"
      , subBullet "Not allowed in Elm"
      , bullet "Arrowized FRP"
      , subBullet "Arrowized FRP allows &ldquo;dynamic switching and dynamic collections&rdquo;"
      , subBullet "foldp can fully express Arrowized FRP"
      , subBullet "Implemented as an Elm library"
      , bullet "Parallel FRP"
      , subBullet "Variation of Monadic FRP designed for servers"
      , subBullet "Reordering events within a signal"
      , subBullet "Combines nicely with our core language, but not ideal for GUIs"
      ]

    , [ superTitle "Guaranteeing Efficiency"
      , subTitle "Evaluation strategies, types, and concurrency"
      ]

    , [ title "Efficiency via Evaluation"
      , bullet "Elm uses a two-tiered intermediate language"
      , subBullet "Influenced by Real-Time FRP, which proved efficiency bounds"
      , Anything . container 800 340 middle . width 600 <| image 996 252 "images/values.png"
      ]

    , [ title "Efficiency via Types"
      , bullet "Elm does not allow signals-of-signals (Monadic FRP)"
      , subBullet "Monadic FRP has serious semantic and efficiency problems in pure languages"
      , subBullet "Elm has a two-tiered type system rules out Signals-of-Signals"
      , Anything . container 800 200 middle . width 600 <| image 726 168 "images/types.png"
      , bullet "Signal graphs can be built programmatically"
      , subBullet "Type system ensures that source code maps to intermediate language"
      ]

    , [ title "Efficiency via Types"
      , bullet "Proved soundness of our type system"
      , subBullet "Well-typed source produces a well-formed intermediate value"
      , Anything . container 800 380 middle . width 600 <| image 805 345 "images/judgements.png"
      ]

    , [ title "Efficiency via Implementation"
      , subTitle "Mapping from intermediate language to Concurrent ML"
      ]

    , [ title "Implementations of Elm"
      , bullet "Elm to JavaScript Compiler"
      , subBullet "Primary implementation of Elm, many libraries"
      , subBullet "Visible and viable platform for a functional GUI language"
      , subBullet "Limited support for async, only for HTTP and file I/O"
      , bullet "Elm to Haskell Interpreter"
      , subBullet "Implementation of core primitives"
      , subBullet "Fully supports asynchrony and pipelining"
      , subBullet "Laziness is not a good match for Elm"
      , subBullet "Cross-platform graphics support is weak"
      ]

    , [ Anything [markdown|<iframe src="http://localhost:8000/edit/examples/Intermediate/Mario.elm" width=800 height=600 style="border:none;"></iframe>|]
      ]

    , [ superTitle "Thank you!"
      , subTitle "And remember to try out Elm at elm-lang.org!"
      ]

    ]

{--}
showFrame pos frame =
    layers [ spacer 800 600 |> color (rgb 245 245 245)
           , flow down <| map (showPart pos) frame ]

allFrames pos = flow down . intersperse (color white <| spacer 800 10) <| map (showFrame pos) frames

scene w pos = let elem = allFrames pos
              in  container w (heightOf elem) middle elem

main = lift2 scene Window.width Mouse.position
--}
{--
steps =
    let f frame i = map ((,) i) [0..length frame - 1]
    in  concat <| zipWith f frames [0 .. length frames]

lenSteps = length steps

mouseSteps =
    let combine i j frm = case frm of
                            Mouse _ -> [(i,j)]
                            _ -> []
        f frame i = concat <| zipWith (combine i) [0..length frame - 1] frame
    in  concat <| zipWith f frames [0 .. length frames]

data Event = KeyPress Int | TimeDelta Time

arrows = dropIf (\n -> n == 0) 0 <| .x <~ Keyboard.arrows

input : Signal Event
input = merge (KeyPress <~ arrows)
        (TimeDelta <~ (30 `fpsWhen` ((2*second) `since` arrows)))

data Dir = Forward | Reverse

step : Event -> (Int,Float) -> (Int,Float)
step event (index, fraction) =
    case event of
      KeyPress x -> ((index + x) `mod` lenSteps, 0)
      TimeDelta dt -> (index, fraction + (1-fraction) * dt/500)

(#) : [a] -> Int -> a
xs # i = case xs of
           h::t -> if i == 0 then h else t # (i-1)
           [] -> []

showFrame w h pos showing fading fraction =
    let lastShow = map (Graphics.Element.opacity fraction . showPart pos) fading
        frame = container 800 600 topLeft . flow down <| map (showPart pos) showing ++ lastShow
    in  collage w h [ rect w h |> filled (rgb 245 245 245)
                    , toForm frame
                        |> scale (min (toFloat w / 800) (toFloat h / 600))
                    ]

scene (w,h) pos (i,j) fraction =
    let frame = frames # i
        showing = take j frame
        fading = case drop j frame of
                   h::t -> [h]
                   [] -> []
    in  showFrame w h pos showing fading fraction

state = foldp step (0,1) input

index = lift (\(i,_) -> steps # i) state

position =
    let isMatch (i,j) =
            not . isEmpty <| filter (\(i',j') -> i == i' && j >= j') mouseSteps
    in keepWhen (isMatch <~ index) (0,0) Mouse.position

main = scene <~ Window.dimensions ~ position ~ index ~ (snd <~ state)
--}
{--
intro : [[(Animation,Form)]]
intro = map (map static)
  [ [ move (0,100) <| toForm [markdown|# Asynchoronous<br/>Functional Reactive Programming<br/><span style="float: right;">for GUIs</span>|]
    , move (0-200, 0-150) <| toForm [markdown|<span style="font-size: 1.4em; color:rgb(156,156,156)">Evan Czaplicki</span>|]
    , move (0-200, 0-175) <| toForm [markdown|<span style="font-size: 1em; color:rgb(156,156,156)">Prezi</span>|]
    , move (200, 0-150) <| toForm [markdown|<span style="font-size: 1.4em; color:rgb(156,156,156)">Stephen Chong</span>|]
    , move (200, 0-175) <| toForm [markdown|<span style="font-size: 1em; color:rgb(156,156,156)">Harvard</span>|]
    ]
  , [ move (0,200) <| toForm [markdown|# Functional Reactive Programming|]
    , move (0,140) <| toForm [markdown|## Allow interactivity in a purely functional setting |]
    , move (0,110) <| toForm [markdown|### Promising approach for GUIs, games, and robotics |]
    , move (0,70) <| toForm [markdown|## FRP has struggled with semantics and performance. E.g: |]
    , move (0,40) <| toForm [markdown|### Needless recomputation |]
    , move (0,10) <| toForm [markdown|### Freezing on long computations |]
    , move (0,0-20) <| toForm [markdown|### Bad interactions with lazy evaluation |]
    ]
  , [ move (0,200) <| toForm [markdown|# Synchronous FRP |]
    , move (0,150) <| toForm [markdown| everything is processed sequentially |]
    , move (0,120) <| toForm [markdown| everything is recomputed on each step |]
    , nodeLift3
    , move (0,0-120) <| toForm [markdown|## Serious Performance Problems |]
    , move (0,0-160) <| toForm [markdown| expensive updates block all pending updates |]
    , move (0,0-190) <| toForm [markdown| many values are recomputed needlessly |]
    ]
  , [ move (0,200) <| toForm [markdown|# Concurrency is a fundamental part of GUIs|]
    , move (0-190,60) <| group [ outlined (dotted lineColor) <| rect 200 100
                                , toForm [markdown| computations over here |]
                                ]
    , move (150,0-40) <| group [ outlined (dotted lineColor) <| rect 300 150
                               , toForm [markdown| should not block computations over here |]
                               ]
    , move (0,0-190) <| toForm [markdown| We make it easy to express this in FRP |]
    ]
  , [ move (0-150,200) <| toForm [markdown|# Signal Graphs |]
    , move (0-150,60) <| biggerText "avoid recomputation"
    , move (0-150,0) <| biggerText "allow concurrency"
    , move (0-150,0-60) <| biggerText "allow asynchrony"
    , move (250,0) signalGraph
    ]
  ]

recompStatic : [(Animation,Form)]
recompStatic =
  [ static . move (0-150,200) <| toForm [markdown|# Avoid Recomputation |]
  , static . move (0-150,60) <| toForm [markdown|only send updates to relevant dependencies|]
  , static . move (250,0) <| signalGraph
  ]

at : (Float,Float) -> Form -> (Animation,Form)
at pos form = (None, move pos form)

blueChng = blueChange |> move (250,0)
pinkChng = pinkChange |> move (250,0)

makePath : Form -> [(Float,Float)] -> [(Animation,Form)]
makePath obj path =
    let pair (sx,sy) (ex,ey) =
            if sx == ex && sy == ey then obj |> at (sx,sy)
                                    else obj |> slide {sx=sx,sy=sy,ex=ex,ey=ey}
    in  zipWith pair path (tail path)

repeat : Int -> a -> [a]
repeat n x = if n <= 0 then [] else x :: repeat (n-1) x

leftTraverse : Int -> [(Animation,Form)]
leftTraverse delay =
    let path = (0,200) :: (0-52,30) :: repeat (1+delay) (0-52,0-30) ++ [(0,0-130), (0,0-400)]
    in  makePath blueChng path

rightTraverse : [(Animation,Form)]
rightTraverse =
    makePath pinkChng [(0,200), (52,60), (52,0), (52,0-60), (0,0-130), (0,0-400), (0,0-400)]

wait : Form -> Int -> [(Animation,Form)]
wait form n =
    let pos = 200 + toFloat n * 30
    in  makePath form [ (0,pos+30), (0,pos), (0,pos), (0,pos), (0,pos), (0,pos), (0,pos), (0,pos) ]

queueUp : [[(Animation,Form)]]
queueUp =
  [ [], [ blueChng |> slide { sx=0, sy=400, ex=0, ey=200 } ]
  , [ blueChng |> at (0,200), pinkChng |> slide { sx=0, sy=400, ex=0, ey=230 } ]
  ]

recomp : [[(Animation,Form)]]
recomp = map ((++) recompStatic) <|
    queueUp
    ++ zipWith (\x y -> [x,y]) (wait pinkChng 0) (leftTraverse 0)
    ++ map (\x -> [x]) rightTraverse

concurrentStatic : [(Animation,Form)]
concurrentStatic =
  [ static . move (0-150,200) <| toForm [markdown|# Concurrency |]
  , static . move (0-150,60) <| toForm [markdown|run each node as an independent actor|]
  , static . move (250,0) <| signalGraph
  ]

concurrent : [[(Animation,Form)]]
concurrent = map ((++) concurrentStatic) <|
  queueUp ++ zipWith (\x y -> [x,y]) (leftTraverse 2) rightTraverse

changeStatic : [(Animation,Form)]
changeStatic =
  [ static . move (0-150,200) <| toForm [markdown|# Synchronization |]
  , static . move (0-150,60) <| toForm [markdown| send dummy events to preserve the order of events |]
  , static . move (250,0) <| signalGraph
  ]

change : [[(Animation,Form)]]
change = map ((++) changeStatic) <|
  queueUp ++ zipWith (\x y -> [x,y]) (leftTraverse 2) rightTraverse

asyncStatic : [(Animation,Form)]
asyncStatic =
  [ static . move (0-150,200) <| toForm [markdown|# Asynchrony |]
  , static . move (0-150,60) <| toForm [markdown| split off signal graphs to safely allow events to get out of order |]
  , static . move (250,0) <| signalGraph
  ]

async : [[(Animation,Form)]]
async = map ((++) changeStatic) <|
  queueUp ++ zipWith (\x y -> [x,y]) (leftTraverse 2) rightTraverse


partTwo : [[(Animation,Form)]]
partTwo = map (map static)
  [ [ toForm [markdown|# Practical Programming in Elm |]
    , move (0,0-30) <| toForm [markdown| turning theory into a practical language |]
    ]
  , [ move (0,200) <| toForm [markdown|# Core Language|]
    , move (0,0) <| toForm [markdown|```haskell
lift  : (a -> b) -> Signal a -> Signal b

foldp : (a -> b -> b) -> b -> Signal a -> Signal b

async : Signal a -> Signal a
```|]
    ]
  , [ image 1003 1485 "nodes.png"
          |> toForm
          |> move (200,0)
          |> scale 0.4
    ]
  , [ move (0,200) <| toForm [markdown|```haskell
lift asText Mouse.position
```|]
    , image 400 400 "position.png"
          |> toForm
          |> move (0,0-50)
    ]
  , [ move (0,200) <| toForm [markdown|```haskell
foldp (\\ _ c -> c+1) 0 Mouse.clicks
```|]
    , image 400 400 "count.png"
          |> toForm
          |> move (0,0-50)
    ]
  , [ move (0,200) <| toForm [markdown|# Properties of Signal Graphs|]
    , move (0,100) <| toForm [markdown|## All signal graphs are acyclic |]
    , move (0,60) <| toForm [markdown|Infinite loops of events are ruled out statically |]
    , move (0,35) <| toForm [markdown|An event is guaranteed to cause exactly one round of updates |]
    , move (0,0-50) <| toForm [markdown|## No signals of signals|]
    , move (0,0-100) <| toForm [markdown| Signals of signals caused serious semantic and performance issues in early formulations of FRP |]
    , move (0,0-140) <| toForm [markdown|```haskell
join : Signal (Signal a) -> Signal a
```|]
    , move (0,0-180) <| toForm [markdown| This is ruled out by Elm's type system |]
    ]
  , [ move (0,200) <| toForm [markdown|# Two Stage Evaluation |]
    , move (0,155) <| toForm [markdown| Similar to [Real-Time FRP](http://haskell.cs.yale.edu/?post_type=publication&p=194) and [Event-Driven FRP](http://www.cs.yale.edu/homes/zwan/papers/mcu/efrp.pdf) |]
    , move (0,40) <| toForm [markdown|## Build the Signal Graph at compile time|]
    , move (0,0-40) <| toForm [markdown|## Start sending events through the graph at runtime |]
    ]
  , [ move (0,200) <| toForm [markdown|# Elm |]
    , move (0,155) <| toForm [markdown| A language for practical and accessible FRP |]
    , move (0,90) <| toForm [markdown|
* Simple and expressive FRP primitives
* Strong / Static Typing
* Extensible records with structural typing
* Module system and core libraries
|]
    , move (0,0-100) <| toForm [markdown|Arrowized FRP is implemented as an Elm library|]
    ]
  , [ move (0,0) <| toForm [markdown|<iframe src="http://elm-lang.org/edit/examples/Intermediate/Mario.elm" width=700 height=500 style="border:none;"></iframe>|]
    ]
  ]


frames : [[(Animation,Form)]]
frames = intro ++ recomp ++ concurrent ++ change ++ async ++ partTwo
--}