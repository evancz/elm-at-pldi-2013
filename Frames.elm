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
    | ClickCount

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
              in  flow down [ spacer wid (if offset < 50 then 20 else 4)
                            , spacer offset (heightOf t') `beside` t' ]

myBlue : Color
myBlue = rgb 96 181 204

myBlue' : Color
myBlue' = rgb 90 99 120

myGrey = rgb 80 80 80

showPart : Int -> (Int,Int) -> Part -> Element
showPart clicks (x,y) part =
    case part of
      SuperTitle str -> spacer 900 250 `above` txt 900 Center 4 myBlue str
      Title str -> spacer 900 20 `above` txt 900 Center 3.2 myBlue str
      SubTitle str -> spacer 900 10 `above` txt 900 Center 2 myGrey str
      Bullet str -> txt 900 (Offset 20) 2 myGrey str
      SubBullet str -> txt 900 (Offset 60) 1.7 myGrey str
      Anything elem -> elem
      Mouse f -> f x y
      ClickCount -> txt 900 Center 8 myBlue' . monospace . toText <| show clicks
      _ -> asText 42

paperTitle = [markdown|
<span style="font-size:4em; color: rgb(96,181,204);">
Asynchronous<br>Functional Reactive Programming<br>
<span style="float:right;">for GUIs</span>
</span>
|]

titlePage : Element
titlePage = 
    let style hght = txt (900 `div` 2) Center hght myGrey . toText
        author name affl = flow down [ style 2.2 name, style 1.8 affl ]
    in flow down [ spacer 900 130,
                   container 900 (heightOf paperTitle) middle paperTitle,
                   spacer 900 100,
                   flow right [ author "Evan Czaplicki" "Prezi",
                                author "Stephen Chong" "Harvard" ] ]

center = container 900 100 middle

mousePosition : Element
mousePosition = center [markdown|```haskell
Mouse.position : Signal (Int,Int)
```
|]

myLift1 : Element
myLift1 = center [markdown|```haskell
lift  : (a -> b) -> Signal a -> Signal b
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
    collage 900 340
    [ move (0-160,0) . scale 0.75 <| toForm shapesCode
    , ngon 5 60 |> filled myBlue'
                |> rotate (degrees (toFloat x))
                |> scale (toFloat y / 400)
                |> move (310,0)
    ]

myFoldp : Element
myFoldp = center [markdown|```haskell
foldp : (a -> b -> b) -> b -> Signal a -> Signal b
```
|]

myCount : Element
myCount = center [markdown|```haskell
foldp (\\_ c -> c+1) 0 Mouse.clicks
```
|]

myAsync : Element
myAsync = center [markdown|```haskell
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
        in  Mouse (\x y -> container 900 140 middle . text . monospace . typeface "inconsolata" . Text.height 2 <| concat [ t myBlue  "("
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
      , Mouse shapes
      ]

    , [ title "Stateful Signals"
      , subTitle "Signals that depend on the past\n "
      , Anything myFoldp
      , Anything myCount
      , ClickCount
      ]
    , [ title "Signals so far"
      , Anything myLift1
      , Anything myFoldp
      , Anything (spacer 900 60)
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

    , [ superTitle "Signal Graph"
      , subTitle "A graph interpretation for Elm&rsquo;s core FRP primitives"
      ]

    , [ title "Signal Graph"
      , Anything [markdown|```haskell
      positions = lift asText Mouse.position
```
|]
      , Anything <|
          collage 900 400
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
          collage 900 350
            [ scale 0.8 . move (0-200,0) . toForm <| image 309 385 "images/translations.png"
            , helpAt (140,0) . toForm <| translationHelp ]
      ]

    , [ title "Concurrency and Pipelining"
      , Anything [markdown|```haskell
       lift2 display positions translations
```
|]
      , Anything <|
          collage 900 400
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
          collage 900 330
          [ move (0-200,0) . toForm <| image 212 323 "images/positions.png"
          , move (200,0) . toForm <| image 205 291 "images/asyncPositions.png"
          ]
      ]

    , [ title "Asynchrony"
      , Anything [markdown|```haskell
   lift2 display positions (async translations)
```
|]
      , Anything <| collage 900 400
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
      , bullet "Monadic FRP [Elliott and Hudak '97]"
      , subBullet "Has prohibative performance problems in pure languages"
      , subBullet "Not allowed in Elm"
      , bullet "Arrowized FRP"
      , subBullet "Efficiently allows dynamic switching and dynamic collections\n[Liu, Cheng, Hudak, '07 '09; Courtney, Nilsson, Peterson, '02 '03 '05]"
      , SubBullet <| monospace (toText "foldp") ++ toText " can fully express Arrowized FRP"
      , subBullet "Implemented as an Elm library"
      , bullet "Parallel FRP [Peterson, Trifonov, Serjantov, '00]"
      , subBullet "Monadic FRP for servers, allows reordering events within a signal"
      , subBullet "Combines nicely with our core language, but not ideal for GUIs"
      ]

    , [ superTitle "Guaranteeing Efficiency"
      , subTitle "Evaluation strategies, types, and concurrency"
      ]

    , [ title "Evaluation"
      , bullet "Eager evaluation by default"
      , bullet "Compile to a two-tiered intermediate language"
      , subBullet "Influenced by Real-Time FRP, which proved efficiency bounds\n[Wan, Taha, Hudak, '01 '02]"
      , Anything . container 900 200 midBottom . width 600 <| image 996 252 "images/values.png"
      , bullet "Runtime semantics given by translation to Concurrent ML"
      ]

    , [ title "Types"
      , bullet "Elm does not allow signals-of-signals (Monadic FRP)"
      , subBullet "Monadic FRP has serious semantic and efficiency problems in pure languages"
      , subBullet "Elm has a two-tiered type system rules out Signals-of-Signals"
      , Anything . container 900 200 middle . width 600 <| image 726 168 "images/types.png"
      , bullet "Signal graphs can be built programmatically"
      , subBullet "Type system ensures that source code maps to intermediate language"
      , subBullet "Proved that well-typed source produces a well-formed signal graph"
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

    , [ Anything [markdown|<iframe src="http://localhost:8000/edit/examples/Intermediate/Mario.elm" width=900 height=600 style="border:none;"></iframe>|]
      ]

    , [ title "Summary"
      , bullet "Key Contributions:"
      , subBullet "Simple and efficient semantics for FRP"
      , subBullet "Elm, a practical language for purely functional GUIs"
      , Bullet <| toText "And remember to try out Elm at " ++ Text.link "/" (toText "elm-lang.org") ++ toText "!\n "
      , Anything [markdown|<iframe src="http://localhost:8000/edit/examples/Intermediate/Clock.elm" width=900 height=240 style="border:none;"></iframe>|]
      ]

    ]

{--
showFrame clicks pos frame =
    layers [ spacer 900 600 |> color (rgb 245 245 245)
           , flow down <| map (showPart clicks pos) frame ]

allFrames clicks pos =
    flow down . intersperse (color white <| spacer 900 10) <| map (showFrame clicks pos) frames

scene w clicks pos =
    let elem = allFrames clicks pos
    in  container w (heightOf elem) middle elem

main = scene <~ Window.width ~ count Mouse.clicks ~ Mouse.position
--}
{--}
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

showFrame w h clicks pos showing fading fraction =
    let (showing', fading') =
            if isEmpty showing then (fading, []) else (showing, fading)
        lastShow = map (Graphics.Element.opacity fraction . showPart clicks pos) fading'
        frame = container 900 600 topLeft . flow down <| map (showPart clicks pos) showing' ++ lastShow
    in  collage w h [ rect w h |> filled (rgb 245 245 245)
                    , toForm frame
                        |> scale (min (toFloat w / 900) (toFloat h / 600))
                    ]

scene (w,h) clicks pos (i,j) fraction =
    let frame = frames # i
        showing = take j frame
        fading = case drop j frame of
                   h::t -> [h]
                   [] -> []
    in  showFrame w h clicks pos showing fading fraction

state = foldp step (0,1) input

index = lift (\(i,_) -> steps # i) state

position =
    let isMatch (i,j) =
            not . isEmpty <| filter (\(i',j') -> i == i' && j >= j') mouseSteps
    in keepWhen (isMatch <~ index) (0,0) Mouse.position

clickCount =
    dropRepeats <|
    foldp (\evt c -> case evt of
                       Nothing -> 0
                       Just _ -> c + 1) 0 (merges [(\_ -> Nothing) <~ Keyboard.arrows, Just <~ Mouse.clicks])

main = scene <~ Window.dimensions ~ clickCount ~ position ~ index ~ (snd <~ state)
--}
