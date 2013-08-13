module Frames where

import Keyboard
import Mouse
import Window

txt : Float -> Color -> Text -> Element
txt hght colr content =
    text . Text.height (14*hght) <| Text.color colr content

myBlue : Color
myBlue = rgb 96 181 204

myBlue' : Color
myBlue' = rgb 90 99 120

myYellow : Color
myYellow = rgb 240 173 0

myGreen : Color
myGreen = rgb 127 209 59

myPink : Color
myPink = rgb 234 21 122

myGrey : Color
myGrey = rgb 80 80 80

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

forms x y = group
  [ txt 8 myBlue (toText "Ideas Matter")
      |> toForm
      |> move (0,350)
  , txt 2.4 myGrey (toText "Programming Languages at Prezi")
      |> toForm
      |> move (0,260)
  , txt 2 myBlue (toText "Simplify")
      |> toForm
      |> move (0-220,120)
  , txt 0.7 myGrey (toText "Turn 10 ideas into 1")
      |> toForm
      |> move (0-220,95)
  , txt 0.7 myGrey (toText "Elm " ++ italic (toText "simplifies") ++ toText " front-end programming")
      |> toForm
      |> move (0-220,70)
  , txt 0.7 myGrey (toText "by introducing one key idea")
      |> toForm
      |> move (0-220,55)
  , let t clr = Text.color clr . toText
        pos   = concat [ t myBlue' "Mouse.position == ", t myBlue  "(", t myBlue' (show x), t myBlue  ",", t myBlue' (show y), t myBlue  ")" ]
        elem  = text . monospace . typeface "inconsolata" <| Text.height 14 pos
    in  toForm elem |> move (0-220,20)
  , move (0-255,0-40) . scale 0.2 <| toForm shapesCode
  , ngon 5 30 |> filled myBlue'
              |> rotate (degrees (toFloat x))
              |> scale (toFloat y / 400)
              |> move (0-140,0-40)
  , txt 1.2 myGrey (toText "This Prezi is an Elm program!")
      |> toForm
      |> move (0-220,0-120)
  , txt 2 myBlue (toText "Inspire")
      |> toForm
      |> move (220,120)
  , txt 0.7 myGrey (toText "Turn ideas into action")
      |> toForm
      |> move (220,95)
  , txt 0.7 myGrey (toText "Programmers are excited about this idea")
      |> toForm
      |> move (220,60)
  , txt 0.5 myGrey (toText "Reputation, Hiring, Influence")
      |> toForm
      |> move (220,45)
  , txt 0.7 myGrey (toText "Language and Library designers are using this idea")
      |> toForm
      |> move (220,0)
  , txt 0.5 myGrey (toText "Fay, Reactive Cocoa, Dart")
      |> toForm
      |> move (220,0-15)
  , txt 0.7 myGrey (toText "Make better programmers in " ++ italic (toText "every") ++ toText " language")
      |> toForm
      |> move (220,0-60)
  , txt 4 myGrey (toText "Hire Developers")
      |> toForm
      |> move (0-100,880)
  , txt 4 myGrey (toText "Create the tools Prezi needs")
      |> toForm
      |> move (300,750)
  , txt 4 myGrey (toText "Guide the future of programming")
      |> toForm
      |> move (600,620)
  ]

positions = [ { sx=0, sy=360, ss=1, ex=0, ey=360, es=1 }
            , { sx=0, sy=360, ss=1, ex=0, ey=300, es=1 }
            , { sx=0, sy=300, ss=1, ex=0-220, ey=80, es=0.24 }
            , { sx=-220, sy=80, ss=0.24, ex=0-220, ey=70, es=0.3 }
            , { sx=-220, sy=70, ss=0.3, ex=0-220, ey=0-25, es=0.3 }
            , { sx=-220, sy=0-25, ss=0.3, ex=0-220, ey=0-80, es=0.35 }
            , { sx=-220, sy=0-40, ss=0.4, ex=220, ey=80, es=0.3 }
            , { sx=220, sy=80, ss=0.3, ex=220, ey=50, es=0.4 }
            , { sx=220, sy=50, ss=0.4, ex=220, ey=20, es=0.6 }
            , { sx=220, sy=20, ss=0.6, ex=350, ey=600, es=2 }
            , { sx=350, sy=600, ss=2, ex=0, ey=360, es=1 }
            ]

scene w h x y position fraction =
  let {sx,sy,ss,ex,ey,es} = position
      pos = (0 - sx - (ex - sx) * fraction, 0 - sy - (ey - sy) * fraction)
      scl = 1 / (ss + (es - ss) * fraction)
      everything = scale scl <| group [ move pos <| forms x y ]
      (w',h') = (toFloat w, toFloat h)
  in  collage w h [ rect w' h' |> filled (rgb 235 235 235)
                  , collage 800 450 [everything]
                      |> toForm
                      |> scale (min (w' / 800) (h' / 450))
                  ]

data Action = KeyPress Int | TimeDelta Time

arrows = dropIf (\n -> n == 0) 0 <| .x <~ Keyboard.arrows

input : Signal Action
input = merge (KeyPress <~ arrows)
        (TimeDelta <~ (30 `fpsWhen` ((2*second) `since` arrows)))

data Dir = Forward | Reverse

step : Action -> (Int,Float) -> (Int,Float)
step event (index, fraction) =
    case event of
      KeyPress x -> ((index + x) `mod` length positions, 0)
      TimeDelta dt -> (index, fraction + (1-fraction) * dt/500)

state = foldp step (0,1) input

(#) : [a] -> Int -> a
xs # i = case xs of
           h::t -> if i == 0 then h else t # (i-1)

position = lift (\pair -> case pair of (i,_) -> positions # i) state

main = scene <~ Window.width ~ Window.height ~ Mouse.x ~ Mouse.y ~ position ~ (lift snd state)
