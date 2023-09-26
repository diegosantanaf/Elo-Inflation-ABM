extensions [ rnd ]

turtles-own
[elo  ;elo rating
  ability  ;natural ability
  games_played ; games _played
  games_won ; games won
  games_lost ; games lost
  games_drawn ; games drawn
  new_old ; indicate if they are new or old
]

globals
[ mean_elo ]

to setup
  clear-all ;; clear-all
  reset-ticks ;; reset-ticks
  create-turtles number-of-turtles ;; creating turtles depending of the number decided
  ask turtles [
  set shape "person"
    set ability ((random-normal 0 1)^ 2 + (random-normal 0 1)^ 2 +
      (random-normal 0 1)^ 2 + (random-normal 0 1)^ 2 + (random-normal 0 1)^ 2 + (random-normal 0 1)^ 2  ) * 120  ;; distribution of player ability chi-sqaured
    set games_played 0 ;; setting counting variables to 0
    set games_won 0
    set games_lost 0
    set games_drawn 0
  setxy random-xcor random-ycor ] ;; positiioning the players
  ask turtle 0 [ set (ability) (max_ability)]   ;; creating the best player
  ask turtles with [ability > max_ability] [die] ;; filtering for better players (outliers)
  ask turtles with [ability < min_ability] [die] ;; filtering for the worst players
  set mean_elo (sum [ability] of turtles) / (count turtles)
  ask turtles [
    ifelse Elo_equal_ability [
    set elo mean_elo]      ;; initial elo
    [set elo 1000]
    set new_old "old"
  ]

end

to go
     clear-links ;; clearing past links
  ask turtles [
        wiggle   ;; make turtles move randomly
        forward 1
    play    ;; command turtles to play
    ]
  ask turtles [create-links-with other turtles-here] ;; to see who played with who in a single tick
    tick
end

to wiggle
  rt random 90
  lt random 90
end


to play
  let other-player one-of other turtles-here  ;; selecting an oponnent close by
   if other-player != nobody [   ;; if someone is selected, then...
    let ra elo                      ;; variable for our turtles elo
    let rb [elo] of other-player    ;; creating a variable for the oponent's elo
    let r_diff ra - rb
    let aa ability                  ;; creating a variable for our turtles ability
    let ab [ability] of other-player ;; creating a variable for the oponents ability
    let diff aa - ab                 ;; difference in ability
        if (abs r_diff ) < range_of_play [
    let draw_probability (1 /(sqrt(2 * pi * e))) * exp(- ((diff / 200) ^ 2) / ( 2 * ( e ^ 2 ))) * draw_factor  ;; calculating the probaility of drawing
    let player1_win_probability (1 / (1 + 10 ^ ((ab - aa) / 400))) - draw_probability / 2  ;; calculating the probaility of winning
    let player2_win_probability (1 / (1 + 10 ^ ((aa - ab) / 400))) - draw_probability / 2    ;; calculating the probaility of losing
        if player1_win_probability > 0 [   ;; filtering cases were the proability was negative
          if player2_win_probability > 0 [
 let items [ 2 1 0 ]   ;; 2 for winning, 1 for losing and 0 for drawing
    let weights (list player1_win_probability player2_win_probability draw_probability) ;; the weights/probabilities of each outcome according to the ability
let pairs (map list items weights)  ;; creating a single list for the two attributes: results and probability
    let result first rnd:weighted-one-of-list pairs [ [p] -> last p ]  ;; Random decision of results
  if result = 2 [            ;; case were the player wins
    set (elo) (elo + 16 * (1 - (1 / (1 + 10 ^ ((rb - ra) / 400))) )) ;; Elo gained by the player for winning
      ask other-player [set elo elo + 16 * (0 - (1 / (1 + 10 ^ ((ra - rb) / 400))) )] ;; Elo lost by the other-player
    set (games_won) games_won + 1 ;; count the game won for the player
    ask other-player [set games_lost games_lost + 1]  ;; count the game lost for the other-player
          ]
  if result = 1 [  ;; case were the player loses
  set (elo) (elo + 16 * (0 - (1 / (1 + 10 ^ ((rb - ra) / 400))) )) ;; Elo lost by the player for losing
    ask other-player [set elo elo + 16 * (1 - (1 / (1 + 10 ^ ((ra - rb) / 400))) )] ;; Elo gained by the other-player
          set (games_lost) games_lost + 1 ;; count the game lost for the player
    ask other-player [set games_won games_won + 1]  ;; count the game won for the other-player
          ]
    if result = 0 [  ;; case for draws, depends on the difference of ratings
  set (elo) (elo + (16 * (1 - (1 / (1 + 10 ^ ((rb - ra) / 400))) ) ) / 2 +
        (16 * (0 - (1 / (1 + 10 ^ ((rb - ra) / 400))) ) ) / 2) ;; adjusting player elo for a draw
    ask other-player [set elo elo + (16 * (1 - (1 / (1 + 10 ^ ((ra - rb) / 400))) ) ) / 2 +
       (16 * (0 - (1 / (1 + 10 ^ ((rb - ra) / 400))) ) ) / 2]  ;; adjusting other-player elo for a draw
    set (games_drawn) games_drawn + 1 ;; count the game drawn for the player
    ask other-player [set games_drawn games_drawn + 1]  ;; count the game drawn for the other-player
    ]
  set (games_played) games_played + 1
              ask other-player [set games_played games_played + 1]  ;; counting that a game was played by both players
]
  ]

      if player1_win_probability < 0 or player2_win_probability < 0 [ ;; game scenario in case the draw probability of winning is negative (adjusting draws to 0)
       set draw_probability 0  ;; calculating the probaility of drawing
    set player1_win_probability (1 / (1 + 10 ^ ((ab - aa) / 400)))  ;; calculating the probaility of winning
    set player2_win_probability (1 / (1 + 10 ^ ((aa - ab) / 400)))  ;; calculating the probaility of losing
     let items [ 2 1 0 ]   ;; 2 for winning, 1 for losing and 0 for drawing
    let weights (list player1_win_probability player2_win_probability draw_probability) ;; the weights/probabilities of each outcome according to the ability
let pairs (map list items weights)  ;; creating a single list for the two attributes: results and probability
    let result first rnd:weighted-one-of-list pairs [ [p] -> last p ]  ;; Random decision of results
  if result = 2 [            ;; case were the player wins
    set (elo) (elo + 16 * (1 - (1 / (1 + 10 ^ ((rb - ra) / 400))) )) ;; Elo gained by the player for winning
      ask other-player [set elo elo + 16 * (0 - (1 / (1 + 10 ^ ((ra - rb) / 400))) )] ;; Elo lost by the other-player
    set (games_won) games_won + 1 ;; count the game won for the player
    ask other-player [set games_lost games_lost + 1]  ;; count the game lost for the other-player
          ]
  if result = 1 [  ;; case were the player loses
  set (elo) (elo + 16 * (0 - (1 / (1 + 10 ^ ((rb - ra) / 400))) )) ;; Elo lost by the player for losing
    ask other-player [set elo elo + 16 * (1 - (1 / (1 + 10 ^ ((ra - rb) / 400))) )] ;; Elo gained by the other-player
          set (games_lost) games_lost + 1 ;; count the game lost for the player
    ask other-player [set games_won games_won + 1]  ;; count the game won for the other-player
          ]
    if result = 0 [  ;; case for draws, depends on the difference of ratings
  set (elo) (elo + (16 * (1 - (1 / (1 + 10 ^ ((rb - ra) / 400))) ) ) / 2 +
        (16 * (0 - (1 / (1 + 10 ^ ((rb - ra) / 400))) ) ) / 2) ;; adjusting player elo for a draw
    ask other-player [set elo elo + (16 * (1 - (1 / (1 + 10 ^ ((ra - rb) / 400))) ) ) / 2 +
       (16 * (0 - (1 / (1 + 10 ^ ((rb - ra) / 400))) ) ) / 2]  ;; adjusting other-player elo for a draw
    set (games_drawn) games_drawn + 1 ;; count the game drawn for the player
    ask other-player [set games_drawn games_drawn + 1]  ;; count the game drawn for the other-player
    ]
  set (games_played) games_played + 1
              ask other-player [set games_played games_played + 1]  ;; counting that a game was played by both players
      ]
  ]
  ]
end

to add_new_players ;; in order to add turtles (with the chi distribution) to a running model
  create-turtles 100 ;; add 100 turtles
  ask turtles [
  if new_old != "old"  ;; only change new turtles
    [
    set shape "person"
    set ability ((random-normal 0 1)^ 2 + (random-normal 0 1)^ 2 +
      (random-normal 0 1)^ 2 + (random-normal 0 1)^ 2 + (random-normal 0 1)^ 2 + (random-normal 0 1)^ 2  ) * 120  ;; distribution of player ability
    set games_played 0 ;; setting counting variables to 0
    set games_won 0
    set games_lost 0
    set games_drawn 0
  setxy random-xcor random-ycor ;; positiioning the players
  ] ]
  ask turtles with [ability > max_ability] [die] ;; filtering for better players (outliers)
  ask turtles with [ability < min_ability] [die] ;; filtering for the worst players
  set mean_elo (sum [ability] of turtles) / (count turtles)
  ask turtles [ if new_old != "old"
      [
    ifelse Elo_equal_ability [
    set elo mean_elo]      ;; initial elo
    [set elo 1000]
        set new_old "old" ]] ;; now that we created the turtles with all necessary attributes, we change them to old
end

to add_new_players_mean  ;; in order to add turtles (with the same constant average ability) to a running model
  let mean_ability (sum [ability] of turtles) / (count turtles)  ;; mean ability
  create-turtles 100
  ask turtles [
  if new_old != "old"  ;; only change new turtles
    [
    set shape "person"
    set ability mean_ability ;; assigning the mean ability to the turtles
    set games_played 0 ;; setting counting variables to 0
    set games_won 0
    set games_lost 0
    set games_drawn 0
  setxy random-xcor random-ycor ;; positiioning the players
  ] ]
  set mean_elo (sum [ability] of turtles) / (count turtles)
  ask turtles [ if new_old != "old"
      [
    ifelse Elo_equal_ability [
    set elo mean_elo]      ;; initial elo
    [set elo 1000]
        set new_old "old" ]] ;; now that we created the turtles with all necessary attributes, we change them to old
end



to ability_code
  let other-player one-of other turtles-here  ;;selecting an oponnent close by
   if other-player != nobody [
    let aa ability
    let ab [ability] of other-player
    let diff aa - ab
    let ave (aa + ab) / 2
    let expected_score (1 - (1 / (1 + 10 ^ ((ab - aa) / 400))) )
    let eloPerPawn exp (ave / 1200) * 26.59
    let eloShift eloPerPawn * 0.6
    let player1_win_probability (1 - (1 / (1 + 10 ^ ((ab - aa) / (diff - eloShift)))) )
    let draw_probability expected_score - player1_win_probability
    let player2_win_probability 1 - player1_win_probability - draw_probability
  ]
end



@#$#@#$#@
GRAPHICS-WINDOW
210
10
647
448
-1
-1
13.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
54
65
120
98
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
33
155
205
188
number-of-turtles
number-of-turtles
0
2000
38.0
2
1
NIL
HORIZONTAL

PLOT
664
27
864
177
Max elo
Time (ticks)
Elo
0.0
100.0
0.0
2000.0
true
false
"" ""
PENS
"" 1.0 0 -16777216 true "" "plot max [elo] of turtles"

BUTTON
144
64
207
97
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
670
197
771
242
Max elo
max [elo] of turtles
1
1
11

MONITOR
669
251
773
296
Mean ability
mean [ability] of turtles
2
1
11

BUTTON
143
110
206
143
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
805
198
1054
402
Ability histogram
Ability
Players
0.0
2000.0
0.0
10.0
true
false
"\nset-histogram-num-bars 50" ""
PENS
"default" 1.0 0 -2674135 true "" "histogram [ability] of turtles"

PLOT
897
20
1097
170
Elo histogram
Elo
Players
0.0
2800.0
0.0
150.0
false
false
"\nset-histogram-num-bars 25" ""
PENS
"default" 1.0 0 -12345184 true "" "histogram [elo] of turtles"

MONITOR
670
304
773
349
Max ability
max [ability] of turtles
1
1
11

SLIDER
33
195
205
228
Draw_factor
Draw_factor
0
1
0.4
0.1
1
NIL
HORIZONTAL

SLIDER
34
242
206
275
max_ability
max_ability
2200
3200
3000.0
100
1
NIL
HORIZONTAL

SLIDER
33
284
205
317
min_ability
min_ability
0
400
50.0
50
1
NIL
HORIZONTAL

SLIDER
32
320
204
353
range_of_play
range_of_play
100
1000
200.0
100
1
NIL
HORIZONTAL

SWITCH
32
14
191
47
Elo_equal_ability
Elo_equal_ability
0
1
-1000

MONITOR
673
361
773
406
Mean Elo
mean [elo] of turtles
5
1
11

BUTTON
39
366
180
399
Add Players (Rnd)
add_new_players
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
671
412
794
457
Number of turtles
count turtles
0
1
11

BUTTON
38
407
187
440
Add Players (Mean)
add_new_players_mean
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

The objective of this model is to evaluate the effect that the total number of players and the distriution of their ability can have on the average and maximum Elo, a rating system used in games like chess. 

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)
Trabajos de referencia:

https://wismuth.com/elo/calculator.html#rating1=1600&rating2=1200
https://en.chessbase.com/post/a-look-at-the-elo-ratings-in-the-year-2021
https://en.chessbase.com/post/the-elo-ratings-inflation-or-deflation
https://en.chessbase.com/post/problems-of-calculating-ratings-of-junior-players
https://www.researchgate.net/publication/309662241_Mathematical_Model_of_Ranking_Accuracy_and_Popularity_Promotion
https://www.youtube.com/watch?v=B-TCYO3SLDI

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
