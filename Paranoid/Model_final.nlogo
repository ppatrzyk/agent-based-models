globals [payoffs notobs obs worldstate cue generation biass deterred committed enforced CCs CDs DDs Tp Tn Fp Fn]

breed [agents agent]

agents-own [partnered observ threshold decision fitness]

to setup
  clear-all
  set payoffs [[6 0][10 2]] ;cooperate: 0, defect: 1
  set-distr
  set generation 1
  repeat population [create 1 0]
  agent-look
  reset-ticks
end

;this sets means of ditributions respresenting two situation types
to set-distr
  if distribution = "certain"
  [set notobs -5 set obs 5]
  if distribution = "uncertain"
  [set notobs -1.5 set obs 1.5]
  if distribution = "veryuncertain"
  [set notobs -0.5 set obs 0.5]
end

;one interaction round among agents
to interact
  pair
  foreach sort links [
    ask ? [
      sit-type
      decide
      play
      punish
    ]
  ask agents [
    set partnered 0
    set observ 0
    ]
  ]
  clear-links
end

;determine the type of situation agents are in (probability controlled by baserate parameter)
to sit-type
  let r random-float 1
  ifelse (r < baserate)
  [set worldstate obs] ;sets mean of ditribution + if>0 then denotes actually observed
  [set worldstate notobs]
  set cue random-normal worldstate 1
end

to decide
  ask both-ends [
    ;track if actually observed
    ifelse (worldstate > 0)
    [set observ 1]
    [set observ 0]
    ;determine agents' belief based on cue
      ifelse (cue < threshold)
      [set decision 1 set committed (committed + 1)] ;decision = 1 denotes defection
      [set decision 0 set deterred (deterred + 1)]
    ]
end

;helper function for pairing agents
to pair
  let total count agents
  let pairs floor (total / 2)
  repeat pairs [
    ask one-of agents with [partnered = 0] [
      create-link-with one-of other agents with [partnered = 0]
      set partnered 1
      ask link-neighbors [set partnered 1]
      ]
  ]
end

;helper function for the PD game
to play
  if [decision] of end1 = 0 and [decision] of end2 = 0
  [set CCs (CCs + 1)]
  if [decision] of end1 = 1 and [decision] of end2 = 1
  [set DDs (DDs + 1)]
  if ([decision] of end1 = 0 and [decision] of end2 = 1) or ([decision] of end1 = 1 and [decision] of end2 = 0)
  [set CDs (CDs + 1)]
  let add1 item [decision] of end2 item [decision] of end1 payoffs
  ask end1 [set fitness fitness + add1]
  let add2 item [decision] of end1 item [decision] of end2 payoffs
  ask end2 [set fitness fitness + add2]
end

; helper function for creating agents
to create [numagents ag_bias]
  create-agents numagents [
    set threshold ag_bias
    set fitness 50
    setxy random-pxcor random-pycor
    ]
end

to agent-look
  ask agents [
    set shape "person"
    set color yellow
    ]
end

;helper function for punishment
to punish
   ask both-ends [
   ifelse (observ = 1)
   [ ;agent actually observed
     if decision = 1 [
       set Fn (Fn + 1) ;was observed, thought he wasn't
       set enforced (enforced + 1)
       set fitness (fitness - punishment)
       ]
     if decision = 0 ;was observed, thought he was
     [set Tp (Tp + 1)]
     ]
   [ ;agent actually not observed
     if decision = 1 [
       set Tn (Tn + 1) ;was nopt observed, thought he wasn't
       ]
     if decision = 0 ;was not observed, thought he was
     [set Fp (Fp + 1)]
     ]
   ]
end

; transition from one generation to another
to replicate
  set biass []
  repeat population [
    let reproducer lottery-winner
    let agent_bias [threshold] of reproducer
    let child_bias (agent_bias + random-normal 0 0.01)
    set biass fput child_bias biass
    ]
  ask agents [die]
  let start 0
  loop [
    let current_bias item start biass
    create 1 current_bias
    set start (start + 1)
    if start = population [stop]
    ]
end

;Adapted from the Lottery Example
to-report lottery-winner
  let pick random-float sum [fitness] of agents
  ;show pick
  let winner nobody
  ask agents
    [ ;; if there's no winner yet...
      if winner = nobody
        [ ifelse fitness > pick
            [ set winner self ]
            [ set pick pick - fitness ] ] ]
  report winner
end

to reset-stats
  set committed 0
  set deterred 0
  set enforced 0
  set CCs 0
  set DDs 0
  set CDs 0
  set Tp 0
  set Tn 0
  set Fp 0
  set Fn 0
end

to go
  agent-look
  update-plots
  reset-stats
  set generation (generation + 1)
  repeat 1 [interact]
  replicate
  tick-advance 1
end
@#$#@#$#@
GRAPHICS-WINDOW
323
40
832
570
16
16
15.121212121212121
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
0
0
1
ticks
30.0

BUTTON
66
334
129
367
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

BUTTON
129
334
192
367
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

BUTTON
192
334
255
367
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

TEXTBOX
10
13
259
53
Institutionalized Punishment Model
16
0.0
1

SLIDER
95
128
233
161
no_generations
no_generations
0
10000
1400
1
1
NIL
HORIZONTAL

PLOT
843
225
1043
375
Mean threshold
Generation
Threshold
0.0
10.0
-1.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -5825686 true "" "plot mean [threshold] of agents"

PLOT
-1
411
159
531
Population size
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

CHOOSER
94
241
232
286
distribution
distribution
"certain" "uncertain" "veryuncertain"
0

SLIDER
95
162
231
195
population
population
0
500
100
1
1
NIL
HORIZONTAL

PLOT
840
387
1040
537
Punishment
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"crimes" 1.0 0 -16777216 true "" "plot committed"
"enforced" 1.0 0 -5825686 true "" "plot enforced"
"deterred" 1.0 0 -10899396 true "" "plot deterred"

PLOT
843
73
1043
223
CC rate
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -10899396 true "" "plot CCs / (CCs + DDs + CDs + 0.1)"
"pen-1" 1.0 0 -13345367 true "" "plot CDs / (CCs + DDs + CDs + 0.1)"
"pen-2" 1.0 0 -2674135 true "" "plot DDs / (CCs + DDs + CDs + 0.1)"

PLOT
162
410
322
530
Classifications
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot Tp"
"pen-1" 1.0 0 -7500403 true "" "plot Tn"
"pen-2" 1.0 0 -10899396 true "" "plot Fp"
"pen-3" 1.0 0 -8053223 true "" "plot Fn"

SLIDER
95
287
232
320
baserate
baserate
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
95
195
231
228
punishment
punishment
0
100
20
1
1
NIL
HORIZONTAL

@#$#@#$#@
## Institutionalized Punishment Model

This is a Model described in the paper "Cognitive Adaptations to Criminal Justice Lead to ‘Paranoid’ Norm Obedience" by Piotr M. Patrzyk and Martin Takac published in Adaptive Behavior:

http://dx.doi.org/10.1177/1059712317693889

Note:
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

ghost
false
0
Polygon -7500403 true true 30 165 13 164 -2 149 0 135 -2 119 0 105 15 75 30 75 58 104 43 119 43 134 58 134 73 134 88 104 73 44 78 14 103 -1 193 -1 223 29 208 89 208 119 238 134 253 119 240 105 238 89 240 75 255 60 270 60 283 74 300 90 298 104 298 119 300 135 285 135 285 150 268 164 238 179 208 164 208 194 238 209 253 224 268 239 268 269 238 299 178 299 148 284 103 269 58 284 43 299 58 269 103 254 148 254 193 254 163 239 118 209 88 179 73 179 58 164
Line -16777216 false 189 253 215 253
Circle -16777216 true false 102 30 30
Polygon -16777216 true false 165 105 135 105 120 120 105 105 135 75 165 75 195 105 180 120
Circle -16777216 true false 160 30 30

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
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="M3run" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>generation = no_generations + 1</exitCondition>
    <metric>generation</metric>
    <metric>mean [threshold] of agents</metric>
    <metric>committed</metric>
    <metric>deterred</metric>
    <metric>enforced</metric>
    <metric>CCs</metric>
    <metric>CDs</metric>
    <metric>DDs</metric>
    <metric>Tp</metric>
    <metric>Tn</metric>
    <metric>Fp</metric>
    <metric>Fn</metric>
    <enumeratedValueSet variable="population">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_generations">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment">
      <value value="10"/>
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution">
      <value value="&quot;certain&quot;"/>
      <value value="&quot;uncertain&quot;"/>
      <value value="&quot;veryuncertain&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baserate">
      <value value="0.2"/>
      <value value="0.5"/>
      <value value="0.8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>generation = no_generations + 1</exitCondition>
    <metric>generation</metric>
    <metric>mean [threshold] of agents</metric>
    <metric>committed</metric>
    <metric>deterred</metric>
    <metric>enforced</metric>
    <metric>CCs</metric>
    <metric>CDs</metric>
    <metric>DDs</metric>
    <metric>Tp</metric>
    <metric>Tn</metric>
    <metric>Fp</metric>
    <metric>Fn</metric>
    <enumeratedValueSet variable="population">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_generations">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution">
      <value value="&quot;certain&quot;"/>
      <value value="&quot;uncertain&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baserate">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
