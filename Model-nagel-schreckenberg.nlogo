patches-own[v v']
globals[vmax mean-flow]

to setup
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  set vmax 3 ;;max velocity of any car
  empty-patches
  fill-patches
end

to empty-patches
  ask patches
    [
    if pxcor mod 2 = 0 [set pcolor 3] ;; set every second patch to be a different color
    set v -1
    set v' -1
    ]
end

to fill-patches
  let n world-width * density
  ask n-of (floor n) patches
    [
    set v 0
    set-label
    ]
end

to set-label
  ifelse v > -1
  [
    set plabel v
    set pcolor (66 - v)
  ]
  [
    set plabel ""
    ifelse pxcor mod 2 = 0 [set pcolor 3] [set pcolor 0]
  ]
end

to go
  tick
  ask patches with [v > -1]
    [ ;; Every patch which represents a car has to accelerate, brake, randomize and move every tick.
    accelerate
    brake
    randomization
    move
    ]
  do-plots
end

to accelerate
;; v_j(t+1/3) = min(v_j(t) + 1, v_(max))
  let newv v + 1
  set v min (list newv vmax)
end

to brake
;; v_j(t+2/3) = min(g_j(t), v_j(t+1/3))
;; Set the velocity to max the amount of open patches ahead.
;; I based this implementation on the accelerate function of http://ccl.northwestern.edu/netlogo/models/community/Freeway%20Traffic%20CA
  let dist 1 ;; Distance from the car
  let n 0    ;;
  let car-here? false
  while [dist <= vmax + 1 and car-here? = false]
    [
    ifelse pxcor + dist > world-width - 1 [set n pxcor + dist - world-width] [set n pxcor + dist]
    ask patch n 0 [ifelse v >= 0 [set car-here? true] [set dist dist + 1] ]
    ]
  set v min (list v (dist - 1))
end

to randomization
;; v_j(t+1) = max(v_j(t+2/3) - 1, 0) [with probability p]
;; Based on the implementation from http://ccl.northwestern.edu/netlogo/models/community/Freeway%20Traffic%20CA
   if v > 0 and random-float 1 < p [set v 0]
end

to move ;; move
  let n 0 ;; n will be the patch y coordinate of car
  let newv v
  ifelse pxcor + v > world-width - 1 [set n pxcor + v - world-width] [set n pxcor + v]
  ask patch n 0
    [
    set v' newv
    ]
end




;;; ======== Plotting functions ========

to update-positions
  ;; copied from http://ccl.northwestern.edu/netlogo/models/community/Freeway%20Traffic%20CA
  set v v'
  set v' -1
  set-label
end

to update-plot
  if v > -1
    [
    set-plot-pen-color 15 - 2 * v ;;; color depends on velocity
    plotxy pxcor + 1 ticks
    ]
end

to do-plots
  let f 0
  set-current-plot "Positions"
  set-plot-x-range 1 world-width
  set-plot-y-range ticks - 1000 ticks
  ask patches [update-plot update-positions]
  set-current-plot "Flow"
  set-plot-x-range (ticks - 1000) ticks
  set-plot-y-range -1 (vmax * density * world-width)
  set-current-plot-pen "current"
  ;; Flow is the sum of the velocities of all cars.
  set f sum [v] of patches with [v > -1]
  plot f
  set-current-plot-pen "mean"
  ifelse ticks = 1
    [
    set mean-flow f
    ]
    [
    set mean-flow (mean-flow * (ticks - 1) + f) / (ticks)
    ]
  plot mean-flow
end

;;; ======== Reporter functions ========
to-report mean-flowrate
  ; Reports the mean flow rate
  report mean-flow
end
@#$#@#$#@
GRAPHICS-WINDOW
15
259
1843
308
-1
0
18.0
1
10
1
1
1
0
1
1
1
0
100
0
0
1
1
1
ticks
30.0

BUTTON
12
10
76
43
Setup
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
79
10
142
43
Go
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

BUTTON
145
10
208
43
Step
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

SLIDER
220
10
392
43
density
density
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
220
41
392
74
p
p
0
1
1
0.1
1
NIL
HORIZONTAL

TEXTBOX
400
23
513
65
Initial traffic density\n\nRandom p of slowing
11
0.0
1

TEXTBOX
521
61
671
79
NIL
11
0.0
1

PLOT
94
323
640
748
Flow
time
rate (cars per patch per tick)
0.0
10.0
0.0
999.0
false
false
"" ""
PENS
"current" 1.0 0 -16777216 true "" ""
"mean" 1.0 0 -2674135 true "" ""

TEXTBOX
403
109
553
127
Maximum velocity of cars
11
0.0
1

PLOT
678
323
1464
738
Positions
xcor
time
0.0
10.0
0.0
999.0
false
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

@#$#@#$#@
## WHAT IS IT?

This is a replication of Kai Nagel and Michael Schreckenberg's model of traffic flow, as presented in "A cellular automaton model for freeway traffic".

The code is adapted from code of isw3@le.ac.uk (Modeling Commons)


## THE MODEL

This is a CA model of car movement.

It is a 1D CA where each cell represents 1 space of a vehicle on the road (typically 7.5m). All vehicles are the same size (fit in one cell) and all vehicles behave in the same way. The model works with cars moving from right to left. The boundary is periodic so cars will move from the cell on the far left to the right most cell.

Each cell can be in one of [-1, Vmax] states, where a positive state represents a cell with a car and the value indicates the current velocity of the car. A -1 state indicates an empty space in the road.

##SETUP & PARAMETERS

The model has three control parameters, one changing the initial conditions and the two other changing the behaviour of the model.

Density - this sets the initial number of cars on the road, it is defined as the fraction of cells that should be occupied. A system with 100 cells and density 0.8 will result in 80 cars on the road.

P - this is a probability p in the model that indicates the likelihood of random braking by each car. See the model description below.

## HOW IT WORKS

The setup procedure initially clears all existing cars and randomly distributes new cars at the specified density.

At each simulation step the following things occur (this is taken from the model as presented to us in the lectures of Michael Lees) to EACH car:

1) Acceleration: if the velocity v of a vehicle is lower than vmax and if the distance to the next car ahead is larger than v + 1, the speed is increased by one [v = v + 1].

2) Breaking (due to other cars): if a vehicle sees another car y steps ahead where y<v, it reduces its speed to y [v = y - 1].

3) RandomBreaking: with probability p, the velocity of each vehicle (if greater than zero) is decreased to zero [v = 0].

4) Move: each vehicle is moved v cells to the left.

## HOW TO USE IT

Setup occupies a proportion 'density' of patches with a car with an initial velocity of 0.

The plot "Positions" places a point for each site occupied by a car over the last 1,000 ticks. Points are shaded from red to black for stationary and high speed cars, allowing fine structure to be seen in jams with many cars.

"Flow" shows the total flow rate along the road over the last 1,000 ticks given by the mean speed of cars multiplied by the density. The red line shows the time average of the flow.

## THINGS TO NOTICE

Try setting p to 0.5, and changing the density over a range of values. What different behavior can you see in the plots?

For a low density such as 0.1, try varying p during the animation. Can you see any sudden changes in the plots?

Try increasing the world width to 500, or 1,000 patches.

Density < 0.1
At low densities, we see generally steady, free-flow. Cars tend to be non-interacting so flow rate increases linearly with increasing density.

Density ~ 0.1
At this density, the "Positions" plot periodically shows regions of high density points corresponding to small traffic jams. These are mirrored in the "Flow" plot by a sudden drop in flow rate. This density shows a mixed-state in the rapid phase transition between free-flow and congested-flow.

Density > 0.1
At high densities, the traffic experiences continuous congestion. Flow rate is low, and decreases with increasing density

Altering p during the animation has a very similar effect to altering the density. Close to the critical density ~0.1, the system is very sensitive. Increasing p from 0 to 0.5 shows gradual decrease in flow rate. Further increase shows a sudden decrease, and phase transition to congested-flow.

The model scales appropriately with world-width. Very long roads help remove the effects of the periodic boundary, and allow us to see very interest fractal structures in the model traffic jams with the "Positions" plot.

## CREDITS AND REFERENCES

Kai Nagel and Michael Schreckenberg, "A cellular automaton model for freeway traffic"
Philip Ball - "Critical Mass"

Any suggestions of questions? e-mail: isw3@le.ac.uk
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
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

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
  <experiment name="1 Phase Transition" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2001"/>
    <metric>mean-flowrate</metric>
    <enumeratedValueSet variable="p">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="density" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="world-width">
      <value value="101"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2 Undersampling" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5"/>
    <metric>mean-flowrate</metric>
    <enumeratedValueSet variable="p">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="density" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="world-width">
      <value value="101"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="3 Human error" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>mean-flowrate</metric>
    <steppedValueSet variable="p" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="density">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-width">
      <value value="101"/>
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
