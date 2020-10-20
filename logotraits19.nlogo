extensions[ csv profiler ]

globals[

  ;---------------------------------------------------------
  ; TRANSITION VARIABLES TO USE IN PERTURBATIONS
  threat-phase?
  simulation-over?
  ;*********************************************************

  ;---------------------------------------------------------
  ; VARIABLES USED TO SPEED UP THE CODE
  resource-patches     ; agentset of patches with resources.
  ;*********************************************************


  ;---------------------------------------------------------
  ; VARIABLES USED WITHIN THE STOPPING CONDITIONS SUBMODEL
  next-checkpoint           ; timesteps until next check of stopping conditions

  mean-body-size            ; mean size of agents during the current check of stopping conditions.
  mean-maturity-age         ; mean maturity-age of agents.
  mean-disp-ability         ; current mean dispersal ability
  mean-fecundity            ; current mean fecundity of agents
  nr-organisms              ; number of organisms

  ; lists that contain the previous checkpoint measurements of traits.
  ; the size of each list is given by nr-of-means
  means-body-size           ; list, system level mean of body sizes
  means-maturity-age        ; list, system level mean of body sizes
  means-disp-ability        ; list, system level mean of body sizes
  means-fecundity           ; list, system level mean of body sizes
  means-nr-organisms        ; list, system level number of organisms
  ;***************************************************************

  ;------------------------------------------------------
  ; OUTPUT VARIABLES (INITIAL AND FINAL VALUES OF FOCAL TRAITS)
  ; mean trait values at the end of the burn-in period
  init-mean-body-size      ;
  init-mean-maturity-age   ;
  init-mean-disp-ability   ;
  init-mean-fecundity      ;
  init-nr-organisms        ;
  ; standard deviation of traits at the end of the burn-in period
  init-sd-body-size        ;
  init-sd-maturity-age     ;
  init-sd-disp-ability     ;
  init-sd-fecundity        ;
  ; mean trait values at the end of the perturbation phase
  final-mean-body-size     ;
  final-mean-maturity-age  ;
  final-mean-disp-ability  ;
  final-mean-fecundity     ;
  final-nr-organisms       ;
  ; standard deviation of trait values at the end of the perturbation phase
  final-sd-body-size       ;
  final-sd-maturity-age    ;
  final-sd-disp-ability    ;
  final-sd-fecundity       ;
  ;*****************************************************

]

turtles-own[

  ;------------------------------------------------------
  ;; DYNAMIC STATE VARIABLES
  ; name                                                    type, unit
  energy              ; current energy                      ; real, arbitrary unit of energy
  age                 ; age of organism                     ; discrete, timestep
  energy-intake       ; energy income of current timestep   ; real, energy
  ;heading            ; orientation                         ; integer, degrees
  ;xcor, ycor         ; location                            ; continuous, -
  ;******************************************************

  ;--------------------------------------------------------
  ;; STATIC STATE VARIABLES (FOCAL TRAITS)
  ; name                                                      unit
  body-size       ; body size                               ; real, arbitrary unit of mass
  maturity-age    ; age at which the organism becomes adult ; real, patch side
  fecundity       ; fecundity (number of offspring)         ; integer, offspring
  disp-ability    ; dispersal ability                       ; integer, timestep
  ;********************************************************

  ;-------------------------------------------------------
  ;; DERIVED VARIABLES (CALCULATED UPON ORGANISM INITIALIZATION TO SPEED UP THE CODE)
  ; name                 ; construction
  max-energy             ; body-size * metabolic-allometric-exponent
  max-energy-intake      ; max-energy * max-energy-intake-coef

  dispersal-cost         ; max-energy * dispersal-cost-coef
  maturation-cost        ; max-energy * maturation-cost-coef
  maintenance-cost       ; max-energy * maintenance-cost-coef
  energy-to-reproduce    ; max-energy * energy-to-reproduce-coef
  energy-after-reprod    ; max-energy * energy-after-reprod-coef
  longevity              ; maturity-age * longevity-maturity-coef
  ;********************************************************

  ;--------------------------------------------------------
  ; VARIABLES USED BY THE STOPPING CONDITIONS SUBMODEL
  age-at-reprod  ; age at which the last reproduction event occurred.
  ;********************************************************

]

patches-own[
  ;PRIMARY ATTRIBUTES
  ;dynamic state variables                                  type/unit
  max-resources    ; maximum amount of resources            real, unit of energy
  resource-regen   ; resource regeneration                  real, unit of energy
  resources        ; amount of resources                    real, unit of energy

  ; STATISTICS
  list-of-body-sizes           ; list containing the body size values of all organisms stepping on a given patch at a given timestep
  list-of-maturity-ages        ; list containing the maturity ages of all organisms stepping on a given patch at a given timestep
  list-of-fecundities          ; list containing the fecundity values of any organisms stepping on a given patch at a given timestep
  list-of-dispersal-abilities  ; list containing the dispersal ability values of any organisms stepping on a given patch at a given timestep

]

breed [organisms organism]
breed [aliens alien]


;---------------------------- SETUP AND MAP GENERATION ----------------------

to setup
  ifelse load-world? [
    load-world
    tick ;otherwise the tick in which the simulation ended will run twice
    set simulation-over? false
    add-perturbations
  ]
  ;else
  [
    clear-all
    reset-timer
    generate-map
    set threat-phase? false
    set simulation-over? false

    ;create organisms
    create-organisms starting-nr
    [
      set shape "bug"
      set color blue
      set xcor random-xcor
      set ycor random-ycor

      set body-size         random-exponential 3
      set disp-ability      random-exponential 3
      set fecundity         random-exponential 3
      set maturity-age      random-exponential 3

      organism-initialization

      set age 0
      set energy max-energy / 2
    ]
    reset-stats
    collect-stats
    set next-checkpoint 100
    reset-ticks
  ]
end

to generate-map
  _resize-world ; sets world dimensions based w-width and w-height

  if map-seed != 0 [random-seed map-seed]
  ask patches [
    set max-resources 0
    set resource-regen 0
    set resources 0
    set list-of-body-sizes ( list )
    set list-of-maturity-ages ( list )
    set list-of-fecundities ( list )
    set list-of-dispersal-abilities ( list )
  ]

  let i 1
  let nr-viable-patches 0
  let max-nr-viable-patches count patches * resource-patch-fraction

  ask n-of nr-resource-clusters patches with [pcolor = 0]
  [
    set pcolor green
    set max-resources 1
    set nr-viable-patches nr-viable-patches + 1
  ]
  set i (i + 1)

  while [nr-viable-patches < max-nr-viable-patches]
  [
    ask patches with [pcolor = 0]
    [
      if nr-viable-patches >= max-nr-viable-patches [stop]
      let n neighbors with [pcolor != 0]
      if any? n
      [
        let rand-patch one-of n
        set max-resources [max-resources] of rand-patch
        set pcolor green
        set nr-viable-patches nr-viable-patches + 1
      ]
    ]
  ]
  set resource-patches patches with [pcolor != 0] ;patch set of patches with resources
  ask resource-patches [
    set pcolor green
    set resources      max-resources / 2
    set resource-regen regen-rate * max-resources]
  ask patches with [pcolor = 0] [set pcolor brown] ;resourceless patches with color brown
end

;---------------------------------------------------------------------


to go
  turtles-go
  update-patches

  ;this one ends the simulation if it is already past the burn in and stopping conditions are met
  if threat-phase? and simulation-over? = false and stopping-conditions-met? [
    type "simulation ended at tick" print ticks
    set simulation-over? true
    collect-final-pars
  ]
  ;this one ends the burn-in phase if stopping conditions are met
  if not threat-phase? and stopping-conditions-met?
  [
    set threat-phase? true
    type "burn-in over at tick" print ticks

    collect-init-pars

    if  only-run-burn-in? [
      set only-run-burn-in? false
      export-world filename
      set simulation-over? true ; in R, simulation stops at this point.
    ]
    ;simulation stops if we only want to run the burn-in.

    add-perturbations
  ]
  tick
  if not any? turtles [stop]
end

;--------------------------- PERTURBATIONS -----------------------------

to add-perturbations
  habitat-loss-process
  habitat-fragmentation-process
  habitat-degradation-process
  invasion-process
end

to habitat-loss-process
  ; first calculate number of patches to be affected by perturbation
  let max-nr-affected-patches floor count patches * (habitat_loss / 100)

  let nr-affected-patches 0 ; number of patches changed

  if max-nr-affected-patches > 0 [
    ask one-of patches [
      set max-resources 0
      set resource-regen 0
      set pcolor 0
      set nr-affected-patches 0
      set resource-patches other resource-patches ; remove this patch from resource patches
    ]
  ]
  while [nr-affected-patches < max-nr-affected-patches]
  [
    ask patches with [pcolor != 0]
    [
      if nr-affected-patches >= max-nr-affected-patches [stop]
      let n neighbors4 with [pcolor = 0]
      if any? n
      [
        let rand-patch one-of n
        set max-resources 0
        set pcolor black
        set nr-affected-patches nr-affected-patches + 1
        set resource-patches other resource-patches ; remove this patch from resource patches
      ]
    ]
  ]
  ask patches with [pcolor = 0] [set pcolor brown]
end

;; Destruction of some patches
; sets max-resources and resource-regen to some patches
; patches affected at random. % of patches given by habitat_loss
to habitat-fragmentation-process
  ask n-of ceiling (count patches * habitat_fragmentation / 100) patches [
    set max-resources  0
    set resource-regen 0
    set resource-patches other resource-patches ; remove this patch from resource patches
    set pcolor brown
  ]
end

;; Decreases max-resources and resource-regen of all patches
; Amount of decrease given by habitat_degradation
to habitat-degradation-process
  ask patches [
    set max-resources     max-resources  * (1 - habitat_degradation / 100)
    set resource-regen    resource-regen * (1 - habitat_degradation / 100)
  ]
end


;; Create aliens
to invasion-process
  ask n-of (invaders / 100 * count organisms) organisms [
    hatch-aliens 1
    [
      set shape "butterfly"
      set color red
    ]
  ]
end

;--------------------------------------------------------------------------


;------------------------ PATCH PROCEDURES --------------------------------
to update-patches
  ask resource-patches
  [
    regen-resources
    ;if patch-color-scales-with-resources? [
    ;set pcolor scale-color base-color resources -10 (max-resources * 2)
  ]
  if simulation-over? [
    ask patches [
      ask organisms-here [
        set list-of-body-sizes lput body-size list-of-body-sizes
        set list-of-maturity-ages lput maturity-age list-of-maturity-ages
        set list-of-fecundities lput fecundity list-of-fecundities
        set list-of-dispersal-abilities lput disp-ability list-of-dispersal-abilities
      ]
    ]
  ]
end

to regen-resources
  set resources resources + resource-regen
  ;check if resources is higher than max-resources:
  set resources min (list resources max-resources)
end


;---------------------- TURTLE PROCEDURES -----------------------------

to turtles-go
  ;foreach sort-on [-1 * random-normal body-size (body-size * stand-dev-to-body-size)] turtles
  foreach sort-on [-1 * random-normal energy ((abs energy) * stand-dev-to-body-size)] turtles
  [ the-turtle -> ask the-turtle [
    if is-organism? self [maintenance]
    feeding
    movement
    if is-organism? self [
      ifelse age < maturity-age [maturation][reproduction]
      mortality
      ageing
    ]
    ]
  ]
end

to feeding
  set energy-intake 0
  let available-storage max-energy - energy ;space available for the storage of energy
  set energy-intake min (list available-storage max-energy-intake [resources] of patch-here)
  ask patch-here [set resources resources - [energy-intake] of myself]
  if is-organism? self [set energy energy + energy-intake]
end

to movement
  if energy-intake < maintenance-cost + maturation-cost
  [
    let disp-distance random-float disp-ability
    set heading heading + random 360
    fd disp-distance
    if is-organism? self [set energy energy - dispersal-cost * disp-distance]
  ]
  ;if energy < 0 [die] ;2020-05-28 commented this, seems not to be necessary
end

to maturation
  set energy energy - maturation-cost
end

to reproduction
  if energy > energy-to-reproduce [
    let offspring-energy 0
    if fecundity > 0 [set offspring-energy (energy - energy-after-reprod-coef) / fecundity]
    repeat fecundity
    [
      hatch 1 [
        mutate-traits [body-size] of myself
                      [disp-ability] of myself
                      [fecundity] of myself
                      [maturity-age] of myself
        organism-initialization
        ;;state variables
        set age 0
        set energy offspring-energy
      ]
    ]
    set energy energy-after-reprod
    set age-at-reprod age
  ]
end

to maintenance
  set energy energy - maintenance-cost
  if energy < 0 [die]
end

to mortality
  if (energy < 0) or (random age > longevity) or
  (threat-phase? and random-float 1 < direct_killing / 100) [die]
end

to ageing
  set age age + 1
end

to mutate-traits [bsize dispab fec matage]
  ;PRIMARY ATTRIBUTES
  ;(attributes or state variables sensu Grimm et al 2010 (ODD))
  ;
  ;;focal traits
  ifelse body-size-mutation?    [set body-size random-normal bsize (bsize * mutation-amplitude)][set body-size bsize]
  ifelse disp-ability-mutation? [set disp-ability random-normal dispab (dispab * mutation-amplitude)][set disp-ability dispab]
  set fecundity fec
  ifelse fecundity-mutation? [set fecundity random-normal fec (fec * mutation-amplitude)][set fecundity fec]
  if fecundity < 1 [set fecundity 1]
  ifelse maturity-age-mutation? [set maturity-age random-normal matage (matage * mutation-amplitude)][set maturity-age matage]
  if maturity-age < 1 [set maturity-age 1]
  ifelse random-float 1 < (fecundity - floor fecundity) [set fecundity ceiling fecundity][set fecundity floor fecundity]
  ifelse random-float 1 < (maturity-age - floor maturity-age) [set maturity-age ceiling maturity-age][set maturity-age floor maturity-age]
end

to organism-initialization
  calculate-energy-budgets
  calculate-longevity
end

to calculate-energy-budgets
  ;attributes that are calculated using other model parameters and/or primary attributes
  set max-energy body-size ^ metabolic-allometric-exponent

  ;;constants
  set dispersal-cost           dispersal-cost-coef * max-energy
  set maturation-cost         maturation-cost-coef * max-energy
  set maintenance-cost       maintenance-cost-coef * max-energy
  set max-energy-intake     max-energy-intake-coef * max-energy
  set energy-to-reproduce energy-to-reproduce-coef * max-energy
  set energy-after-reprod energy-after-reprod-coef * max-energy
  ;

  ; VISUALIZATION
  ; no influence in the model, just aesthetics
  if turtle-aesthetics? [
    set size body-size * 2
  ]
end

to calculate-longevity
  set longevity longevity-maturity-coef * maturity-age
  if random-float 1 < (longevity - floor longevity) [set longevity ceiling longevity]
end

; --------------------------------------------------------------------------



;----------- STOPPING CONDITIONS ----------------------------------------------
;sets time until next StopCond check. If ticks = 0, next check is set to 100, meaning that the first check will be on tick 100.
to set-next-checkpoint
  set next-checkpoint round (stopping-conditions-interval * mean [age-at-reprod] of organisms with [age-at-reprod > 0])
end

to-report stopping-conditions-met?
  set next-checkpoint next-checkpoint - 1
  if not any? organisms [report true]  ; to avoid the model breaking if there are no organisms remaining
  if next-checkpoint > 0 [report false]

  set-next-checkpoint

  ; updates mean values for traits and nr-organisms, and checks if their range is within 10% of the previous value for all of them
  collect-stats

  if length means-body-size > nr-of-means
  [
    let mean-of-means-body-size     mean sublist means-body-size    1 (nr-of-means + 1)
    let mean-of-means-maturity-age  mean sublist means-maturity-age 1 (nr-of-means + 1)
    let mean-of-means-disp-ability  mean sublist means-disp-ability 1 (nr-of-means + 1)
    let mean-of-means-fecundity     mean sublist means-fecundity    1 (nr-of-means + 1)
    let mean-of-means-nr-organisms  mean sublist means-nr-organisms 1 (nr-of-means + 1)

    set mean-body-size     mean [body-size] of organisms
    set mean-maturity-age  mean [maturity-age] of organisms
    set mean-disp-ability  mean [disp-ability] of organisms
    set mean-fecundity     mean [fecundity] of organisms
    set nr-organisms       count organisms

    if mean-body-size > mean-of-means-body-size * (1 + stopping-conditions-threshold) or mean-body-size < mean-of-means-body-size * (1 - stopping-conditions-threshold) [report false]
    if mean-maturity-age > mean-of-means-maturity-age * (1 + stopping-conditions-threshold) or mean-maturity-age < mean-of-means-maturity-age * (1 - stopping-conditions-threshold) [report false]
    if mean-disp-ability > mean-of-means-disp-ability * (1 + stopping-conditions-threshold) or mean-disp-ability < mean-of-means-disp-ability * (1 - stopping-conditions-threshold) [report false]
    if mean-fecundity > mean-of-means-fecundity * (1 + stopping-conditions-threshold) or mean-fecundity < mean-of-means-fecundity * (1 - stopping-conditions-threshold) [report false]
    if nr-organisms > mean-of-means-nr-organisms * (1 + stopping-conditions-threshold) or nr-organisms < mean-of-means-nr-organisms * (1 - stopping-conditions-threshold) [report false]

    report true
  ]
  report false
end

;-------------------------------------------------------------------------



;--------------------- STATS---------------------
to collect-stats
  ;collects means of focal attributes + nr of organisms
  set means-body-size    fput mean [body-size] of organisms means-body-size
  set means-maturity-age fput mean [maturity-age] of organisms means-maturity-age
  set means-disp-ability fput mean [disp-ability] of organisms means-disp-ability
  set means-fecundity    fput mean [fecundity] of organisms means-fecundity
  set means-nr-organisms fput count organisms means-nr-organisms
end

to reset-stats
  ;initializes means-... as lists

  set means-body-size    []
  set means-maturity-age []
  set means-disp-ability []
  set means-fecundity    []
  set means-nr-organisms []

  set init-mean-body-size     -999
  set init-mean-maturity-age  -999
  set init-mean-disp-ability  -999
  set init-mean-fecundity     -999
  set init-nr-organisms       -999

  set init-sd-body-size     -999
  set init-sd-maturity-age  -999
  set init-sd-disp-ability  -999
  set init-sd-fecundity     -999

  set final-mean-body-size    -999
  set final-mean-maturity-age -999
  set final-mean-disp-ability -999
  set final-mean-fecundity    -999
  set final-nr-organisms      -999

  set final-sd-body-size    -999
  set final-sd-maturity-age -999
  set final-sd-disp-ability -999
  set final-sd-fecundity    -999


end

to collect-init-pars
  set init-mean-body-size     item 0 means-body-size
  set init-mean-maturity-age  item 0 means-maturity-age
  set init-mean-disp-ability  item 0 means-disp-ability
  set init-mean-fecundity     item 0 means-fecundity
  set init-nr-organisms       item 0 means-nr-organisms

  set init-sd-body-size     standard-deviation [body-size] of organisms
  set init-sd-maturity-age  standard-deviation [maturity-age] of organisms
  set init-sd-disp-ability  standard-deviation [disp-ability] of organisms
  set init-sd-fecundity     standard-deviation [fecundity] of organisms
end

to collect-final-pars
  set final-mean-body-size    item 0 means-body-size
  set final-mean-maturity-age item 0 means-maturity-age
  set final-mean-disp-ability item 0 means-disp-ability
  set final-mean-fecundity    item 0 means-fecundity
  set final-nr-organisms      item 0 means-nr-organisms

  set final-sd-body-size     standard-deviation [body-size] of organisms
  set final-sd-maturity-age  standard-deviation [maturity-age] of organisms
  set final-sd-disp-ability  standard-deviation [disp-ability] of organisms
  set final-sd-fecundity     standard-deviation [fecundity] of organisms
end

;-------------------------------------------------



;---------------WORLD RESIZING-------------------------------

;wrapper function of resize-world that takes only two parameters
to _resize-world
  ; wrapper function
  let x _set-min-max w-width
  let y _set-min-max w-length
  let min-xcor item 0 x
  let max-xcor item 1 x
  let min-ycor item 0 y
  let max-ycor item 1 y

  resize-world min-xcor max-xcor min-ycor max-ycor
end

;just to avoid copy pastying
to-report _set-min-max [dim]
  let mincord 0
  let maxcord 0
  ifelse dim mod 2 = 0 [
    set maxcord floor (dim / 2)
    set mincord -1 * (maxcord - 1)
  ];else
  [set maxcord floor (dim / 2)
   set mincord -1 * maxcord
  ]
  report (list mincord maxcord)
end

;---------------------------------------------------------

; --------------- LOAD WORLD --------------------------------

;loads a saved world, and replaces old perturbation values with new ones.
to load-world
  let temp-habitat_loss habitat_loss
  let temp-habitat_fragmentation habitat_fragmentation
  let temp-habitat_degradation habitat_degradation
  let temp-direct_killing direct_killing
  let temp-invaders invaders

  import-world filename

  set habitat_loss temp-habitat_loss
  set habitat_fragmentation temp-habitat_fragmentation
  set habitat_degradation temp-habitat_degradation
  set direct_killing temp-direct_killing
  set invaders temp-invaders
end

;--------------------------------------------------------------
@#$#@#$#@
GRAPHICS-WINDOW
408
43
911
547
-1
-1
15.0
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

TEXTBOX
1242
208
1392
226
MAP
13
0.0
1

SLIDER
1239
228
1421
261
nr-resource-clusters
nr-resource-clusters
1
(count patches * resource-patch-fraction)
20.0
1
1
NIL
HORIZONTAL

BUTTON
408
10
463
43
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
462
10
525
43
go
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
525
10
588
43
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

PLOT
5
161
206
310
mean body-size
NIL
NIL
0.0
10.0
1.0
2.0
true
false
"" "if threat-phase? [set-plot-pen-color red]\nif simulation-over? [set-plot-pen-color green]"
PENS
"mean" 1.0 0 -16777216 true "let mean-bs mean [body-size] of turtles" "plot mean [body-size] of turtles with [shape = \"bug\"]"

PLOT
5
606
205
756
mean fecundity
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" "if threat-phase? [set-plot-pen-color red]\nif simulation-over? [set-plot-pen-color green]"
PENS
"default" 1.0 0 -16777216 true "" "plot mean [fecundity] of turtles with [shape = \"bug\"]"

PLOT
5
11
206
161
Number of organisms
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" "if threat-phase? [set-plot-pen-color red]\nif simulation-over? [set-plot-pen-color green]"
PENS
"organisms" 1.0 0 -16777216 true "" "plot count organisms"
"invaders" 1.0 0 -7500403 true "" "plot count aliens"

PLOT
5
308
205
458
mean maturity-age
NIL
NIL
0.0
10.0
1.0
6.0
true
false
"" "if threat-phase? [set-plot-pen-color red]\nif simulation-over? [set-plot-pen-color green]"
PENS
"default" 1.0 0 -16777216 true "" "plot mean [maturity-age] of turtles with [shape = \"bug\"]"

SLIDER
928
338
1137
371
longevity-maturity-coef
longevity-maturity-coef
1
10
3.0
0.1
1
NIL
HORIZONTAL

PLOT
205
161
405
309
ln (body-size) hist
NIL
NIL
-10.0
1.0
0.0
10.0
true
false
"set-plot-pen-mode 1\nset-plot-pen-interval 0.01" ""
PENS
"default" 1.0 0 -16777216 true "" "histogram [ln (body-size)] of turtles"

INPUTBOX
926
132
1025
192
dispersal-cost-coef
0.01
1
0
Number

INPUTBOX
926
192
1039
252
maturation-cost-coef
0.1
1
0
Number

INPUTBOX
926
72
1060
132
maintenance-cost-coef
0.1
1
0
Number

INPUTBOX
926
253
1046
313
max-energy-intake-coef
0.3
1
0
Number

INPUTBOX
1066
68
1202
128
energy-to-reproduce-coef
0.7
1
0
Number

INPUTBOX
1066
128
1191
188
energy-after-reprod-coef
0.3
1
0
Number

INPUTBOX
1082
233
1165
293
starting-nr
500.0
1
0
Number

INPUTBOX
1210
423
1281
483
regen-rate
0.2
1
0
Number

INPUTBOX
1342
294
1421
354
patch-n-pixels
15.0
1
0
Number

INPUTBOX
928
373
1032
433
metabolic-allometric-exponent
0.75
1
0
Number

INPUTBOX
1033
373
1137
433
stand-dev-to-body-size
1.0
1
0
Number

TEXTBOX
1213
399
1311
418
PATCHES
15
0.0
1

TEXTBOX
1068
46
1214
74
Reproduction thresholds
13
0.0
1

TEXTBOX
926
44
1094
72
Energy budgets
13
0.0
1

BUTTON
589
10
659
43
NIL
profiler:start
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
659
10
729
43
NIL
profiler:stop
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
729
10
801
43
NIL
profiler:reset
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
801
10
906
43
NIL
print profiler:report
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
205
456
405
606
ln (disp-ability) hist
NIL
NIL
-10.0
5.0
0.0
10.0
true
false
"set-plot-pen-mode 1\nset-plot-pen-interval 0.1" ""
PENS
"default" 1.0 0 -16777216 true "" "histogram [ln disp-ability] of turtles"

PLOT
205
308
405
458
ln (maturity-age) hist
NIL
NIL
-5.0
15.0
0.0
10.0
true
false
"set-plot-pen-mode 1\nset-plot-pen-interval 0.1" ""
PENS
"default" 1.0 0 -16777216 true "" "histogram [ln maturity-age] of turtles"

PLOT
205
606
405
756
fecundity histogram
NIL
NIL
-5.0
5.0
0.0
10.0
true
false
"set-plot-pen-mode 1\nset-plot-pen-interval 0.1" ""
PENS
"default" 1.0 0 -16777216 true "" "histogram [ln fecundity] of turtles"

BUTTON
474
619
551
652
NIL
clear-drawing
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
929
461
1101
494
mutation-amplitude
mutation-amplitude
0
1
0.05
0.01
1
NIL
HORIZONTAL

SWITCH
929
493
1096
526
body-size-mutation?
body-size-mutation?
0
1
-1000

SWITCH
929
561
1102
594
disp-ability-mutation?
disp-ability-mutation?
0
1
-1000

SWITCH
929
527
1115
560
maturity-age-mutation?
maturity-age-mutation?
0
1
-1000

SWITCH
929
593
1095
626
fecundity-mutation?
fecundity-mutation?
0
1
-1000

BUTTON
419
619
474
652
pen down
ask turtles [pen-down]
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
551
619
606
652
pen up
ask turtles [pen-up]
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
1239
261
1421
294
resource-patch-fraction
resource-patch-fraction
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
1237
39
1455
72
stopping-conditions-threshold
stopping-conditions-threshold
0
1
0.05
0.01
1
NIL
HORIZONTAL

PLOT
5
457
205
607
mean disp-ability
NIL
NIL
0.0
10.0
0.0
5.0
true
false
"" "if threat-phase? [set-plot-pen-color red]\nif simulation-over? [set-plot-pen-color green]"
PENS
"default" 1.0 0 -16777216 true "" "plot mean [disp-ability] of turtles with [shape = \"bug\"]"

INPUTBOX
1237
72
1392
132
stopping-conditions-interval
50.0
1
0
Number

INPUTBOX
1238
132
1393
192
nr-of-means
5.0
1
0
Number

SWITCH
419
585
606
618
turtle-aesthetics?
turtle-aesthetics?
0
1
-1000

INPUTBOX
1289
294
1343
354
w-length
33.0
1
0
Number

INPUTBOX
1239
294
1289
354
w-width
33.0
1
0
Number

BUTTON
1191
830
1313
863
NIL
export-world \"world\"
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
1313
830
1432
863
NIL
import-world \"world\"
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1191
763
1305
829
filename
world
1
0
String (reporter)

SWITCH
1305
763
1456
796
only-run-burn-in?
only-run-burn-in?
1
1
-1000

SWITCH
1305
796
1424
829
load-world?
load-world?
1
1
-1000

TEXTBOX
1190
720
1486
738
SAVE TO DISK (to recycle burn-in period)
13
0.0
1

SLIDER
1193
572
1422
605
habitat_loss
habitat_loss
0
100
0.0
1
1
%
HORIZONTAL

SLIDER
1193
638
1422
671
habitat_degradation
habitat_degradation
0
100
0.0
1
1
%
HORIZONTAL

SLIDER
1193
539
1422
572
direct_killing
direct_killing
0
100
1.0
1
1
%
HORIZONTAL

SLIDER
1193
671
1422
704
invaders
invaders
0
100
0.0
1
1
%
HORIZONTAL

TEXTBOX
1199
504
1349
523
THREATS
15
0.0
1

TEXTBOX
934
437
1084
455
MUTATION
13
0.0
1

TEXTBOX
925
15
1075
35
ORGANISM
16
0.0
1

TEXTBOX
1192
741
1342
759
used within R
11
0.0
1

TEXTBOX
938
318
1088
336
Other
13
0.0
1

TEXTBOX
1088
213
1166
231
Starting number
11
0.0
1

TEXTBOX
1239
15
1417
37
STOPPING CONDITIONS
15
0.0
1

TEXTBOX
423
560
573
579
VISUALIZATION
15
0.0
1

SLIDER
1193
605
1422
638
habitat_fragmentation
habitat_fragmentation
0
100
0.0
1
1
%
HORIZONTAL

TEXTBOX
242
34
392
52
black line: burn-in phase
11
0.0
1

TEXTBOX
242
54
403
72
red line: threat phase
11
15.0
1

TEXTBOX
242
72
399
100
green line: simulation has ended
11
55.0
1

BUTTON
667
581
730
614
hd
habitat-degradation-process\n  ask patches [\n    set max-resources     1 * (1 - habitat_degradation / 100)\n    set resource-regen    regen-rate * (1 - habitat_degradation / 100)\n  ]
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
683
667
770
700
no display
no-display
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
807
642
945
675
NIL
ask organisms [die]
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
808
675
937
708
spatial body size
let min-val min [mean list-of-body-sizes] of resource-patches with [length list-of-body-sizes > 0]\nlet max-val max [mean list-of-body-sizes] of resource-patches with [length list-of-body-sizes > 0]\nask resource-patches with [length list-of-body-sizes > 0][\nlet mean-val mean list-of-body-sizes\nset pcolor scale-color orange mean-val (min-val) max-val\n]\nask turtles [ hide-turtle ]
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
808
708
937
741
spatial maturity ages
let min-val min [mean list-of-maturity-ages] of resource-patches with [length list-of-maturity-ages > 0]\nlet max-val max [mean list-of-maturity-ages] of resource-patches with [length list-of-maturity-ages > 0]\nask resource-patches with [length list-of-maturity-ages > 0][\nlet mean-val mean list-of-maturity-ages\nset pcolor scale-color violet mean-val (min-val) max-val\n]\nask turtles [ hide-turtle ]
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
808
741
937
774
spatial fecundity
let min-val min [mean list-of-fecundities] of resource-patches with [length list-of-fecundities > 0]\nlet max-val max [mean list-of-fecundities] of resource-patches with [length list-of-fecundities > 0]\nask resource-patches with [length list-of-fecundities > 0][\nlet mean-val mean list-of-fecundities\nset pcolor scale-color blue mean-val (min-val) max-val\n]\nask turtles [ hide-turtle ]
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
808
774
937
807
spatial disp ability
let min-val min [mean list-of-dispersal-abilities] of resource-patches with [length list-of-dispersal-abilities > 0]\nlet max-val max [mean list-of-dispersal-abilities] of resource-patches with [length list-of-dispersal-abilities > 0]\nask resource-patches with [length list-of-dispersal-abilities > 0][\nlet mean-val mean list-of-dispersal-abilities\nset pcolor scale-color lime mean-val (min-val) max-val\n]\nask turtles [ hide-turtle ]
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
1239
354
1421
387
map-seed
map-seed
0
100
0.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

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
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="diff&amp;frag" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0.11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.77"/>
    </enumeratedValueSet>
    <steppedValueSet variable="fragmentation" first="1" step="20" last="401"/>
    <steppedValueSet variable="diffusion-rate" first="0.35" step="0.02" last="0.95"/>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exploitation_all" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0.01" step="0.01" last="0.3"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="401"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.35"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exploitation_bodySize" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0.01" step="0.01" last="0.3"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="401"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.35"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exploitation_dispAbility" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0.01" step="0.01" last="0.3"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="401"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.35"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exploitation_fecundity" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0.01" step="0.01" last="0.3"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="401"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.35"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exploitation_maturityAge" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0.01" step="0.01" last="0.3"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="401"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.35"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exploitation_resourceSpec" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0.01" step="0.01" last="0.3"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="401"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.35"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="habitatChange_all" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0.05" step="0.05" last="0.95"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="401"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.35"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="habitatChange_bodySize" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0.05" step="0.05" last="0.95"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="401"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.35"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="habitatChange_dispAbility" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0.05" step="0.05" last="0.95"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="401"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.35"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="habitatChange_fecundity" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0.05" step="0.05" last="0.95"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="401"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.35"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="habitatChange_maturityAge" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0.05" step="0.05" last="0.95"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="401"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.35"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="habitatChange_resourceSpec" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0.05" step="0.05" last="0.95"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="401"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.35"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="diff&amp;frag_20190926" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0.11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
    </enumeratedValueSet>
    <steppedValueSet variable="diffusion-rate" first="0.05" step="0.05" last="1"/>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20190927_habitatChange_all" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0.05" step="0.1" last="0.95"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="401"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.35"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20190927_exploitation_all" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0.01" step="0.01" last="0.3"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="401"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.35"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20190927_exploitationExplicit_all" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0.1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-coverage" first="0.5" step="0.1" last="0.95"/>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="401"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.35"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20190928_exploitation&amp;Frag_allTraits" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20190928_habitatLoss&amp;Frag_allTraits" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20190928_exploitation&amp;Frag_dAbility" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20190928_exploitation&amp;Frag_fec" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20190928_exploitation&amp;Frag_matAge" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20190928_exploitation&amp;Frag_resSpec" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20190928_exploitation&amp;Frag_bSize" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20190928_habitatLoss&amp;Frag_bSize" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20190928_habitatLoss&amp;Frag_dAbility" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20190928_habitatLoss&amp;Frag_fec" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20190928_habitatLoss&amp;Frag_matAge" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20190928_habitatLoss&amp;Frag_resSpec" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20190928_energyReprod&amp;exploitation_allTraits" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="energy-after-reprod-coef" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <steppedValueSet variable="energy-to-reproduce-coef" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="mixedPatches" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1"/>
    <metric>count patches with [item 0 carrying-capacity &gt; 0.4 and item 0 carrying-capacity &lt; 0.6]</metric>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-aesthetics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;0.5,0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0.18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_exploitation&amp;Frag_noReSpec" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_habitatLoss&amp;Frag_noReSpec" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_exploitation&amp;Frag_noReSpec_bsize" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_exploitation&amp;Frag_noReSpec_dispab" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_exploitation&amp;Frag_noReSpec_fec" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_exploitation&amp;Frag_noReSpec_matage" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_habitatLoss&amp;Frag_noReSpec_bsize" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_habitatLoss&amp;Frag_noReSpec_dispab" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_habitatLoss&amp;Frag_noReSpec_fec" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_habitatLoss&amp;Frag_noReSpec_matage" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_ResFract&amp;Frag_noReSpec" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="resource-patch-fraction" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_ResFract&amp;Frag_noReSpec_bsize" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="resource-patch-fraction" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191015_ResFract&amp;Frag_noReSpec_dispab" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="resource-patch-fraction" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_ResFract&amp;Frag_noReSpec_fec" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="resource-patch-fraction" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_ResFract&amp;Frag_noReSpec_matage" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="resource-patch-fraction" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_Frag_noReSpec" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_Frag_noReSpec_bsize" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_Frag_noReSpec_dispab" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_Frag_noReSpec_fec" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191002_Frag_noReSpec_matage" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191013_exploitation&amp;Frag_noReSpec" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191014_habitatLoss&amp;Frag_noReSpec" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191014_exploitation&amp;Frag_noReSpec_bsize" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191014_exploitation&amp;Frag_noReSpec_dispab" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191014_exploitation&amp;Frag_noReSpec_fec" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191015_ResFract&amp;Frag_noReSpec_dispab" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="resource-patch-fraction" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191014_exploitation&amp;Frag_noReSpec_matage" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191014_habitatLoss&amp;Frag_noReSpec_bsize" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191014_habitatLoss&amp;Frag_noReSpec_dispab" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191014_habitatLoss&amp;Frag_noReSpec_fec" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191014_habitatLoss&amp;Frag_noReSpec_matage" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191018_exploitation&amp;Frag_noReSpec" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>init-mean-body-size-specialists</metric>
    <metric>init-mean-maturity-age-specialists</metric>
    <metric>init-mean-disp-ability-specialists</metric>
    <metric>init-mean-fecundity-specialists</metric>
    <metric>init-sd-body-size-specialists</metric>
    <metric>init-sd-maturity-age-specialists</metric>
    <metric>init-sd-disp-ability-specialists</metric>
    <metric>init-sd-fecundity-specialists</metric>
    <metric>final-mean-body-size-specialists</metric>
    <metric>final-mean-maturity-age-specialists</metric>
    <metric>final-mean-disp-ability-specialists</metric>
    <metric>final-mean-fecundity-specialists</metric>
    <metric>final-sd-body-size-specialists</metric>
    <metric>final-sd-maturity-age-specialists</metric>
    <metric>final-sd-disp-ability-specialists</metric>
    <metric>final-sd-fecundity-specialists</metric>
    <metric>init-mean-body-size-generalists</metric>
    <metric>init-mean-maturity-age-generalists</metric>
    <metric>init-mean-disp-ability-generalists</metric>
    <metric>init-mean-fecundity-generalists</metric>
    <metric>init-sd-body-size-generalists</metric>
    <metric>init-sd-maturity-age-generalists</metric>
    <metric>init-sd-disp-ability-generalists</metric>
    <metric>init-sd-fecundity-generalists</metric>
    <metric>final-mean-body-size-generalists</metric>
    <metric>final-mean-maturity-age-generalists</metric>
    <metric>final-mean-disp-ability-generalists</metric>
    <metric>final-mean-fecundity-generalists</metric>
    <metric>final-sd-body-size-generalists</metric>
    <metric>final-sd-maturity-age-generalists</metric>
    <metric>final-sd-disp-ability-generalists</metric>
    <metric>final-sd-fecundity-generalists</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191018_habitatLoss&amp;Frag_noReSpec" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>init-mean-body-size-specialists</metric>
    <metric>init-mean-maturity-age-specialists</metric>
    <metric>init-mean-disp-ability-specialists</metric>
    <metric>init-mean-fecundity-specialists</metric>
    <metric>init-sd-body-size-specialists</metric>
    <metric>init-sd-maturity-age-specialists</metric>
    <metric>init-sd-disp-ability-specialists</metric>
    <metric>init-sd-fecundity-specialists</metric>
    <metric>final-mean-body-size-specialists</metric>
    <metric>final-mean-maturity-age-specialists</metric>
    <metric>final-mean-disp-ability-specialists</metric>
    <metric>final-mean-fecundity-specialists</metric>
    <metric>final-sd-body-size-specialists</metric>
    <metric>final-sd-maturity-age-specialists</metric>
    <metric>final-sd-disp-ability-specialists</metric>
    <metric>final-sd-fecundity-specialists</metric>
    <metric>init-mean-body-size-generalists</metric>
    <metric>init-mean-maturity-age-generalists</metric>
    <metric>init-mean-disp-ability-generalists</metric>
    <metric>init-mean-fecundity-generalists</metric>
    <metric>init-sd-body-size-generalists</metric>
    <metric>init-sd-maturity-age-generalists</metric>
    <metric>init-sd-disp-ability-generalists</metric>
    <metric>init-sd-fecundity-generalists</metric>
    <metric>final-mean-body-size-generalists</metric>
    <metric>final-mean-maturity-age-generalists</metric>
    <metric>final-mean-disp-ability-generalists</metric>
    <metric>final-mean-fecundity-generalists</metric>
    <metric>final-sd-body-size-generalists</metric>
    <metric>final-sd-maturity-age-generalists</metric>
    <metric>final-sd-disp-ability-generalists</metric>
    <metric>final-sd-fecundity-generalists</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191018_exploitation&amp;Frag_noReSpec_bsize" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>init-mean-body-size-specialists</metric>
    <metric>init-mean-maturity-age-specialists</metric>
    <metric>init-mean-disp-ability-specialists</metric>
    <metric>init-mean-fecundity-specialists</metric>
    <metric>init-sd-body-size-specialists</metric>
    <metric>init-sd-maturity-age-specialists</metric>
    <metric>init-sd-disp-ability-specialists</metric>
    <metric>init-sd-fecundity-specialists</metric>
    <metric>final-mean-body-size-specialists</metric>
    <metric>final-mean-maturity-age-specialists</metric>
    <metric>final-mean-disp-ability-specialists</metric>
    <metric>final-mean-fecundity-specialists</metric>
    <metric>final-sd-body-size-specialists</metric>
    <metric>final-sd-maturity-age-specialists</metric>
    <metric>final-sd-disp-ability-specialists</metric>
    <metric>final-sd-fecundity-specialists</metric>
    <metric>init-mean-body-size-generalists</metric>
    <metric>init-mean-maturity-age-generalists</metric>
    <metric>init-mean-disp-ability-generalists</metric>
    <metric>init-mean-fecundity-generalists</metric>
    <metric>init-sd-body-size-generalists</metric>
    <metric>init-sd-maturity-age-generalists</metric>
    <metric>init-sd-disp-ability-generalists</metric>
    <metric>init-sd-fecundity-generalists</metric>
    <metric>final-mean-body-size-generalists</metric>
    <metric>final-mean-maturity-age-generalists</metric>
    <metric>final-mean-disp-ability-generalists</metric>
    <metric>final-mean-fecundity-generalists</metric>
    <metric>final-sd-body-size-generalists</metric>
    <metric>final-sd-maturity-age-generalists</metric>
    <metric>final-sd-disp-ability-generalists</metric>
    <metric>final-sd-fecundity-generalists</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191018_exploitation&amp;Frag_noReSpec_dispab" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>init-mean-body-size-specialists</metric>
    <metric>init-mean-maturity-age-specialists</metric>
    <metric>init-mean-disp-ability-specialists</metric>
    <metric>init-mean-fecundity-specialists</metric>
    <metric>init-sd-body-size-specialists</metric>
    <metric>init-sd-maturity-age-specialists</metric>
    <metric>init-sd-disp-ability-specialists</metric>
    <metric>init-sd-fecundity-specialists</metric>
    <metric>final-mean-body-size-specialists</metric>
    <metric>final-mean-maturity-age-specialists</metric>
    <metric>final-mean-disp-ability-specialists</metric>
    <metric>final-mean-fecundity-specialists</metric>
    <metric>final-sd-body-size-specialists</metric>
    <metric>final-sd-maturity-age-specialists</metric>
    <metric>final-sd-disp-ability-specialists</metric>
    <metric>final-sd-fecundity-specialists</metric>
    <metric>init-mean-body-size-generalists</metric>
    <metric>init-mean-maturity-age-generalists</metric>
    <metric>init-mean-disp-ability-generalists</metric>
    <metric>init-mean-fecundity-generalists</metric>
    <metric>init-sd-body-size-generalists</metric>
    <metric>init-sd-maturity-age-generalists</metric>
    <metric>init-sd-disp-ability-generalists</metric>
    <metric>init-sd-fecundity-generalists</metric>
    <metric>final-mean-body-size-generalists</metric>
    <metric>final-mean-maturity-age-generalists</metric>
    <metric>final-mean-disp-ability-generalists</metric>
    <metric>final-mean-fecundity-generalists</metric>
    <metric>final-sd-body-size-generalists</metric>
    <metric>final-sd-maturity-age-generalists</metric>
    <metric>final-sd-disp-ability-generalists</metric>
    <metric>final-sd-fecundity-generalists</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191018_exploitation&amp;Frag_noReSpec_fec" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>init-mean-body-size-specialists</metric>
    <metric>init-mean-maturity-age-specialists</metric>
    <metric>init-mean-disp-ability-specialists</metric>
    <metric>init-mean-fecundity-specialists</metric>
    <metric>init-sd-body-size-specialists</metric>
    <metric>init-sd-maturity-age-specialists</metric>
    <metric>init-sd-disp-ability-specialists</metric>
    <metric>init-sd-fecundity-specialists</metric>
    <metric>final-mean-body-size-specialists</metric>
    <metric>final-mean-maturity-age-specialists</metric>
    <metric>final-mean-disp-ability-specialists</metric>
    <metric>final-mean-fecundity-specialists</metric>
    <metric>final-sd-body-size-specialists</metric>
    <metric>final-sd-maturity-age-specialists</metric>
    <metric>final-sd-disp-ability-specialists</metric>
    <metric>final-sd-fecundity-specialists</metric>
    <metric>init-mean-body-size-generalists</metric>
    <metric>init-mean-maturity-age-generalists</metric>
    <metric>init-mean-disp-ability-generalists</metric>
    <metric>init-mean-fecundity-generalists</metric>
    <metric>init-sd-body-size-generalists</metric>
    <metric>init-sd-maturity-age-generalists</metric>
    <metric>init-sd-disp-ability-generalists</metric>
    <metric>init-sd-fecundity-generalists</metric>
    <metric>final-mean-body-size-generalists</metric>
    <metric>final-mean-maturity-age-generalists</metric>
    <metric>final-mean-disp-ability-generalists</metric>
    <metric>final-mean-fecundity-generalists</metric>
    <metric>final-sd-body-size-generalists</metric>
    <metric>final-sd-maturity-age-generalists</metric>
    <metric>final-sd-disp-ability-generalists</metric>
    <metric>final-sd-fecundity-generalists</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191018_exploitation&amp;Frag_noReSpec_matage" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>init-mean-body-size-specialists</metric>
    <metric>init-mean-maturity-age-specialists</metric>
    <metric>init-mean-disp-ability-specialists</metric>
    <metric>init-mean-fecundity-specialists</metric>
    <metric>init-sd-body-size-specialists</metric>
    <metric>init-sd-maturity-age-specialists</metric>
    <metric>init-sd-disp-ability-specialists</metric>
    <metric>init-sd-fecundity-specialists</metric>
    <metric>final-mean-body-size-specialists</metric>
    <metric>final-mean-maturity-age-specialists</metric>
    <metric>final-mean-disp-ability-specialists</metric>
    <metric>final-mean-fecundity-specialists</metric>
    <metric>final-sd-body-size-specialists</metric>
    <metric>final-sd-maturity-age-specialists</metric>
    <metric>final-sd-disp-ability-specialists</metric>
    <metric>final-sd-fecundity-specialists</metric>
    <metric>init-mean-body-size-generalists</metric>
    <metric>init-mean-maturity-age-generalists</metric>
    <metric>init-mean-disp-ability-generalists</metric>
    <metric>init-mean-fecundity-generalists</metric>
    <metric>init-sd-body-size-generalists</metric>
    <metric>init-sd-maturity-age-generalists</metric>
    <metric>init-sd-disp-ability-generalists</metric>
    <metric>init-sd-fecundity-generalists</metric>
    <metric>final-mean-body-size-generalists</metric>
    <metric>final-mean-maturity-age-generalists</metric>
    <metric>final-mean-disp-ability-generalists</metric>
    <metric>final-mean-fecundity-generalists</metric>
    <metric>final-sd-body-size-generalists</metric>
    <metric>final-sd-maturity-age-generalists</metric>
    <metric>final-sd-disp-ability-generalists</metric>
    <metric>final-sd-fecundity-generalists</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191018_habitatLoss&amp;Frag_noReSpec_bsize" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>init-mean-body-size-specialists</metric>
    <metric>init-mean-maturity-age-specialists</metric>
    <metric>init-mean-disp-ability-specialists</metric>
    <metric>init-mean-fecundity-specialists</metric>
    <metric>init-sd-body-size-specialists</metric>
    <metric>init-sd-maturity-age-specialists</metric>
    <metric>init-sd-disp-ability-specialists</metric>
    <metric>init-sd-fecundity-specialists</metric>
    <metric>final-mean-body-size-specialists</metric>
    <metric>final-mean-maturity-age-specialists</metric>
    <metric>final-mean-disp-ability-specialists</metric>
    <metric>final-mean-fecundity-specialists</metric>
    <metric>final-sd-body-size-specialists</metric>
    <metric>final-sd-maturity-age-specialists</metric>
    <metric>final-sd-disp-ability-specialists</metric>
    <metric>final-sd-fecundity-specialists</metric>
    <metric>init-mean-body-size-generalists</metric>
    <metric>init-mean-maturity-age-generalists</metric>
    <metric>init-mean-disp-ability-generalists</metric>
    <metric>init-mean-fecundity-generalists</metric>
    <metric>init-sd-body-size-generalists</metric>
    <metric>init-sd-maturity-age-generalists</metric>
    <metric>init-sd-disp-ability-generalists</metric>
    <metric>init-sd-fecundity-generalists</metric>
    <metric>final-mean-body-size-generalists</metric>
    <metric>final-mean-maturity-age-generalists</metric>
    <metric>final-mean-disp-ability-generalists</metric>
    <metric>final-mean-fecundity-generalists</metric>
    <metric>final-sd-body-size-generalists</metric>
    <metric>final-sd-maturity-age-generalists</metric>
    <metric>final-sd-disp-ability-generalists</metric>
    <metric>final-sd-fecundity-generalists</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191018_habitatLoss&amp;Frag_noReSpec_dispab" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>init-mean-body-size-specialists</metric>
    <metric>init-mean-maturity-age-specialists</metric>
    <metric>init-mean-disp-ability-specialists</metric>
    <metric>init-mean-fecundity-specialists</metric>
    <metric>init-sd-body-size-specialists</metric>
    <metric>init-sd-maturity-age-specialists</metric>
    <metric>init-sd-disp-ability-specialists</metric>
    <metric>init-sd-fecundity-specialists</metric>
    <metric>final-mean-body-size-specialists</metric>
    <metric>final-mean-maturity-age-specialists</metric>
    <metric>final-mean-disp-ability-specialists</metric>
    <metric>final-mean-fecundity-specialists</metric>
    <metric>final-sd-body-size-specialists</metric>
    <metric>final-sd-maturity-age-specialists</metric>
    <metric>final-sd-disp-ability-specialists</metric>
    <metric>final-sd-fecundity-specialists</metric>
    <metric>init-mean-body-size-generalists</metric>
    <metric>init-mean-maturity-age-generalists</metric>
    <metric>init-mean-disp-ability-generalists</metric>
    <metric>init-mean-fecundity-generalists</metric>
    <metric>init-sd-body-size-generalists</metric>
    <metric>init-sd-maturity-age-generalists</metric>
    <metric>init-sd-disp-ability-generalists</metric>
    <metric>init-sd-fecundity-generalists</metric>
    <metric>final-mean-body-size-generalists</metric>
    <metric>final-mean-maturity-age-generalists</metric>
    <metric>final-mean-disp-ability-generalists</metric>
    <metric>final-mean-fecundity-generalists</metric>
    <metric>final-sd-body-size-generalists</metric>
    <metric>final-sd-maturity-age-generalists</metric>
    <metric>final-sd-disp-ability-generalists</metric>
    <metric>final-sd-fecundity-generalists</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191018_habitatLoss&amp;Frag_noReSpec_fec" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>init-mean-body-size-specialists</metric>
    <metric>init-mean-maturity-age-specialists</metric>
    <metric>init-mean-disp-ability-specialists</metric>
    <metric>init-mean-fecundity-specialists</metric>
    <metric>init-sd-body-size-specialists</metric>
    <metric>init-sd-maturity-age-specialists</metric>
    <metric>init-sd-disp-ability-specialists</metric>
    <metric>init-sd-fecundity-specialists</metric>
    <metric>final-mean-body-size-specialists</metric>
    <metric>final-mean-maturity-age-specialists</metric>
    <metric>final-mean-disp-ability-specialists</metric>
    <metric>final-mean-fecundity-specialists</metric>
    <metric>final-sd-body-size-specialists</metric>
    <metric>final-sd-maturity-age-specialists</metric>
    <metric>final-sd-disp-ability-specialists</metric>
    <metric>final-sd-fecundity-specialists</metric>
    <metric>init-mean-body-size-generalists</metric>
    <metric>init-mean-maturity-age-generalists</metric>
    <metric>init-mean-disp-ability-generalists</metric>
    <metric>init-mean-fecundity-generalists</metric>
    <metric>init-sd-body-size-generalists</metric>
    <metric>init-sd-maturity-age-generalists</metric>
    <metric>init-sd-disp-ability-generalists</metric>
    <metric>init-sd-fecundity-generalists</metric>
    <metric>final-mean-body-size-generalists</metric>
    <metric>final-mean-maturity-age-generalists</metric>
    <metric>final-mean-disp-ability-generalists</metric>
    <metric>final-mean-fecundity-generalists</metric>
    <metric>final-sd-body-size-generalists</metric>
    <metric>final-sd-maturity-age-generalists</metric>
    <metric>final-sd-disp-ability-generalists</metric>
    <metric>final-sd-fecundity-generalists</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191018_habitatLoss&amp;Frag_noReSpec_matage" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>init-mean-body-size-specialists</metric>
    <metric>init-mean-maturity-age-specialists</metric>
    <metric>init-mean-disp-ability-specialists</metric>
    <metric>init-mean-fecundity-specialists</metric>
    <metric>init-sd-body-size-specialists</metric>
    <metric>init-sd-maturity-age-specialists</metric>
    <metric>init-sd-disp-ability-specialists</metric>
    <metric>init-sd-fecundity-specialists</metric>
    <metric>final-mean-body-size-specialists</metric>
    <metric>final-mean-maturity-age-specialists</metric>
    <metric>final-mean-disp-ability-specialists</metric>
    <metric>final-mean-fecundity-specialists</metric>
    <metric>final-sd-body-size-specialists</metric>
    <metric>final-sd-maturity-age-specialists</metric>
    <metric>final-sd-disp-ability-specialists</metric>
    <metric>final-sd-fecundity-specialists</metric>
    <metric>init-mean-body-size-generalists</metric>
    <metric>init-mean-maturity-age-generalists</metric>
    <metric>init-mean-disp-ability-generalists</metric>
    <metric>init-mean-fecundity-generalists</metric>
    <metric>init-sd-body-size-generalists</metric>
    <metric>init-sd-maturity-age-generalists</metric>
    <metric>init-sd-disp-ability-generalists</metric>
    <metric>init-sd-fecundity-generalists</metric>
    <metric>final-mean-body-size-generalists</metric>
    <metric>final-mean-maturity-age-generalists</metric>
    <metric>final-mean-disp-ability-generalists</metric>
    <metric>final-mean-fecundity-generalists</metric>
    <metric>final-sd-body-size-generalists</metric>
    <metric>final-sd-maturity-age-generalists</metric>
    <metric>final-sd-disp-ability-generalists</metric>
    <metric>final-sd-fecundity-generalists</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191018_exploitation&amp;Frag_allTraits" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>init-mean-body-size-specialists</metric>
    <metric>init-mean-maturity-age-specialists</metric>
    <metric>init-mean-disp-ability-specialists</metric>
    <metric>init-mean-fecundity-specialists</metric>
    <metric>init-sd-body-size-specialists</metric>
    <metric>init-sd-maturity-age-specialists</metric>
    <metric>init-sd-disp-ability-specialists</metric>
    <metric>init-sd-fecundity-specialists</metric>
    <metric>final-mean-body-size-specialists</metric>
    <metric>final-mean-maturity-age-specialists</metric>
    <metric>final-mean-disp-ability-specialists</metric>
    <metric>final-mean-fecundity-specialists</metric>
    <metric>final-sd-body-size-specialists</metric>
    <metric>final-sd-maturity-age-specialists</metric>
    <metric>final-sd-disp-ability-specialists</metric>
    <metric>final-sd-fecundity-specialists</metric>
    <metric>init-mean-body-size-generalists</metric>
    <metric>init-mean-maturity-age-generalists</metric>
    <metric>init-mean-disp-ability-generalists</metric>
    <metric>init-mean-fecundity-generalists</metric>
    <metric>init-sd-body-size-generalists</metric>
    <metric>init-sd-maturity-age-generalists</metric>
    <metric>init-sd-disp-ability-generalists</metric>
    <metric>init-sd-fecundity-generalists</metric>
    <metric>final-mean-body-size-generalists</metric>
    <metric>final-mean-maturity-age-generalists</metric>
    <metric>final-mean-disp-ability-generalists</metric>
    <metric>final-mean-fecundity-generalists</metric>
    <metric>final-sd-body-size-generalists</metric>
    <metric>final-sd-maturity-age-generalists</metric>
    <metric>final-sd-disp-ability-generalists</metric>
    <metric>final-sd-fecundity-generalists</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191018_habitatLoss&amp;Frag_allTraits" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>init-mean-body-size-specialists</metric>
    <metric>init-mean-maturity-age-specialists</metric>
    <metric>init-mean-disp-ability-specialists</metric>
    <metric>init-mean-fecundity-specialists</metric>
    <metric>init-sd-body-size-specialists</metric>
    <metric>init-sd-maturity-age-specialists</metric>
    <metric>init-sd-disp-ability-specialists</metric>
    <metric>init-sd-fecundity-specialists</metric>
    <metric>final-mean-body-size-specialists</metric>
    <metric>final-mean-maturity-age-specialists</metric>
    <metric>final-mean-disp-ability-specialists</metric>
    <metric>final-mean-fecundity-specialists</metric>
    <metric>final-sd-body-size-specialists</metric>
    <metric>final-sd-maturity-age-specialists</metric>
    <metric>final-sd-disp-ability-specialists</metric>
    <metric>final-sd-fecundity-specialists</metric>
    <metric>init-mean-body-size-generalists</metric>
    <metric>init-mean-maturity-age-generalists</metric>
    <metric>init-mean-disp-ability-generalists</metric>
    <metric>init-mean-fecundity-generalists</metric>
    <metric>init-sd-body-size-generalists</metric>
    <metric>init-sd-maturity-age-generalists</metric>
    <metric>init-sd-disp-ability-generalists</metric>
    <metric>init-sd-fecundity-generalists</metric>
    <metric>final-mean-body-size-generalists</metric>
    <metric>final-mean-maturity-age-generalists</metric>
    <metric>final-mean-disp-ability-generalists</metric>
    <metric>final-mean-fecundity-generalists</metric>
    <metric>final-sd-body-size-generalists</metric>
    <metric>final-sd-maturity-age-generalists</metric>
    <metric>final-sd-disp-ability-generalists</metric>
    <metric>final-sd-fecundity-generalists</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191018_exploitation&amp;Frag_allTraits_resSpec" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>init-mean-body-size-specialists</metric>
    <metric>init-mean-maturity-age-specialists</metric>
    <metric>init-mean-disp-ability-specialists</metric>
    <metric>init-mean-fecundity-specialists</metric>
    <metric>init-sd-body-size-specialists</metric>
    <metric>init-sd-maturity-age-specialists</metric>
    <metric>init-sd-disp-ability-specialists</metric>
    <metric>init-sd-fecundity-specialists</metric>
    <metric>final-mean-body-size-specialists</metric>
    <metric>final-mean-maturity-age-specialists</metric>
    <metric>final-mean-disp-ability-specialists</metric>
    <metric>final-mean-fecundity-specialists</metric>
    <metric>final-sd-body-size-specialists</metric>
    <metric>final-sd-maturity-age-specialists</metric>
    <metric>final-sd-disp-ability-specialists</metric>
    <metric>final-sd-fecundity-specialists</metric>
    <metric>init-mean-body-size-generalists</metric>
    <metric>init-mean-maturity-age-generalists</metric>
    <metric>init-mean-disp-ability-generalists</metric>
    <metric>init-mean-fecundity-generalists</metric>
    <metric>init-sd-body-size-generalists</metric>
    <metric>init-sd-maturity-age-generalists</metric>
    <metric>init-sd-disp-ability-generalists</metric>
    <metric>init-sd-fecundity-generalists</metric>
    <metric>final-mean-body-size-generalists</metric>
    <metric>final-mean-maturity-age-generalists</metric>
    <metric>final-mean-disp-ability-generalists</metric>
    <metric>final-mean-fecundity-generalists</metric>
    <metric>final-sd-body-size-generalists</metric>
    <metric>final-sd-maturity-age-generalists</metric>
    <metric>final-sd-disp-ability-generalists</metric>
    <metric>final-sd-fecundity-generalists</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="exploitation-intensity" first="0" step="0.02" last="0.18"/>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="20191018_habitatLoss&amp;Frag_allTraits_resSpec" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <exitCondition>simulation-over? or not any? turtles</exitCondition>
    <metric>init-mean-body-size</metric>
    <metric>init-mean-maturity-age</metric>
    <metric>init-mean-disp-ability</metric>
    <metric>init-mean-fecundity</metric>
    <metric>init-mean-evenness</metric>
    <metric>init-nr-organisms</metric>
    <metric>final-mean-body-size</metric>
    <metric>final-mean-maturity-age</metric>
    <metric>final-mean-disp-ability</metric>
    <metric>final-mean-fecundity</metric>
    <metric>final-mean-evenness</metric>
    <metric>final-nr-organisms</metric>
    <metric>init-sd-body-size</metric>
    <metric>init-sd-maturity-age</metric>
    <metric>init-sd-disp-ability</metric>
    <metric>init-sd-fecundity</metric>
    <metric>init-sd-evenness</metric>
    <metric>final-sd-body-size</metric>
    <metric>final-sd-maturity-age</metric>
    <metric>final-sd-disp-ability</metric>
    <metric>final-sd-fecundity</metric>
    <metric>final-sd-evenness</metric>
    <metric>init-mean-body-size-specialists</metric>
    <metric>init-mean-maturity-age-specialists</metric>
    <metric>init-mean-disp-ability-specialists</metric>
    <metric>init-mean-fecundity-specialists</metric>
    <metric>init-sd-body-size-specialists</metric>
    <metric>init-sd-maturity-age-specialists</metric>
    <metric>init-sd-disp-ability-specialists</metric>
    <metric>init-sd-fecundity-specialists</metric>
    <metric>final-mean-body-size-specialists</metric>
    <metric>final-mean-maturity-age-specialists</metric>
    <metric>final-mean-disp-ability-specialists</metric>
    <metric>final-mean-fecundity-specialists</metric>
    <metric>final-sd-body-size-specialists</metric>
    <metric>final-sd-maturity-age-specialists</metric>
    <metric>final-sd-disp-ability-specialists</metric>
    <metric>final-sd-fecundity-specialists</metric>
    <metric>init-mean-body-size-generalists</metric>
    <metric>init-mean-maturity-age-generalists</metric>
    <metric>init-mean-disp-ability-generalists</metric>
    <metric>init-mean-fecundity-generalists</metric>
    <metric>init-sd-body-size-generalists</metric>
    <metric>init-sd-maturity-age-generalists</metric>
    <metric>init-sd-disp-ability-generalists</metric>
    <metric>init-sd-fecundity-generalists</metric>
    <metric>final-mean-body-size-generalists</metric>
    <metric>final-mean-maturity-age-generalists</metric>
    <metric>final-mean-disp-ability-generalists</metric>
    <metric>final-mean-fecundity-generalists</metric>
    <metric>final-sd-body-size-generalists</metric>
    <metric>final-sd-maturity-age-generalists</metric>
    <metric>final-sd-disp-ability-generalists</metric>
    <metric>final-sd-fecundity-generalists</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="habitat-change-coverage" first="0" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="128"/>
      <value value="256"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>timer</metric>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-ycor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-xcor">
      <value value="-16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-aesthetics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ycor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-xcor">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="fragmentation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only-run-burn-in?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="filename">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="load-world?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-aesthetics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w-length">
      <value value="33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w-width">
      <value value="33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>simulation-over?</exitCondition>
    <metric>count turtles</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="fragmentation">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-n-pixels">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-nr">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-intensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-dev-to-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp-ability-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-patch-fraction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatially-explicit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metabolic-allometric-exponent">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-habitat-change">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-disp-ability">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-means">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nr-of-resource-types">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="body-size-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longevity-maturity-coefficient">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-habitat-change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="filename">
      <value value="&quot;world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-after-reprod-coef">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="only-run-burn-in?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="load-world?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-cost-coef">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-coverage">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-spec-mutation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-aesthetics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-to-reproduce-coef">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeostasis-cost-coef">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-body-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost-coef">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-energy-intake-coef">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-maturity-age">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="habitat-change-coverage">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diffusion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clusters-exploitation">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-fecundity">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-conditions-interval">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="big-move-first?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-size">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-resource-spec">
      <value value="&quot;1,0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploitation-intensity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturity-age-mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulate-exploitation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w-length">
      <value value="33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w-width">
      <value value="33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regen-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity-mutation?">
      <value value="false"/>
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
