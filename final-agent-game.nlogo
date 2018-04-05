;Bromley's Tower ( a dungeon crawling game)
;Peter Bromley & Robert Lichenstein


patches-own [ wall-group path-to-end? ]
breed [monsters monster]
breed [heroes hero]
breed [bolts bolt]
breed [bullets bullet]
monsters-own [ last-visited health class ]
heroes-own [ health ]
bolts-own [ dist ]

globals[
  player         ;the players avatar
  strength       ;the melee damage skill
  constitution      ;player health
  vision           ; how much the player can see
  magic    ; ranged magic damage damage
  skillpoints    ;the players skill points to be allocated
  mmdam          ;monster melee damage
  msdam          ;monster bullet damage
  mnum           ;number of monsters variable
  start-patch
  end-patch
  level
  freeze         ;freezes game state for point allocation
  cooldown       ;delay btw player actions
  magic-cooldown
]

;Observer context
;Sets up the game board, to be used only on the starting of a new game
; not bewteen rounds
to setup
  ca
  ask patches [ set wall-group -1 set pcolor white ]
  ask patches [ set path-to-end? false ]
  init-globals
  init-start-and-end
  create-walls
  create-player
  create-mons
  vision-update     ;important to include, otherwise the player can see the whole map
  reset-ticks
  user-message("Please allocate all points, then click the move button to start the game")
end

;Observer Context
;Initializes our important globals, only to be used on game start
to init-globals
  set skillpoints 5
  set level 1
  set mmdam 1
  set mnum 1
  set msdam 1
  set freeze true
  set cooldown 20000
end


;Observer Context
;updates the global values between each round
;gives more skill points, and scales monster damage/health/spawn rates
;updates player health
to update-globals
  set skillpoints 5 + skillpoints
  set level level + 1
  if level mod 5 = 0 [
    set mmdam mmdam + 1
    set msdam msdam + 1
    set mnum mnum + 1
  ]
  ask player [set health 10 + (2 * constitution)]
  set freeze true
end

; OBSERVER CONTEXT
; Wall Generation

; randomly generates a number of wall groups.  each wall group is made up of a min to max number of patches
to create-walls
  let num-wall-groups 15                   ; number of wall groups
  let num-walls-min 10                     ; min num walls per group
  let num-walls-max 20                     ; max num walls per group
  while [num-wall-groups > 0] [
    let wall-start one-of patches with [ valid-wall-patch ]           ; pick starting patch to grow wall around
    ask wall-start [                                                  ; grow wall group w/ random num of walls
      create-wall-group (random (num-walls-max - num-walls-min) + num-walls-min) num-wall-groups
    ]
    set num-wall-groups num-wall-groups - 1      ; wall group created so decrement
  ]
end


; recursively creates wall group by randomly building around existing patches
to create-wall-group [ num-walls wg ]
  set pcolor gray
  set wall-group wg
  if num-walls > 0 [            ; add a wall by randomly selecting a valid neighbor. If no valid neighbors, stop
    if any? neighbors4 with [ valid-wall-patch ] [
      ask one-of neighbors4 with [ valid-wall-patch ] [ create-wall-group num-walls - 1 wg ]
    ]
  ]
end


; report whether or not a wall group is empty and has any empty neighbors with empty neighbors, and not on path
; from start to finish
to-report valid-wall-patch
  report (wall-group = -1) and (any? neighbors4 with [ wall-group = -1 ]) and (path-to-end? = false)
end


; initialize a start patch on left wall, end patch on right wall.  create a valid, direct path from
; start to end somewhat randomly. this ensures that walls will not block the player from getting to the end
to init-start-and-end
  set start-patch one-of patches with [
    pxcor = min-pxcor and abs pycor > (max-pycor - (max-pycor / 2)) ; start patch is always in top or bottom quarter
  ]
  ask start-patch [set pcolor green]
  set end-patch one-of patches with [pxcor = max-pxcor]             ; end patch completely random
  ask end-patch [set pcolor red]
  let iter-patch start-patch
  while [iter-patch != end-patch] [               ; iterate from start to end patch
    let ip-xcor [pxcor] of iter-patch
    let ip-ycor [pycor] of iter-patch
    ask iter-patch [set path-to-end? true]        ; add current patch to start-end path
    set iter-patch select-patch ip-xcor ip-ycor   ; select next patch in path based on x, y cor
  ]
  ask end-patch [set path-to-end? true]           ; add end-patch to the path
end


; helper for algorithm that creates path from start to end, selects the next patch in the path
; based on the current furthest patch's xcor and ycor compare to the end patch.
to-report select-patch [ip-xcor ip-ycor]
  if ip-ycor = [pycor] of end-patch [ report patch (ip-xcor + 1) ip-ycor ]  ; if the ycor is level with end, just go right
  let movement random 2
  if ip-ycor < [pycor] of end-patch [                                ; if the ycor is under the end patch, either go up or right
    if ip-xcor = max-pxcor [ report patch ip-xcor (ip-ycor + 1) ]
    ifelse movement = 0 [ report patch (ip-xcor + 1) ip-ycor ]
                        [ report patch ip-xcor (ip-ycor + 1) ]
  ]
  if ip-xcor = max-pxcor [ report patch ip-xcor (ip-ycor - 1) ]      ; if the ycor is above the end patch, either go down or right
  ifelse movement = 0 [report patch (ip-xcor + 1) ip-ycor]
                      [report patch ip-xcor (ip-ycor - 1)]
end

;Observer Context
;Creates a player on a random patch that is not a wall
to create-player
  ask start-patch [
    sprout-heroes 1[
      set player self
      set shape "person"
      set color blue
      set health 10
    ]
  ]
end

;Observer Context
;creates monsters in random locations, not too close to the player
to create-mons
  let temp 0;
  while[temp <= level][      ;spawns amounts of monsters proportional to level
    ask one-of patches with [distance player > 15 and wall-group = -1] [
      sprout-monsters 1[
        set shape "fish"
        set color blue
        set health 5 + level
        set class "runner" ;;class determines strategy of movement
      ]
    ]
          set temp temp + 1
  ]
  set temp 0
  while[temp <= level][   ;spawns amounts of monsters proportional to level
    ask one-of patches with [distance player > 15 and wall-group = -1] [
      sprout-monsters 1[
        set shape "plant"
        set color orange
        set health 5 + level
        set class "shooter"
      ]
    ]
    set temp temp + 2
  ]
  set temp 0
  ask one-of patches with [distance end-patch < 8 and wall-group = -1] [
    sprout-monsters mnum[     ;every fifth ( + 1) level adds a new tanky boss monster
      set shape "face sad"
      set color blue
      set health 10 + (level * 2)
      set class "tank"
    ]
  ]
end
;;Movement


;Observer context //only accessed from button
;;checks if there are any patches directly above it that are not walls
;; if so we can execute our movement function 'move-player'
;;also remains frozen if there are unallocated skill points
;only moves if the player actions are not on cooldown
to move-up
  if freeze = false and cooldown = 0 [
    ask player[
      let temp ycor
      let temp2 xcor
      if any? patches with [pycor = temp + 1 and pxcor = temp2 and wall-group = -1] [
        move-player patch xcor (ycor + 1)
      ]
    ]
    reset-cooldown
  ]
end

to move-down
  if freeze = false and cooldown = 0 [
    ask player[
      let temp ycor
      let temp2 xcor
      if any? patches with [pycor = temp - 1 and pxcor = temp2 and wall-group = -1] [
        move-player patch xcor (ycor - 1)
      ]
    ]
    reset-cooldown
  ]
end

to move-left
  if freeze = false and cooldown = 0 [
    ask player[
      let temp ycor
      let temp2 xcor
      if any? patches with [pycor = temp and pxcor = temp2 - 1 and wall-group = -1] [
        move-player patch (xcor - 1) ycor
      ]
    ]
    reset-cooldown
  ]
end

to move-right
  if freeze = false and cooldown = 0 [
    ask player[
      let temp ycor
      let temp2 xcor
      if any? patches with [pycor = temp and pxcor = temp2 + 1 and wall-group = -1] [
        move-player patch (xcor + 1) ycor
      ]
    ]
    reset-cooldown
  ]
end

;Player Context
;moves player to a patch, or if a monster occupies that patch
;does damage to it in place of a move
to move-player [ movepatch ]
  ifelse any? monsters-on movepatch [   ;checks for a monster on the desired patch
    ask one-of monsters-on movepatch [
      let temp color
      set color red
      set health health - (strength + 1)  ;deals damage to it
      if health < 0 [die]
      wait 0.1
      set color temp
    ]
  ]
  [
    move-to movepatch  ;simply move to the patch we desire if it is open.
  ]
end

;;Observer context
;Accessed from move forever button,
;handler function for monster movement.
to move-monsters
  ask monsters [ ;addresses all monsters with each style of movement
    runner-monster-mvmt ;;specific to class
    shooter-monster-mvmt
    big-monster-mvmt
  ]
end

;;Observer Context
;;Moves all the autonomous agents on certain ticks
;handles bullet/bolt damage
;handles player cooldowns
to move
  if freeze = false [               ;doesn't move if there are unallocated skill points
    if ticks mod 100000 = 0 [
      move-monsters
    ]
    if ticks mod 50000 = 1 [
      move-bolts
      move-bullets
      vision-update
    ]

    if ticks mod 5000 = 0 [
      check-bullet-and-bolt-damage
    ]

    ifelse any? heroes [
      if [health] of player <= 0 [
        ask player [die]
        user-message("You are dead. Press Halt, then setup to start a new life.")
      ]
    ] [
      user-message("You are dead. Press Halt, then setup to start a new life.")
    ]
    if cooldown > 0 [ set cooldown cooldown - 1 ]
    if magic-cooldown > 0 [ set magic-cooldown magic-cooldown - 1 ]
    tick
  ]
end



; MONSTER CONTEXT
; the shooters, at each of their timesteps, either move or shoot at the player.  if the player is in
; a direct line of sight in any of the 4 directions, they shoot.  otherwise, they try to move to a radius
; 8 away from the player
to shooter-monster-mvmt
  if class = "shooter" [
    set heading 0                                             ; for each of the 4 directions, check if player
    ifelse player-in-line xcor ycor 10 0 [ shoot ] [          ; is in direct line of sight. if so, shoot
      set heading 90
      ifelse player-in-line xcor ycor 10 90 [ shoot ] [
        set heading 180
        ifelse player-in-line xcor ycor 10 180 [ shoot ] [
          set heading 270
          ifelse player-in-line xcor ycor 10 270 [ shoot ] [
            let step shooter-mvmt-step                        ; player not in line of sight, so take a step
            move-to step
          ]
        ]
      ]
    ]

  ]
end

; MONSTER CONTEXT
; defines the movement pattern of the shooters. if shooter is close to the player, it tries to run away to a
; distance of 8 away. if the shooter is furhter away, it exhibits same behavior as runner monster
to-report shooter-mvmt-step
  if not any? heroes in-radius 8 [report runner-mvmt-step]             ; if far away from player, act like runner
  let last-patch last-visited
  let valid-neighbors neighbors4 with [wall-group = -1 and self != last-patch and not any? monsters-on self]
  let next-step patch-here
  if any? valid-neighbors [
    set next-step min-one-of valid-neighbors [abs (distance player - 8)]   ; keeps the shooter moving toward a
  ]                                                                        ; dist of 8 away from player
  set last-visited patch-here
  report next-step
end


; PATCH CONTEXT
; recursively checks all four directions to see if player is in direct line of sight.
; iter is the range of vision, dir is the direction checking
to-report player-in-line [x y iter dir]
  if patch x y != nobody [
    if any? heroes-on patch x y [ report true ]                     ; hero is in line of sight
    if [wall-group] of patch x y != -1 [ report false ]             ; wall is in line of sight before hero, so false
    if iter = 0 [ report false ]                                    ; hero not within range of vision
    if dir = 0 [ report player-in-line x (y + 1) (iter - 1) dir ]   ; recursively check the next patches based on dir
    if dir = 90 [ report player-in-line (x + 1) y (iter - 1) dir ]
    if dir = 180 [ report player-in-line x (y - 1) (iter - 1) dir ]
    report player-in-line (x - 1) y (iter - 1) dir
  ]
  report false
end


; MONSTER CONTEXT
; defines the behavior of the runner monster. if it is directly next to the player, it hits the player and deals
; damage. otherwise it takes a step based on it's movement algorithm
to runner-monster-mvmt
  if class = "runner" [
    ifelse any? heroes-on neighbors4 [           ; melee the player
      ask one-of heroes-on neighbors4 [
        set color red
        set health health - mmdam
        wait 0.1
        set color blue
      ]
    ] [
      let step runner-mvmt-step                  ; move
      move-to step
    ]
  ]
end

; MONSTER CONTEXT
; defines the movement algorithm of the runner monsters. if the player is far away, it moves randomly. when the
; player gets close enough, it moves toward the player by simply choosing the neighbor that is closest to the
; player.
to-report runner-mvmt-step
  let last-patch last-visited
  let valid-neighbors neighbors4 with [wall-group = -1 and self != last-patch and not any? monsters-on self]
  let next-step patch-here
  if any? valid-neighbors [
    ifelse not any? heroes in-radius 15 [
      set next-step one-of valid-neighbors                       ; move randomly
    ] [
      set next-step min-one-of valid-neighbors [distance player] ; move to closest neighbor patch
    ]
  ]
  set last-visited patch-here
  report next-step
end


; MONSTER CONTEXT
; hatches a bullet
to shoot
  hatch-bullets 1 [
    set shape "dot"
    set color yellow
  ]
end


; OBSERVER
; moves all existing bullets
to move-bullets
  ask bullets [
    let ahead patch-ahead 1
    ifelse ahead = nobody [die] [
      ifelse [wall-group] of ahead = -1 [ fd 1 ] [die]  ; check if next space is empty
    ]
  ]
end

;Monster Context
;Handles movement for our "tank" or gate-guardian, he lurks around the end of
;the level for the player to either get by or defeat.
;This function just handles the damaging of the player given a patch to move
;onto.
to big-monster-mvmt
  if class = "tank" [
    ifelse any? heroes-on neighbors4 [
      ask one-of heroes-on neighbors4 [  ;"combat" and player damage
        set color red
        set health health - (mmdam + 1)
        wait 0.1
        set color blue
      ]
    ] [
      let step big-monster-step     ;this is where we determine a valid movement patch
      move-to step
    ]
  ]

end


;Monster Context
;Helper function to determine a valid patch for the big monster
;to move to.
;His behavior allows him only to stay in a certain radius of the end-patch, but within
;that radius he will pursue the player, and also follow the player around the edge.
to-report big-monster-step
  let next-step patch-here
  let valid-neighbors neighbors4 with [wall-group = -1 and not any? monsters-here]
  ;The above is basically any patch that is not a wall or a patch with a monster, we don't
  ;worry about last visited because we just need to let the monster chase the player or wander.
  ifelse (any? heroes in-radius 6) and (distance end-patch < 8) [  ;This lets the monster see the player outside
    set next-step min-one-of valid-neighbors [distance player]     ; of its movable area and make its way over to the
  ]                                                                ; player within a certain distance
  [
    ifelse distance end-patch < 8 [   ;;otherwise we can just randomly wander in our radius
      set next-step one-of valid-neighbors
    ][
      set next-step min-one-of valid-neighbors [distance end-patch]   ;or return to it quickly if we have stepped out
    ]
  ]
  report next-step   ;we just report the patch we choose
end



;Observer Context
;This moves any magic bolts that the player has fired off.
to move-bolts
  ask bolts[
    fd 1
    set dist dist + 1
    if [wall-group] of patch-here != -1 or dist > 5 [die]
  ]
end

;Observer Context
;needs to be a separate function from move-player to remain observer context
;to call newround and vision-update
;This just checks if the player is on the end-patch and calls vision-update
to move-check
  if [patch-here] of player = end-patch[
    newround                              ;start a new round if we have hit the end-patch
  ]
  vision-update                           ;update our vision so we don't see things we shouldn't
end

to-report p-health
  if any? heroes[
    report [health] of player
  ]
end


;Observer context
;Restricts the players vision to a certain radius (scaled by the vision skill)
;Things the player can see is scaled on a gradient to get blacker the farther it is.
;Monsters and bullets are hidden until they enter the radius.
to vision-update
  if any? heroes [
    let vradius (4 + vision)  ;define our vision radius based on the skill
    ask patches [
      ifelse distance player < vradius [        ;if a patch is in our radius
        ifelse wall-group = -1 [                                  ;for each type of patch, we divide the range of the color pallete
          set pcolor white - ((9.9 / vradius) * distance player)  ;over the size of the radius, and multiply that by the distance the
        ][                                                        ;patch is from the player, to determine how much to darken it
          set pcolor grey - ((5 / vradius) * distance player)     ;with subtraction
        ]
        if self = end-patch [
          set pcolor red - ((5 / vradius) * distance player)
        ]
        if self = start-patch[
          set pcolor green -((5 / vradius) * distance player)
        ]
      ]
      [ ;If it is not in our radius, it just goes black
        set pcolor black
      ]
    ]
    ask monsters[  ;This hides the monsters we aren't allowed to see.
      ifelse distance player > vradius [
        set hidden? true
      ][
        set hidden? false
      ]
    ]
    ask bullets[ ;;hides bullets appropriately
      ifelse distance player > vradius [
        set hidden? true
      ] [
        set hidden? false
      ]
    ]
    ;;This is a fun little thing that gives our bolts a little flash of light into the darkness
    ;;if they go outside our vision radius, it is only mildly helpful, but makes canonical sense
    ;;because we are firing magic balls of light.
    ask bolts [
      if distance player > (2 + vision) [
        let lights patches in-radius 1.5
        let source self
        ask lights[
          ifelse wall-group = -1 [
            set pcolor white - (distance myself) * 2
          ][
            set pcolor grey - (distance myself) * 2
            if pcolor > 5 [
              set pcolor 0
            ]
          ]
        ]
      ]
    ]
  ]
end

;Observer Context
;starts a new round, similar to setup but not the same.
to newround
  clear-patches                                     ;we want to clear patches and turtles but not globals
  clear-turtles                                     ;therefore we dont use ca
  ask patches [ set wall-group -1 set pcolor white ]
  ask patches [ set path-to-end? false ]
  init-start-and-end
  create-walls
  create-player
  create-mons
  update-globals             ;we update our globals rather than re-initialize them
  vision-update
  reset-cooldown
  reset-ticks                ;and we reset our ticks to prevent wildly high tick numbers
  user-message ("Welcome to the next round. Please allocate your skill points.")
                             ;we also have to remind our players to use their skill points before they can move.
end


; handles skill count allocation at beginning of every round. global variable "freeze" freezes the
; whole game until all skill points allocated.
to skillup [ skill ]
  if skillpoints > 0 [
    if "strength" = skill [                    ; add strength
      set strength strength + 1
    ]
    if "magic" = skill [                       ; add magic
      set magic magic + 1
    ]
    if "constitution" = skill [                ; add health
      set constitution constitution + 1
      ask player [set health health + 2]
    ]
    if "vision" = skill [                      ; add vision
      set vision vision + 1
    ]
    set skillpoints skillpoints - 1
    if skillpoints = 0 [ set freeze false ]    ; unfreeze game
  ]
end

; player performs a magic blast by sprouting 4 bolts that face all four directions.
; magic blast leads to a cooldown
to magic-blast
  if freeze = false and magic-cooldown = 0 [
    ask player[
      ask patch-here[                         ; sprout 4 bolts heading in all four directions
        sprout-bolts 1[
          set dist 0
          set heading 0
          set shape "sun"
          set color yellow - .7
        ]
        sprout-bolts 1[
          set dist 0
          set heading 90
          set shape "sun"
          set color yellow - .7
        ]
        sprout-bolts 1[
          set dist 0
          set heading 180
          set shape "sun"
          set color yellow - .7
        ]
        sprout-bolts 1[
          set dist 0
          set heading 270
          set shape "sun"
          set color yellow - .7
        ]
      ]
    ]
    reset-magic-cooldown
  ]
end

; resets player cooldown ticks to 20000
to reset-cooldown
  set cooldown 20000
end

; resets magic cooldown ticks to 200000
to reset-magic-cooldown
  set magic-cooldown 200000
end

; checks if any bullets have hit the player or bolts have hit the monsters and
; deals damage
to check-bullet-and-bolt-damage
  ask bullets [
    if any? heroes-on patch-here [         ; if any bullets on same patch as player do damage
      ask player [
        set color red
        set health health - msdam
        wait 0.1
        set color blue
      ]
      die
    ]
  ]
  ask bolts [
    if any? monsters-on patch-here [       ; if any bolts on same patch as monster do damage
      ask one-of monsters-on patch-here[
        let temp color
        set color red
        set health health - (magic + 1)
        if health < 0 [die]
        wait 0.1
        set color temp
      ]
      die
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
210
11
647
449
-1
-1
13.0
1
10
1
1
1
0
0
0
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
26
47
92
80
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
80
98
135
131
up
move-up\nmove-check
NIL
1
T
OBSERVER
NIL
W
NIL
NIL
1

BUTTON
80
142
135
177
down
move-down\nmove-check
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
136
118
191
152
right
move-right\nmove-check
NIL
1
T
OBSERVER
NIL
D
NIL
NIL
1

BUTTON
23
118
78
151
left
move-left\nmove-check
NIL
1
T
OBSERVER
NIL
A
NIL
NIL
1

BUTTON
122
42
187
75
NIL
move
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
812
202
871
247
Strength
strength
17
1
11

MONITOR
813
253
870
298
Vision
vision
17
1
11

MONITOR
693
148
816
193
Points Remaining:
skillpoints
17
1
11

MONITOR
812
303
890
348
Constitution
constitution
17
1
11

MONITOR
814
355
871
400
Magic
magic
17
1
11

BUTTON
715
205
770
238
+
skillup \"strength\"
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
715
253
770
286
+
skillup \"vision\"
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
716
304
771
338
+
skillup \"constitution\"
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
716
357
772
393
+
skillup \"magic\"
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
666
51
723
96
NIL
level
17
1
11

BUTTON
67
310
161
343
NIL
magic-blast
NIL
1
T
OBSERVER
NIL
K
NIL
NIL
1

MONITOR
339
468
441
513
Health
p-health
17
1
11

@#$#@#$#@
## Authors

Peter Bromley & Robert Lichenstein

## Instructions

  You are the player, spawned at one side of the dungeon. Each round the goal is to make it to the exit to the next level of the dungeon. You must allocate all your skill points each round to improve your player before you can begin the round. You must navigate around walls and enemies to find your way to the end of the dungeon.
  Move with WASD and use your magic with K.

## Brief Summary of Game/Agents

  There are 3 types of autonomous agents that function as enemies in this game. The first are "runners." These small enemies will wander randomly until you enter their detection radius, then charge directly at you and attempt to melee attack you until you are dead. They are relatively frail.
  The second are "shooters." These also wander until you enter their vision radius, and then attempt to get within shooting range of you. They will shoot at you from a distance, and continue to stay at shooting range and even run away from you to maintain this distance. They are also frail but hard to pin down.
  The third type are "tanks." These monsters effectively guard the exit to each dungeon. They will not leave a certain radius of the exit, and will pursue you while you are in the radius to prevent you from exiting. They have much more health than the average monster and deal twice as much damage. Beware.
	General strategies for the game are increasing your vision, strenght and health. Magic can be useful for taking out enemies at a distance and lighting distant objects. 

## Credits

Special Thanks to Professor Dickerson for teaching us a lot of the NetLogo we employed.
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

sun
false
0
Circle -7500403 true true 75 75 150
Polygon -7500403 true true 300 150 240 120 240 180
Polygon -7500403 true true 150 0 120 60 180 60
Polygon -7500403 true true 150 300 120 240 180 240
Polygon -7500403 true true 0 150 60 120 60 180
Polygon -7500403 true true 60 195 105 240 45 255
Polygon -7500403 true true 60 105 105 60 45 45
Polygon -7500403 true true 195 60 240 105 255 45
Polygon -7500403 true true 240 195 195 240 255 255

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
NetLogo 6.0.2
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
