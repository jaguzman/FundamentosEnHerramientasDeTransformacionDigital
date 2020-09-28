;;; Aqui empieza a modificar y poner el codigo













;;;Librerias y archivos iniciales

;;; File to be included in NetLogo Mutliagent Models
;;; BDI Architecture for NetLogo Models
;;; Includes belief revision procedures and Intentions Handling and Execution.
;;; Original Version for Netlogo 2 (2005) I. Sakellariou
;;; Adapted to NetLogo 4 (2008) I. Sakellariou
;;; Adapted to NetLogo 6 (2020) A. Jimenez

;;; Requirements
;;; 1) All agents that modeled as "BDI" agent have two declared -own variables beliefs intentions.
;;; These are the variables to which all beliefs and intentions are recorded. So, in your model if there is a breed of turtles
;;; which you desire to model as BDI, then you should have a BREED-own [beliefs intentions] declaration (along with any other
;;; variables that you wish to include in your model.
;;; MAKE SURE that when you create the variables you set their initial values to empty list ([]).
;;; 2) YOU also must have ticks!! in your model (or timeout will not work).
;;; 3) Your model should have a switch (check NetLogo manual) named "show-intentions"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; BELIEFS
;; There is nothing magical about beliefs: they are simply items
;; of a specific list. Check list processing in NetLogo. However
;; here are some usefull procedures.

;;; creates a new belief. (does not stores it in belief memory).
to-report create-belief [b-type content]
  report (list b-type content)
end

;;; reports type of a belief.
to-report belief-type [bel]
  report first bel
end

;; reports the coontent of belief belief
to-report belief-content [bel]
  report item 1 bel
end

;; Adding information to the beliefs structure
to add-belief [bel]
  if member? bel beliefs [stop]
  set beliefs fput bel beliefs
end
;; Removing a belief from the list of beliefs.
to remove-belief [bel]
 set beliefs remove bel beliefs
end

;;; return true if a specific belief belong to the set of beliefs
to-report exists-belief [bel]
   ifelse member? bel beliefs [report true] [report false]
end

;;; Reports true if a belief in the form of ["b-type" etc etc etc ...] exist in beliefs list
to-report exist-beliefs-of-type [b-type]
  let blfs filter [ s -> first s = b-type ] beliefs
  ifelse empty? blfs [report false] [report true]
end

;;; Returns all beliefs of b-type in a list
to-report beliefs-of-type [b-type]
  report filter [ s -> first s = b-type ] beliefs
end

;;; Returns the first belief of a certain type and removes it
to-report get-belief [b-type]
  ifelse exist-beliefs-of-type b-type
  [let bel first filter [ s -> first s = b-type ] beliefs
   remove-belief bel
   report bel
  ]
  [report false]
end

to-report read-first-belief-of-type [b-type]
   report first beliefs-of-type b-type
end

to update-belief [bel]
   remove-belief read-first-belief-of-type belief-type bel
   add-belief bel
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; CODE FOR HANDLING INTENTIONS-A simple deliberative architecture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to execute-intentions
;;;locals [myInt]  ;; first intentions
  if empty? intentions [stop]
  let myInt get-intention
  run intention-name myInt
  if runresult intention-done myInt [remove-intention myInt]
  if show-intentions [set label intentions] ;; Just for debugging.
end

;;;; Intentions Structure Access Functions
;; returns the current intention of the agent
to-report current-intention
  report intention-name first intentions
end

;; Reports the full intention structure
to-report get-intention
  report first intentions
end

;; Returns the intetnion name (the executable)
to-report intention-name [intention]
  report item 0 intention
end

;; return the done-methods (arguments) of the intention. If it evaluates to true
;; then the intention is removed.
to-report intention-done [intention]
  report item 1 intention
end

to pop-intention
  set intentions but-first intentions
end

;; Removes a specific intention from the intention stack
to remove-intention [bdi-lib##intention]
  set intentions remove-item (position bdi-lib##intention intentions) intentions
end



;; Adds an intention in the intentions list. REMEMBER that intentions are
;; stored in a STACK!
;; The first argument is the intention name that should be some executable procedure
;; you encode in NetLogo. The second argument should be a REPORTER that when evaluates to
;; true the intention is removed (either accomplished or dropped).
;; BOTH ARGUMENTS HAVE TO BE STRINGS (see run/runresult primitive procedures in NetLogo)

to add-intention [name done]
  set intentions fput (list name done) intentions
end

;;;; SPECIAL ACTIONS
;;; a null action
to do-nothing
end

;;; wait for something until the timeout expires.
to wait-for-timeout
  do-nothing
end

;;;
to-report timeout_expired [timeout]
report (word "timeout_has_expired " ticks " " timeout)
end

;;; INTERNAL not to be USED.
;;; reports the end of the timeout.
to-report timeout_has_expired [start interval]
  report (start + interval < ticks )
end


;;; File to be included in NetLogo Mutliagent Models
;;; Communication for NetLogo Multiagent models
;;; Includes primitives for message creation and handling in NetLogo
;;; Original Version for Netlogo 2 (2005) I. Sakellariou
;;; Adapted to NetLogo 4 (2008) I. Sakellariou
;;; Adapted to NetLogo 6 (2020) A. Jimenez

;;; Requirements
;;; 1) All agents that are able to communicate MUST have a declated -own variable incoming-queue.
;;; This is the variable to which all messages are recorded. So, in your model if there is a breed of turtles
;;; which you desire to communicate, then you should have a BREED-own [incoming-queue] declaration (along with any other
;;; variables that you wish to include in your model.
;;; MAKE SURE that when you create the variables you set its values to empty list ([]).
;;; 2) Your model should have a switch (check NetLogo manual) named "show_messages"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; COMMUNICATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MESSAGE PROCESSING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sending Messages
;; (One man's send is another man's receive..)
;; The second commented out line presents an alternative send implementation.
;; The commented out line represents an alternative method.
;; Problem: What if the agent I am sending the message is "killed"
;; Solution: Nothing Happens. Could yield an error message Alternative: create a safe send.
to send [msg]
  let recipients get-receivers msg
  let recv 0
  foreach recipients [ s ->
   set recv turtle (read-from-string s)
   if recv != nobody [ask recv [receive msg]] ;; read-from-string is required to convert the string to number
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Message reception deals with updating incoming-queue
to receive [msg]
   if show_messages [show msg]
   set incoming-queue lput msg incoming-queue
end

;; This reporter returns the next message in the list and removes it from the queue.
to-report get-message
  if empty? incoming-queue [report "no_message"]
  let nextmsg first incoming-queue
  remove-msg
  report nextmsg
end

;; This reporter returns the next message in the list WITHOUT removimg it from the queue.
to-report get-message-no-remove
  if empty? incoming-queue [report "no_message"]
  report first incoming-queue
end

;; Explicit remove-msg.
;; This is needed since reporters *cannot* change a variable's values (apparently).
to remove-msg
  set incoming-queue but-first incoming-queue
end

;; broadcasting to all agents of breed t-breed
to broadcast-to [t-breed msg]
  foreach [who] of t-breed [ s ->
     send add-receiver s msg
  ]
end


;; Creating Messages and adding the sender.
to-report create-message [performative]
 report (list performative (word "sender:" who) )
end

to-report create-reply [performative msg]
let msgOut 0

 set msgOut create-message performative
 set msgOut add-receiver (get-sender msg) msgOut
 report msgOut
end


;; Accesing information on Messages
;; Reports the sender of a message
to-report get-sender [msg]
  report remove "sender:" first (filter [s -> not is-number? s and member? "sender:" s] msg)
  ;;report item ((position "sender:" msg) + 1) msg
end

;; Reports (returns) the content of a message
to-report get-content [msg]
  report item (position "content:" msg + 1) msg
end

;; Reports the list of receivers of a message
to-report get-receivers [msg]
  report map [s -> remove "receiver:" s] filter [s -> not is-number? s and member? "receiver:" s] msg
end

;; reports the message performative.
to-report get-performative [msg]
  report first msg
end

;;; ADDING FIELDS TO A MESSAGE
;; Adding a sender to a message.
to-report add-sender [sender msg]
  report add msg "sender:" sender
end

;; add a receiver
to-report add-receiver [receiver msg]
  report add msg "receiver:" receiver
end

;; adding multiple recipients
to-report add-multiple-receivers [receivers msg]
  foreach receivers
  [ s ->
    set msg add-receiver s msg
  ]
  report msg
end

;; Adding content to a message
to-report add-content [content msg]
  report add msg "content:" content
end

;; Primitive Add command
to-report add [msg field value]
  ifelse field = "content:"
  [report lput value lput field msg]
  [report lput (word field value) msg]
end
@#$#@#$#@
GRAPHICS-WINDOW
444
10
947
514
-1
-1
15.0
1
8
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
1
1
1
ticks
30.0

BUTTON
6
25
79
58
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

SWITCH
183
262
354
295
Mostrar intentions
show-intentions
0
1
-1000

BUTTON
85
25
148
58
Run
run-simulation
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
5
63
191
96
Run (cont)
run-simulation
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
4
103
168
148
Cantidad cervezas en el propietario
[cerveza] of one-of propietarios
17
1
11

MONITOR
175
104
338
149
Robot cerveza
[cerveza] of one-of robots
17
1
11

SLIDER
201
64
373
97
robot-dinero
robot-dinero
0
100
20.0
1
1
NIL
HORIZONTAL

MONITOR
7
156
169
201
Dinero del robot
[dinero] of one-of robots
17
1
11

SWITCH
1
262
175
295
Mostrar mensajes
show_messages
0
1
-1000

PLOT
6
315
373
465
Cerveza 
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
"default" 1.0 0 -2674135 true "" "plot [cerveza] of one-of propietarios"
"pen-1" 1.0 0 -14730904 true "" "plot [cerveza] of one-of robots"

MONITOR
175
157
335
202
Total cervezas
[cervezas-totales-entregadas] of one-of robots
17
1
11

@#$#@#$#@
## WHAT IS IT?

This is the famous "Robot Beer" example from wooldridge. A robot is instructed to get some beer from the supermarket. 

## HOW IT WORKS

The propietario requests beer from the robot, which in turn travels to the fridge and gets one. If the fridge is empty, then it rushes to the supermarket to get some. That is in the case it does have some money to afford it; if the latter is not the case, then it rushes to the bank first.  

## HOW TO USE IT

Set the amount of money the user has press setup and then run the experiment. 

## NETLOGO FEATURES

Model Uses bdi.nls for intention handling and communication.nls for passing FIPA like messages. 

## CREDITS AND REFERENCES

Check the following web address for updates on the libraries mentioned above.
http://users.uom.gr/~iliass
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

wolf
false
0
Polygon -7500403 true true 135 285 195 285 270 90 30 90 105 285
Polygon -7500403 true true 270 90 225 15 180 90
Polygon -7500403 true true 30 90 75 15 120 90
Circle -1 true false 183 138 24
Circle -1 true false 93 138 24

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
