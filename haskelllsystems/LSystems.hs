module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type Angle
  = Float

type Axiom
  = String

type LSystem
  = (Angle, Axiom, Rules)

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Command
  = Char

type Commands
  = [Command]

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (angle, _, _)
  = angle

-- Returns the axiom string for the given system.
axiom :: LSystem -> String
axiom (_, axiom, _)
  = axiom

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules
rules (_, _, rules)
  = rules

-- Return the binding for the given character in the list of rules
lookupChar :: Char -> Rules -> String
-- Pre: the character has a binding in the Rules list
lookupChar c rules
  = head [y | (x,y) <- rules, x == c] 

-- Expand command string s once using rule table r
expandOne :: String -> Rules -> String
expandOne [] _ = [] 
expandOne (s: xs) rules = lookupChar s rules ++ expandOne xs rules
  

-- Expand command string s n times using rule table r
-- expand :: String -> Int -> Rules -> String
-- expand s 0 rules = s
-- expand s 1 rules = expandOne s rules
-- expand s no rules = expand (expandOne s rules) (no - 1) rules

-- Expand command string s n times using rule table r
expand :: String -> Int -> Rules -> String
expand s n rules
  = (iterate (flip expandOne rules) s ) !! n

 

move :: Command -> Angle -> TurtleState -> TurtleState
move 'L' ang ((x,y), tAng) = ((x,y), tAng + ang)
move 'R' ang ((x,y), tAng) = ((x,y), tAng - ang)
move 'F' ang ((x,y), tAng) = ((x + cos (tAng * (pi / 180)), y + sin (tAng * (pi / 180))), tAng) 
--
-- Trace lines drawn by a turtle using the given colour, following the
-- commands in `cs' and assuming the given angle of rotation.

trace1 :: Commands -> Angle -> Colour -> [ColouredLine]
trace1 commands ang col = fst (trace ((0,0),90) commands)
  where
    trace :: TurtleState -> Commands -> ([ColouredLine], Commands)
    trace _ "" = ([],"")
    trace pos@((startX, startY), startAng) ('F':cs) = ((((startX, startY), (endX, endY), col) : newList), newCom)
      where
        ((endX, endY), newAng) = move 'F' ang pos
        (newList,newCom) = trace ((endX, endY), newAng) cs 

    trace pos ('[':cs) = (newList ++ newList', newCom')
      where
        (newList, newCom) = trace pos cs 
        (newList', newCom') = trace pos newCom 

    trace pos (']':cs) = ([],cs)        
    
    trace pos (lr:cs)
      = trace (move lr ang pos) cs 

                  

trace2 :: Commands -> Angle -> Colour -> [ColouredLine]
trace2 commands ang col = stackHelper posStack ((0,0),90) commands
  where
    posStack = []
    stackHelper :: [TurtleState] -> TurtleState -> Commands -> [ColouredLine]
    stackHelper posStack pos ""  
      = []
    stackHelper posStack pos@((startX, startY), startAng) ('F':cs) 
      = ((startX, startY), fst (move 'F' ang pos), col) : stackHelper posStack (move 'F' ang pos) cs

    stackHelper posStack pos ('[':cs)
      = stackHelper (pos : posStack) pos cs

    stackHelper posStack pos (']':cs)
      = stackHelper posStack (head posStack) cs

    stackHelper posStack pos (lr:cs)
      = stackHelper posStack (move lr ang pos) cs


----------------------------------------------------------
-- Some given functions

expandLSystem :: LSystem -> Int -> String
expandLSystem (_, axiom, rs) n
  = expandOne (expand axiom n rs) commandMap

drawLSystem1 :: LSystem -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (expandLSystem system n) (angle system) colour)

drawLSystem2 :: LSystem -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (expandLSystem system n) (angle system) colour)

----------------------------------------------------------
-- Some test systems.

cross, triangle, arrowHead, peanoGosper, dragon, snowflake, tree, bush :: LSystem

cross
  = (90,
     "M-M-M-M",
     [('M', "M-M+M+MM-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

triangle
  = (90,
     "-M",
     [('M', "M+M-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

arrowHead
  = (60,
     "N",
     [('M', "N+M+N"),
      ('N', "M-N-M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

peanoGosper
  = (60,
     "M",
     [('M', "M+N++N-M--MM-N+"),
      ('N', "-M+NN++N+M--M-N"),
      ('+', "+"),
      ('-', "-")
     ]
    )

dragon
  = (45,
     "MX",
     [('M', "A"),
      ('X', "+MX--MY+"),
      ('Y', "-MX++MY-"),
      ('A', "A"),
      ('+', "+"),
      ('-', "-")
     ]
    )

snowflake
  = (60,
     "M--M--M",
     [('M', "M+M--M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

tree
  = (45,
     "M",
     [('M', "N[-M][+M][NM]"),
      ('N', "NN"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

bush
  = (22.5,
     "X",
     [('X', "M-[[X]+X]+M[+MX]-X"),
      ('M', "MM"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

commandMap :: Rules
commandMap
  = [('M', "F"),
     ('N', "F"),
     ('X', ""),
     ('Y', ""),
     ('A', ""),
     ('[', "["),
     (']', "]"),
     ('+', "L"),
     ('-', "R")
    ]
