module Elements where

import NetworkData
import NetworkFunction
import Utils
import Flow

--This file describes the language used for representing a network (also used in the tests)

-- This element represents a network wire connecting two ports.
-- i.e. "a" :-> "b" The port "a" is connected to "b"
data Wire = String :-> String deriving (Show, Eq)

-- From such an element one should be able to produce a pair of match and apply rules.
instance Element Wire where
	getMatchAndModify (src :-> dst) = (\x -> subset x (rewrite Port (StringAtom src) mostGeneralF) ,\x->rewrite Port (StringAtom dst ) x)

-- The filter element combines a wire and a set of filter conditions.
-- For instance ("a" :-> "b") :?: [(Src, ["p"])] says the wire form "a" to "b"
-- is going to be traversed if and only if Src is bound to "p"
-- ("a" :-> "b") :?: [(Src, ["p", "q"])] says the wire form "a" to "b"
-- is going to be traversed if and only if Src is bound to "p" OR "q"
-- ("a" :-> "b") :?: [(Src, ["p", "q"]), (Dst, ["happiness"])] says the wire form "a" to "b"
-- is going to be traversed if and only if Src is bound to "p" OR "q" AND Dst is bound to "happiness"
data Filter = Wire :?: [(Header, [String])]

-- This two is part of Element type class.
instance Element Filter where
	getMatchAndModify (wire :?: conditions) = (\x->((fst $ getMatchAndModify wire) x) && (intersectNotEmpty x (makeall conditions)),
											\x->intersect (makeall conditions) ((snd $ getMatchAndModify wire) x))

-- A rewriter is an element built on top of a wire or a filter.
-- It performs the logic of the previously specified elements plus a number of rewritings of headers.

--("a" :-> "b") :!!: [] - behaves as a wire from a to b (the rewrite list is empty)

--("P:0" :-> "P:2") :!!: [(Dst, "2")] - is a rewriter from port P:0 to port P:2 that rewrites Dst to 2 for every flow it processes.

--("P:0" :-> "P:2") :!!: [(Dst, "2"), (Src, "1")] - is a rewriter from port P:0 to port P:2 that rewrites Dst to 2
-- and Src to 1 for every flow it processes.

--(("F:0" :-> "F:1") :?: [(Src, ["1", "3"])]) :!?!: [(Dst, "1")] - this represents a filter + a rewriter.
-- It does the following: If traffic on "F:0" has Src "1" or "3", rewrite its Dst to "1" and forward it on port "F:1"
data Rewriter = Wire :!!: [(Header, String)] |  Filter :!?!: [(Header, String)] 

instance Element Rewriter where
   getMatchAndModify (flt :!?!:[]) = getMatchAndModify flt
   getMatchAndModify (wire :!!: []) = getMatchAndModify wire
   getMatchAndModify (wire :!!: rewritings) =  if length(rewritings) == 1 then (\x ->(fst $getMatchAndModify wire) x,
   	                                          \x->makejustone ( head rewritings) ((snd $ getMatchAndModify wire) x))
    	                                           else (\x->(fst $ getMatchAndModify wire) x,
    	                                           	\x-> makejustone (head rewritings) (makejustone (last rewritings) ((snd $ getMatchAndModify wire) x)) )
   getMatchAndModify (flt :!?!: rewritings) = (\x -> (fst $ getMatchAndModify flt) x,\x -> if length (rewritings) == 1 then 
   	                                                          makejustone ( head rewritings) ((snd $ getMatchAndModify flt) x) 
   	                                                          else 
   	                                                          	makejustone (head rewritings) (makejustone (last rewritings) ((snd $ getMatchAndModify flt) x))
                                                            )

--Elements to network config
toNetwork elems = map getMatchAndModify elems