module Test where

import NetworkData
import CompactFlow
import Flow
import Utils
import Elements
import Reachability
import Data.List ((\\))

-- Reachability tests.
reachabilityTestLink = let
		net = ["a" :-> "b", "b" :-> "c"]
		res = reachability (toNetwork net) "a" "c"
	in res == read "[Flow [CompactFlow [(Port,StringAtom \"c\"),(Src,Any),(Dst,Any)]]]"

reachabilityTestDiamond = let
		net = ["a" :-> "b", "b" :-> "c", "a" :-> "d", "d" :-> "c"]
		res = reachability (toNetwork net) "a" "c"
	in res == read "[Flow [CompactFlow [(Port,StringAtom \"c\"),(Src,Any),(Dst,Any)]]]"

reachabilityTestLoop = let
		net = ["a" :-> "b", "b" :-> "a"]
		res = reachability (toNetwork net) "a" "a"
	in res == read "[Flow [CompactFlow [(Port,StringAtom \"a\"),(Src,Any),(Dst,Any)]]]"

filterTest = let
		net = [ ("a" :-> "b") :?: [(Src, ["p"])] ]
		res = reachability (toNetwork net) "a" "b"
	in res == read "[Flow [CompactFlow [(Src,StringAtom \"p\"),(Dst,Any),(Port,StringAtom \"b\")]]]"

oneRuleFilterTest = let
		net = [ ("a" :-> "b") :?: [(Src, ["p", "q"])] ]
		res = reachability (toNetwork net) "a" "b"
	in res == read "[Flow [CompactFlow [(Src,StringAtom \"p\"),(Dst,Any),(Port,StringAtom \"b\")],CompactFlow [(Src,StringAtom \"q\"),(Dst,Any),(Port,StringAtom \"b\")]]]"

cascadeFilterTest1 = let
		net = [ ("a" :-> "b") :?: [(Src, ["p", "q"])], ("b" :-> "c") :?: [(Src, ["q"])] ]
		res = reachability (toNetwork net) "a" "c"
	in res == read "[Flow [CompactFlow [(Src,StringAtom \"q\"),(Dst,Any),(Port,StringAtom \"c\")]]]"

cascadeFilterTest2 = let
		net = [ ("a" :-> "b") :?: [(Src, ["p", "q"])], ("b" :-> "c") :?: [(Src, ["s"])] ]
		res = reachability (toNetwork net) "a" "c"
	in null res

rewriteTest = let
		net = [ ("a" :-> "b") :!!: [], ("b" :-> "c") :!!: [(Src, "192.0.0.1")]]
		res = reachability (toNetwork net) "a" "c"
	in res == read "[Flow [CompactFlow [(Src,StringAtom \"192.0.0.1\"),(Port,StringAtom \"c\"),(Dst,Any)]]]"

finalTest = let
		net = [ (("F:0" :-> "F:1") :?: [(Src, ["1", "3"])]) :!?!: [],
			(("F:2" :-> "F:1") :?: [(Src, ["1", "3"])]) :!?!: [],
			("F:1" :-> "P:0") :!!: [],
			("P:0" :-> "P:1") :!!: [(Dst, "2")],
			("P:0" :-> "P:2") :!!: [(Dst, "2")],
			("P:2" :-> "R:0") :!!: [],
			("R:0" :-> "R:1") :!!: [(Dst, "3"), (Src, "3")],
			("R:1" :-> "F:2") :!!: []
			]
		res = reachability (toNetwork net) "F:0" "P:1"
	in res == read "[Flow [CompactFlow [(Dst,StringAtom \"2\"),(Port,StringAtom \"P:1\"),(Src,StringAtom \"1\")],CompactFlow [(Dst,StringAtom \"2\"),(Port,StringAtom \"P:1\"),(Src,StringAtom \"3\")]],Flow [CompactFlow [(Dst,StringAtom \"2\"),(Port,StringAtom \"P:1\"),(Src,StringAtom \"3\")]]]"

--Tests
test = let

	reachabilityTests = [reachabilityTestLink, reachabilityTestDiamond, reachabilityTestLoop, filterTest, oneRuleFilterTest, cascadeFilterTest1,
		cascadeFilterTest2, rewriteTest, finalTest]
	
	in zip3 [1..] reachabilityTests (repeat 5)

score = sum $ map (\(_, _, score) -> score) $ filter (\(_, result, _) -> result) test