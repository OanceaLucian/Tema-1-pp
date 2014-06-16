module Test where

import NetworkData
import CompactFlow
import Flow
import Utils

--Tests for Flow
test = let
	
	cf1 = rewrite Src (StringAtom "a") mostGeneralCF
	cf2 = rewrite Src (StringAtom "b") mostGeneralCF
	cf3 = rewrite Dst (StringAtom "c") mostGeneralCF

	p = rewrite Port (StringAtom "x") mostGeneralCF
	pf = Flow [p]

	f1 = Flow [cf1]
	f2 = Flow [cf2]
	f3 = Flow [cf3]

	ff2 = Flow [cf1, cf2]
	ff3 = Flow [cf3, cf1]
	ff4 = Flow [cf1, cf2, cf3]
	
	tests = [ -- CompactFlow tests
		mostGeneralF == mostGeneralF,
		f1 == f1,
		not $ f1 == ff2,
		not $ f1 == ff3,
		not $ f1 == ff4,
		not $ f1 == mostGeneralF,		
		not $ ff3 == ff2,
		not $ ff2 == ff4,
		not $ ff3 == ff4,
		subset f1 ff2,
		subset f1 ff3,
		subset f1 mostGeneralF,
		subset f1 f1,
		not $ subset f1 f2,
		subset ff2 ff4,
		subset ff3 ff4,
		subset f3 ff3,
		subset ff3 mostGeneralF,
		not $ subset ff2 f1,
		not $ subset ff3 f1,
		not $ subset mostGeneralF ff3,
		intersect f1 ff2 == f1,
		subset f1 $ intersect ff3 ff2,
		subset f2 $ intersect ff4 ff2,
		subset f3 $ intersect ff4 ff3,
		not $ subset f2 $ intersect ff3 ff2,
		not $ subset f3 $ intersect ff3 ff2,
		not $ subset pf ff2,
		intersect pf ff2 /= VoidFlow,
		not $ intersect ff3 ff2 == f2,
		not $ intersect ff3 ff2 == f3,
		intersect ff3 mostGeneralF == ff3,
		subset ff3 $ intersect ff3 mostGeneralF,
		not $ subset ff3 $ intersect ff2 mostGeneralF,
		intersect mostGeneralF mostGeneralF == mostGeneralF
		]
	in zip3 [1..] tests (repeat 1)

score = sum $ map (\(_, _, score) -> score) $ filter (\(_, result, _) -> result) test