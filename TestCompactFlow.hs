module TestCompactFlow where

import NetworkData
import CompactFlow
import Utils

--Test compact flow
test = let

	cf1 = rewrite Src (StringAtom "a") mostGeneralCF
	cf2 = rewrite Src (StringAtom "b") mostGeneralCF
	cf3 = rewrite Dst (StringAtom "c") mostGeneralCF

	p = rewrite Port (StringAtom "x") mostGeneralCF
	
	tests = [ -- CompactFlow tests
		intersect cf1 mostGeneralCF == CompactFlow [(Src, StringAtom "a"), (Dst, Any), (Port, Any)],
		intersect mostGeneralCF cf1 == CompactFlow [(Dst, Any), (Port, Any), (Src, StringAtom "a")],
		intersect mostGeneralCF cf1 == CompactFlow [(Dst, Any), (Src, StringAtom "a"), (Port, Any)],
		intersect mostGeneralCF mostGeneralCF == mostGeneralCF,
		intersect cf1 cf1 == cf1,
		intersect cf1 cf2 == Void,
		intersect cf1 cf3 == CompactFlow [(Dst,  StringAtom "c"), (Src, StringAtom "a"), (Port, Any)],
		intersect cf3 cf1 == CompactFlow [(Src, StringAtom "a"), (Dst,  StringAtom "c"), (Port, Any)],
		subset mostGeneralCF mostGeneralCF,
		subset cf1 cf1,
		subset (intersect cf1 cf3) cf1,
		subset (intersect cf3 cf1) cf3,
		subset (intersect cf1 cf2) cf1,
		not $ subset cf1 (intersect cf1 cf3),
		not $ subset cf3 (intersect cf3 cf1),
		not $ subset cf1 (intersect cf1 cf2),
		not $ subset cf1 cf2,
		not $ subset cf1 cf3,
		rewrite Src (StringAtom "a") cf2 == cf1,
		rewrite Src Any cf2 == mostGeneralCF
		]
	in zip3 [1..] tests (repeat 1)

score = sum $ map (\(_, _, score) -> score) $ filter (\(_, result, _) -> result) test