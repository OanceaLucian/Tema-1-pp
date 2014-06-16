module Flow where

-- Avoid overlapping of names.
import CompactFlow
import NetworkData

--functii ajutatoare pentru subset si intersectie
interFlow :: CompactFlow -> [CompactFlow] ->CompactFlow
interFlow x [] =Void
interFlow x ( l:list ) = if (intersect x l) /=Void then  intersect x l else  interFlow x list  

subFlow :: CompactFlow ->[CompactFlow] ->Bool
subFlow x [] =False
subFlow x (l:list) = if(subset x l )/=False then True else subFlow x list
subFlowmake :: [CompactFlow] ->[CompactFlow] ->[Bool]
subFlowmake acfs bcfs = map(\x->subFlow x bcfs) acfs
interFlowmake:: [CompactFlow] -> [CompactFlow]  ->[CompactFlow]
interFlowmake acfs bcfs  =  map (\x-> interFlow x bcfs ) acfs


instance FlowLike Flow where	
	--Applies a rewrite in all member CompactFlows
	rewrite _ _ VoidFlow = VoidFlow
	rewrite h v (Flow cfs) = Flow ( map (\x->(rewrite h v x) ) cfs)

	-- Intersection of Flows works the same as the intersection of math sets.
	-- Check this out if confused: http://en.wikipedia.org/wiki/Union_(set_theory)#Union_and_intersection
	intersect _ VoidFlow = VoidFlow
	intersect VoidFlow _ = VoidFlow
	intersect (Flow acfs) (Flow bcfs) = if filter (\x->x/=Void) (interFlowmake acfs bcfs) /= [] then Flow(filter (\x->x/=Void) (interFlowmake acfs bcfs)) 
											else VoidFlow

	-- A flow is a subset of another if all its CompactFlows have a corresponding CompactFlow in the
	-- second flow to which they are a subset of.
	-- i.e. if for all CompactFlows in the first flow you can find another CompactFlow in the second flow
	-- that includes the CompactFlow picked from the first Flow.
	subset _ VoidFlow = False
	subset VoidFlow _ = True
	subset (Flow acfs) (Flow bcfs) = if filter(\x->x ==False) (subFlowmake acfs bcfs) == [] then True else False

-- Two flow are equal when they represent the same set of network packets.
-- Hint: You should implement this after the above functions were implemented and use them.
instance Eq Flow where
	VoidFlow == VoidFlow = True
	VoidFlow == (Flow _) = False
	(Flow _) == VoidFlow = False
	b == a = (subset a b) && (subset b a)

intersectNotEmpty :: Flow -> Flow -> Bool
intersectNotEmpty a b = intersect a b /= VoidFlow