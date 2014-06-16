module CompactFlow where
--This module defines CompactFlow behavior.
import NetworkData

--extrage lista de field-uri
takeawayall:: CompactFlow -> [Field]
takeawayall (CompactFlow field) = field
--extrage cate un field
takeawayonly :: Int->[Field] -> Field
takeawayonly i field = head ( drop (i-1) field)
--verifica daca 2 field-uri sunt egale
checkequal :: Field ->Field->Bool

checkequal ( h1 , v1 ) (h2 , v2) = if h1 == h2 then if v1 == v2 then True 
	                                                else False 
	                             else False
--verifica daca un field este subset al altui field
checksubset:: Field -> Field -> Bool
checksubset ( x , _ ) ( y , Any ) = if x == y then True  else False
checksubset ( x , Any ) ( y , _) = False
checksubset ( h1 , v1 ) (h2 , v2) = if h1 == h2 then if v1 == v2 then True 
	                                                else False 
	                             else False

--face intersectia a 2 field-uri
checkintersect :: Field->Field->Field

checkintersect ( x , a ) ( y , Any ) = if  x == y then ( x , a ) else ( x , Null )
checkintersect ( x , Any ) ( y , b) = if x == y then ( x , b) else ( x , Null )
checkintersect ( h1 , v1 ) ( h2 , v2) = if h1 == h2 then if v1 == v2 then ( h1 , v1 ) 
											    else ( h1 ,Null )
									else ( h1 , Null )

intersectare:: CompactFlow->CompactFlow->[Field]
intersectare a@(CompactFlow ba) b@(CompactFlow bb) = ( checkintersect ( takeawayonly 1 ba ) ( head (filter (\(h,_) -> h == fst (takeawayonly 1 ba)  ) bb )) ) : [] ++ 
													( checkintersect ( takeawayonly 2 ba ) ( head (filter (\(h,_) -> h == fst (takeawayonly 2 ba)  ) bb )) ) : [] ++  
													( checkintersect ( takeawayonly 3 ba ) ( head (filter (\(h,_) -> h == fst (takeawayonly 3 ba)  ) bb )) ) : []

findsecond :: [Field]  ->Bool
findsecond field  = ( if snd ( takeawayonly 1 field ) == Null then False else True) && 
 			         ( if snd ( takeawayonly 2 field ) == Null then False else True) &&
 			         ( if snd ( takeawayonly 3 field ) == Null then False else True) 
-- Two compact flows are equal if they carry the same headers, bound to the same values.
instance Eq CompactFlow where
	Void == Void = True
	Void == (CompactFlow _) = False
	(CompactFlow _) == Void = False
	a == b = ( checkequal ( takeawayonly 1 $ takeawayall a ) ( head (filter (\(h,_) -> h == fst (takeawayonly 1 $ takeawayall a)  ) $ takeawayall b )) ) && 
			 ( checkequal ( takeawayonly 2 $ takeawayall a ) (head (filter (\(h,_) -> h == fst (takeawayonly 2  $ takeawayall a)  ) $ takeawayall b )) ) && 
			 ( checkequal ( takeawayonly 3 $ takeawayall a ) ( head (filter (\(h,_) -> h == fst (takeawayonly 3 $takeawayall a)  ) $ takeawayall b )) ) 



 
instance FlowLike CompactFlow where
	-- Intersection of CompactFlows should be performed for each pair of corresponding headers.
	-- i.e. {(Src, "A"), (Dst, ANY), (Port, ANY)} intersect {(Src, ANY), (Dst, "B"), (Port, ANY)} ->
	-- {(Src, "A"), (Dst, "B"), (Port, ANY)}
	-- Intersection with the Void CompactFlow is Void
	intersect Void _ = Void
	intersect _ Void = Void
	intersect a b =  if findsecond ( intersectare a b ) then CompactFlow (intersectare a b )
	                   else Void 

	-- Checks if CompactFlow a is subset of b
	-- Void is subset of any CompactFlow
	-- Nothing is subset of Void (except from Void)
	-- The subset check works as in the case of intersect - the check should be applied
	-- in sequence for every corresponding header pairs in the two CompactFlows.
	subset Void _ = True
	subset _ Void = False
	subset a@(CompactFlow ba) b@(CompactFlow bb) = ( checksubset ( takeawayonly 1 ba ) ( head (filter (\(h,_) -> h == fst (takeawayonly 1 ba)  ) bb )) ) && 
													( checksubset ( takeawayonly 2 ba ) (head (filter (\(h,_) -> h == fst (takeawayonly 2 ba)  ) bb )) ) && 
													( checksubset ( takeawayonly 3 ba ) ( head (filter (\(h,_) -> h == fst (takeawayonly 3 ba)  ) bb )) ) 

	-- Rewrite a header value from a CompactFlow.
	-- This does nothing to the Void flow.
	rewrite _ _ Void = Void
	rewrite h v (CompactFlow cf) = CompactFlow  (map (\(x,a) -> if h == x then (h,v) else (x,a)) cf)