
external_d_C_incDepth :: (a -> Cover -> ConstStore -> b) 
                         -> Cover -> ConstStore 
                         -> (a -> Cover -> ConstStore -> b)
external_d_C_incDepth f _ _ v cd c = f v (incCover cd) c

external_nd_C_incDepth :: (Func a b) 
                          -> IDSupply -> Cover -> ConstStore 
                          -> (Func a b)
external_nd_C_incDepth (Func f) _ _ _ = Func (\x s cd c -> f x s (incCover cd) c)
external_nd_C_incDepth _ _ _ _ = 
  internalError "External_SetFunctions.external_nd_C_incDepth: \
                \functional argument no ground term"