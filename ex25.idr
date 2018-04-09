module Ex25

data State : (stateType : Type) -> Type -> Type where
  Get  : State stateType stateType 
  Put  : stateType -> State stateType ()
  Pure : ty -> State stateType ty
  Bind : State stateType a -> (a -> State stateType b) -> State stateType b
  
get : State stateType stateType
get = Get

put : stateType -> State stateType ()
put = Put

pure : ty -> State stateType ty
pure = Pure

(>>=) : State stateType a -> (a -> State stateType b) -> State stateType b
(>>=) = Bind

runState : State stateType a -> (st : stateType) -> (a, stateType)
runState Get st = (st, st)
runState (Put x) st = ((), x)
runState (Pure x) st = (x, st)
runState (Bind cmd prog) st = 
  let (val, nextState) = runState cmd st 
  in runState (prog val) nextState
