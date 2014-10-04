{-# LANGUAGE RankNTypes #-}
module StateCont (
   -- | inject pure value inside a state full computation
   inject, 
   -- |Chain statfull computations
   (>>>=), 
   -- | Consult the state
   get,
   -- Change the state
   put,
   -- | modify the state using the given function
    modify, 
   -- | applies the given function to result curried by one statefull computation
   mapS, 
   -- | State type constructor
   State)
where

newtype State s a = State { runState :: forall r. s -> (a -> s -> r) -> r }

-- | alternative implementation of return
-- | inject a = State $ \s f -> f a s
inject :: a -> State s a
inject a = State $ flip ($ a) 

mapS :: (a -> b) -> State s a -> State s b
mapS f = (>>>= inject . f)

bind :: (a -> State s b) -> State s a -> State s b
f `bind` st =  State $ \s0 -> runState st s0 $ \a s' k -> runState (f a) s' k

infixr 1 >>>=
(>>>=) :: State s a -> (a -> State s b) -> State s b
(>>>=) = flip bind

get :: State s s
get = State $ \s k ->  k s s

put :: s -> State s ()
put s = State $ \ _ f -> f () s

modify  :: (s -> s) -> State s ()
modify f = State $ \s k -> k () $ f s

execState :: s -> State s a -> s
execState s st = runState st s $ flip const

evalState :: s -> State s a -> a
evalState s st = runState st s const



-- | making State S a an instance fo Monad to make do notation availalbe
instance Monad (State s) where
 return = inject
 (>>=) = (>>>=)

-- | simple function that manipulate the state, just to show how we can use it
useState ::State Int Int
useState = do 
          s <- get
          let r = s*3 + 1
          put r
          modify (* 3)
          return r

-- | Run computations using State S a
main = do
      print $ (runState $ inject 1) 3 (+)
      print $ (runState $ get >>>= (\a -> inject $ a + 2)) 0 (+)
      putStrLn "using execState with get and put : "
      print $  execState 1 useState 
      putStrLn "using evalState with get and put : "
      print $ evalState 1  useState
          
