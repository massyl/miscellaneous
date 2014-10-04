{-# LANGUAGE RankNTypes #-}

module StateCont (
   -- | inject pure value inside a statefull computation
   inject, 
   -- |Chain statfull computations
   (>>>=), 
   -- | Consult the state
   get,
   -- Change the state
   put,
   -- | modify the state using the given function
   modify, 
   -- | map a given function throught a given state
   mapS, 
   -- | State type constructor
   State)
where

newtype State s a = State { runState :: forall r. s -> (a -> s -> r) -> r }

-- | alternative implementation of return
-- | inject a = State $ \s f -> f a s
inject :: a -> State s a
inject a = State $ flip ($ a) 

-- | maps a give function through a given state to get new state
mapS :: (a -> b) -> State s a -> State s b
mapS f = (>>>= inject . f)

-- | executes the given statefull computation using intial state s0, extract intermediate state s' and
-- | intermediate result. Applies the given function to intermediate result to get an new statefull
-- | computation and then executes this new statefull computation with intermediate state
bind ::  State s a -> (a -> State s b) -> State s b
st `bind` f =  State $ \s0 -> runState st s0 $ \a s' k -> runState (f a) s' k

-- | Infix operation equivalent of bind
infixr 1 >>>=
(>>>=) :: State s a -> (a -> State s b) -> State s b
(>>>=) = bind

-- | gets current state
get :: State s s
get = State $ \s k ->  k s s

-- | updates current states by the give value
put :: s -> State s ()
put s = State $ \ _ f -> f () s

-- | An alternative implementation for put
put' :: s -> State s ()
put' s =  State $ \_ -> flip ($ ()) s

-- | uplies the given function to current State
modify  :: (s -> s) -> State s ()
modify f = get >>= put . f 

-- | Alternative implementation of modify
modify'  :: (s -> s) -> State s ()
modify' f = State $ \s k -> k () $ f s

-- | extract current State and discards the result
execState :: s -> State s a -> s
execState s st = runState st s $ flip const

-- | extracts current result and discards the State
evalState :: s -> State s a -> a
evalState s st = runState st s const

-- | making State s a an instance fo monad to make do notation availalbe
instance Monad (State s) where
 return = inject
 (>>=) = (>>>=)

-- | simple function that manipulate the State, just to show how we can use it
useState ::State Int Int
useState = do 
          s <- get
          let r = s*3 + 1
          put r
          modify (* 3)
          return r

funcToMap :: Show a => a -> String
funcToMap a = show a ++ "_mapped"

-- | Run computations using State S a
main = do
      print $ (runState $ return 1) 3 (+)
      print $ (runState $ get >>= return . (+2)) 1 const
      print $ (runState $ get >>= return . (+2)) 1 (+)
      putStrLn "using execState with get and put : "
      print $  execState 1 useState 
      putStrLn "using evalState with get and put : "
      print $ evalState 1  useState
      print $ evalState 4 (mapS funcToMap (get >>= return . (+2)))
