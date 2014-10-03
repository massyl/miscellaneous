{-# LANGUAGE RankNTypes #-}

newtype State s a = State { runState :: forall r. s -> (a -> s -> r) -> r }

stateReturn :: a -> State s a
stateReturn a = State $ \s f -> f a s

stateMap :: (a -> b) -> State s a -> State s b
stateMap f =  (stateReturn . f >>>=)

infixr 1 >>>=
(>>>=) :: (a -> State s b) -> State s a -> State s b
f >>>= st =  State $ \s0 -> runState st s0 $ \a s' k -> runState (f a) s' k

get :: State s s
get = State $ \s f -> f s s

put :: s -> State s ()
put s = State $ \ _ f -> f () s

modify  :: (s -> s) -> State s ()
modify f = State $ \s k -> k () $ f s

execState :: s -> State s a -> s
execState s st = runState st s $ \_ s' -> s'

evalState :: s -> State s a -> a
evalState s st = runState st s $ \a _ -> a
