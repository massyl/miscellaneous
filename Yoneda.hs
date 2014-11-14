{-# LANGUAGE RankNTypes  #-}

------------------------------------------------------------------------------------------------------
-- | Note that this implementation, mainly machine exampes are inspired by : Reverse Engineering Machines
-- | With Yoneda Lemma from :  A Neighborhood of Infinity Blog
------------------------------------------------------------------------------------------------------ 
module Yoneda where
import Data.Functor.Identity

------------------------------------------------------------------------------------
-- | Let's abstact over a Functor and write generic version of check and uncheck
-----------------------------------------------------------------------------------
newtype Yoneda f a = Yoneda {runYoneda :: forall b. (a -> b) -> f b }

machine :: Functor f => Yoneda f a
machine =  Yoneda $ \f -> fmap f a  where a = undefined

uncheck :: Yoneda f a -> f a
uncheck f = runYoneda f id

check :: Functor f =>  f a -> Yoneda f a
check fa = Yoneda $ \f -> fmap f fa

--------------------------------------------------------------
-- | Let's define a machine whith the following signature
-- | forall b. (a -> b) -> b
--------------------------------------------------------------
type IdentityYoneda a = Yoneda Identity a

machine1 :: IdentityYoneda a
machine1 = Yoneda $ \f -> Identity $ f a where a = undefined

--------------------------------------------------------------
-- | Let's look what's inside our machine
--------------------------------------------------------------
uncheck1:: IdentityYoneda a -> a
uncheck1 f = runIdentity $ runYoneda f $ id 

--------------------------------------------------------------
-- | Let's construct a new machine out of a given : a
--------------------------------------------------------------
check1:: a -> IdentityYoneda a
check1  a  = Yoneda $ \f -> Identity $ f a

--------------------------------------------------------------
-- | let's define a new machine that holds a list of a : [a]
-- | forall b. (a -> b) -> [b]
--------------------------------------------------------------
type ListYoneda a = Yoneda [] a

machine2 :: ListYoneda a
machine2 = Yoneda $ \f -> fmap f xs where xs = undefined

--------------------------------------------------------------
-- | Let's look what's inside our machine
--------------------------------------------------------------
uncheck2 :: ListYoneda a -> [a]
uncheck2 f = runYoneda f id

--------------------------------------------------------------
-- | Let's construct a new machine out of a given : as
--------------------------------------------------------------
check2 :: [a] -> ListYoneda a
check2 as  = Yoneda $ \f ->  fmap  f as 

--------------------------------------------------------------
-- | let's define a new machine that holds a function c -> a
--------------------------------------------------------------
type ArrowYoneda a c = Yoneda ((->) c) a

machine3 :: ArrowYoneda a c
machine3 = Yoneda $ \f -> f . g  where g = undefined

uncheck3 :: ArrowYoneda a c -> c -> a
uncheck3 f = runYoneda f id

check3 :: (c -> a) -> ArrowYoneda a c
check3 f = Yoneda $ \g -> g . f

