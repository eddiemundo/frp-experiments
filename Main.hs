{-# language DeriveFunctor #-}
module Main where

merge :: [(Time, a)] -> [(Time, a)] -> [(Time, a)]
merge [] es2 = es2
merge es1 [] = es1
merge (e1@(t1, a1) : es1) (e2@(t2, a2) : es2) =
  if t1 <= t2 then e1 : e2 : merge es1 es2 else e2 : e1 : merge es1 es2

-- Time
data Time = NegInfTime | Time Double | PosInfTime deriving (Eq, Ord, Show)

instance Bounded (Time) where
  minBound = NegInfTime
  maxBound = PosInfTime

-- CStream
data CStream a = CStream { unCStream :: Time -> a } deriving Functor

instance Applicative (CStream) where
  pure a = CStream (const a)
  (<*>) (CStream f) (CStream x) = CStream (\t -> (f t) (x t))

-- DStream
data DStream a = DStream { unDStream :: [(Time, a)] } deriving Functor

instance Semigroup (DStream a) where
  (<>) (DStream es1) (DStream es2) = DStream (merge es1 es2)

instance Monoid (DStream a) where
  mempty = DStream []

instance Applicative (DStream) where
  pure = return
  (<*>) dsf dsx = dsf >>= \f -> fmap f dsx

instance Monad (DStream) where
  return a = DStream [(minBound, a)]  
  (>>=) (DStream es) fn = foldr (\(t, a) s -> delayUntil t (fn a) <> s) mempty es

delayUntil :: Time -> DStream a -> DStream a
delayUntil t (DStream events) = DStream (foldr (\(te, a) s -> (max t te, a) : s) [] events)


switcher :: CStream a -> DStream (CStream a) -> CStream a
switcher cs (DStream []) = cs
switcher cs (DStream ((te, newcs) : es)) =
  let
  in
    CStream (\t -> if t < te then unCStream cs t
                   else unCStream (switcher newcs (DStream es)) t )

stepper :: a -> DStream a -> CStream a 
stepper a ds = switcher (pure a) (fmap (pure) ds) 

main :: IO ()
main = 
  let
    time :: CStream Time
    time = CStream id

    secondEvents :: DStream Time
    secondEvents = DStream ((\i -> let t = Time i in (t, t)) <$> [1..])

    timeFlow :: CStream Time
    timeFlow = stepper (Time 0) secondEvents
    
    times :: [Time]
    times = unCStream timeFlow <$> (Time <$> [0.5, 1.5..100000])
  in
    putStrLn (show times )
