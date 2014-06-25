{-# Language FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Free
import qualified Control.Monad.Free.Church as C
import Control.Monad.Trans
import Data.Functor.Identity
import Data.Functor.Yoneda
import Data.Traversable as T
import Microbench

-- stuffs

type Username = String
newtype User = User { username :: Username } deriving (Show)

users :: Int -> [Username]
users n = map mkUsername $ takeWhile (<n) [0..]
    where mkUsername = ("user-"++) . show

user :: Monad m => Username -> m User
user = return . User

gof :: (Functor f, Monad f, MonadFree f m) => Int -> m [User]
gof n = T.mapM (liftF . user) (users n)

--liftFree fa = wrap $ liftM return fa

avanti :: Int -> IO [User]
avanti n = return $ runIdentity $ retract val
    where
      val :: Free Identity [User]
      val = gof n

coAvanti :: Int -> IO [User]
coAvanti n = return $ runIdentity $ retract $ lowerCodensity val
    where
      val :: Codensity (Free Identity) [User]
      val = gof n

yoAvanti :: Int -> IO [User]
yoAvanti n = return $ runIdentity $ retract $ lowerYoneda val
    where
      val :: Yoneda (Free Identity) [User]
      val = gof n

type Rec f r = (f r -> r) -> r

newtype F f a = F { runF :: forall r. (a -> r) -> (f r -> r) -> r }

-- newtype F f a = F { runF :: forall r. (a -> r) -> Rec f r }
-- type F f a = Yoneda (Rec f) a

instance Functor (F f) where
    fmap f (F g) = F (\kp -> g (kp . f))
instance Monad (F f) where
    return a = F (\kp _ -> kp a)
    F m >>= f = F (\kp kf -> m (\a -> runF (f a) kp kf) kf)
--instance MonadTrans f where
--    lift f = F (\kp kf -> kf (liftM kp f))
instance Functor f => MonadFree f (F f) where
    wrap f = F (\kp kf -> kf (fmap (\ (F m) -> m kp kf) f))

fromF :: MonadFree f m => F f a -> m a
fromF (F m) = m return wrap

yofAvanti :: Int -> IO [User]
yofAvanti n = return $ runIdentity $ retract $ fromF val
    where
      val :: F Identity [User]
      val = gof n

fAvanti :: Int -> IO [User]
fAvanti n = return $ runIdentity $ C.retract val
    where
      val :: C.F Identity [User]
      val = gof n

main = do
  allUsers <- fAvanti 20000
  putStrLn $ show (last allUsers)
  --let usernames = users 4
  --users <- avanti 10
  --putStrLn $ show users
  --microbench "1000 users" $ fmap void (avanti 1000)
