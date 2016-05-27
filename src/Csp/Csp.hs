module Csp.Csp where

import Text.Show.Functions
import Prelude hiding ((/), (||))
import qualified Data.Set as S
import qualified Data.Map.Strict as Map

data Event = Ev String deriving (Eq, Ord, Show)
newtype Process = Process { run :: Event -> Maybe Process}
                  deriving (Show)
type Trace = [Event]

prefix :: Event -> Process -> Process
prefix c p = Process (\x -> if x == c then Just p else Nothing)

choice :: Map.Map Event Process -> Process
choice m = Process (`Map.lookup` m)

(⌒) :: Trace -> Trace -> Trace
(⌒) = (++)

catenate :: Trace -> Trace -> Trace
catenate = (⌒)

(↾) :: Trace -> S.Set Event -> Trace
(↾) t es = filter (`S.member` es) t

restrict :: Trace -> S.Set Event -> Trace
restrict = (↾)

(↓) :: Trace -> Event -> Int
(↓) s x = length $ s ↾ S.singleton x

isTrace :: Trace -> Process -> Bool
isTrace (e:es) p = case run p e of
                      Nothing -> False
                      Just p' -> isTrace es p'
isTrace [] p   = True

(/) :: Process -> Trace -> Maybe Process
(/) p (e:es) = p' >>= (/ es)
               where p' = run p e
(/) p []   = Just p

-- simple implementation of ||, assuming p and q have the same alphabets
(||) :: Process -> Process -> Process
(||) p q = Process (\e -> do
                            p' <- run p e
                            q' <- run q e
                            Just (p' || q'))

concurrent :: Process -> S.Set Event -> S.Set Event -> Process -> Process
concurrent p a b q = Process (\e -> do
                                p' <- run p e
                                q' <- run q e
                                aux p' q' e)
                      where aux p' q' e
                              | S.member e a && S.member e b  = aux p' q' e
                              | S.member e a                  = aux p' q  e
                              | S.member e b                  = aux p  q' e
                              | otherwise                     = Nothing
