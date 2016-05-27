module Csp.Examples where

import Csp.Csp as Csp
import Data.Map.Strict as Map

stop :: Process
stop = Process (const Nothing)

coin :: Event
coin = Ev "coin"

choc :: Event
choc = Ev "choc"

toffee :: Event
toffee = Ev "toffee"

tick :: Event
tick = Ev "tick"

coinAccepter :: Process
coinAccepter = Process (\x -> case x of
                                (Ev "coin") -> Just stop
                                _           -> Nothing)

clock :: Process
clock = Process (\x -> case x of
                          (Ev "tick") -> Just clock
                          _           -> Nothing)

-- a greedy customer will always accept a toffee or chocolate without paying. Else,
-- he will pay a coin, but will then only accept a chocolate.
greedyCustomer :: Process
greedyCustomer = choice $ Map.fromList [(Ev "toffee", greedyCustomer)
                                      , (Ev "choc", greedyCustomer)
                                      , (Ev "coin", greedyCustomer')]
                    where greedyCustomer' = choice $ Map.fromList [(Ev "choc", greedyCustomer)]

-- \X . (coin -> (choc -> X | toffee -> X))
vendingMachine :: Process
vendingMachine = choice $ Map.fromList [(Ev "coin", choice $
                                                          Map.fromList [(Ev "choc", vendingMachine)
                                                                      , (Ev "toffee", vendingMachine)])]

-- acts like \X . (coin -> choc -> X), so we can do:
-- run greedyCustomerAndVendingMachine coin >>= (`run` choc) >>= (`run` coin) >>= (`run` choc)
-- forever (or, alternatively: Just greedyCustomerAndVendingMachine >>= ((`run` coin) >> (`run` choc) >> (`run` coin)))
greedyCustomerAndVendingMachine :: Process
greedyCustomerAndVendingMachine = greedyCustomer Csp.|| vendingMachine

-- acts like a -> b -> STOP
abStop :: Process
abStop = choice $ Map.fromList [(Ev "a", choice $ Map.fromList [(Ev "b", stop)])]
