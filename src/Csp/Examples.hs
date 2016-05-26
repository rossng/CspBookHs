module Csp.Examples where

import Csp.Csp as Csp
import Data.Map.Strict as Map

stop :: Process
stop = Process (const Nothing)

coin :: Event
coin = Ev "coin"

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
greedyCustomer = Process (choice $ Map.fromList
                                    [(Ev "toffee", greedyCustomer)
                                     (Ev "choc", greedyCustomer)
                                     (Ev "coin", greedyCustomer')])
                                     where greedyCustomer' = Process (choice $ Map.fromList [(Ev "choc", greedyCustomer)])

vendingMachine :: Process
vendingMachine = Process (\x -> case x of
                                  (Ev "coin") -> Just (choice $
                                                          Map.fromList [(Ev "choc", vendingMachine)
                                                                      , (Ev "toffee", vendingMachine)])
                                  _           -> Nothing)

greedyCustomerAndVendingMachine :: Process
greedyCustomerAndVendingMachine = greedyCustomer Csp.|| vendingMachine
