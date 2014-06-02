{-# LANGUAGE TemplateHaskell #-}
module Chapter6.Lenses where

import Control.Lens

data Client i = GovOrg  {_identifier :: i, _name :: String}
              | Company {_identifier :: i, _name :: String, _person :: Person, _duty :: String}
              | Individual {_identifier :: i, _person :: Person}
              deriving Show
data Person = Person {_firstName :: String, _lastName :: String}
              deriving Show
makeLenses ''Client
makeLenses ''Person

data TimeMachine = TimeMachine {_manufacturer :: String,
                                _model :: Integer,
                                _tmName :: String,
                                _destination :: Destination,
                                _tmPrice :: Float} deriving Show
data Destination = Past | Future deriving Show
makeLenses ''TimeMachine
makeLenses ''Destination

increasePriceBy :: Float -> [TimeMachine] -> [TimeMachine]
increasePriceBy p tms = tms & traversed.tmPrice %~ (\q -> q + q*p)
