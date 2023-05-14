module Unit (Unit(..)) where

data Unit = Meter | Kilometer | Mile
    deriving (Eq)

instance Show Unit where
    show Meter = "м"
    show Kilometer = "км"
    show Mile = "mi" 

