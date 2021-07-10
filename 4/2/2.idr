module Main

import Data.Vect

-- integerToFin
-- index

data PowerSource = Petrol | Pedal | Electric

data Vehicle: PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Unicycle: Vehicle Pedal
  Motorcycle: (fuel: Nat) -> Vehicle Petrol
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  ElectricCar : (energy: Nat) -> Vehicle Electric
  Tramway : (energy: Nat) -> Vehicle Electric

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels Unicycle = 1
wheels (Motorcycle fuel) = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (Tramway energy) = 4
wheels (ElectricCar energy) = 4


refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200

refuel (ElectricCar energy) impossible
refuel (Tramway energy) impossible
refuel Bicycle impossible
refuel Unicycle impossible

recharge : Vehicle Electric -> Vehicle Electric
recharge (ElectricCar energy) = ElectricCar 100
recharge (Tramway energy) = Tramway 200

recharge (Motorcycle fuel) impossible
recharge (Car fuel) impossible
recharge (Bus fuel) impossible
recharge Bicycle impossible
recharge Unicycle impossible
