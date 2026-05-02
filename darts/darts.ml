let within (radius:float) (x:float) (y:float) =
  sqrt (x ** 2. +. y ** 2.) <= radius

let score (x: float) (y: float): int =
  if within 1. x y then 10
  else if within 5. x y then 5
  else if within 10. x y then 1
  else 0
