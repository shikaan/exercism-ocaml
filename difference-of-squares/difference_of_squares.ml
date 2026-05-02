let square_of_sum n = 
  let x = Float.of_int n in 
  Float.to_int( ( x *. (x +. 1.) /. 2. ) ** 2. )

let sum_of_squares n =
  let x = Float.of_int n in
  Float.to_int( (x *. (x +. 1.) *. (2.*.x +. 1.) ) /. 6. )

let difference_of_squares n = square_of_sum n - sum_of_squares n
