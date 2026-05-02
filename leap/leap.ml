let leap_year y = 
  match y mod 100 with 
    | 0 -> y mod 400 == 0 
    | _ -> y mod 4 == 0;; 
