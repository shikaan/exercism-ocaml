type dna = [ `A | `C | `G | `T ]
type rna = [ `A | `C | `G | `U ]

let to_rna = List.map (function `A -> `U | `G -> `C | `T -> `A | `C -> `G)
