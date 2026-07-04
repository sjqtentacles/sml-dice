(* dice.sig — RPG dice notation parser, exact distributions, seeded sampling *)
signature DICE =
sig
  (* ---- AST ---- *)
  datatype expr
    = Const of int           (* fixed number *)
    | Roll  of int * int     (* NdM: roll N dice each with M sides *)
    | Add   of expr * expr
    | Sub   of expr * expr
    | KeepHigh of expr * int (* kh k: keep k highest results (advantage) *)
    | KeepLow  of expr * int (* kl k: keep k lowest  results (disadvantage) *)

  (* ---- Parser ---- *)
  (* Parse a dice expression string like "2d6", "3d6+2", "2d20kh1", "4d6kh3".
     Returns NONE for malformed input. Numeric fields (counts/sides/keep-k) are
     bounded: any value exceeding 2147483647 (2^31-1) is rejected with NONE
     rather than raising Overflow, so results are portable and identical under
     32-bit MLton and 63-bit Poly/ML. Never raises. *)
  val parse : string -> expr option

  (* ---- Exact probability distribution ---- *)
  (* Returns a sorted list of (value, count) pairs.
     'count' is the number of equally-likely outcomes giving 'value'.
     The total count = product of all dice sides in the expression. *)
  val distribution : expr -> (int * int) list

  (* Expected value (numerator, denominator) as an exact fraction *)
  val expectedValue : expr -> int * int   (* value = num/den *)

  (* ---- Seeded sampling ---- *)
  (* Evaluate the expression using the provided PRNG state.
     Returns (result, newState). *)
  val roll : expr -> Prng.SplitMix64.state -> int * Prng.SplitMix64.state
end
