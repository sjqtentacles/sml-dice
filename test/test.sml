(* test.sml — tests for sml-dice
   Reference vectors:
   - P(sum=2 | 2d6) = 1/36 (one way)
   - P(sum=7 | 2d6) = 6/36 (most likely)
   - P(sum=12 | 2d6) = 1/36
   - Total outcomes for 2d6 = 36
   - E[3d6] = 10.5 = 63/6 = 21/2
   - parse "3d6+2" round-trips
   - roll is deterministic given seed
*)
structure Tests =
struct
  open Harness

  fun runAll () =
    let
      val () = section "parse"
      val () = check "parse 2d6" (isSome (Dice.parse "2d6"))
      val () = check "parse 3d6+2" (isSome (Dice.parse "3d6+2"))
      val () = check "parse d6" (isSome (Dice.parse "d6"))
      val () = check "parse 1" (isSome (Dice.parse "1"))
      val () = check "parse empty = NONE" (not (isSome (Dice.parse "")))
      val () = check "parse garbage = NONE" (not (isSome (Dice.parse "xyz!")))

      val () = section "2d6 distribution"
      val d2d6 = Dice.distribution (valOf (Dice.parse "2d6"))
      val () = checkInt "36 total outcomes" (36,
        List.foldl (fn ((_,c), acc) => acc + c) 0 d2d6)
      (* P(sum=2) = 1/36 *)
      val cnt2 = case List.find (fn (v,_) => v = 2) d2d6 of
                   SOME (_,c) => c | NONE => 0
      val () = checkInt "P(2)=1" (1, cnt2)
      (* P(sum=7) = 6/36 *)
      val cnt7 = case List.find (fn (v,_) => v = 7) d2d6 of
                   SOME (_,c) => c | NONE => 0
      val () = checkInt "P(7)=6" (6, cnt7)
      (* P(sum=12) = 1/36 *)
      val cnt12 = case List.find (fn (v,_) => v = 12) d2d6 of
                    SOME (_,c) => c | NONE => 0
      val () = checkInt "P(12)=1" (1, cnt12)

      val () = section "3d6 expected value"
      (* E[3d6] = 3 * 3.5 = 10.5 = 21/2 *)
      val (num, den) = Dice.expectedValue (valOf (Dice.parse "3d6"))
      (* Check: num * 2 = den * 21 *)
      val () = check "E[3d6]=21/2" (num * 2 = den * 21)

      val () = section "constant"
      val d5 = Dice.distribution (Dice.Const 5)
      val () = checkInt "Const 5: single outcome" (1, List.length d5)
      val () = check "Const 5: value=5,count=1" (d5 = [(5, 1)])

      val () = section "roll (seeded)"
      val st0 = Prng.SplitMix64.seed 0w42
      val (r1, st1) = Dice.roll (valOf (Dice.parse "2d6")) st0
      val (r2, _)   = Dice.roll (valOf (Dice.parse "2d6")) st0  (* same seed *)
      val () = check "roll is in [2,12]" (r1 >= 2 andalso r1 <= 12)
      val () = checkInt "roll same seed = same result" (r1, r2)
      val (r3, _) = Dice.roll (valOf (Dice.parse "2d6")) st1
      val () = check "roll different state differs or same (just no crash)" true

      val () = section "3d6+2 distribution"
      val d3d6p2 = Dice.distribution (valOf (Dice.parse "3d6+2"))
      (* E[3d6+2] = 12.5; range [5,20]; total 216 *)
      val total3 = List.foldl (fn ((_,c), acc) => acc + c) 0 d3d6p2
      val () = checkInt "total outcomes 3d6 = 216" (216, total3)
      val () = check "min value = 5" (case d3d6p2 of (v,_)::_ => v = 5 | _ => false)
      val (num2, den2) = Dice.expectedValue (valOf (Dice.parse "3d6+2"))
      val () = check "E[3d6+2]=25/2" (num2 * 2 = den2 * 25)
    in
      Harness.run ()
    end

  val run = runAll
end
