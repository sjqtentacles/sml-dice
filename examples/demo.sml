(* examples/demo.sml — sml-dice *)
fun fmtDist dist =
  String.concatWith " " (List.map (fn (v,c) =>
    Int.toString v ^ ":" ^ Int.toString c) dist)

val () =
  let
    val () = print "=== Dice ===\n"
    val e2d6 = valOf (Dice.parse "2d6")
    val dist = Dice.distribution e2d6
    val () = print "2d6 distribution (value:count):\n"
    val () = List.app (fn (v,c) =>
      let val bars = String.concat (List.tabulate (c, fn _ => "*"))
      in print ("  " ^ (if v < 10 then " " else "") ^ Int.toString v ^ " | " ^ bars ^ "\n")
      end) dist

    val (num, den) = Dice.expectedValue e2d6
    val () = print ("E[2d6] = " ^ Int.toString num ^ "/" ^ Int.toString den ^ "\n")

    val () = print "\n3d6+2 expected value: "
    val (n2,d2) = Dice.expectedValue (valOf (Dice.parse "3d6+2"))
    val () = print (Int.toString n2 ^ "/" ^ Int.toString d2 ^ "\n")

    val () = print "\nSeeded rolls of 3d6 (seed=99):\n"
    val e3d6 = valOf (Dice.parse "3d6")
    val st0 = Prng.SplitMix64.seed 0w99
    fun loop 0 _ = ()
      | loop n s =
          let val (r, s2) = Dice.roll e3d6 s
          in print ("  " ^ Int.toString r ^ "\n") ; loop (n-1) s2 end
    val () = loop 5 st0

    val () = print "\n4d6kh3 (drop lowest, common for D&D stats):\n"
    val e4kh3 = valOf (Dice.parse "4d6kh3")
    val st1 = Prng.SplitMix64.seed 0w1
    val () = loop 6 st1
  in () end
