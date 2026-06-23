(* dice.sml — RPG dice notation, exact distributions, seeded sampling *)
structure Dice :> DICE =
struct

  datatype expr
    = Const of int
    | Roll  of int * int
    | Add   of expr * expr
    | Sub   of expr * expr
    | KeepHigh of expr * int
    | KeepLow  of expr * int

  (* ---- Parser ---- *)
  fun isDigit c = c >= #"0" andalso c <= #"9"

  fun skipSpaces cs =
    case cs of #" " :: rest => skipSpaces rest | _ => cs

  fun parseInt cs =
    let
      fun takeDigits [] acc = (List.rev acc, [])
        | takeDigits (c :: rest) acc =
            if isDigit c then takeDigits rest (c :: acc)
            else (List.rev acc, c :: rest)
      val (digits, rest) = takeDigits cs []
    in
      if null digits then NONE
      else SOME (valOf (Int.fromString (String.implode digits)), rest)
    end

  fun parseTerm cs =
    let
      val cs = skipSpaces cs
      val (nOpt, cs2) =
        case cs of
          (#"d" :: _) => (SOME 1, cs)   (* "d6" = "1d6" *)
        | _ => (case parseInt cs of
                  NONE => (NONE, cs)
                | SOME (n, r) => (SOME n, r))
    in
      case (nOpt, cs2) of
        (NONE, _) => NONE
      | (SOME n, #"d" :: rest) =>
          (case parseInt rest of
             NONE => NONE
           | SOME (m, rest2) =>
               let val nDie = if n = 0 then 1 else n
               in
                 case rest2 of
                   (#"k" :: #"h" :: rest3) =>
                     (case parseInt rest3 of
                        SOME (k, rest4) => SOME (KeepHigh (Roll (nDie, m), k), rest4)
                      | NONE => SOME (Roll (nDie, m), rest2))
                 | (#"k" :: #"l" :: rest3) =>
                     (case parseInt rest3 of
                        SOME (k, rest4) => SOME (KeepLow (Roll (nDie, m), k), rest4)
                      | NONE => SOME (Roll (nDie, m), rest2))
                 | _ => SOME (Roll (nDie, m), rest2)
               end)
      | (SOME n, _) => SOME (Const n, cs2)
    end

  fun parseExpr cs =
    case parseTerm (skipSpaces cs) of
      NONE => NONE
    | SOME (e, rest) =>
        let
          fun loop e2 cs2 =
            case skipSpaces cs2 of
              (#"+" :: rest2) =>
                (case parseTerm (skipSpaces rest2) of
                   NONE => SOME (e2, cs2)
                 | SOME (e3, rest3) => loop (Add (e2, e3)) rest3)
            | (#"-" :: rest2) =>
                (case parseTerm (skipSpaces rest2) of
                   NONE => SOME (e2, cs2)
                 | SOME (e3, rest3) => loop (Sub (e2, e3)) rest3)
            | _ => SOME (e2, cs2)
        in loop e rest end

  fun parse s =
    if s = "" then NONE
    else
      case parseExpr (String.explode s) of
        NONE => NONE
      | SOME (e, rest) =>
          if null (skipSpaces rest) then SOME e else NONE

  (* ---- Exact distribution ---- *)
  fun insertSorted (v, c) [] = [(v, c)]
    | insertSorted (v, c) ((v2, c2) :: rest) =
        if v = v2 then (v, c + c2) :: rest
        else if v < v2 then (v, c) :: (v2, c2) :: rest
        else (v2, c2) :: insertSorted (v, c) rest

  fun addDistrib d1 d2 =
    List.foldl (fn ((v1,c1), acc1) =>
      List.foldl (fn ((v2,c2), acc2) =>
        insertSorted (v1+v2, c1*c2) acc2) acc1 d2)
      [] d1

  fun shiftDistrib delta d = List.map (fn (v,c) => (v+delta, c)) d

  fun dieDist m = List.tabulate (m, fn i => (i+1, 1))

  fun distribution e =
    case e of
      Const n     => [(n, 1)]
    | Roll (n, m) =>
        let val d = dieDist m
        in  List.foldl (fn (_, acc) => addDistrib acc d)
                       [(0, 1)]
                       (List.tabulate (n, fn i => i))
        end
    | Add (a, b)  => addDistrib (distribution a) (distribution b)
    | Sub (a, b)  =>
        let val db = List.map (fn (v,c) => (~v, c)) (distribution b)
        in  addDistrib (distribution a) db end
    | KeepHigh (Roll (n, m), k) => keepHighDist n m k
    | KeepLow  (Roll (n, m), k) => keepLowDist n m k
    | KeepHigh (e2, _) => distribution e2
    | KeepLow  (e2, _) => distribution e2

  and keepHighDist n m k =
    let
      val counts = Array.array (n * m + 2, 0)
      fun sort [] = []
        | sort (x::xs) =
            let val lo = List.filter (fn y => y <= x) xs
                val hi = List.filter (fn y => y > x) xs
            in sort hi @ [x] @ sort lo end
      fun gen 0 rolls =
            let val sorted = sort rolls
                val top = List.take (sorted, Int.min (k, List.length sorted))
                val s = List.foldl op+ 0 top
            in  Array.update (counts, s, Array.sub (counts, s) + 1) end
        | gen i rolls =
            List.app (fn d => gen (i-1) (d :: rolls))
                     (List.tabulate (m, fn j => j+1))
      val () = gen n []
    in
      Array.foldri (fn (i, c, acc) => if c > 0 then (i,c) :: acc else acc) [] counts
    end

  and keepLowDist n m k =
    let
      val counts = Array.array (n * m + 2, 0)
      fun sort [] = []
        | sort (x::xs) =
            let val lo = List.filter (fn y => y < x) xs
                val hi = List.filter (fn y => y >= x) xs
            in sort lo @ [x] @ sort hi end
      fun gen 0 rolls =
            let val sorted = sort rolls
                val bot = List.take (sorted, Int.min (k, List.length sorted))
                val s = List.foldl op+ 0 bot
            in  Array.update (counts, s, Array.sub (counts, s) + 1) end
        | gen i rolls =
            List.app (fn d => gen (i-1) (d :: rolls))
                     (List.tabulate (m, fn j => j+1))
      val () = gen n []
    in
      Array.foldri (fn (i, c, acc) => if c > 0 then (i,c) :: acc else acc) [] counts
    end

  fun gcd a b = if b = 0 then a else gcd b (a mod b)

  fun expectedValue e =
    let val dist = distribution e
        val total = List.foldl (fn ((_,c), acc) => acc + c) 0 dist
        val sumNum = List.foldl (fn ((v,c), acc) => acc + v * c) 0 dist
        val g = gcd (abs sumNum) total
    in  (sumNum div g, total div g) end

  (* ---- Seeded sampling ---- *)
  fun sortDesc [] = []
    | sortDesc (x::xs) =
        let val hi = List.filter (fn y => y > x) xs
            val lo = List.filter (fn y => y <= x) xs
        in sortDesc hi @ [x] @ sortDesc lo end

  fun sortAsc [] = []
    | sortAsc (x::xs) =
        let val lo = List.filter (fn y => y < x) xs
            val hi = List.filter (fn y => y >= x) xs
        in sortAsc lo @ [x] @ sortAsc hi end

  fun rollN 0 m rs s = (rs, s)
    | rollN i m rs s =
        let val (r, s2) = Prng.SplitMix64.intRange (1, m) s
        in rollN (i-1) m (r :: rs) s2 end

  fun roll e st =
    case e of
      Const n => (n, st)
    | Roll (n, m) =>
        let fun loop 0 acc s = (acc, s)
              | loop i acc s =
                  let val (r, s2) = Prng.SplitMix64.intRange (1, m) s
                  in loop (i-1) (acc + r) s2 end
        in loop n 0 st end
    | Add (a, b) =>
        let val (ra, s2) = roll a st
            val (rb, s3) = roll b s2
        in  (ra + rb, s3) end
    | Sub (a, b) =>
        let val (ra, s2) = roll a st
            val (rb, s3) = roll b s2
        in  (ra - rb, s3) end
    | KeepHigh (Roll (n, m), k) =>
        let val (rolls, s2) = rollN n m [] st
            val sorted = sortDesc rolls
            val top = List.take (sorted, Int.min (k, n))
        in  (List.foldl op+ 0 top, s2) end
    | KeepHigh (e2, k) => roll e2 st
    | KeepLow (Roll (n, m), k) =>
        let val (rolls, s2) = rollN n m [] st
            val sorted = sortAsc rolls
            val bot = List.take (sorted, Int.min (k, n))
        in  (List.foldl op+ 0 bot, s2) end
    | KeepLow (e2, k) => roll e2 st

end
