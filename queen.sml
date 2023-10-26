type board = int list
type queen = int * int

 fun printInt (a:int) =
    print(Int.toString(a)^" ");

fun is_safe (q: queen) (b: board): bool =
  let fun check_row (r: int) (c: int): bool =
    if r < 0 then true else if List.mem q (r, c) then false else check_row (r - 1) c
  in
  let fun check_col (r: int) (c: int): bool =
    if c < 0 then true else if List.mem q (r, c) then false else check_col r (c - 1)
  in
  let fun check_diag1 (r: int) (c: int): bool =
    if r < 0 || c < 0 then true else if List.mem q (r, c) then false else check_diag1 (r - 1) (c - 1)
  in
  let fun check_diag2 (r: int) (c: int): bool =
    if r < 0 || c >= n then true else if List.mem q (r, c) then false else check_diag2 (r - 1) (c + 1)
  in
  check_row (fst q) (snd q) && check_col (fst q) (snd q) && check_diag1 (fst q) (snd q) && check_diag2 (fst q) (snd q)
  end
  end
  end
  end
fun solve (b: board , q: queen option): queen option =
  let fun place_queen (b: board) (q: queen option): queen option =
    if b = [] then Some q else
      let val next_row = fst (List.hd b) + 1
      and next_board = List.tl b
      and next_queen = Some (next_row, fst q)
      in
      if is_safe next_queen next_board then place_queen next_board next_queen else place_queen next_board None
      end
  in
  place_queen b None
  end

fun count_solutions (b: board): int =
  let fun count (b: board) (q: queen option): int =
    if b = [] then 1 else
      let val next_row = fst (List.hd b) + 1
      and next_board = List.tl b
      and next_queen = Some (next_row, fst q)
      in
      if is_safe next_queen next_board then count next_board next_queen + count next_board None else count next_board None
      end
  in
  count b None
  end

val n = 8
val initial_board = List.init n (fn i => (0, i))
val solutions = count_solutions initial_board
printInt( solutions)
