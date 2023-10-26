 fun printInt (a:int) =
    print(Int.toString(a)^" ");

fun getInt () =
    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);
    
fun printIntList ( [] ) = ()
  | printIntList ( x::xs ) = 
    let
	val tmp = printInt(x)
    in
	printIntList(xs)
    end;

fun getIntList ( 0 ) = []
  | getIntList ( N:int) = getInt()::getIntList(N-1);

fun split [ ]  = ([ ], [ ]) 
    | split [x] = ([ ], [x])
    | split (x::y::L) =
	let val (A, B) =split L
	in (x::A, y::B) 	
	end;

datatype tree = Empty | Br of tree * int * tree; 

fun trav(Br(t1,a,t2)) = trav(t1)@(a::trav(t2))
    |trav empty = [];

fun listToTree ([] : int list) : tree = Empty
  | listToTree (x::l) = let val (l1, l2) = split l
    	in Br(listToTree l1, x, listToTree l2)
  end;


(*begin*)
fun treecompare (Empty,Empty) = EQUAL
  | treecompare (Empty,Br(l,x,r))=LESS
  | treecompare (Br(l,x,r),Empty)=GREATER
  | treecompare (Br(l1,x,r1),Br(l2,y,r2))=Int.compare(x,y);

fun SwapDown(Empty : tree) : tree = Empty
  | SwapDown(T as Br(a, x, b)) =
    let
        val (st, bt) = case treecompare(a, b) of 
          GREATER => (b, a)
          | _     => (a, b)
    in
        case st of
          Empty => Br(st, x, bt)
        | Br(c, y, d) =>
          case treecompare(T, st) of
            GREATER => Br(SwapDown(Br(c, x, d)), y, bt)
            | _     => T
    end

fun heapify(Empty : tree) = Empty
  | heapify(Br(t1, n, t2)) = SwapDown(Br(heapify t1, n, heapify t2))
(*end*)


val L = getIntList(7);

printIntList (trav((listToTree L)));

printIntList (trav(heapify(listToTree L)));
