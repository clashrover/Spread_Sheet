type value = E | F of float ;;
type column = value list;;
type sheet = column list;;
type range = int*int*int*int;;
type index = int*int;;
exception InvalidInput1;;
exception InvalidInput2;;
exception InvalidInput3;;
exception InvalidInput4;;
exception InvalidInput5;;

let (temp:sheet) = [[F(1.0);F(1.1);F(1.2)];[F(1.0);F(1.1);F(1.2)];[E;E;E]];;

(* temp;; *)

let rec colcount (col:column) (a:int) (c:int) (ans:int) (itr:int) (i:index): int =
  if itr>=a then
    if itr<c then
      match col with
      | []    -> ans
      | x::xs -> match x with
                | E    -> colcount xs a c ans (itr+1) i
                | F(t) -> colcount xs a c (ans+1) (itr+1) i
    else
      ans
  else
    match col with 
    | []    -> raise InvalidInput5
    | x::xs -> colcount xs a c ans (itr+1) i ;;

let rec newcol (x:colum) ()

let rec fill_ans (s:sheet) (i:index) (ans:int): sheet = 
  let (a,b) = i in
    if b = 0 then
      match s with
      | [] -> raise InvalidInput1
      | x::xs -> (newcol x a ans) :: xs
    else
      match s with
      | [] -> raise InvalidInput1
      | x::xs -> x :: fill_ans xs (a,(b-1)) ans;;

let rec fc (s:sheet) (t:sheet) (r:range) (m:int) (n:int) (ans:int) (i:index) : sheet = 
  if t = [] then raise InvalidInput2 
  else
    let (a,b,c,d) = r in
      if n<b then
        match t with
        | []    -> raise InvalidInput3
        | x::xs -> fc s xs r m (n+1) ans i
      else
        if n = d then fill_ans s i ans
        else
          match t with
          | [] -> raise InvalidInput1
          | x::xs -> let z = colcount x a c 0 0 i in
                     fc s xs r m (n+1) (ans+z) i;;
  

let rec full_count (s:sheet) (r:range) (i:index): sheet = 
  if s = [] then raise InvalidInput1
else
    fc s s r 0 0 0 i;;

full_count temp (0,0,1,1) (0,2);;
