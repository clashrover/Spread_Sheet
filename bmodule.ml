open Array;;
type value = E | F of float | I of int;;
type column = value array;;
type sheet = column array;;
type range = int*int*int*int;;
type index = int*int;;
type ref = sheet array;;
exception Out_of_bound_input;;
exception BadStack;;
exception IncompatibleRanges;;
exception EmptyCell;;
(* testcases are based on -----
 let temp:sheet = [|[|F(1.1);F(1.1);F(1.3);F(1.3);F(1.1)|];[|F(1.1);F(1.1);F(1.1);F(1.3);F(1.1)|];[|F(1.1);F(1.1);F(1.1);F(1.3);F(1.1)|];[|E;E;E;E;E|];[|E;E;E;E;E|]|];; *)




(* --------------------------------------------------------------------- *)

let rec printrow (s:sheet) (row:int) (itr:int): unit =
	let c = length s in 
	if itr = c then print_string "\n"
	else
		if itr = c-1 then
			match s.(itr).(row) with
			| E     -> print_string "E"; printrow s row (itr+1)
			| F(t)  -> print_float t; printrow s row (itr+1)
			| I(x)  -> print_int x ; printrow s row (itr+1)

		else
			match s.(itr).(row) with
			| E     -> print_string "E || "; printrow s row (itr+1)
			| F(t)  -> print_float t; print_string " || "; printrow s row (itr+1)
			| I(x)  -> print_int x ; print_string " || "; printrow s row (itr+1);;


let rec printsheet (s:sheet) (itr:int) : sheet= 
	if itr = length s.(0) then s
	else
		let al = printrow s itr 0 in
			printsheet s (itr+1);;

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec fill_col (col:column) (ans:sheet) (c:int) (itr:int) =
  if itr < (length col) then
    let al1 = ans.(c).(itr) <- col.(itr) in
      fill_col col ans c (itr+1)
  else
    ();;

let rec fill_sheet (s:sheet) (ans:sheet) (itr:int) : sheet =
  if itr < length s then
    let al1 = fill_col s.(itr) ans itr 0 in
      fill_sheet s ans (itr+1)
  else
    ans;;

let rec makesheet (s:sheet) (l:column) (itr:int) (size:int) = 
	if itr<size then
	let al1 = s.(itr) <- copy l in
		makesheet s l (itr+1) size
	else
		s;;

let rec makeEmptySheet (x:int) (y:int) : sheet = 
	let l:column = make x E in
	let s_exp:sheet = make y [||] in
	makesheet s_exp l 0 y;;

let rec expandSheet (s:sheet) (x:int) (y:int) :sheet = 
	let col = length s in let row = length s.(0) in
		if col < y then
			if row < x then
				let a = makeEmptySheet x y in
					fill_sheet s a 0
			else
				let a = makeEmptySheet row y in
					fill_sheet s a 0
		else
			if row < x then
				let a = makeEmptySheet x col in
					fill_sheet s a 0
			else
				s;;
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let m1 = int_of_string Sys.argv.(3);; 
let n1 = int_of_string Sys.argv.(4);;

let temp = makeEmptySheet m1 n1;;
let refarr = [|temp|];;

let rec print_list (itr:int) (s: string list) (row:int) =
	match s with  
	| [] -> ()
	| e::l ->   
							if e = "" then let al1 = temp.(itr).(row) <- E in print_list (itr+1) l row
							else
							let al1 = temp.(itr).(row) <- F(float_of_string e) in print_list (itr+1) l row;;

let _ =
	try
		let in_stream = open_in Sys.argv.(2) in
				for i=0 to (m1-1) do
					let line = input_line in_stream in
					let split = Str.split (Str.regexp ",") in
					let values = split line in
						print_list 0 values i;
						(* print_string "\n" *)
				done;
				close_in in_stream; 
	with e ->
		Printf.printf "File not found!";
		raise e;;

(* printsheet temp 0;; *)




let rec my_col (col:column) (a:int) (c:int) (itr:int) (ans:int) = 
		if itr = c+1 then ans
		else
			if itr <a then 
				my_col col a c (itr+1) ans
			else
				if col.(itr) = E then
					my_col col a c (itr+1) ans
				else
					my_col col a c (itr+1) (ans+1);; 


let rec my_full_count (s:sheet) (r:range) (i:index) (n:int) (ans:int): sheet = 
	let (a,b,c,d) = r in
		if n<b then
			my_full_count s r i (n+1) ans
		else
			if n<=d then
				let c = my_col s.(n) a c 0 0 in
					my_full_count s r i (n+1) (ans+c) 
			else if n=d+1 then
				let (x,y) = i in
					(* print_int ans; print_string "\n" ; *)
					let al = s.(y).(x) <- I(ans) in
						let temp_sheet = printsheet s 0 in s
			else
				raise BadStack;;


let rec full_count (s:sheet) (r:range) (i:index): sheet = 
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0 then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+1) (n+1) in
			my_full_count l r i 0 0;;

(* full_count temp (0,0,1,1) (2,2);; *)


(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec rowc (s:sheet) (b:int) (d:int) (row:int) (itr:int) (ans:int) (x:int) (y:int) : int =
	if itr < b then
		rowc s b d row (itr+1) ans x y
	else
		if itr<=d then
			match s.(itr).(row) with
			| E     -> rowc s b d row (itr+1) ans x y
			| F(t)  -> rowc s b d row (itr+1) (ans+1) x y
			| I(p)  -> rowc s b d row (itr+1) (ans+1) x y
		else
			let al = s.(y).(x) <- I(ans) in ans;;

let rec my_row_count (s:sheet) (r:range) (i:index) (itr:int) :sheet = 
	let (a,b,c,d) = r in
		if itr < a then my_row_count s r i (itr+1)
		else
			if itr<=c then 
				let (m,n) = i in
				let z = rowc s b d itr 0 0 (m+(itr-a)) n  in
					my_row_count s r i (itr+1)
			else
				let temp_sheet = printsheet s 0 in
				s;;




let rec row_count (s:sheet) (r:range) (i:index): sheet = 
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0 then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+(c-a)+1) (n+1) in
			my_row_count l r i 0;;

(* [3,4] := ROWCOUNT ([3,2]:[4,3]); *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec colc (s:sheet) (a:int) (c:int) (col:int) (itr:int) (ans:int) (x:int) (y:int) : int =
	if itr < a then
		colc s a c col (itr+1) ans x y
	else
		if itr<=c then
			match s.(col).(itr) with
			| E     -> colc s a c col (itr+1) ans x y
			| F(t)  -> colc s a c col (itr+1) (ans+1) x y
			| I(p)  -> colc s a c col (itr+1) (ans+1) x y
		else
			let al = s.(y).(x) <- I(ans) in ans;;

let rec my_col_count (s:sheet) (r:range) (i:index) (itr:int) :sheet =
	let (a,b,c,d) = r in
		if itr < b then my_col_count s r i (itr+1)
		else
			if itr<=d then 
				let (m,n) = i in
				let z = colc s a c itr 0 0 m (n+itr-b)  in
					my_col_count s r i (itr+1)
			else
				let temp_sheet = printsheet s 0 in s;;


let rec col_count (s:sheet) (r:range) (i:index): sheet =
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0 then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+1) (n+(d-b)+1) in
			my_col_count l r i 0;;
(* 
[0,3] := COUNT ([0,0]:[2,2]);
[3,4] := ROWCOUNT ([3,2]:[4,3]);
[3,0] := COLCOUNT ([0,0]:[2,4]); *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec my_colsum (col:column) (a:int) (c:int) (itr:int) (ans:float):float = 
		if itr = c+1 then ans
		else
			if itr <a then 
				my_colsum col a c (itr+1) ans
			else
				match col.(itr) with
				| E     -> my_colsum col a c (itr+1) ans
				| F(p1) -> let z = ans +. p1 in my_colsum col a c (itr+1) z 
				| I(p2) -> let z = ans +. float_of_int p2 in my_colsum col a c (itr+1) z ;;


let rec my_full_sum (s:sheet) (r:range) (i:index) (n:int) (ans:float): sheet = 
	let (a,b,c,d) = r in
		if n<b then
			my_full_sum s r i (n+1) ans
		else
			if n<=d then
				let c = my_colsum s.(n) a c 0 0. in
					my_full_sum s r i (n+1) (ans+.c) 
			else if n=d+1 then
				let (x,y) = i in
					(* print_int ans; print_string "\n" ; *)
					let al = s.(y).(x) <- F(ans) in
						let temp_sheet = printsheet s 0 in s
			else
				raise BadStack;;


let rec full_sum (s:sheet) (r:range) (i:index): sheet = 
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0 then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+1) (n+1) in
			my_full_sum l r i 0 0.;;

(* [0,3] := COUNT ([0,0]:[2,2]);
[3,4] := ROWCOUNT ([3,2]:[4,3]);
[3,0] := COLCOUNT ([0,0]:[2,4]);
[2,3] := SUM ([0,0]:[2,2]); *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec rows (s:sheet) (b:int) (d:int) (row:int) (itr:int) (ans:float) (x:int) (y:int) =
	if itr < b then
		rows s b d row (itr+1) ans x y
	else
		if itr<=d then
			match s.(itr).(row) with
			| E     -> rows s b d row (itr+1) ans x y
			| F(p1)  -> rows s b d row (itr+1) (ans+. p1) x y
			| I(p2)  -> rows s b d row (itr+1) (ans+. (float_of_int p2)) x y
		else
			let al = s.(y).(x) <- F(ans) in ans;;

let rec my_row_sum (s:sheet) (r:range) (i:index) (itr:int) :sheet = 
	let (a,b,c,d) = r in
		if itr < a then my_row_sum s r i (itr+1)
		else
			if itr<=c then 
				let (m,n) = i in
				let z = rows s b d itr 0 0.0 (m+(itr-a)) n  in
					my_row_sum s r i (itr+1)
			else
				let temp_sheet = printsheet s 0 in
				s;;




let rec row_sum (s:sheet) (r:range) (i:index): sheet = 
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0 then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+(c-a)+1) (n+1) in
			my_row_sum l r i 0;;

(* There was a bug in lexer. I had written rowcount in place of rowsum.
Similarly others were displace because of it. *)

(* [0,4] := ROWSUM ([0,0]:[1,3]); *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec cols (s:sheet) (a:int) (c:int) (col:int) (itr:int) (ans:float) (x:int) (y:int) : float =
	if itr < a then
		cols s a c col (itr+1) ans x y
	else
		if itr<=c then
			match s.(col).(itr) with
			| E     -> cols s a c col (itr+1) ans x y
			| F(t)  -> cols s a c col (itr+1) (ans+. t) x y
			| I(p)  -> cols s a c col (itr+1) (ans+. (float_of_int p)) x y
		else
			let al = s.(y).(x) <- F(ans) in ans;;

let rec my_col_sum (s:sheet) (r:range) (i:index) (itr:int) :sheet =
	let (a,b,c,d) = r in
		if itr < b then my_col_sum s r i (itr+1)
		else
			if itr<=d then 
				let (m,n) = i in
				let z = cols s a c itr 0 0. m (n+itr-b)  in
					my_col_sum s r i (itr+1)
			else
				let temp_sheet = printsheet s 0 in s;;


let rec col_sum (s:sheet) (r:range) (i:index): sheet =
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0 then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+1) (n+(d-b)+1) in
			my_col_sum l r i 0;;

(* [0,3] := COUNT ([0,0]:[2,2]);
[3,4] := ROWCOUNT ([3,2]:[4,3]);
[3,0] := COLCOUNT ([0,0]:[2,4]);
[2,3] := SUM ([0,0]:[2,2]);
[3,0] := COLSUM ([0,0]:[2,3]); *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

type fraction= float*int;;
type mutarr = fraction array;;


let rec my_colavg (col:column) (a:int) (c:int) (itr:int) (ans:float) (mut:mutarr):float = 
		if itr = c+1 then ans
		else
			if itr <a then 
				my_colavg col a c (itr+1) ans mut
			else
				match col.(itr) with
				| E     -> my_colavg col a c (itr+1) ans mut
				| F(p1) -> let z = ans +. p1 in
								let (a1,a2) = mut.(0) in
						   			let al1 = mut.(0) <- (z,(a2+1)) in 
						   				my_colavg col a c (itr+1) z mut
				| I(p2) -> let z = ans +. float_of_int p2 in 
								let (a1,a2) = mut.(0) in
						   			let al1 = mut.(0) <- (z,(a2+1)) in 
						   				my_colavg col a c (itr+1) z mut;;


let rec my_full_avg (s:sheet) (r:range) (i:index) (n:int) (ans:float) (mut:mutarr): sheet = 
	let (a,b,c,d) = r in
		if n<b then
			my_full_avg s r i (n+1) ans mut
		else
			if n<=d then
				let c = my_colavg s.(n) a c 0 0. mut in
					let (a1,a2) = mut.(0) in
						let al1 = mut.(0) <- (ans+.c,a2) in 
							my_full_avg s r i (n+1) (ans+.c) mut
			else if n=d+1 then
				let (x,y) = i in
					(* print_int ans; print_string "\n" ; *)
					let (a1,a2) = mut.(0) in
							let a3 = a1/. float_of_int a2 in
								let al = s.(y).(x) <- F(a3) in
									let temp_sheet = printsheet s 0 in s
			else
				raise BadStack;;


let rec full_avg (s:sheet) (r:range) (i:index): sheet = 
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0 then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+1) (n+1) in
			let mut:mutarr = [|(0.0,0)|] in
			my_full_avg l r i 0 0. mut;;

(* [0,3] := COUNT ([0,0]:[2,2]);
[3,4] := ROWCOUNT ([3,2]:[4,3]);
[3,0] := COLCOUNT ([0,0]:[2,4]);
[2,3] := SUM ([0,0]:[2,2]);
[3,0] := COLSUM ([0,0]:[2,3]);
[0,4] := AVG ([0,0]:[2,3]); *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec rowa (s:sheet) (b:int) (d:int) (row:int) (itr:int) (ans:float) (x:int) (y:int) (ent:int) =
	if itr < b then
		rowa s b d row (itr+1) ans x y ent
	else
		if itr<=d then
			match s.(itr).(row) with
			| E     -> rowa s b d row (itr+1) ans x y ent
			| F(p1)  -> rowa s b d row (itr+1) (ans+. p1) x y (ent+1)
			| I(p2)  -> rowa s b d row (itr+1) (ans+. (float_of_int p2)) x y (ent+1)
		else
			let avg_ans = ans /. float_of_int ent in
				let al = s.(y).(x) <- F(avg_ans) in avg_ans;;

let rec my_row_avg (s:sheet) (r:range) (i:index) (itr:int) :sheet = 
	let (a,b,c,d) = r in
		if itr < a then my_row_avg s r i (itr+1)
		else
			if itr<=c then 
				let (m,n) = i in
				let z = rowa s b d itr 0 0.0 (m+(itr-a)) n 0  in
					my_row_avg s r i (itr+1)
			else
				let temp_sheet = printsheet s 0 in
				s;;




let rec row_avg (s:sheet) (r:range) (i:index): sheet = 
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0 then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+(c-a)+1) (n+1) in
			my_row_avg l r i 0;;

(* [0,3] := COUNT ([0,0]:[2,2]);
[3,4] := ROWCOUNT ([3,2]:[4,3]);
[3,0] := COLCOUNT ([0,0]:[2,4]);
[2,3] := SUM ([0,0]:[2,2]);
[3,0] := COLSUM ([0,0]:[2,3]);
[0,4] := AVG ([0,0]:[2,3]);
[0,4] := ROWAVG ([0,0]:[4,3]); *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)


let rec cola (s:sheet) (a:int) (c:int) (col:int) (itr:int) (ans:float) (x:int) (y:int) (ent:int) : float =
	if itr < a then
		cola s a c col (itr+1) ans x y ent
	else
		if itr<=c then
			match s.(col).(itr) with
			| E     -> cola s a c col (itr+1) ans x y ent
			| F(t)  -> cola s a c col (itr+1) (ans+. t) x y (ent+1)
			| I(p)  -> cola s a c col (itr+1) (ans+. (float_of_int p)) x y (ent+1)
		else
			let a1 = ans /. float_of_int ent in
				let al = s.(y).(x) <- F(a1) in ans;;

let rec my_col_avg (s:sheet) (r:range) (i:index) (itr:int) :sheet =
	let (a,b,c,d) = r in
		if itr < b then my_col_avg s r i (itr+1)
		else
			if itr<=d then 
				let (m,n) = i in
				let z = cola s a c itr 0 0. m (n+itr-b) 0  in
					my_col_avg s r i (itr+1)
			else
				let temp_sheet = printsheet s 0 in s;;


let rec col_avg (s:sheet) (r:range) (i:index): sheet =
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0 then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+1) (n+(d-b)+1) in
			my_col_avg l r i 0;;

(* [0,3] := COUNT ([0,0]:[2,2]);
[3,4] := ROWCOUNT ([3,2]:[4,3]);
[3,0] := COLCOUNT ([0,0]:[2,4]);
[2,3] := SUM ([0,0]:[2,2]);
[3,0] := COLSUM ([0,0]:[2,3]);
[0,4] := AVG ([0,0]:[2,3]);
[0,4] := ROWAVG ([0,0]:[4,3]);
[3,0] := COLAVG ([0,0]:[2,4]); *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec my_colmin (col:column) (a:int) (c:int) (itr:int) (ans:float):float = 
		if itr = c+1 then ans
		else
			if itr <a then 
				my_colmin col a c (itr+1) ans
			else
				match col.(itr) with
				| E     -> my_colmin col a c (itr+1) ans
				| F(p1) -> if p1<ans then my_colmin col a c (itr+1) p1 
							else
								 my_colmin col a c (itr+1) ans
				| I(p2) -> 	let p3 = float_of_int p2 in
							if ans < p3 then my_colmin col a c (itr+1) ans
							else
								 my_colmin col a c (itr+1) p3


let rec my_full_min (s:sheet) (r:range) (i:index) (n:int) (ans:float): sheet = 
	let (a,b,c,d) = r in
		if n<b then
			my_full_min s r i (n+1) ans
		else
			if n<=d then
				let c = my_colmin s.(n) a c 0 32000. in
					if c<ans then
						my_full_min s r i (n+1) c
					else
						 my_full_min s r i (n+1) ans
			else if n=d+1 then
				let (x,y) = i in
					(* print_int ans; print_string "\n" ; *)
					let al = s.(y).(x) <- F(ans) in
						let temp_sheet = printsheet s 0 in s
			else
				raise BadStack;;


let rec full_min (s:sheet) (r:range) (i:index): sheet = 
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0 then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+1) (n+1) in
			my_full_min l r i 0 32000.0;;
(* 
[0,3] := COUNT ([0,0]:[2,2]);
[3,4] := ROWCOUNT ([3,2]:[4,3]);
[3,0] := COLCOUNT ([0,0]:[2,4]);
[2,3] := SUM ([0,0]:[2,2]);
[3,0] := COLSUM ([0,0]:[2,3]);
[0,4] := AVG ([0,0]:[2,3]);
[0,4] := ROWAVG ([0,0]:[4,3]);
[3,0] := COLAVG ([0,0]:[2,4]);
[3,2] := MIN ([0,3]:[0,4]); *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec rowm (s:sheet) (b:int) (d:int) (row:int) (itr:int) (ans:float) (x:int) (y:int) =
	if itr < b then
		rowm s b d row (itr+1) ans x y
	else
		if itr<=d then
			match s.(itr).(row) with
			| E     -> rowm s b d row (itr+1) ans x y
			| F(p1)  -> if p1 < ans then rowm s b d row (itr+1) p1 x y
						else rowm s b d row (itr+1) ans x y
			| I(p2)  -> let p3 = float_of_int p2 in
						if p3 < ans then rowm s b d row (itr+1) p3 x y
						else rowm s b d row (itr+1) ans x y
		else
			let al = s.(y).(x) <- F(ans) in ans;;

let rec my_row_min (s:sheet) (r:range) (i:index) (itr:int) :sheet = 
	let (a,b,c,d) = r in
		if itr < a then my_row_min s r i (itr+1)
		else
			if itr<=c then 
				let (m,n) = i in
				let z = rowm s b d itr 0 32000.0 (m+(itr-a)) n  in
					my_row_min s r i (itr+1)
			else
				let temp_sheet = printsheet s 0 in
				s;;




let rec row_min (s:sheet) (r:range) (i:index): sheet = 
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0  then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+(c-a)+1) (n+1) in
			my_row_min l r i 0;;

(* [0,3] := COUNT ([0,0]:[2,2]);
[3,4] := ROWCOUNT ([3,2]:[4,3]);
[3,0] := COLCOUNT ([0,0]:[2,4]);
[2,3] := SUM ([0,0]:[2,2]);
[3,0] := COLSUM ([0,0]:[2,3]);
[0,4] := AVG ([0,0]:[2,3]);
[0,4] := ROWAVG ([0,0]:[4,3]);
[3,0] := COLAVG ([0,0]:[2,4]);
[3,2] := MIN ([0,3]:[0,4]);
[2,4] := ROWMIN ([2,2]:[3,3]); *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec colm (s:sheet) (a:int) (c:int) (col:int) (itr:int) (ans:float) (x:int) (y:int) : float =
	if itr < a then
		colm s a c col (itr+1) ans x y
	else
		if itr<=c then
			match s.(col).(itr) with
			| E     -> colm s a c col (itr+1) ans x y
			| F(p1)  -> if p1<ans then colm s a c col (itr+1) p1 x y
					   else colm s a c col (itr+1) ans x y
			| I(p2)  -> let p3 = float_of_int p2 in
						if p3<ans then colm s a c col (itr+1) p3 x y
						else colm s a c col (itr+1) ans x y
		else
			let al = s.(y).(x) <- F(ans) in ans;;

let rec my_col_min (s:sheet) (r:range) (i:index) (itr:int) :sheet =
	let (a,b,c,d) = r in
		if itr < b then my_col_min s r i (itr+1)
		else
			if itr<=d then 
				let (m,n) = i in
				let z = colm s a c itr 0 32000. m (n+itr-b)  in
					my_col_min s r i (itr+1)
			else
				let temp_sheet = printsheet s 0 in s;;


let rec col_min (s:sheet) (r:range) (i:index): sheet =
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0 then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+1) (n+(d-b)+1) in
			my_col_min l r i 0;;

(* [0,3] := COUNT ([0,0]:[2,2]);
[3,4] := ROWCOUNT ([3,2]:[4,3]);
[3,0] := COLCOUNT ([0,0]:[2,4]);
[2,3] := SUM ([0,0]:[2,2]);
[3,0] := COLSUM ([0,0]:[2,3]);
[0,4] := AVG ([0,0]:[2,3]);
[0,4] := ROWAVG ([0,0]:[4,3]);
[3,0] := COLAVG ([0,0]:[2,4]);
[3,2] := MIN ([0,3]:[0,4]);
[2,4] := ROWMIN ([2,2]:[3,3]);
[4,0] := COLMIN ([0,0]:[3,3]); *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec my_colmax (col:column) (a:int) (c:int) (itr:int) (ans:float):float = 
		if itr = c+1 then ans
		else
			if itr <a then 
				my_colmax col a c (itr+1) ans
			else
				match col.(itr) with
				| E     -> my_colmax col a c (itr+1) ans
				| F(p1) -> if p1>ans then my_colmax col a c (itr+1) p1 
							else
								 my_colmax col a c (itr+1) ans
				| I(p2) -> 	let p3 = float_of_int p2 in
							if ans > p3 then my_colmax col a c (itr+1) ans
							else
								 my_colmax col a c (itr+1) p3

let minf = -32000.0;;

let rec my_full_max (s:sheet) (r:range) (i:index) (n:int) (ans:float): sheet = 
	let (a,b,c,d) = r in
		if n<b then
			my_full_max s r i (n+1) ans
		else
			if n<=d then
				let c = my_colmax s.(n) a c 0 minf in
					if c>ans then
						my_full_max s r i (n+1) c
					else
						 my_full_max s r i (n+1) ans
			else if n=d+1 then
				let (x,y) = i in
					(* print_int ans; print_string "\n" ; *)
					let al = s.(y).(x) <- F(ans) in
						let temp_sheet = printsheet s 0 in s
			else
				raise BadStack;;


let rec full_max (s:sheet) (r:range) (i:index): sheet = 
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0 then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+1) (n+1) in
			my_full_max l r i 0 minf;;

(* [0,3] := COUNT ([0,0]:[2,2]);
[3,4] := ROWCOUNT ([3,2]:[4,3]);
[3,0] := COLCOUNT ([0,0]:[2,4]);
[2,3] := SUM ([0,0]:[2,2]);
[3,0] := COLSUM ([0,0]:[2,3]);
[0,4] := AVG ([0,0]:[2,3]);
[0,4] := ROWAVG ([0,0]:[4,3]);
[3,0] := COLAVG ([0,0]:[2,4]);
[3,2] := MAX ([0,3]:[0,4]); *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec rowm (s:sheet) (b:int) (d:int) (row:int) (itr:int) (ans:float) (x:int) (y:int) =
	if itr < b then
		rowm s b d row (itr+1) ans x y
	else
		if itr<=d then
			match s.(itr).(row) with
			| E      -> rowm s b d row (itr+1) ans x y
			| F(p1)  -> if p1 > ans then rowm s b d row (itr+1) p1 x y
						else rowm s b d row (itr+1) ans x y
			| I(p2)  -> let p3 = float_of_int p2 in
						if p3 > ans then rowm s b d row (itr+1) p3 x y
						else rowm s b d row (itr+1) ans x y
		else
			let al = s.(y).(x) <- F(ans) in ans;;

let rec my_row_max (s:sheet) (r:range) (i:index) (itr:int) :sheet = 
	let (a,b,c,d) = r in
		if itr < a then my_row_max s r i (itr+1)
		else
			if itr<=c then 
				let (m,n) = i in
				let z = rowm s b d itr 0 minf (m+(itr-a)) n  in
					my_row_max s r i (itr+1)
			else
				let temp_sheet = printsheet s 0 in
				s;;




let rec row_max (s:sheet) (r:range) (i:index): sheet = 
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0 then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+(c-a)+1) (n+1) in
			my_row_max l r i 0;;

(* [0,3] := COUNT ([0,0]:[2,2]);
[3,4] := ROWCOUNT ([3,2]:[4,3]);
[3,0] := COLCOUNT ([0,0]:[2,4]);
[2,3] := SUM ([0,0]:[2,2]);
[3,0] := COLSUM ([0,0]:[2,3]);
[0,4] := AVG ([0,0]:[2,3]);
[0,4] := ROWAVG ([0,0]:[4,3]);
[3,0] := COLAVG ([0,0]:[2,4]);
[3,2] := MAX ([0,3]:[0,4]);
[2,4] := ROWMAX ([2,2]:[3,3]); *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec colm (s:sheet) (a:int) (c:int) (col:int) (itr:int) (ans:float) (x:int) (y:int) : float =
	if itr < a then
		colm s a c col (itr+1) ans x y
	else
		if itr<=c then
			match s.(col).(itr) with
			| E     -> colm s a c col (itr+1) ans x y
			| F(p1)  -> if p1>ans then colm s a c col (itr+1) p1 x y
					   else colm s a c col (itr+1) ans x y
			| I(p2)  -> let p3 = float_of_int p2 in
						if p3>ans then colm s a c col (itr+1) p3 x y
						else colm s a c col (itr+1) ans x y
		else
			let al = s.(y).(x) <- F(ans) in ans;;

let rec my_col_max (s:sheet) (r:range) (i:index) (itr:int) :sheet =
	let (a,b,c,d) = r in
		if itr < b then my_col_max s r i (itr+1)
		else
			if itr<=d then 
				let (m,n) = i in
				let z = colm s a c itr 0 minf m (n+itr-b)  in
					my_col_max s r i (itr+1)
			else
				let temp_sheet = printsheet s 0 in s;;


let rec col_max (s:sheet) (r:range) (i:index): sheet =
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0 then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+1) (n+(d-b)+1) in
			my_col_max l r i 0;;

(* [0,3] := COUNT ([0,0]:[2,2]);
[3,4] := ROWCOUNT ([3,2]:[4,3]);
[3,0] := COLCOUNT ([0,0]:[2,4]);
[2,3] := SUM ([0,0]:[2,2]);
[3,0] := COLSUM ([0,0]:[2,3]);
[0,4] := AVG ([0,0]:[2,3]);
[0,4] := ROWAVG ([0,0]:[4,3]);
[3,0] := COLAVG ([0,0]:[2,4]);
[3,2] := MAX ([0,3]:[0,4]);
[2,4] := ROWMAX ([2,2]:[3,3]);
[4,0] := COLMAX ([0,0]:[3,3]); *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)


let rec coladd (s:sheet) (a:int) (c:int) (col:int) (itr:int) (f:float) (r:range) (i:index): float =
	let (a1,a2,a3,a4) = r in
	let (m,n) = i in
	if itr < a then
		coladd s a c col (itr+1) f r i
	else
		if itr<=c then
			match s.(col).(itr) with
			| E     -> let al1 = s.(n+col-a2).(m+itr-a1) <- E in 
						coladd s a c col (itr+1) f r i
			| F(t)  -> 	let al1 = s.(n+col-a2).(m+itr-a1) <- F(t+.f) in 
						coladd s a c col (itr+1) f r i
			| I(p)  -> let al1 = s.(n+col-a2).(m+itr-a1) <- F(f +. float_of_int p) in 
						coladd s a c col (itr+1) f r i
		else
			f;;

let rec my_add_const (s:sheet) (r:range) (i:index) (itr:int) (f:float):sheet =
	let (a,b,c,d) = r in
		if itr < b then my_add_const s r i (itr+1) f
		else
			if itr<=d then 
				let (m,n) = i in
				let z = coladd s a c itr 0 f r i in
					my_add_const s r i (itr+1) f
			else
				let temp_sheet = printsheet s 0 in s;;


let rec add_const (s:sheet) (r:range)  (f:float) (i:index) : sheet =
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0  then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+(c-a)+1) (n+(d-b)+1) in
			my_add_const l r i 0 f;;

(* [0,3] := COUNT ([0,0]:[2,2]);
[0,0] := ADD -30 ([0,0]:[2,4]);
[0,0] := ADD ([0,0]:[2,4]) 30; *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec colRangeAdd (s:sheet) (a:int) (c:int) (col:int) (itr:int) (ro:range) (i:index) (r1:range) (r2:range) =
	let (a1,a2,a3,a4) = ro in
	let (b1,b2,b3,b4) = r1 in
	let (c1,c2,c3,c4) = r2 in
	let (m,n) = i in
	if itr < a then
		colRangeAdd s a c col (itr+1) ro i r1 r2
	else
		if itr<=c then
			match s.(b2+col).(b1+itr) with
			| E     ->  raise EmptyCell
			| F(t)  -> 	match s.(c2+col).(c1+itr) with
						| E 	-> raise EmptyCell

						| F(p1) -> let al1 = s.(n+col).(m+itr) <- F(t+.p1) in
								   colRangeAdd s a c col (itr+1) ro i r1 r2 

						| I(p2) -> let al1 = s.(n+col).(m+itr) <- F(t+. (float_of_int p2)) in
								   colRangeAdd s a c col (itr+1) ro i r1 r2

			| I(p)  ->  match s.(c2+col).(c1+itr) with
						| E 	-> raise EmptyCell

						| F(p1) -> let al1 = s.(n+col).(m+itr) <- F(p1 +. (float_of_int p)) in
								   colRangeAdd s a c col (itr+1) ro i r1 r2 

						| I(p2) -> let al1 = s.(n+col).(m+itr) <- F((float_of_int p)+. (float_of_int p2)) in
								   colRangeAdd s a c col (itr+1) ro i r1 r2
		else
			();;

let rec my_add_range (s:sheet) (r1:range) (r2:range) (i:index) (itr:int) (ro : range) : sheet =
	let (a,b,c,d) = ro in
		if itr < b then my_add_range s r1 r2 i (itr+1) ro
		else
			if itr<=d then 
				let (m,n) = i in
				let z = colRangeAdd s a c itr 0 ro i r1 r2 in
					my_add_range s r1 r2 i (itr+1) ro
			else
				let temp_sheet = printsheet s 0 in s;;

let rec checkCompatible (r1:range) (r2:range) : bool = 
	let (a1,b1,c1,d1) = r1 in
		let (a2,b2,c2,d2) = r2 in
			if (a1-c1) = (a2-c2) && (b1-d1) = (b2-d2) then
				true
			else
				false;;

let rec add_range (s:sheet) (r1:range) (r2:range) (i:index) : sheet = 
	if checkCompatible r1 r2 then
		let (a,b,c,d) = r1 in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0  then
			raise Out_of_bound_input
		else
			let (a,b,c,d) = r2 in let (m,n) = i in let col = length s in let row = length s.(0) in
			if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0  then
				raise Out_of_bound_input
			else
				let l = expandSheet s (m+(c-a)+1) (n+(d-b)+1) in
				my_add_range l r1 r2 i 0 (0,0,(c-a),(d-b))
	else
		raise IncompatibleRanges;;


(* [4,0] := ADD ([0,0]:[0,2]) ([2,0]:[2,2]) ;
[0,4] := ADD ([0,0]:[2,0]) ([0,2]:[2,2]) ; *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec colsubt (s:sheet) (a:int) (c:int) (col:int) (itr:int) (f:float) (r:range) (i:index): float =
	let (a1,a2,a3,a4) = r in
	let (m,n) = i in
	if itr < a then
		colsubt s a c col (itr+1) f r i
	else
		if itr<=c then
			match s.(col).(itr) with
			| E     -> let al1 = s.(n+col-a2).(m+itr-a1) <- E in 
						colsubt s a c col (itr+1) f r i
			| F(t)  -> 	let al1 = s.(n+col-a2).(m+itr-a1) <- F(t-.f) in 
						colsubt s a c col (itr+1) f r i
			| I(p)  -> let al1 = s.(n+col-a2).(m+itr-a1) <- F((float_of_int p)-.f) in 
						colsubt s a c col (itr+1) f r i
		else
			f;;

let rec my_subt_const (s:sheet) (r:range) (i:index) (itr:int) (f:float):sheet =
	let (a,b,c,d) = r in
		if itr < b then my_subt_const s r i (itr+1) f
		else
			if itr<=d then 
				let (m,n) = i in
				let z = colsubt s a c itr 0 f r i in
					my_subt_const s r i (itr+1) f
			else
				let temp_sheet = printsheet s 0 in s;;


let rec subt_const (s:sheet) (r:range)  (f:float) (i:index) : sheet =
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0  then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+(c-a)+1) (n+(d-b)+1) in
			my_subt_const l r i 0 f;;

(* [0,3] := COUNT ([0,0]:[2,2]);
[0,0] := SUBT -30 ([0,0]:[2,4]);
[0,0] := SUBT ([0,0]:[2,4]) 30;  *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec colRangeSubt (s:sheet) (a:int) (c:int) (col:int) (itr:int) (ro:range) (i:index) (r1:range) (r2:range) =
	let (a1,a2,a3,a4) = ro in
	let (b1,b2,b3,b4) = r1 in
	let (c1,c2,c3,c4) = r2 in
	let (m,n) = i in
	if itr < a then
		colRangeSubt s a c col (itr+1) ro i r1 r2
	else
		if itr<=c then
			match s.(b2+col).(b1+itr) with
			| E     ->  raise EmptyCell
			| F(t)  -> 	match s.(c2+col).(c1+itr) with
						| E 	-> raise EmptyCell

						| F(p1) -> let al1 = s.(n+col).(m+itr) <- F(t -.p1) in
								   colRangeSubt s a c col (itr+1) ro i r1 r2 

						| I(p2) -> let al1 = s.(n+col).(m+itr) <- F(t -. (float_of_int p2)) in
								   colRangeSubt s a c col (itr+1) ro i r1 r2

			| I(p)  ->  match s.(c2+col).(c1+itr) with
						| E 	-> raise EmptyCell

						| F(p1) -> let al1 = s.(n+col).(m+itr) <- F((float_of_int p)-.p1) in
								   colRangeSubt s a c col (itr+1) ro i r1 r2 

						| I(p2) -> let al1 = s.(n+col).(m+itr) <- F((float_of_int p)-. (float_of_int p2)) in
								   colRangeSubt s a c col (itr+1) ro i r1 r2
		else
			();;

let rec my_subt_range (s:sheet) (r1:range) (r2:range) (i:index) (itr:int) (ro : range) : sheet =
	let (a,b,c,d) = ro in
		if itr < b then my_subt_range s r1 r2 i (itr+1) ro
		else
			if itr<=d then 
				let (m,n) = i in
				let z = colRangeSubt s a c itr 0 ro i r1 r2 in
					my_subt_range s r1 r2 i (itr+1) ro
			else
				let temp_sheet = printsheet s 0 in s;;


let rec subt_range (s:sheet) (r1:range) (r2:range) (i:index) : sheet = 
	if checkCompatible r1 r2 then
		let (a,b,c,d) = r1 in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0  then
			raise Out_of_bound_input
		else
			let (a,b,c,d) = r2 in let (m,n) = i in let col = length s in let row = length s.(0) in
			if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0  then
				raise Out_of_bound_input
			else
				let l = expandSheet s (m+(c-a)+1) (n+(d-b)+1) in
				my_subt_range l r1 r2 i 0 (0,0,(c-a),(d-b))
	else
		raise IncompatibleRanges;;


(* [4,0] := SUBT ([0,0]:[0,2]) ([2,0]:[2,2]) ;
[0,4] := SUBT ([0,0]:[2,0]) ([0,2]:[2,2]) ; *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec colmult (s:sheet) (a:int) (c:int) (col:int) (itr:int) (f:float) (r:range) (i:index): float =
	let (a1,a2,a3,a4) = r in
	let (m,n) = i in
	if itr < a then
		colmult s a c col (itr+1) f r i
	else
		if itr<=c then
			match s.(col).(itr) with
			| E     -> let al1 = s.(n+col-a2).(m+itr-a1) <- E in 
						colmult s a c col (itr+1) f r i
			| F(t)  -> 	let al1 = s.(n+col-a2).(m+itr-a1) <- F(t *. f) in 
						colmult s a c col (itr+1) f r i
			| I(p)  -> let al1 = s.(n+col-a2).(m+itr-a1) <- F(f *. float_of_int p) in 
						colmult s a c col (itr+1) f r i
		else
			f;;

let rec my_mult_const (s:sheet) (r:range) (i:index) (itr:int) (f:float):sheet =
	let (a,b,c,d) = r in
		if itr < b then my_mult_const s r i (itr+1) f
		else
			if itr<=d then 
				let (m,n) = i in
				let z = colmult s a c itr 0 f r i in
					my_mult_const s r i (itr+1) f
			else
				let temp_sheet = printsheet s 0 in s;;


let rec mult_const (s:sheet) (r:range)  (f:float) (i:index) : sheet =
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0  then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+(c-a)+1) (n+(d-b)+1) in
			my_mult_const l r i 0 f;;

(* [0,3] := COUNT ([0,0]:[2,2]);
[0,0] := MULT -3.1 ([0,0]:[2,4]);
[0,0] := MULT ([0,0]:[2,4]) 3.1; *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec colRangemult (s:sheet) (a:int) (c:int) (col:int) (itr:int) (ro:range) (i:index) (r1:range) (r2:range) =
	let (a1,a2,a3,a4) = ro in
	let (b1,b2,b3,b4) = r1 in
	let (c1,c2,c3,c4) = r2 in
	let (m,n) = i in
	if itr < a then
		colRangemult s a c col (itr+1) ro i r1 r2
	else
		if itr<=c then
			match s.(b2+col).(b1+itr) with
			| E     ->  raise EmptyCell
			| F(t)  -> 	match s.(c2+col).(c1+itr) with
						| E 	-> raise EmptyCell

						| F(p1) -> let al1 = s.(n+col).(m+itr) <- F(t *. p1) in
								   colRangemult s a c col (itr+1) ro i r1 r2 

						| I(p2) -> let al1 = s.(n+col).(m+itr) <- F(t *. (float_of_int p2)) in
								   colRangemult s a c col (itr+1) ro i r1 r2

			| I(p)  ->  match s.(c2+col).(c1+itr) with
						| E 	-> raise EmptyCell

						| F(p1) -> let al1 = s.(n+col).(m+itr) <- F(p1 *. (float_of_int p)) in
								   colRangemult s a c col (itr+1) ro i r1 r2 

						| I(p2) -> let al1 = s.(n+col).(m+itr) <- F((float_of_int p) *. (float_of_int p2)) in
								   colRangemult s a c col (itr+1) ro i r1 r2
		else
			();;

let rec my_mult_range (s:sheet) (r1:range) (r2:range) (i:index) (itr:int) (ro : range) : sheet =
	let (a,b,c,d) = ro in
		if itr < b then my_mult_range s r1 r2 i (itr+1) ro
		else
			if itr<=d then 
				let (m,n) = i in
				let z = colRangemult s a c itr 0 ro i r1 r2 in
					my_mult_range s r1 r2 i (itr+1) ro
			else
				let temp_sheet = printsheet s 0 in s;;



let rec mult_range (s:sheet) (r1:range) (r2:range) (i:index) : sheet = 
	if checkCompatible r1 r2 then
		let (a,b,c,d) = r1 in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0  then
			raise Out_of_bound_input
		else
			let (a,b,c,d) = r2 in let (m,n) = i in let col = length s in let row = length s.(0) in
			if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0  then
				raise Out_of_bound_input
			else
				let l = expandSheet s (m+(c-a)+1) (n+(d-b)+1) in
				my_mult_range l r1 r2 i 0 (0,0,(c-a),(d-b))
	else
		raise IncompatibleRanges;;


(* [4,0] := MULT ([0,0]:[0,2]) ([2,0]:[2,2]) ;
[0,4] := MULT ([0,0]:[2,0]) ([0,2]:[2,2]) ; *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec coldiv (s:sheet) (a:int) (c:int) (col:int) (itr:int) (f:float) (r:range) (i:index): float =
	let (a1,a2,a3,a4) = r in
	let (m,n) = i in
	if itr < a then
		coldiv s a c col (itr+1) f r i
	else
		if itr<=c then
			match s.(col).(itr) with
			| E     -> let al1 = s.(n+col-a2).(m+itr-a1) <- E in 
						coldiv s a c col (itr+1) f r i
			| F(t)  -> 	let al1 = s.(n+col-a2).(m+itr-a1) <- F(t/.f) in 
						coldiv s a c col (itr+1) f r i
			| I(p)  -> let al1 = s.(n+col-a2).(m+itr-a1) <- F((float_of_int p)/.f) in 
						coldiv s a c col (itr+1) f r i
		else
			f;;

let rec my_div_const (s:sheet) (r:range) (i:index) (itr:int) (f:float):sheet =
	let (a,b,c,d) = r in
		if itr < b then my_div_const s r i (itr+1) f
		else
			if itr<=d then 
				let (m,n) = i in
				let z = coldiv s a c itr 0 f r i in
					my_div_const s r i (itr+1) f
			else
				let temp_sheet = printsheet s 0 in s;;


let rec div_const (s:sheet) (r:range)  (f:float) (i:index) : sheet =
	let (a,b,c,d) = r in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0  then
			raise Out_of_bound_input
		else
			let l = expandSheet s (m+(c-a)+1) (n+(d-b)+1) in
			my_div_const l r i 0 f;;

(* [0,3] := COUNT ([0,0]:[2,2]);
[1,0] := DIV -30 ([0,0]:[0,2]);
[2,0] := DIV ([1,0]:[1,2]) 30;  *) 

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

let rec colRangediv (s:sheet) (a:int) (c:int) (col:int) (itr:int) (ro:range) (i:index) (r1:range) (r2:range) =
	let (a1,a2,a3,a4) = ro in
	let (b1,b2,b3,b4) = r1 in
	let (c1,c2,c3,c4) = r2 in
	let (m,n) = i in
	if itr < a then
		colRangediv s a c col (itr+1) ro i r1 r2
	else
		if itr<=c then
			match s.(b2+col).(b1+itr) with
			| E     ->  raise EmptyCell
			| F(t)  -> 	match s.(c2+col).(c1+itr) with
						| E 	-> raise EmptyCell

						| F(p1) -> let al1 = s.(n+col).(m+itr) <- F(t /.p1) in
								   colRangediv s a c col (itr+1) ro i r1 r2 

						| I(p2) -> let al1 = s.(n+col).(m+itr) <- F(t /. (float_of_int p2)) in
								   colRangediv s a c col (itr+1) ro i r1 r2

			| I(p)  ->  match s.(c2+col).(c1+itr) with
						| E 	-> raise EmptyCell

						| F(p1) -> let al1 = s.(n+col).(m+itr) <- F((float_of_int p)/.p1) in
								   colRangediv s a c col (itr+1) ro i r1 r2 

						| I(p2) -> let al1 = s.(n+col).(m+itr) <- F((float_of_int p)/. (float_of_int p2)) in
								   colRangediv s a c col (itr+1) ro i r1 r2
		else
			();;

let rec my_div_range (s:sheet) (r1:range) (r2:range) (i:index) (itr:int) (ro : range) : sheet =
	let (a,b,c,d) = ro in
		if itr < b then my_div_range s r1 r2 i (itr+1) ro
		else
			if itr<=d then 
				let (m,n) = i in
				let z = colRangediv s a c itr 0 ro i r1 r2 in
					my_div_range s r1 r2 i (itr+1) ro
			else
				let temp_sheet = printsheet s 0 in s;;



let rec div_range (s:sheet) (r1:range) (r2:range) (i:index) : sheet = 
	if checkCompatible r1 r2 then
		let (a,b,c,d) = r1 in let (m,n) = i in let col = length s in let row = length s.(0) in
		if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0  then
			raise Out_of_bound_input
		else
			let (a,b,c,d) = r2 in let (m,n) = i in let col = length s in let row = length s.(0) in
			if col = 0 || b>col || d>col || a>row || c>row || a<0 || b<0 || c<0 || d<0  then
				raise Out_of_bound_input
			else
				let l = expandSheet s (m+(c-a)+1) (n+(d-b)+1) in
				my_div_range l r1 r2 i 0 (0,0,(c-a),(d-b))
	else
		raise IncompatibleRanges;;

(* [4,0] := DIV ([0,0]:[0,2]) ([2,0]:[2,2]) ;
[0,4] := DIV ([0,0]:[2,0]) ([0,2]:[2,2]) ; *)

(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)
(* ----------------------------------------------------------------- *)

(* [2,0] := ADD ([0,0]:[1,2]) [3,0];
[2,0] := ADD [3,0] ([0,0]:[1,2]);
[2,0] := SUBT ([0,0]:[1,2]) [3,0];
[2,0] := SUBT [3,0] ([0,0]:[1,2]);
[2,0] := MULT ([0,0]:[1,2]) [3,0];
[2,0] := MULT [3,0] ([0,0]:[1,2]);
[2,0] := DIV ([0,0]:[1,2]) [3,0];
[2,0] := DIV [3,0] ([0,0]:[1,2]); *)

(* [0,3] := COUNT ([0,0]:[2,2]);
[3,4] := ROWCOUNT ([3,2]:[4,3]);
[3,0] := COLCOUNT ([0,0]:[2,4]);
[2,3] := SUM ([0,0]:[2,2]);
[3,0] := COLSUM ([0,0]:[2,3]);
[0,4] := AVG ([0,0]:[2,3]);
[0,4] := ROWAVG ([0,0]:[4,3]);
[3,0] := COLAVG ([0,0]:[2,4]);
[3,2] := MAX ([0,3]:[0,4]);
[2,4] := ROWMAX ([2,2]:[3,3]);
[8,0] := MULT ([0,0]:[1,2]) ([2,0]:[3,2]) ;
[11,0] := COLMAX ([0,0]:[3,3]);
[0,3] := COUNT ([0,0]:[2,2]);
[3,4] := ROWCOUNT ([3,2]:[4,3]);
[3,0] := COLCOUNT ([0,0]:[2,4]);
[2,3] := SUM ([0,0]:[2,2]);
[3,0] := COLSUM ([0,0]:[2,3]);
[0,4] := AVG ([0,0]:[2,3]);
[0,4] := ROWAVG ([0,0]:[4,3]);
[3,0] := COLAVG ([0,0]:[2,4]);
[3,2] := MAX ([0,3]:[0,4]);
[15,7] := ROWMAX ([2,2]:[3,3]); *)