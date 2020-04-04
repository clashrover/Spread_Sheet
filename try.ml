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
(* Sheet dimensions *)


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


	let m1 = int_of_string Sys.argv.(2);; 
	let n1 = int_of_string Sys.argv.(3);;

let read = makeEmptySheet m1 n1;;

let rec print_list (itr:int) (s: string list) (row:int) =
	match s with  
	| [] -> ()
	| e::l ->  print_string e; print_string " ";  
							if e = "" then let al1 = read.(itr).(row) <- E in print_list (itr+1) l row
							else
							let al1 = read.(itr).(row) <- F(float_of_string e) in print_list (itr+1) l row;;

let _ =
	try
		let in_stream = open_in Sys.argv.(1) in
				for i=0 to (m1-1) do
					let line = input_line in_stream in
					let split = Str.split (Str.regexp ",") in
					let values = split line in
						print_list 0 values i;
						print_string "\n"
				done;
				close_in in_stream; 
	with e ->
		Printf.printf "File not found!";
		raise e;;

printsheet read 0;;