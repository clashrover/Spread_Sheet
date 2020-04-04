/*Declaring tokens*/
%{
    open Bmodule
%}
%token  <float> Float 
%token  LParen RParen LBracket RBracket
%token  Comma Colon
%token  <int*int> Indices
%token  <int*int*int*int> Ranges
%token  SUM AVG
%token  MIN MAX
%token  COUNT
%token  ROWCOUNT COLCOUNT
%token  ROWSUM COLSUM
%token  ROWAVG COLAVG
%token  ROWMIN COLMIN 
%token  ROWMAX COLMAX 
%token  ADD SUBT
%token  MULT DIV
%token  Assignment Termination

%start line
%type  <unit> line                /*for now kept the start token to be of type unit as I am not returning anything yet. Later will be of type sheet */

%%
line:
    | formula                     { print_newline() }
    | formula line                { print_newline() }
;

formula:        
    | Indices Assignment COUNT Ranges Termination           { print_newline() ; let al1 = refarr.(0) <- full_count refarr.(0) $4 $1 in () }
    | Indices Assignment ROWCOUNT Ranges Termination        { print_newline() ; let al1 = refarr.(0) <- row_count refarr.(0) $4 $1 in () }
    | Indices Assignment COLCOUNT Ranges Termination        { print_newline() ; let al1 = refarr.(0) <- col_count refarr.(0) $4 $1 in () }

    | Indices Assignment SUM Ranges Termination             { print_newline() ; let al1 = refarr.(0) <- full_sum refarr.(0) $4 $1 in () }
    | Indices Assignment ROWSUM Ranges Termination          { print_newline() ; let al1 = refarr.(0) <- row_sum refarr.(0) $4 $1 in () }
    | Indices Assignment COLSUM Ranges Termination          { print_newline() ; let al1 = refarr.(0) <- col_sum refarr.(0) $4 $1 in () }

    | Indices Assignment AVG Ranges Termination             { print_newline() ; let al1 = refarr.(0) <- full_avg refarr.(0) $4 $1 in () }
    | Indices Assignment ROWAVG Ranges Termination          { print_newline() ; let al1 = refarr.(0) <- row_avg refarr.(0) $4 $1 in () }
    | Indices Assignment COLAVG Ranges Termination          { print_newline() ; let al1 = refarr.(0) <- col_avg refarr.(0) $4 $1 in () }

    | Indices Assignment MIN Ranges Termination             { print_newline() ; let al1 = refarr.(0) <- full_min refarr.(0) $4 $1 in () }
    | Indices Assignment ROWMIN Ranges Termination          { print_newline() ; let al1 = refarr.(0) <- row_min refarr.(0) $4 $1 in () }
    | Indices Assignment COLMIN Ranges Termination          { print_newline() ; let al1 = refarr.(0) <- col_min refarr.(0) $4 $1 in () }

    | Indices Assignment MAX Ranges Termination             { print_newline() ; let al1 = refarr.(0) <- full_max refarr.(0) $4 $1 in () }
    | Indices Assignment ROWMAX Ranges Termination          { print_newline() ; let al1 = refarr.(0) <- row_max refarr.(0) $4 $1 in () }
    | Indices Assignment COLMAX Ranges Termination          { print_newline() ; let al1 = refarr.(0) <- col_max refarr.(0) $4 $1 in () }
    
    /* Different types of ADD */
    | Indices Assignment ADD Ranges Ranges Termination      { print_newline() ; let al1 = refarr.(0) <- add_range refarr.(0) $4 $5 $1 in () }
    | Indices Assignment ADD Float Ranges Termination       { print_newline() ; let al1 = refarr.(0) <- add_const refarr.(0) $5 $4 $1 in () }
    | Indices Assignment ADD Ranges Float Termination       { print_newline() ; let al1 = refarr.(0) <- add_const refarr.(0) $4 $5 $1 in () }
    | Indices Assignment ADD Indices Ranges Termination     { print_newline() ; let (a1,a2) = $4 in
                                                                                let temp = refarr.(0) in
                                                                                let z = temp.(a2).(a1) in
                                                                                    match z with
                                                                                    | E     -> raise EmptyCell
                                                                                    | F(t1) -> let al1 = refarr.(0) <- add_const temp $5 t1 $1 in ()
                                                                                    | I(t2) -> let al1 = refarr.(0) <- add_const temp $5 (float_of_int t2) $1 in ()
                                                            }
    | Indices Assignment ADD Ranges Indices Termination     { print_newline() ; let (a1,a2) = $5 in
                                                                                let temp = refarr.(0) in
                                                                                let z = temp.(a2).(a1) in
                                                                                    match z with
                                                                                    | E     -> raise EmptyCell
                                                                                    | F(t1) -> let al1 = refarr.(0) <- add_const temp $4 t1 $1 in ()
                                                                                    | I(t2) -> let al1 = refarr.(0) <- add_const temp $4 (float_of_int t2) $1 in ()
                                                            }
    
    /* Different types of SUBT */
    | Indices Assignment SUBT Ranges Ranges Termination      { print_newline() ; let al1 = refarr.(0) <- subt_range refarr.(0) $4 $5 $1 in () }
    | Indices Assignment SUBT Float Ranges Termination       { print_newline() ; let al1 = refarr.(0) <- subt_const refarr.(0) $5 $4 $1 in () }
    | Indices Assignment SUBT Ranges Float Termination       { print_newline() ; let al1 = refarr.(0) <- subt_const refarr.(0) $4 $5 $1 in () }
    | Indices Assignment SUBT Indices Ranges Termination     { print_newline() ; let (a1,a2) = $4 in
                                                                                let temp = refarr.(0) in
                                                                                let z = temp.(a2).(a1) in
                                                                                    match z with
                                                                                    | E     -> raise EmptyCell
                                                                                    | F(t1) -> let al1 = refarr.(0) <- subt_const temp $5 t1 $1 in ()
                                                                                    | I(t2) -> let al1 = refarr.(0) <- subt_const temp $5 (float_of_int t2) $1 in ()
                                                            }
    | Indices Assignment SUBT Ranges Indices Termination     { print_newline() ; let (a1,a2) = $5 in
                                                                                let temp = refarr.(0) in
                                                                                let z = temp.(a2).(a1) in
                                                                                    match z with
                                                                                    | E     -> raise EmptyCell
                                                                                    | F(t1) -> let al1 = refarr.(0) <- subt_const temp $4 t1 $1 in ()
                                                                                    | I(t2) -> let al1 = refarr.(0) <- subt_const temp $4 (float_of_int t2) $1 in ()
                                                            }
    
    /* Different types of MULT */
    | Indices Assignment MULT Ranges Ranges Termination     { print_newline() ; let al1 = refarr.(0) <- mult_range refarr.(0) $4 $5 $1 in () }
    | Indices Assignment MULT Float Ranges Termination      { print_newline() ; let al1 = refarr.(0) <- mult_const refarr.(0) $5 $4 $1 in () }
    | Indices Assignment MULT Ranges Float Termination      { print_newline() ; let al1 = refarr.(0) <- mult_const refarr.(0) $4 $5 $1 in () }
    | Indices Assignment MULT Indices Ranges Termination    { print_newline() ; let (a1,a2) = $4 in
                                                                                let temp = refarr.(0) in
                                                                                let z = temp.(a2).(a1) in
                                                                                    match z with
                                                                                    | E     -> raise EmptyCell
                                                                                    | F(t1) -> let al1 = refarr.(0) <- mult_const temp $5 t1 $1 in ()
                                                                                    | I(t2) -> let al1 = refarr.(0) <- mult_const temp $5 (float_of_int t2) $1 in ()
                                                            }
    | Indices Assignment MULT Ranges Indices Termination    { print_newline() ; let (a1,a2) = $5 in
                                                                                let temp = refarr.(0) in
                                                                                let z = temp.(a2).(a1) in
                                                                                    match z with
                                                                                    | E     -> raise EmptyCell
                                                                                    | F(t1) -> let al1 = refarr.(0) <- mult_const temp $4 t1 $1 in ()
                                                                                    | I(t2) -> let al1 = refarr.(0) <- mult_const temp $4 (float_of_int t2) $1 in ()
                                                            }
    
    /* Different types of DIV */
    | Indices Assignment DIV Ranges Ranges Termination      { print_newline() ; let al1 = refarr.(0) <- div_range refarr.(0) $4 $5 $1 in () }
    | Indices Assignment DIV Float Ranges Termination       { print_newline() ; let al1 = refarr.(0) <- div_const refarr.(0) $5 $4 $1 in () }
    | Indices Assignment DIV Ranges Float Termination       { print_newline() ; let al1 = refarr.(0) <- div_const refarr.(0) $4 $5 $1 in () }
    | Indices Assignment DIV Indices Ranges Termination     { print_newline() ; let (a1,a2) = $4 in
                                                                                let temp = refarr.(0) in
                                                                                let z = temp.(a2).(a1) in
                                                                                    match z with
                                                                                    | E     -> raise EmptyCell
                                                                                    | F(t1) -> let al1 = refarr.(0) <- div_const temp $5 t1 $1 in ()
                                                                                    | I(t2) -> let al1 = refarr.(0) <- div_const temp $5 (float_of_int t2) $1 in ()
                                                            }
    | Indices Assignment DIV Ranges Indices Termination     { print_newline() ; let (a1,a2) = $5 in
                                                                                let temp = refarr.(0) in
                                                                                let z = temp.(a2).(a1) in
                                                                                    match z with
                                                                                    | E     -> raise EmptyCell
                                                                                    | F(t1) -> let al1 = refarr.(0) <- div_const temp $4 t1 $1 in ()
                                                                                    | I(t2) -> let al1 = refarr.(0) <- div_const temp $4 (float_of_int t2) $1 in ()
                                                            }
    
;