{
  open Parser
  exception Eof
  exception InvalidToken
}

let d = ['0'-'9']                                                       (* for naturals *)
let d1 = ['1'-'9']                                                      (* for naturals - 0 *)
let f = ['+''-']?('0'|(d1+d*))('.'d*d1)?                                (* for detecting floats and integers with optional +/- sign *)
let z = '0' | d1 d*                                                     (* for detecting integer only *)
let sp = ' '                                                            (* for space *)
let indice = '[' sp* (z as z1) sp* ',' sp* (z as z2) sp* ']'            (* my rule has extra utility of avoiding spaces after each symbol *)
let range = '(' sp* '[' sp* (z as z1) sp* ',' sp* (z as z2) sp* ']' sp* ':' sp* '[' sp* (z as z3) sp* ',' sp* (z as z4) sp* ']' sp* ')'
                                                                        (* my rule has extra utility of avoiding spaces after each symbol *)

rule token = parse
    [' ' '\t']          { token lexbuf }                                                                          (* again call lexbuf *)
  | ['\n' ]             { token lexbuf }                                                                          (* again call lexbuf *)
  | f as lxm            { Float (float_of_string lxm) }                                                           (* I ve returned Float of int so that I can use the integer later *)
  | '('                 { LParen }                                | ')'                 { RParen }                
  | '['                 { LBracket }                              | ']'                 { RBracket }
  | ','                 { Comma }
  | ':'                 { Colon }
  | indice              { Indices ((int_of_string z1),(int_of_string z2)) }                                       (* return Indice of int*int *)
  | range               { Ranges ((int_of_string z1),(int_of_string z2),(int_of_string z3),(int_of_string z4)) }  (* return Range of int*int*int*int *)
  | "SUM"               { SUM }
  | "AVG"               { AVG }        
  | "MIN"               { MIN }    
  | "MAX"               { MAX }
  | "COUNT"             { COUNT }
  | "ROWCOUNT"          { ROWCOUNT }                  | "COLCOUNT"          { COLCOUNT }
  | "ROWSUM"            { ROWSUM }                    | "COLSUM"            { COLSUM }
  | "ROWAVG"            { ROWAVG }                    | "COLAVG"            { COLAVG }
  | "ROWMIN"            { ROWMIN }                    | "COLMIN"            { COLMIN }
  | "ROWMAX"            { ROWMAX }                    | "COLMAX"            { COLMAX }
  | "ADD"               { ADD }                       | "SUBT"              { SUBT }
  | "MULT"              { MULT }                      | "DIV"               { DIV }
  | ":="                { Assignment }
  | ';'                 { Termination }
  | eof                 { raise Eof }
  | _                   { raise InvalidToken }
