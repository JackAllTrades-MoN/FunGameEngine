type error =
  Unclosed of Location.t * string * Location.t * string
| Expecting of Location.t * string
| Not_expecting of Location.t * string
| Other of Location.t

exception Error of error
exception Escape_error

let location_of_error = function
  | Unclosed (l,_,_,_)
    | Expecting (l,_)
    | Not_expecting (l,_)
    | Other l -> l


