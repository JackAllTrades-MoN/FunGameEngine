type 'a loc = 'a Location.loc = {
    txt : 'a;
    loc : Location.t;
  }

type rec_flag = Nonrecursive | Recursive

type label = string

type arg_label = Nolabel
(* | Labelled | Optional *)
