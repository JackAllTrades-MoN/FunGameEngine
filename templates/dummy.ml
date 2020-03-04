open Core

type window_info = {w: int; h:int}
type game_state  = GameTitle | GameMain | GameOver
type state = {wnidow_info:window_info;
              game_state:game_state}

type 'a d2state = state -> ('a, string) Result.t * state

module D2Base : Monad.Basic = struct
  type 'a t = 'a d2state

  let bind t ~f:f =
    fun s ->
    match t s with
    | (Ok a', s') ->
       begin
         match f a' s' with
         | (Ok a'', s'') -> Ok a'', s''
         | (Error msg, s) -> Error msg, s
       end
    | (Error msg, s) -> Error msg, s

  let map = `Define_using_bind

  let return a = (fun s -> Ok a, s)
end

module D2Monad = Monad.Make(D2Base)

module D2 : sig
  type 'a t
  val init : unit -> unit t
end = struct
  type 'a t = 'a D2Base.t
  include D2Monad

  let init () = return ()
end
