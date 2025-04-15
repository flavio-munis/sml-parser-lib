signature OPTION_EXTENDED_SIG =
sig
	include MONAD_SIG
end

structure Option_Ext : OPTION_EXTENDED_SIG =
struct

type 'a t = 'a option

(* Infix Operators Orders *)
(* FUNCTOR_SIG Operators *)
infix 2 <$>
infix 2 <$
infix 2 $>

(* APPLICATIVE_SIG Operators *)
infix 1 <*>
infix 1 *>
infix 1 <*

(* MONAD_SIG Operators *)
infix 1 >=>
infix 1 >>=


(* FUNCTOR_SIG Definitions *)

(* "Penetrates" the type 'a t and apply f to it's value.
 *
 * f : ('a -> 'b) ->/* 'a option -> 'b option *)
fun fmap f opt =
	case opt of
        NONE => NONE
      | SOME x => SOME (f x)
 
fun op <$> (opt, f) = fmap f opt

(* "Penetrates" type 'b t and change it to 'a t without losing context.
 *
 * f : 'a ->/* 'b option -> 'a option *)
fun rplc_left a opt = opt <$> (fn _ => a)
fun op <$ (a, opt) = rplc_left a opt

(* "Penetrates" type 'a t and change it to 'b t without losing context.
 *
 * f : 'a option ->/* 'b -> 'b option *)
fun rplc_right opt b = b <$ opt
fun op $> (opt, b) = rplc_right opt b


(* APPLICATIVE_SIG Definitions *)

(* Brings a value 'a to context t.
 *
 * f : 'a -> 'a option *)
fun pure x = SOME x


(* Apply Operator, apply a function wrapped in context t to a value in a context t.
 *
 * f : ('a -> 'b) option ->/* 'a option -> 'b option *)
fun apply opt1 opt2 = 
	case opt1 of
		NONE => NONE
	  | SOME f => fmap f opt2

fun op <*> (opt1, opt2) = apply opt1 opt2
	
(* Ignores the left value.
 *
 * f : 'a option ->/* 'b option -> 'a option *)
fun leftsq opt1 opt2 = opt1
fun op <* (opt1, opt2) = leftsq opt1 opt2

(* Ignores the right value.
 *
 * f : 'a option ->/* 'b option -> 'b option *)
fun rightsq opt1 opt2 = opt2
fun op *> (opt1, opt2) = rightsq opt1 opt2


(* Helper function to append to a list. (until List_Ext is made).
 *
 * f : 'a option -> ('a -> b' option) -> 'b option *)
fun append x y = x::y;


(* Turns a type inside out.
 *
 * f : 'a option list -> 'a list option *)
fun sequenceA opts = 
	case opts of
		[] => pure []
	  | x::xs' => x <$> append <*> (sequenceA xs')


(* MONAD_SIG Definitions *)

(* Bind operation, essential to the definition of a Monad.
 *
 * f : 'a option -> ('a -> b' option) -> 'b option *)
fun bind opt f =
	case opt of
		NONE => NONE
	  | SOME x => f x

fun op >>= (opt, f) = bind opt f

(* Fish Bind operation.
 *
 * f : ('a -> 'b option) -> ('b -> c' option) -> 'a -> 'c option *)
fun fish_bind f g a = (f a) >>= g
fun op >=> (f, g) a = fish_bind f g a

end
