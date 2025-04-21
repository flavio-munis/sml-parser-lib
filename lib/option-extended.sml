signature OPTION_EXTENDED_SIG =
sig
	include FUNCTOR_SIG
	include APPLICATIVE_SIG
	include MONAD_SIG
	include ALTERNATIVE_SIG
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

(* ALTERNATIVE_SIG Operators *)
infix 1 <|> <||>


(* ALTERNATIVE_SIG Operators *)

(* Empty value in a type t context.
 *
 * f : unit -> 'a option *)
fun empty () = NONE

(* Or operator for comparing two values of type 'a t.
 *
 * f : 'a option -> 'a option -> 'a option *)
fun op <|> (opt1, opt2) = 
	case opt1 of
		NONE   => opt2
	  | SOME _ => opt1

fun op <||> (opt1, opt2) = opt1 <|> opt2 


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
fun leftsq opt1 opt2 = 
	case opt1 of
		NONE => NONE
	  | SOME x => (
		  case opt2 of
			  NONE => NONE
			| SOME _ => SOME x)

fun op <* (opt1, opt2) = leftsq opt1 opt2

(* Ignores the right value.
 *
 * f : 'a option ->/* 'b option -> 'b option *)
fun rightsq opt1 opt2 = 
	case opt1 of
		NONE => NONE
	  | SOME _ => opt2

fun op *> (opt1, opt2) = rightsq opt1 opt2


(* Elevate a function f to a context and apply both elements.
 *
 * f : ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option *)
fun liftA2 f opt1 opt2 = apply (apply (pure f) opt1) opt2


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


(* Matches type t zero or more times creating a list.
 *
 * f : 'a option -> 'a list option *)
fun many opt =
    let 
        fun step acc opt' =
            case opt' of
                NONE => SOME acc
              | SOME x => step (x::acc) opt'
    in
        SOME [] <|> (step [] opt)
    end

(* Matches type t one or more times creating a list.
 *
 * f : 'a option -> 'a list option *)
fun some opt = opt <$> append <*> many opt


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
