signature OPTION_EXTENDED_SIG =
sig
	include APPLICATIVE_SIG

	val wrap    : 'a option -> 'a t
	val unwrap  : 'a t -> 'a option
	val apply_f : 'a option -> ('a t -> 'b t) -> 'b option
end

structure Option_Ext :> OPTION_EXTENDED_SIG =
struct
type 'a t = 'a option

fun wrap (SOME x) = SOME x
  | wrap NONE = NONE

fun unwrap x = x

fun apply_f opt f = unwrap (f (wrap opt))

fun fmap f opt =
	case opt of
        NONE => unwrap NONE
      | SOME x => SOME (f x)


infix 1 <$>
infix 1 <$
infix 1 $>

fun op <$> (opt, f) = fmap f opt
fun op <$ (a, opt) = opt <$> (fn _ => a)
fun op $> (opt, b) = b <$ opt

fun pure x = SOME x

infix 1 <*>

fun op <*> opt1 opt2 =
	case opt1 of
		NONE => NONE
	  | SOME f => 
		(case opt2 of
			 NONE => NONE
		   | SOME x => SOME (f x))

end
