signature OPTION_EXTENDED_SIG =
sig
	include MONAD_SIG
end

structure Option_Ext : OPTION_EXTENDED_SIG =
struct

type 'a t = 'a option

fun fmap f opt =
	case opt of
        NONE => NONE
      | SOME x => SOME (f x)


infix 1 <$>
infix 1 <$
infix 1 $>

fun op <$> (opt, f) = fmap f opt
fun op <$ (a, opt) = opt <$> (fn _ => a)
fun op $> (opt, b) = b <$ opt

fun pure x = SOME x

infix 1 <*>

fun op <*> (opt1, opt2) =
	case opt1 of
		NONE => NONE
	  | SOME f => 
		(case opt2 of
			 NONE => NONE
		   | SOME x => SOME (f x))


infix 1 >=>

fun op >=> (f, g) a =
	case f a of
		NONE => NONE
	  | SOME b => 
		(case g b of
		     NONE => NONE
		   | SOME c => SOME c)

val return = pure

end
