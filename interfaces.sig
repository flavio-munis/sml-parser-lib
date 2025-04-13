signature TYPE_SIG =
sig
	type 'a t
end


signature FUNCTOR_SIG =
sig
	include TYPE_SIG

	val fmap : ('a -> 'b) -> 'a t -> 'b t
end


signature APPLICATIVE_SIG =
sig
	include TYPE_SIG

	val pure : 'a -> 'a t
end
