structure X = struct
    exception Foo
    fun foo n : int = let
	fun a (x, 0) = d x
	  | a (x, n) = b (x, n - 1)
	and b (x, n) = c (x, n)
	and c (x, n) = a (x, n)
	and d x = e (x, 3)
(*
		  handle e => (print "exn\n"; raise e)
*)
	and e (x, 0) = f x
	  | e (x, n) = e (x, n - 1)
	and f 0 = (* BackTrace.trigger () *) raise Fail "triggered"
	  | f n = n * g (n - 1)
	and g n = a (n, 3)
    in
	f n;
	OS.Process.success
    end

    fun main _ =
	BackTrace.monitor (fn () => foo 3)
end
