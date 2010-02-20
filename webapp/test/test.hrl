-define(assertEq(A,B),
	((fun () ->
	    case (A =:= B) of
		true -> ok;
		__V -> 
		    io:format("Expression:~n~p~n"
			      "Expected:~n~p~n"
			      "Value:~n~p~n", 
			      [B, A, __V]),
		    .erlang:error({assertEq_failed,
				      [{module, ?MODULE},
				       {line, ?LINE}]})
	    end
	  end)())).

-define(assertMatch(Guard, Expr),
	((fun () ->
	    case (Expr) of
		Guard -> ok;
		__V -> 
		    io:format("Expression:~n~p~n"
			      "Expected:~n~p~n"
			      "Value:~n~p~n", 
			      [??Expr, ??Guard, __V]),
		    .erlang:error({assertMatch_failed,
				      [{module, ?MODULE},
				       {line, ?LINE}]})
	    end
	  end)())).
