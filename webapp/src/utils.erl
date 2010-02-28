-module(utils).

-export([check_params/4]).

check_params(Params, [{FieldName, Type}|T], Res, create) ->
    case proplists:lookup(FieldName, Params) of
	none ->
	    throw(bad_request);
	{FieldName, Value} ->
	    NewParams = proplists:delete(FieldName, Params),
	    NewRes = [{FieldName, parse_param(Value, Type)}|Res],
	    check_params(NewParams, T, NewRes, create)
    end;
check_params([{FieldName, Value}|T], Fields, Res, update) ->
    case proplists:lookup(FieldName, Fields) of
	none ->
	    throw(bad_request);
	{FieldName, Type} ->
	    NewFields = proplists:delete(FieldName, Fields),
	    NewRes = [{FieldName, parse_param(Value, Type)}|Res],
	    check_params(T, NewFields, NewRes, update)
    end;
check_params([], [_|_], _, create) ->
    throw(bad_request);
check_params([], [_|_], Res, update) ->
    Res;
check_params([_|_], [], _, _) ->
    throw(bad_request);
check_params([], [], Res, _) ->
    Res.

parse_param(Value, string) when is_list(Value) ->
    try list_to_integer(Value) of
	_ -> throw(bad_request)
    catch
	error:badarg -> Value
    end;
parse_param(Value, int) when is_list(Value) ->
    try 
	Int = list_to_integer(Value),
	Int
    catch
	error:badarg -> throw(bad_request)
    end;
parse_param(Value, no_format) when is_list(Value) ->
    case length(Value) < 4 of
	true -> throw(bad_request);
	false ->
	    Value
    end;
parse_param(_, _) ->
    throw(bad_request).
    
