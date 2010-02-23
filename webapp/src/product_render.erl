-module(product_render).

-export([get/2, get_list/2]).

-include_lib("webapp.hrl").

get(Data, "application/xml") ->
    {ok, Compiled} = sgte:compile_file("priv/template.xml"),
    {ok, Compiled2} = sgte:compile("\n<$field$>$value$</$field$>"),
    Id = proplists:get_value(key, Data),
    Name = proplists:get_value(name, Data),
    Price = proplists:get_value(price, Data),
    Amount = proplists:get_value(amount, Data),
    Description = proplists:get_value(description, Data),
    sgte:render_str(Compiled, [{type, "product"},
			       {attr, Compiled2}, 
			       {attrs, [[{field,id},{value,Id}],
					[{field,name},{value,Name}],
					[{field,price},{value,Price}],
					[{field,amount},{value,Amount}],
					[{field,description},
					 {value,Description}]]}]);
get(_Data, "text/html") ->
    "<html>ok</html>";
get(Data, "application/json") ->
    mochijson2:encode({struct, Data}).


get_list(Data, "application/xml") ->
    {ok, Compiled} = sgte:compile_file("priv/template.xml"),
    {ok, Compiled2} = sgte:compile("\n<product><link>$link$</link>"
				   "<name>$name$</name></product>"),
    List = lists:foldl(
	     fun(Product, Acc) ->
		     Id = proplists:get_value(key, Product),
		     Name = proplists:get_value(name, Product),
		     Link = ?HOSTNAME ++ "products/" ++ integer_to_list(Id),
		     [[{link,Link}, {name,Name}] | Acc]
	     end,
	     [],
	     Data),
    sgte:render_str(Compiled, [{type, "list"}, {attr, Compiled2},
			       {attrs, List}]);
get_list(_Data, "text/html") ->
    "<html>ok</html>";
get_list(Data, "application/json") ->
    NewData = 
	lists:foldl(
	  fun(PropList, Acc) ->
		  Id = proplists:get_value(key, PropList),
		  Name = proplist:get_value(name, PropList),
		  Link = ?HOSTNAME ++ "products/" ++ integer_to_list(Id),
		  [{struct, [{link,Link},{name,Name}]} | Acc]
	  end,
	  [],
	  Data),
    mochijson2:encode(NewData).
