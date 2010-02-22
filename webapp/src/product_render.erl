-module(product_render).

-export([get/2]).

get(Data, "application/xml") ->
    Id = proplists:get_value(key, Data),
    Name = proplists:get_value(name, Data),
    Price = proplists:get_value(price, Data),
    Amount = proplists:get_value(amount, Data),
    Description = proplists:get_value(description, Data),
    {ok, Compiled} = sgte:compile_file("priv/template.xml"),
    {ok, Compiled2} = sgte:compile("\n<$field$>$value$</$field$>"),
    sgte:render_str(Compiled, [{attr, Compiled2}, 
			       {attrs, [[{field,id},{value,Id}],
					[{field,name},{value,Name}],
					[{field,price},{value,Price}],
					[{field,amount},{value,Amount}],
					[{field,description},
					 {value,Description}]]}]);
get(Data, "text/html") ->
    "<html>ok</html>";
get(Data, "application/json") ->
    mochijson2:encode({struct, Data}).
