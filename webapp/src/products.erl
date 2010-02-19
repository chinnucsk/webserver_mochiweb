-module(products).
-export([get_products/1]).



%% Handler for GET requests on URI /products
get_products(Type) ->
    MatchHead = #product{id='$1', _='_', _='_', _='_', _='_'},
    ListProducts = db:select(product, [{MatchHead , [], ['$1']}], 10),
    products_body(ListProducts, Type).
    








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BUILDING RESPONSES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
products_body(ListProducts, "text/html") ->
    "<html>" ++
	"<head></head>" ++
	"<body>" ++
	
