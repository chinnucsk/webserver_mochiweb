-module(db_test).

-export([test/0]).
-include("test.hrl").

test() ->
    ?assertEq(ok, db:start()),
    db:db_delete(products),
    db:db_delete(cart),
    db:db_delete(user),
    db(),
    doc(),
    viewMap(),
    ?assertEq(ok, db:db_delete(products)),
    ?assertEq(ok, db:db_delete(cart)),
    ?assertEq(ok, db:db_delete(user)),
    ?assertEq(ok, db:stop()).

db() ->
    ?assertEq(ok, db:db_create(products)),
    ?assertEq(ok, db:db_create(cart)),
    ?assertEq(ok, db:db_create(user)),
    ?assertMatch({ok, _}, db:db_info(products)),
    ?assertMatch({ok, _}, db:db_info(cart)),
    ?assertMatch({ok, _}, db:db_info(user)),
    ?assertEq({ok, [products,user,cart]}, db:db_list()).
        
doc() ->
    ?assertEq(ok, db:doc_create(products, 1, product(10, 20))),
    ?assertEq(ok, db:doc_create(products, 2, product(40, 2))),
    ?assertEq(ok, db:doc_create(products, 3, product(35, 50))),
    ?assertEq(ok, db:doc_delete(products, 1)),
    ?assertEq({error, not_found}, db:doc_get(products, 1)),
    ?assertEq({error, not_found}, db:doc_delete(products, 1)),
    ?assertEq(ok, db:doc_create(products, 1, product(5, 10))),
    ?assertEq({ok, [{amount, 2}, {price, 40}]},
	      db:doc_get(products, 2)),
    ?assertEq(ok, db:doc_update(products, 2, product(40, 3))),
    ?assertEq({ok, [{amount, 3}, {price, 40}]},
	      db:doc_get(products, 2)).

viewMap() ->
    ?assertEq(ok, db:view_create(products, products, 
				 "function(doc){emit(null,doc);}")),
    {ok, Res1} = db:view_access(products, products),
    check_res1(Res1),
    ?assertEq(ok, db:view_delete(products, products)),
    ?assertEq(ok, db:view_create(products, products, 
				 "function(doc){emit(doc._id, doc);}")),
    {ok, Res2} = db:view_access(products, products),
    check_res2(Res2),
    {ok, Res3} = db:view_access(products, products, [{key, 1}]),
    check_res3(Res3),
    {ok, Res4} = db:view_access(products, products, [{startkey, 1},{endkey,2}]),
    check_res4(Res4),
    {ok, Res5} = db:view_access(products, products,
				[{startkey, 1},{endkey,3},{limit,2}]),
    check_res5(Res5),
    {ok, Res6} = db:view_access(products, products, [{keys, [1, 3]}]),
    check_res6(Res6),
    ?assertEq(ok, db:view_delete(products, products)),
    ?assertEq({error, not_found}, db:view_delete(products, products)).

check_res1(Res) ->
    Expected = [[{amount, 3}, {price,40}, {key, null}], 
		[{amount, 10}, {price, 5}, {key, null}],
		[{amount, 50}, {price, 35}, {key, null}]],
    ?assertEq(Expected, lists:sort(Res)).

check_res2(Res) ->
    Expected = [[{amount, 3}, {price,40}, {key, 2}], 
		[{amount, 10}, {price, 5}, {key, 1}],
		[{amount, 50}, {price, 35}, {key, 3}]],
    ?assertEq(Expected, lists:sort(Res)).

check_res3(Res) ->
    Expected = [[{amount, 10}, {price, 5}, {key, 1}]],
    ?assertEq(Expected, lists:sort(Res)).
    
check_res4(Res) ->
    Expected = [[{amount, 3}, {price,40}, {key, 2}], 
		[{amount, 10}, {price, 5}, {key, 1}]],
    ?assertEq(Expected, lists:sort(Res)).

check_res5(Res) ->
    Expected = [[{amount, 3}, {price,40}, {key, 2}], 
		[{amount, 10}, {price, 5}, {key, 1}]],
    ?assertEq(Expected, lists:sort(Res)).

check_res6(Res) ->
    Expected = [[{amount, 10}, {price,5}, {key, 1}], 
		[{amount, 50}, {price,35}, {key, 3}]],
    ?assertEq(Expected, lists:sort(Res)).
    
product(Price, Amount) ->
    [{"price", Price}, {"amount", Amount}].

