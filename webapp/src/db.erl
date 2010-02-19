-module(db).

-export([init/0, start/0, read_d/2, read_t/2, write_t/2, counter/3,
	delete_object/2, transaction/1, select/3]).

-include_lib("webapp.hrl").


init() ->
    ok = mnesia:create_schema([node()]),
    ok = start(),
    create_tables_d(tabs(), [node()]).

tabs() ->
    [{product, record_info(fields,product), disc_only_copies, set},
     {user, record_info(fields,user), disc_only_copies, set},
     {cart, record_info(fields,cart), disc_only_copies, set},
     {counter, record_info(fields,counter), disc_only_copies, set}
    ].

create_tables_d([{Name, Atts, StoreType, SetBag}|T], Node) ->
    create_table_d(Name, [{StoreType, Node}, 
			  {type, SetBag},
			  {attributes, Atts}]),
    create_tables_d(T, Node);
create_tables_d([], _Node) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% mnesia
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    mnesia:start().

create_table_d(Name, TabDef) ->
    mnesia:create_table(Name, TabDef).

read_d(Tab, Key) ->
    mnesia:dirty_read(Tab, Key).

read_t(Tab, Key) ->
    mnesia:read(Tab, Key, read).

select(Tab, MatchSpec, NObjects) ->
    mnesia:select(Tab, MatchSpec, NObjects, read).

write_t(Tab, Val) ->
    mnesia:write(Tab, Val, write).

delete_object(Tab, Key) ->
    mnesia:delete(Tab, Key, write).

counter(Tab, Key, Int) ->
    mnesia:dirty_update_counter(Tab, Key, Int).

transaction(Fun) ->
    mnesia:transaction(Fun).
