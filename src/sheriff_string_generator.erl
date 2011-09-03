-module(sheriff_string_generator).
-compile(export_all).

-type list_ast():: {nil,integer()}
                |{var,integer(),atom()}
		|{cons,integer(),{var,integer(),atom()},list_ast()}.

%% @doc internal functions for generating function's name and variable's name, 
%% @doc in order to avoid conflict with name given by the user

%% Initialise the ets table used for generating different names
-spec database()->no_return().
database()->
    ets:new(my_table, [named_table, protected, set, {keypos, 1}]),
    ets:insert(my_table, {'sheriff_$_var', 0}).