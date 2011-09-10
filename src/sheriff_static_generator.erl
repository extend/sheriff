%% Copyright (c) 2011, William Dang <malliwi@hotmail.com>,
%%                     Hamza Mahmood <zar_roc@hotmail.fr>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(sheriff_static_generator).
-export([build_f/1]).


-type var_ast_definition()::{var,integer(),atom()}. %% included in tuple().
-type type_definition_ast()::tuple().
-type attribute_type_ast()::{ 
    attribute,integer(),type,{atom(),type_definition_ast(),
    [var_ast_definition()]}}.
-type function_ast()::tuple().

%% The main function
%% @doc build_f get the AST of an attribute type, return the AST of a function
-spec build_f(attribute_type_ast())->function_ast().
build_f({attribute,_L,type,{Type_name,Type_def,List_of_type_arg}})->
    Param=sheriff_string_generator:name_var(),
    {function,1,
        sheriff_string_generator:name_function(Type_name),
        length(List_of_type_arg)+1,
        [{clause,1, [{var,1,Param}|List_of_type_arg],%KEEP List_of_type_arg here
                    [],
                    [build_f(Param,Type_def,List_of_type_arg)]
        }]
    }.

%% for build_f/3, the third param may be removed someday, or changed.
-spec build_f(atom(),type_definition_ast(),[any()])->function_ast().

%% @doc any()
build_f(_,{var,_L,'_'},_)->{atom,1,true};

%% @doc Atom : atom() | Erlang_Atom
% atom()
build_f(Param,{type,_L,atom,[]},_)->
    {call,1,{atom,1,is_atom},[{var,1,Param}]};
% Erlang_Atom
build_f(Param,{atom,_L,Val},_)->
    {op,1,'=:=',{var,1,Param},{atom,1,Val}};

%% @doc Binary : binary() 
build_f(Param,{type,_L,binary,[]},_)->
    {call,1,{atom,1,is_binary},[{var,1,Param}]};

%% @doc float()
build_f(Param,{type,_L,float,[]},_)->
    {call,1,{atom,1,is_float},[{var,1,Param}]};

%% @doc Fun : fun()

%% @doc Integer : integer() | Erlang_Integer | Erlang_Integer..Erlang_Integer
% integer()
build_f(Param,{type,_L,integer,[]},_)->
    {call,1,{atom,1,is_integer},[{var,1,Param}]};
% Erlang_Integer
build_f(Param,{integer,_L,Val},_)->
    {op,1,'=:=',{var,1,Param},{integer,1,Val}};
build_f(Param,{op,_L,'-',{integer,_,Val}},_)->
    {op,1,'=:=',{var,1,Param},{op,1,'-',{integer,1,Val}}};
% Erlang_Integer..Erlang_Integer , range
build_f(Param,{type,_L,range,[{integer,_,Deb},{integer,_,Fin}]},_)->
    {op,1,'andalso',
        {call,1,{atom,1,is_integer},[{var,1,Param}]},
	{op,1,'andalso',
	    {op,1,'>=',{var,1,Param},{integer,1,Deb}},
	    {op,1,'=<',{var,1,Param},{integer,1,Fin}}
	}
    };
build_f(Param,{type,_L,range,[{op,_,'-',{integer,_,Deb}},{integer,_,Fin}]},_)->
    {op,1,'andalso',
        {call,1,{atom,1,is_integer},[{var,1,Param}]},
	{op,1,'andalso',
	    {op,1,'>=',{var,1,Param},{op,1,'-',{integer,1,Deb}}},
	    {op,1,'=<',{var,1,Param},{integer,1,Fin}}
	}
    };
build_f(Param,{type,_L,range,[{op,_,'-',{integer,_,Deb}},
                                            {op,_,'-',{integer,_,Fin}}]},_)->
    {op,1,'andalso',
        {call,1,{atom,1,is_integer},[{var,1,Param}]},
	{op,1,'andalso',
	    {op,1,'>=',{var,1,Param},{op,1,'-',{integer,1,Deb}}},
	    {op,1,'=<',{var,1,Param},{op,1,'-',{integer,1,Fin}}}
	}
    };

%% @doc List : list() | list(Type) | improper_list(Type1, Type2)    
%% @doc          | maybe_improper_list(Type1, Type2) 
% list()
build_f(Param,{type,_L,list,[]},_)->
    {call,1,{atom,1,is_list},[{var,1,Param}]};
% list(Type) , this code may be improved
build_f(Param,{type,_L,list,[Type_def]},List_of_type_arg)->
    {op,1,'andalso',
        {call,1,{atom,1,is_list},[{var,1,Param}]},
	{call,1,
            {'fun',1,{clauses,[{clause,1,[],[],
                [{call,1,{remote,1,{atom,1,lists},{atom,1,all}},
                    [{'fun',1,{clauses,[{clause,1,[{var,1,Param}],[],
                        [
			build_f(Param,Type_def,List_of_type_arg)
		        ]}]}},
	        {var,1,Param}]}]}]}},
        []}};

%% @doc Tuple :: tuple() | {} | {TList}
% tuple()
build_f(Param,{type,_L,tuple,any},_)->
    {call,1,{atom,1,is_tuple},[{var,1,Param}]};
% {}
build_f(Param,{type,_L,tuple,[]},_)->
    {op,1,'andalso',
        {call,1,{atom,1,is_tuple},[{var,1,Param}]},
        {op,24,'==',
            {call,1,{atom,1,tuple_size},[{var,1,Param}]},
            {integer,1,0}
        }
    };
% tuple(TList) / {TList} , this code may be improved
build_f(Param,{type,_L,tuple,List_def},List_of_type_arg)->
    {op,1,'andalso',
        {call,1,{atom,1,is_tuple},[{var,1,Param}]},
        {op,1,'andalso',
            {op,1,'==',
		{call,1,{atom,1,tuple_size},[{var,1,Param}]},
                {integer,1,length(List_def)}
            },
            {call,1,{'fun',1,{clauses,[{clause,1,[{var,1,Param}],[],[
		 {match,1,
                     sheriff_string_generator:name_var_list(length(List_def)),
                     {call,1,{atom,1,tuple_to_list},[{var,47,Param}]}
       	     	 },
		 tuple_match(
		     sheriff_string_generator:name_var_list_lookup(),
		     List_def,List_of_type_arg)
    ]}]}},[{var,1,Param}]}}};

%% @doc Union : union()
build_f(_,{type,_L,union,[]},_)->{atom,1,false};
build_f(Param,{type,_L,union,[H|T]},List_of_type_arg)->
    {op,1,'orelse',
        build_f(Param,H,List_of_type_arg),
        build_f(Param,{type,_L,union,T},List_of_type_arg)
    };

%% @doc UserDefined type: it handel type's parameters value
build_f(Param,{var,_L,Val},_)->
    {call,1,
        {remote,1,{atom,1,sheriff_dynamic_generator},{atom,1,find_f}},
        [{var,1,Param},{var,1,Val}]
    };

%% @doc UserDefined type: In order to use the type defined by the user
build_f(Param,{type,_L,Type_name,Type_param},_)->
    Type_var=lists:map( fun(X)->erl_syntax:revert(erl_syntax:abstract(X)) end,
                   Type_param ),
    case (ets:lookup(my_table,Type_name)==[{Type_name,length(Type_param)}]) of
        true->{call,1,{atom,1,
		sheriff_string_generator:name_function(Type_name)},
	        [{var,1,Param}|Type_var]};
        false-> error("undifined type / not supported yet")
    end;

% @doc For UserDefined type:
% In order to use the type defined by the user, which are in other modules
% NOTE:
% -these modules should have been compile using the {parse_transform,sheriff}
% compiling options, with the same module version for the sheriff module
build_f(Param,{remote_type,_L,[{atom,_,Type_module},{atom,_,Type_name},
		Type_param] },_)->
    Type_var=lists:map( fun(X)->erl_syntax:revert(erl_syntax:abstract(X)) end,
                   Type_param ),
    {call,1,
        {remote,1,{atom,1,Type_module},
	     {atom,1,sheriff_string_generator:name_function(Type_name)}},
              [{var,1,Param}|Type_var]
    }.
%%---------------------------------------------------
%%---------------------------------------------------


% function for building tuple testing code
-spec tuple_match([atom()],[type_definition_ast()],any())->function_ast().
tuple_match([],[],_)->{atom,1,true};
tuple_match([Param|Suite],[Type_def|List_suite],List_of_type_arg)->
    {op,1,'andalso',
        build_f(Param,Type_def,List_of_type_arg),
	tuple_match(Suite,List_suite,List_of_type_arg)
    }.
