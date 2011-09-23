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

%% @doc none() might be removed / check if there is an atom "empty"
build_f(_,{type,_L,none,[]},_)->{atom,1,false};

%% @doc pid()
build_f(Param,{type,_L,pid,[]},_)->
    {call,1,{atom,1,is_pid},[{var,1,Param}]};

%% @doc port()
build_f(Param,{type,_L,port,[]},_)->
    {call,1,{atom,1,is_port},[{var,1,Param}]};

%% @doc reference()
build_f(Param,{type,_L,reference,[]},_)->
    {call,1,{atom,1,is_reference},[{var,1,Param}]};

%% @doc [] 
build_f(Param,{type,_L,nil,[]},_)->
    {op,1,'=:=',{var,1,Param},{nil,1}};

%% @doc Atom : atom() | Erlang_Atom
% atom()
build_f(Param,{type,_L,atom,[]},_)->
    {call,1,{atom,1,is_atom},[{var,1,Param}]};
% Erlang_Atom
build_f(Param,{atom,_L,Val},_)->
    {op,1,'=:=',{var,1,Param},{atom,1,Val}};

%% @doc Binary : binary() |<<>>|<<_:Rem>>|<<_:_*Div>>|<<_:Rem , _:_*Div>>
% binary()
build_f(Param,{type,_L,binary,[]},_)->
    {call,1,{atom,1,is_binary},[{var,1,Param}]};
% <<>>|<<_:Rem>>|<<_:_*Div>>|<<_:Rem , _:_*Div>>
build_f(Param,{type,_L,binary,[{integer,_,Rem},{integer,_,Div}]},_)->
    {op,1,'andalso',
        {call,1,{atom,1,is_bitstring},[{var,1,Param}]},
	{op,1,'==',
            {op,1,'rem',
                {call,1,{atom,1,bit_size},[{var,1,Param}]},
                {integer,1,Div}
            },
            {integer,1,Rem}
        }
    };

%% @doc float()
build_f(Param,{type,_L,float,[]},_)->
    {call,1,{atom,1,is_float},[{var,1,Param}]};

%% @doc Fun : fun()
%% @TODO fun are tested in a generical way, add:
%       fun((...)->integer())|fun(()->integer())
%           |fun((atom(),float())->integer()|float()).
% fun()
build_f(Param,{type,_L,'fun',_},_)->
    {call,1,{atom,1,is_function},[{var,1,Param}]};

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
%% @TODO handel then dynamically
% improper_list(integer(),atom()) | maybe_improper_list(integer(),atom())

%% @doc Tuple :: tuple() | {} | {TList}
% tuple()
build_f(Param,{type,_L,tuple,any},_)->
    {call,1,{atom,1,is_tuple},[{var,1,Param}]};
% {}
build_f(Param,{type,_L,tuple,[]},_)->
    {op,1,'andalso',
        {call,1,{atom,1,is_tuple},[{var,1,Param}]},
        {op,1,'==',
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
                     {call,1,{atom,1,tuple_to_list},[{var,1,Param}]}
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
%% @doc UserDefined <--see in the end, after alias

%% @doc [T,...] (the "nonempty_list")
build_f(Param,{type,_L,nonempty_list,Type_def},List_of_type_arg)->
    {op,1,'andalso',
        build_f(Param,{type,_L,list,Type_def},List_of_type_arg),
        {op,1,'/=',
	    {call,1,{atom,1,length},[{var,1,Param}]},
            {integer,1,0}
        }
    };


%% ===========================================
%% @doc Built-in type / Alias
%% @TODO : when the structure will be ok, remove _V when they are useless
%%         and replace then by [] (in calls, for mfa, string,...)

% term()
build_f(_,{type,_L,term,[]},_)->{atom,1,true};

% boolean()
build_f(Param,{type,_L,boolean,[]},_)->
    {op,1,'orelse',
        {op,1,'/=',{var,1,Param},{atom,1,true}},
        {op,1,'/=',{var,1,Param},{atom,1,false}}
    };

% byte()
build_f(Param,{type,_L,byte,[]},_V)->
    build_f(Param,{type,1,range,[{integer,1,0},{integer,1,255}]},_V);

% char()
build_f(Param,{type,_L,char,[]},_V)->
    build_f(Param,{type,1,range,[{integer,1,0},{integer,1,1114111}]},_V);

% non_neg_integer()
build_f(Param,{type,_L,non_neg_integer,[]},_V)->
    {op,1,'andalso',
	{call,1,{atom,1,is_integer},[{var,1,Param}]},
	{op,1,'>=',{var,1,Param},{integer,1,0}}
    };

% pos_integer()
build_f(Param,{type,_L,pos_integer,[]},_V)->
    {op,1,'andalso',
	{call,1,{atom,1,is_integer},[{var,1,Param}]},
	{op,1,'>',{var,1,Param},{integer,1,0}}
    };

% neg_integer()
build_f(Param,{type,_L,neg_integer,[]},_V)->
    {op,1,'andalso',
        {call,1,{atom,1,is_integer},[{var,1,Param}]},
	{op,1,'<',{var,1,Param},{integer,1,0}}
    };

% number()
build_f(Param,{type,_L,number,[]},_V)->
    {op,1,'orelse',
        {call,1,{atom,1,is_integer},[{var,1,Param}]},
        {call,1,{atom,1,is_float},[{var,1,Param}]}
    };

% list() : see List

%% @TODO handel dynamically
% maybe_improper_list()
% maybe_improper_list(T)

% string()
build_f(Param,{type,_L,string,[]},_V)->
    build_f(Param,{type,_L,list,[{type,_L,char,[]}] },_V);

% nonempty_string()
build_f(Param,{type,_L,nonempty_string,[]},_V)->
    build_f(Param,{type,_L,nonempty_list,[{type,_L,char,[]}] },_V);

%% @TODO handel dynamically iolist()

% module()
build_f(Param,{type,_L,module,[]},_V)->
    build_f(Param,{type,_L,atom,[]},_V);

% mfa()
build_f(Param,{type,_L,mfa,[]},_V)->
    build_f(Param,{type,_L,tuple,
        [{type,1,atom,[]},{type,1,atom,[]},{type,1,byte,[]}] 
                    },_V);

% node()
build_f(Param,{type,_L,node,[]},_V)->
    build_f(Param,{type,_L,atom,[]},_V);

% timeout()
build_f(Param,{type,_L,timeout,[]},_V)->
    {op,1,'orelse',
        build_f(Param,{atom,_L,infinity},_V),
        build_f(Param,{type,_L,non_neg_integer,[]},_V)
    };

% no_return()
build_f(Param,{type,_L,no_return,[]},_V)->
    build_f(Param,{type,_L,none,[]},_V);

%% ===========================================

% for record
build_f(Param,{type,_L,record,[{atom,_,Record_name}] },_V)->
    {call,1,{atom,1,is_record},[{var,1,Param},{atom,1,Record_name}]};

%% @doc For UserDefined type: it handel type's parameters value
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

% @doc UserDefined type:
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
