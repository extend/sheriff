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

-module(sheriff_dynamic_generator).
-export([find_f/3]).


-type type_definition_ast()::tuple().

-spec find_f(atom(),any(),type_definition_ast())->true|false.

%% @doc any()
find_f(_,_,{type,_L,any,[]})->true;
find_f(_,_,{var,_L,'_'})->true;

%% @doc none() might be removed / check if there is an atom "empty"
find_f(_,_,{type,_L,none,[]})->false;

%% @doc pid()
find_f(_Module,Param,{type,_L,pid,[]})->
    is_pid(Param);

%% @doc port()
find_f(_Module,Param,{type,_L,port,[]})->
    is_port(Param);

%% @doc reference()
find_f(_Module,Param,{type,_L,reference,[]})->
    is_reference(Param);

%% @doc [] 
find_f(_Module,Param,{type,_L,nil,[]})->
    (Param=:=[]);

%% @doc Atom : atom() | Erlang_Atom
% atom()
find_f(_Module,Param,{type,_L,atom,[]})->
    is_atom(Param);
% Erlang_Atom
find_f(_Module,Param,{atom,_L,Val})->
    (Param=:=Val);

%% @doc Binary : binary() |<<>>|<<_:Rem>>|<<_:_*Div>>|<<_:Rem , _:_*Div>>
% binary()
find_f(_Module,Param,{type,_L,binary,[]})->
    is_binary(Param);
% <<>>|<<_:Rem>>|<<_:_*Div>>|<<_:Rem , _:_*Div>>
find_f(_Module,Param,{type,_L,binary,[{integer,_,Rem},{integer,_,Div}]})->
    is_bitstring(Param) andalso
    ((bit_size(Param) rem Div) == Rem);

find_f(_Module,Param,{type,_L,float,[]})->
    is_float(Param);

%% @doc Fun : fun()
%% @TODO fun are tested in a generical way, add:
%       fun((...)->integer())|fun(()->integer())
%           |fun((atom(),float())->integer()|float()).
% fun()
find_f(_Module,Param,{type,_L,'fun',_})->
    is_function(Param);

%% @doc Integer : integer() | Erlang_Integer | Erlang_Integer..Erlang_Integer
% integer()
find_f(_Module,Param,{type,_L,integer,[]})->
    is_integer(Param);
% Erlang_Integer
find_f(_Module,Param,{integer,_L,Val})->
    (Param=:=Val);
find_f(_Module,Param,{op,_L,'-',{integer,_,Val}})->
    (Param=:=(-Val));
% Erlang_Integer..Erlang_Integer , range
find_f(_Module,Param,{type,_,range,[{integer,_,Deb},{integer,_,Fin}]})->
    is_integer(Param) andalso (Deb=<Param) andalso (Param=<Fin);
find_f(_Module,Param,{type,_,range,[{op,_,'-',{integer,_,Deb}},
                                                    {integer,_,Fin}]})->
    is_integer(Param) andalso (Param=<Deb) andalso (Param=<Fin);
find_f(_Module,Param,{type,_,range,[{integer,_,Deb},
                                            {op,_,'-',{integer,_,Fin}}]})->
    is_integer(Param) andalso (Deb=<Param) andalso (Fin=<Param);
find_f(_Module,Param,{type,_,range,[{op,_,'-',{integer,_,Deb}},
                                            {op,_,'-',{integer,_,Fin}}]})->
    is_integer(Param) andalso (Param=<Deb) andalso (Fin=<Param);


%% @doc List : list() | list(Type) | improper_list(Type1, Type2)    
%% @doc          | maybe_improper_list(Type1, Type2) 
% list()
find_f(_Module,Param,{type,_L,list,[]})->
    is_list(Param);
% list(Type)
find_f(_Module,Param,{type,_L,list,[Type_def]})->
    is_list(Param) 
    andalso lists:all( fun(X)->(find_f(_Module,X,Type_def)) end,
                    Param );
%% @TODO finish it
% improper_list(integer(),atom()) | maybe_improper_list(integer(),atom())

%% @doc Tuple :: tuple() | {} | {TList}
% tuple()
find_f(_Module,Param,{type,_L,tuple,any})->
    is_tuple(Param);
% {}
find_f(_Module,Param,{type,_L,tuple,[]})->
    is_tuple(Param)
    andalso tuple_size(Param)==0;
% tuple(TList) / {TList} , this code may be improved
find_f(_Module,Param,{type,_L,tuple,List_def})->
    is_tuple(Param) 
    andalso (tuple_size(Param)==length(List_def))
    andalso lists:all( fun({Par,Def})->(find_f(_Module,Par,Def)) end,
                    lists:zip(tuple_to_list(Param),List_def) );

%% @doc Union : union()
find_f(_Module,_,{type,_L,union,[]})->false;
find_f(_Module,Param,{type,_L,union,[H|T]})->
    (find_f(_Module,Param,H)) orelse (find_f(_Module,Param,{type,_L,union,T}));
%% @doc UserDefined <--see in the end, after alias

%% @doc [T,...] (the "nonempty_list")
find_f(_Module,Param,{type,_L,nonempty_list,Type_def})->
    find_f(_Module,Param,{type,_L,list,Type_def})
    andalso (length(Param)/=0);


%% ===========================================
%% @doc Built-in type / Alias
%% @TODO : when the structure will be ok, remove _V when they are useless
%%         and replace then by [] (in calls, for mfa, string,...)

% term()
find_f(_Module,_,{type,_L,term,[]})->true;

% boolean()
find_f(_Module,Param,{type,_L,boolean,[]})->
    (Param=:=true) orelse (Param=:=false);

% byte()
find_f(_Module,Param,{type,_L,byte,[]})->
    find_f(_Module,Param,{type,1,range,[{integer,1,0},{integer,1,255}]});

% char()
find_f(_Module,Param,{type,_L,char,[]})->
    find_f(_Module,Param,{type,1,range,[{integer,1,0},{integer,1,1114111}]});

% non_neg_integer()
find_f(_Module,Param,{type,_L,non_neg_integer,[]})->
    is_integer(Param)
    andalso (Param>=0);


% pos_integer()
find_f(_Module,Param,{type,_L,pos_integer,[]})->
    is_integer(Param)
    andalso (Param>0);
 
% neg_integer()
find_f(_Module,Param,{type,_L,neg_integer,[]})->
    is_integer(Param)
    andalso (Param<0);

% number()
find_f(_Module,Param,{type,_L,number,[]})->
    is_integer(Param) orelse is_float(Param);

% list() : see List

%% @TODO handel dynamically
% maybe_improper_list()
% maybe_improper_list(T)

% string()
find_f(_Module,Param,{type,_L,string,[]})->
    find_f(_Module,Param,{type,_L,list,[{type,_L,char,[]}] });

% nonempty_string()
find_f(_Module,Param,{type,_L,nonempty_string,[]})->
    find_f(_Module,Param,{type,_L,nonempty_list,[{type,_L,char,[]}] });

%% @TODO handel dynamically iolist()

% module()
find_f(_Module,Param,{type,_L,module,[]})->
    find_f(_Module,Param,{type,_L,atom,[]});

% mfa()
find_f(_Module,Param,{type,_L,mfa,[]})->
    find_f(_Module,Param,{type,_L,tuple,
        [{type,1,atom,[]},{type,1,atom,[]},{type,1,byte,[]}] 
                    });

% node()
find_f(_Module,Param,{type,_L,node,[]})->
    find_f(_Module,Param,{type,_L,atom,[]});

% timeout()
find_f(_Module,Param,{type,_L,timeout,[]})->
    find_f(_Module,Param,{atom,_L,infinity})
    orelse find_f(_Module,Param,{type,_L,non_neg_integer,[]});

% no_return()
find_f(_Module,Param,{type,_L,no_return,[]})->
    find_f(_Module,Param,{type,_L,none,[]});

%% ===========================================

% @doc record
% @TODO check if there is a way to add module name
find_f(_Module,Param,{type,_L,record,[{atom,_,Record_name}] })->
    is_record(Param,Record_name); 

%% @doc UserDefined type: In order to use the type defined by the user
find_f(_Module,Param,{type,_L,Type_name,Type_param})->
    apply(_Module,
        sheriff_string_generator:name_function(Type_name),
        [_Module|[Param|Type_param]]);

% @doc UserDefined type:
% In order to use the type defined by the user, which are in other modules
% NOTE: these modules should have been compiled using the 
% {parse_transform,sheriff} compiling options
find_f(_Module,Param,{remote_type,_L,[{atom,_,Type_module},{atom,_,Type_name},
		Type_param] })->
    apply(Type_module,
        sheriff_string_generator:name_function(Type_name),
	[_Module|[Param|Type_param]]).
