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

% specific value of: atom() , integer() , float() , string()
% add negative numbers
find_f(_Module,Param,{atom,_L,Val})->
    Param=:=Val;
find_f(_Module,Param,{integer,_L,Val})->
    Param=:=Val;
find_f(_Module,Param,{float,_L,Val})->
    Param=:=Val;
find_f(_Module,Param,{string,_L,Val})->
    Param=:=Val;

find_f(_Module,_,{var,_L,'_'})->true;

% range  (ex: -10..10) 
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

% atom() , integer() , float() , binary()
% to change if thing like -type int(A)::integer(A)|integer(5).
find_f(_Module,Param,{type,_L,atom,[]})->
    is_atom(Param);
find_f(_Module,Param,{type,_L,integer,[]})->
    is_integer(Param);
find_f(_Module,Param,{type,_L,float,[]})->
    is_float(Param);
find_f(_Module,Param,{type,_L,binary,[]})->
    is_binary(Param);

% union
find_f(_Module,_,{type,_L,union,[]})->false;
find_f(_Module,Param,{type,_L,union,[H|T]})->
    (find_f(_Module,Param,H)) orelse (find_f(_Module,Param,{type,_L,union,T}));

% tuple 
find_f(_Module,Param,{type,_L,tuple,any})->
    is_tuple(Param);
find_f(_Module,Param,{type,_L,tuple,List_def})->
    is_tuple(Param) 
    andalso (length(tuple_to_list(Param))==length(List_def))
    andalso lists:all( fun({Par,Def})->(find_f(_Module,Par,Def)) end,
                    lists:zip(tuple_to_list(Param),List_def) );

% list
find_f(_Module,Param,{type,_L,list,[]})->
    is_list(Param);
find_f(_Module,Param,{type,_L,list,[Type_def]})->
    is_list(Param) 
    andalso lists:all( fun(X)->(find_f(_Module,X,Type_def)) end,
                    Param );

% For using types defined by the user.
find_f(_Module,Param,{type,_L,Type_name,Type_param})->
    apply(_Module,
        sheriff_string_generator:name_function(Type_name),
        [_Module|[Param|Type_param]]);

% For using types exported by other modules.
% NOTE:
% -these modules should have been compile using the {parse_transform,sheriff}
% compiling options, with "same names" for the function prefix(name_function())
find_f(_Module,Param,{remote_type,_L,[{atom,_,Type_module},{atom,_,Type_name},
		Type_param] })->
    apply(Type_module,
        sheriff_string_generator:name_function(Type_name),
	[_Module|[Param|Type_param]]).