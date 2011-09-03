-module(sheriff_dynamic_generator).
-compile(export_all).

-type type_definition_ast()::tuple().

-spec find_f(any(),type_definition_ast()).

% specific value of: atom() , integer() , float() , string()
% add negative numbers
find_f(Param,{atom,_L,Val})->
    Param=:=Val;
find_f(Param,{integer,_L,Val})->
    Param=:=Val;
find_f(Param,{float,_L,Val})->
    Param=:=Val;
find_f(Param,{string,_L,Val})->
    Param=:=Val;

find_f(_,{var,_L,'_'})->true;

% range  (ex: -10..10) 
find_f(Param,{type,_,range,[{integer,_,Deb},{integer,_,Fin}]})->
    is_integer(Param) andalso (Deb=<Param) andalso (Param=<Fin);
find_f(Param,{type,_,range,[{op,_,'-',{integer,_,Deb}},{integer,_,Fin}]})->
    is_integer(Param) andalso (Param=<Deb) andalso (Param=<Fin);
find_f(Param,{type,_,range,[{integer,_,Deb},{op,_,'-',{integer,_,Fin}}]})->
    is_integer(Param) andalso (Deb=<Param) andalso (Fin=<Param);
find_f(Param,{type,_,range,[{op,_,'-',{integer,_,Deb}},
                                            {op,_,'-',{integer,_,Fin}}]})->
    is_integer(Param) andalso (Param=<Deb) andalso (Fin=<Param);

% atom() , integer() , float() , binary()
% to change if thing like -type int(A)::integer(A)|integer(5).
find_f(Param,{type,_L,atom,[]})->
    is_atom(Param);
find_f(Param,{type,_L,integer,[]})->
    is_integer(Param);
find_f(Param,{type,_L,float,[]})->
    is_float(Param);
find_f(Param,{type,_L,binary,[]})->
    is_binary(Param);

% union
find_f(_,{type,_L,union,[]})->false;
find_f(Param,{type,_L,union,[H|T]})->
    (find_f(Param,H)) orelse (find_f(Param,{type,_L,union,T}));

% tuple 
find_f(Param,{type,_L,tuple,List_def})->
    is_tuple(Param) 
    andalso (length(tuple_to_list(Param))==length(List_def))
    andalso lists:all( fun({Par,Def})->(find_f(Par,Def)) end,
                    lists:zip(tuple_to_list(Param),List_def) );

% list
find_f(Param,{type,_L,list,[Type_def]})->
    is_list(Param) 
    andalso lists:all( fun(X)->(find_f(X,Type_def)) end,
                    Param );

% For using types defined by the user.
find_f(Param,{type,_L,Type_name,[Param|Type_param]})->
    apply(sheriff_string_generator:name_function(Type_name),Type_param);

% For using types exported by other modules.
% NOTE:
% -these modules should have been compile using the {parse_transform,sheriff}
% compiling options, with "same names" for the function prefix(name_function())
find_f(Param,{remote_type,_L,[{atom,_,Type_module},{atom,_,Type_name},
		Type_param] })->
    apply(Type_module,
        sheriff_string_generator:name_function(Type_name),
	[Param|Type_param]).