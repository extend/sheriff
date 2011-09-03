-module(sheriff_dynamic_generator).
-compile(export_all).

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
