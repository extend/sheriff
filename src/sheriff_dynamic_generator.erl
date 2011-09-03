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