-module(sheriff_static_generator).
-compile(export_all).


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

% specific value of: atom() , integer() , float() , string() , call are_eq
% TODO to remove, and add negative numbers
% this part isn't static, will be fixed soon
build_f(Param,{atom,_L,Val},_)->{call,1,
			      {remote,1,{atom,1,sheriff_static_generator},
			      {atom,1,are_eq}},
               		      [{var,1,Param},{atom,1,Val}]
			    };
build_f(Param,{integer,_L,Val},_)->{call,1,
			         {remote,1,{atom,1,sheriff_static_generator},
				 {atom,1,are_eq}},
               		         [{var,1,Param},{integer,1,Val}]
			       };
build_f(Param,{float,_L,Val},_)->{call,1,
			       {remote,1,{atom,1,sheriff_static_generator},
			       {atom,1,are_eq}},
               		       [{var,1,Param},{float,1,Val}]
			     };
build_f(Param,{string,_L,Val},_)->{call,1,
			        {remote,1,{atom,1,sheriff_static_generator},
				{atom,1,are_eq}},
               		        [{var,1,Param},{string,1,Val}]
			      };
build_f(_,{var,_L,'_'},_)->{atom,1,true};

% range  (ex: -10..10) , call is_between
% TODO to remove, and make it statical,will be fixed soon
build_f(Param,{type,_L,range,List},_)->
    {call,1,
      {remote,1,{atom,1,sheriff_static_generator},{atom,1,is_between}},
      [{var,1,Param}|List]
    };

% atom() , integer() , float() , binary()
% to change if thing like -type int(A)::integer(A)|integer(5).
build_f(Param,{type,_L,atom,[]},_)->
    {call,1,{atom,1,is_atom},[{var,1,Param}]};
build_f(Param,{type,_L,integer,[]},_)->
    {call,1,{atom,1,is_integer},[{var,1,Param}]};
build_f(Param,{type,_L,float,[]},_)->
    {call,1,{atom,1,is_float},[{var,1,Param}]};
build_f(Param,{type,_L,binary,[]},_)->
    {call,1,{atom,1,is_binary},[{var,1,Param}]};

% union
build_f(_,{type,_L,union,[]},_)->{atom,1,false};
build_f(Param,{type,_L,union,[H|T]},List_of_type_arg)->
    {op,1,'orelse',build_f(Param,H,List_of_type_arg),
		   build_f(Param,{type,_L,union,T},List_of_type_arg)
    };

% tuple , this code is not as elegent as the one for union
% -type a()::{} make stranges reactions
build_f(Param,{type,_L,tuple,List_def},List_of_type_arg)->
    {op,1,'andalso',
        {call,1,{atom,1,is_tuple},[{var,1,Param}]},
        {op,1,'andalso',
            {op,1,'==',
                {call,1,{atom,1,length},
		    [{call,1,{atom,1,tuple_to_list},[{var,1,Param}]}]},
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

%%---------------------------------------------------
%%---------------------------------------------------

%TODO check build_f/3, thid function will be removed soon
are_eq(X,A) -> (X=:=A).
is_between(X,A,B)-> is_integer(X) andalso (A=<X) andalso (X=<B).
