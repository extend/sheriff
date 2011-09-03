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
build_f(_,{var,_L,'_'},_)->{atom,1,true}.


%%---------------------------------------------------
%%---------------------------------------------------

%TODO check build_f/3, thid function will be removed soon
are_eq(X,A) -> (X=:=A).
