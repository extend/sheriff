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
build_f({attribute,_L,type,{Type_name,Type_def,List_of_type_arg}})->{function}.


%% for build_f/3, the third param may be removed someday, or changed.
-spec build_f(atom(),type_definition_ast(),[any()])->function_ast().
build_f(_,{var,_L,'_'},_)->{atom,1,true}.