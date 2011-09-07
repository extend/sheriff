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

-module(sheriff_check_call).
-export([main/2]).

-type form() :: any().
-type forms() :: [form()].
-type options() :: [{atom(), any()}].


%% @doc Function to call.
%% 
%% Search in the tree and replace call function sheriff:check by the appropriate
%% call.
-spec main(forms(), options()) -> forms().
main(Forms, Options) ->
	{NewForms,_} =
		parse_trans:depth_first(fun replace/4, [], Forms, Options),
	parse_trans:revert(NewForms).

%% @doc Return a tuple with the new tree
%% If the tree given is a call to ?MOD:?OPE the function returns a new tree
%% otherwise it returns the same tree.
-spec replace(atom(), forms(), options() ,forms()) -> {form(),forms()}.
replace(application, Form, _Ctxt, Acc) ->
	MFA = erl_syntax_lib:analyze_application(Form),
	case MFA of
		{sheriff, {check, 2}} ->
			Args = erl_syntax:application_arguments(Form),
			RevArgs = parse_trans:revert(Args),
			Pos = erl_syntax:get_pos(Form),
			Var = hd(RevArgs),
			Operator = lists:nth(2, RevArgs),
			NewCall = build(Operator,Var,Pos),
			{NewCall, Acc};
		_ ->
			{Form, Acc}
	end;
replace(_, Form, _Ctxt, Acc) ->
	{Form, Acc}.

%% @doc Build an Erlang tree of a call to the generate function.
-spec build(form(), form(), integer()) -> form().
build(Operator, Var, PosCall) ->
	case erl_syntax:type(Operator) of
		application ->
			%%Pos = erl_syntax:get_pos(Operator),
			Op = erl_syntax:application_operator(Operator),
			Args = erl_syntax:application_arguments(Operator),
			{Mod,FunName} = get_new_name(Op),
			NewOp = build_atom(FunName),
			if Args == [] ->
				NewArgs = [Var];
			true ->
				NewArgs = [Var|make_ast(erl_prettypr:format(Operator))]
			end,
			build_call(Mod,NewOp,NewArgs,PosCall);
		_ ->
			%% @todo Manage errors when the 2nd argument is not a call.
			Operator
	end.

%% @doc Return the module and the name of the new call
-spec get_new_name(form()) -> tuple().
get_new_name(Operator) ->
	Type = erl_syntax:type(Operator),
	if Type == module_qualifier ->
		Mod = erl_syntax:module_qualifier_argument(Operator),
		Body = erl_syntax:module_qualifier_body(Operator),
		Name= "sheriff_$_" ++ erl_syntax:atom_name(Body);
	true ->
		Mod = none,
		Name= "sheriff_$_" ++ erl_syntax:atom_name(Operator)
	end,
	{Mod,Name}.

%% @doc Build an atom Erlang tree
-spec build_atom(string()) -> form().
build_atom(Name) ->
	hd(parse_trans:revert([erl_syntax:atom(Name)])).

%% @doc Build a call in Erlang tree and set his position
-spec build_call(none|form(),form(),forms(),integer()) -> form().
build_call(Module,Operator,ArgsList,Pos) ->
	erl_syntax:set_pos(erl_syntax:application(Module,Operator, ArgsList),Pos).

%% @doc Build a list Erlang tree
-spec make_ast(string()) -> form().
make_ast(A)->
	Full_type=lists:append(["-type sheriff()::",A,"."]),
	{_,Tokens,_}=erl_scan:string(Full_type),
	{_,Ast}=erl_parse:parse_form(Tokens),
	send_ast(Ast).

%% @doc Return an Erlang tree
-spec send_ast(form()) -> list().
send_ast({attribute,_,type,{sheriff,{type,_,_Type_name,List},[]}})->
	lists:map( fun(X)->erl_syntax:revert(erl_syntax:abstract(X)) end, List );
send_ast({attribute,_,type,{sheriff,{remote_type,_,
		[{atom,_,_Type_module},{atom,_,_Type_name},List ]},[]}})->
	lists:map( fun(X)->erl_syntax:revert(erl_syntax:abstract(X)) end, List ).
