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

%% @doc Module and operator of the call function to replace.
-define(MOD, sheriff).
-define(OPE, check).
%% @doc Prefix of the generate function.
-define(PREF, "is_").


%% @doc Function to call.
%% 
%% Search in the tree and replace call function ?MOD:?OPE by the appropriate
%% call.
-spec main(forms(), options()) -> forms().
main(Forms, Options) ->
	{NewForms,_} =
		parse_trans:depth_first(fun replace/4, [], Forms, Options),
	io:format("~p~n",[parse_trans:revert(NewForms)]),
	parse_trans:revert(NewForms).

%% @doc Return a tuple with the new tree
%% If the tree given is a call to ?MOD:?OPE the function returns a new tree
%% otherwise it returns the same tree.
-spec replace(atom(), forms(), options() ,forms()) -> {form(),forms()}.
replace(application, Form, _Ctxt, Acc) ->
	MFA = erl_syntax_lib:analyze_application(Form),
	case MFA of
		{?MOD, {?OPE, 2}} ->
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

%% @doc Build an Erlang tree of a call of the generate function.
-spec build(form(), form(), integer()) -> form().
build(Operator, Var, PosCall) ->
	case erl_syntax:type(Operator) of
		application ->
			Pos = erl_syntax:get_pos(Operator),
			Op = erl_syntax:application_operator(Operator),
			Args = erl_syntax:application_arguments(Operator),
			OpType = erl_syntax:type(Op),
			if OpType == module_qualifier ->
				Mod = erl_syntax:module_qualifier_argument(Op),
				Body = erl_syntax:module_qualifier_body(Op),
				FunName = erl_syntax:atom(?PREF ++ erl_syntax:atom_name(Body));
			true ->
				Mod = none,
				FunName = erl_syntax:atom(?PREF ++ erl_syntax:atom_name(Op))
			end,
			NewOp = hd(parse_trans:revert([erl_syntax:set_pos(FunName,Pos)])),
			if Args == [] ->
				erl_syntax:set_pos(erl_syntax:application(Mod,NewOp, [Var]),PosCall);
			true ->
				NewArg = hd(parse_trans:revert([erl_syntax:string(erl_prettypr:format(Operator))])),
				erl_syntax:set_pos(erl_syntax:application(Mod,NewOp, [Var,NewArg]),PosCall)
				%% @todo with Hamza version.
				%% NewArg = make_ast(erl_prettypr:format(Operator)),
				%% erl_syntax:set_pos(erl_syntax:application(Mod,NewOp, [Var|NewArg]),PosCall)
			end;
		_ ->
			%% @todo handle error when the 2nd argument is not a call.
			Operator
	end.
