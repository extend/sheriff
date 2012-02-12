# See LICENSE for licensing information.

DIALYZER = dialyzer
REBAR = rebar

all: app

app: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

tests: clean app ct

ct:
	@$(REBAR) ct skip_deps=true

build-plt:
	@$(DIALYZER) --build_plt --output_plt .sheriff_dialyzer.plt \
		--apps kernel stdlib

dialyze:
	@$(DIALYZER) --src src --plt .sheriff_dialyzer.plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs

docs:
	@$(REBAR) doc
