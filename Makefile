ERL := erl -pa ebin -pa deps/*/ebin +Bc +K true -smp enable ${ERL_ARGS}

all:
	rebar get-deps compile

quick:
	rebar skip_deps=true compile

run:
	${ERL} -s classifier_app; \

clean:
	rm -f test/*.beam
	rebar clean

doc: all
	rebar skip_deps=true doc

xref: all
	rebar skip_deps=true xref

test: quick
	mkdir -p ./log/ct
	rebar -v skip_deps=true ct ${CT_ARGS}; \
