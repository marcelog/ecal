CWD=$(shell pwd)
NAME=$(shell basename ${CWD})
REBAR?=./rebar

all: clean compile edoc release
	${REBAR} compile

deps:
	export GIT_SSL_NO_VERIFY=true
	${REBAR} get-deps

edoc:
	${REBAR} doc

compile:
	${REBAR} compile

test: compile
	${REBAR} eunit skip_deps=true

release: test
	(cd rel && .${REBAR} generate && cd -)

node:
	(cd rel && .${REBAR} create-node nodeid=${NAME} && cd -)

clean:
	${REBAR} clean
	rm -rf rel/${NAME}

run:
	rel/${NAME}/bin/${NAME} start

runconsole:
	rel/${NAME}/bin/${NAME} console

alldev: clean all runconsole
