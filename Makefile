FRONTEND_API := console/gen/Herd/Console/Remote.elm

all: backend ui

setup:
	@stack setup
	@cd console && make setup

backend-clean:
	@stack clean

ui-clean:
	@cd console && make clean

clean: backend-clean ui-clean

backend:
	@stack build

$(FRONTEND_API): backend
	mkdir -p $(@D) && stack exec herd-node-codegen

ui: $(FRONTEND_API)
	@cd console && make build

test: all
	@stack test

install: test
	@stack install

fmt:
	@./stylize.sh
	@cd console && make fmt