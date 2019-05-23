FRONTEND_API := "console/gen/Herd/Console/Api.elm"

all: backend ui

setup:
	@stack setup
	@cd console && make setup

clean:
	@stack clean
	@cd console && make clean

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