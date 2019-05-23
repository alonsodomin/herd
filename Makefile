all: backend ui

setup:
	@stack setup
	@cd console && make setup

clean:
	@stack clean
	@cd console && make clean

backend:
	@stack test

ui:
	@cd console && make build