PROJECT_NAME := herd

BASE_DIR         := $(shell pwd)
ETC_DIR          := $(BASE_DIR)/etc
BASE_DIST_DIR    := $(BASE_DIR)/dist
DIST_DIR         := $(BASE_DIST_DIR)/$(PROJECT_NAME)
DIST_BIN_FILE    := $(BASE_DIST_DIR)/$(PROJECT_NAME)-bin.zip
DIST_CONSOLE_DIR := $(BASE_DIST_DIR)/$(PROJECT_NAME)/console

STACK_WORK_DIR := $(BASE_DIR)/.stack-work

CONSOLE_DIR      := $(BASE_DIR)/console
CONSOLE_DIST_DIR := $(CONSOLE_DIR)/dist
CONSOLE_GEN_DIR  := $(CONSOLE_DIR)/gen
CONSOLE_VIEW     := $(CONSOLE_DIST_DIR)/index.html
CONSOLE_APP      := $(CONSOLE_DIST_DIR)/app.js

NPM_MODULES      := $(CONSOLE_DIR)/node_modules
NPM_TOOLS_DIR    := $(NPM_MODULES)/.bin
NPM_PACKAGE_JSON := $(CONSOLE_DIR)/package.json
YARN             := $(shell which yarn)

REMOTE_API := $(CONSOLE_GEN_DIR)/Herd/Console/Remote.elm

.PHONY: all dist setup dist-clean backend-clean backend-cache-clean ui-clean ui-cache-clean clean clean-cache clean-all backend backend-test ui ui-test test install fmt

all: dist

# Setup build environment

$(NPM_MODULES): $(YARN) $(NPM_PACKAGE_JSON)
	@cd $(CONSOLE_DIR) && yarn install

$(STACK_WORK_DIR):
	@stack setup

setup: $(STACK_WORK_DIR) $(NPM_MODULES)

# Clean environment

backend-clean:
	@stack clean

ui-clean: $(NPM_MODULES)
	@cd $(CONSOLE_DIR) && $(YARN) clean

clean: backend-clean ui-clean

backend-cache-clean:
	@rm -fr $(STACK_WORK_DIR)

ui-cache-clean:
	@rm -fr $(NPM_MODULES)

dist-clean:
	rm -fr $(BASE_DIST_DIR)

clean-cache: backend-cache-clean ui-cache-clean

clean-all: clean clean-cache dist-clean

# Build targets

backend: $(STACK_WORK_DIR)
	@stack build

$(REMOTE_API): backend
	@mkdir -p $(@D)
	stack exec herd-purs-codegen -- -d $(CONSOLE_GEN_DIR) -o "Herd.Console.Remote"

$(CONSOLE_DIST_DIR):
	mkdir -p $(CONSOLE_DIST_DIR)

$(CONSOLE_APP): $(NPM_MODULES) $(REMOTE_API) $(CONSOLE_DIST_DIR)
	@cd $(CONSOLE_DIR) && $(YARN) dist

ui: $(CONSOLE_APP)

# Testing tagets

backend-test: $(STACK_WORK_DIR)
	@stack test

ui-test: $(NPM_MODULES) ui
	@cd $(CONSOLE_DIR) && $(YARN) test

test: backend-test ui-test

# Misc

$(DIST_DIR):
	@mkdir -p $(DIST_DIR)

$(DIST_DIR)/etc: $(DIST_DIR) $(ETC_DIR)
	@cp -r $(ETC_DIR) $(DIST_DIR)

$(DIST_CONSOLE_DIR)/app.js: $(DIST_DIR) $(CONSOLE_APP)
	@mkdir -p $(DIST_CONSOLE_DIR)
	cp -r $(CONSOLE_DIST_DIR)/* $(DIST_CONSOLE_DIR)/

$(DIST_BIN_FILE): test $(DIST_DIR)/etc $(DIST_CONSOLE_DIR)/app.js
	cd $(BASE_DIST_DIR) && zip -r $(DIST_BIN_FILE) $(PROJECT_NAME)

dist: $(DIST_BIN_FILE)

install: dist
	@stack install

fmt: stylize.sh
	@./stylize.sh
