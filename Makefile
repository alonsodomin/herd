PROJECT_NAME := herd

BASE_DIR := $(shell pwd)
ETC_DIR := $(BASE_DIR)/etc
BASE_DIST_DIR := $(BASE_DIR)/dist
DIST_DIR := $(BASE_DIST_DIR)/$(PROJECT_NAME)
DIST_BIN_FILE := $(BASE_DIST_DIR)/$(PROJECT_NAME)-bin.zip

STACK_WORK_DIR := $(BASE_DIR)/.stack-work

CONSOLE_DIR := $(BASE_DIR)/console
CONSOLE_GEN_DIR := $(CONSOLE_DIR)/gen
CONSOLE_TESTS_DIR := $(CONSOLE_DIR)/tests
CONSOLE_SRC_DIR := $(CONSOLE_DIR)/src
CONSOLE_MAIN := $(CONSOLE_DIR)/Main.elm
CONSOLE_DIST_DIR := $(DIST_DIR)/console
CONSOLE_VIEW := $(CONSOLE_DIST_DIR)/main.html
CONSOLE_APP := $(CONSOLE_DIST_DIR)/main.js

MDC_DIR := $(CONSOLE_DIR)/elm-mdc
MDC_DEPS := $(CONSOLE_DIST_DIR)/elm-mdc.js $(CONSOLE_DIST_DIR)/material-components-web.css

ELM_STUFF := $(CONSOLE_DIR)/elm-stuff
NPM_MODULES := $(CONSOLE_DIR)/node_modules
NPM_TOOLS_DIR := $(NPM_MODULES)/.bin

REMOTE_API := $(CONSOLE_GEN_DIR)/Herd/Console/Remote.elm

ELM := $(NPM_TOOLS_DIR)/elm
ELM_FORMAT := $(NPM_TOOLS_DIR)/elm-format
ELM_TEST := $(NPM_TOOLS_DIR)/elm-test
UGLIFY := $(NPM_TOOLS_DIR)/uglifyjs

NPM_DEV_TOOLS := $(ELM) $(ELM_FORMAT) $(UGLIFY)

all: dist

# Setup build environment

$(ELM):
	@cd $(CONSOLE_DIR) && npm install

$(ELM_FORMAT):
	@cd $(CONSOLE_DIR) && npm install

$(ELM_TEST):
	@cd $(CONSOLE_DIR) && npm install

$(UGLIFY):
	@cd $(CONSOLE_DIR) && npm install

$(STACK_WORK_DIR):
	@stack setup

setup: $(STACK_WORK_DIR) $(NPM_DEV_TOOLS)

# Clean environment

backend-clean:
	@stack clean

ui-clean:
	@rm -fr $(BASE_DIST_DIR) $(ELM_STUFF) $(CONSOLE_GEN_DIR)

clean: backend-clean ui-clean

stack-clean:
	@rm -fr $(STACK_WORK_DIR)

npm-clean:
	@rm -fr $(NPM_MODULES)

clean-all: clean stack-clean npm-clean

# Build targets

backend: $(STACK_WORK_DIR)
	@stack build

$(REMOTE_API): backend
	mkdir -p $(@D) && stack exec herd-node-codegen -- -d $(CONSOLE_GEN_DIR) -o "Herd.Console.Remote"

$(CONSOLE_DIST_DIR):
	mkdir -p $(@D)

$(MDC_DIR)/elm-mdc.js: $(MDC_DIR)/Makefile
	@cd $(MDC_DIR) && make elm-mdc.js

$(MDC_DIR)/material-components-web.css: $(MDC_DIR)/Makefile
	@cd $(MDC_DIR) && make material-components-web.css

$(CONSOLE_VIEW): $(CONSOLE_DIST_DIR)
	@cp $(CONSOLE_DIR)/static/* $(CONSOLE_DIST_DIR)/

$(CONSOLE_DIST_DIR)/elm-mdc.js: $(MDC_DIR)/elm-mdc.js
	cp $(MDC_DIR)/elm-mdc.js $(CONSOLE_DIST_DIR)/elm-mdc.js

$(CONSOLE_DIST_DIR)/material-components-web.css: $(MDC_DIR)/material-components-web.css
	cp $(MDC_DIR)/material-components-web.css $(CONSOLE_DIST_DIR)/material-components-web.css

$(CONSOLE_APP): $(ELM) $(REMOTE_API) $(CONSOLE_MAIN)
	@cd $(CONSOLE_DIR) && $(ELM) make $(CONSOLE_MAIN) --output=$(CONSOLE_APP)

ui: $(CONSOLE_APP) $(CONSOLE_VIEW) $(MDC_DEPS)

uglify: $(CONSOLE_APP) $(UGLIFY)
	$(UGLIFY) $(CONSOLE_APP) --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' --output=$(CONSOLE_APP)
	$(UGLIFY) $(CONSOLE_APP) --mangle --output=$(CONSOLE_APP)

# Testing tagets

backend-test: $(STACK_WORK_DIR)
	@stack test

ui-test: $(ELM_TEST) ui
	@cd $(CONSOLE_DIR) && $(ELM_TEST) --compiler $(ELM)

test: backend-test ui-test

# Misc

$(DIST_BIN_FILE): test uglify
	zip $(DIST_BIN_FILE) $(DIST_DIR)

$(DIST_DIR)/etc:
	@cp -r $(ETC_DIR) $(DIST_DIR)

dist: $(DIST_BIN_FILE) $(DIST_DIR)/etc

install: dist
	@stack install

fmt: $(ELM_FORMAT)
	@./stylize.sh
	@cd $(CONSOLE_DIR) && $(ELM_FORMAT) $(CONSOLE_SRC_DIR)/ --yes
	@cd $(CONSOLE_DIR) && $(ELM_FORMAT) $(CONSOLE_TESTS_DIR)/ --yes