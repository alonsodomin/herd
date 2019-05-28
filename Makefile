BASE_DIR := $(shell pwd)
DIST_DIR := $(BASE_DIR)/dist

STACK_WORK_DIR := $(BASE_DIR)/.stack-work

CONSOLE_DIR := $(BASE_DIR)/console
CONSOLE_GEN_DIR := $(CONSOLE_DIR)/gen
CONSOLE_SRC_DIR := $(CONSOLE_DIR)/src
CONSOLE_MAIN := $(CONSOLE_DIR)/Main.elm
CONSOLE_VIEW := $(DIST_DIR)/main.html
CONSOLE_APP := $(DIST_DIR)/main.js

ELM_STUFF := $(CONSOLE_DIR)/elm-stuff
NPM_MODULES := $(CONSOLE_DIR)/node_modules
NPM_TOOLS_DIR := $(NPM_MODULES)/.bin

REMOTE_API := $(CONSOLE_GEN_DIR)/Herd/Console/Remote.elm

ELM := $(NPM_TOOLS_DIR)/elm
ELM_FORMAT := $(NPM_TOOLS_DIR)/elm-format
UGLIFY := $(NPM_TOOLS_DIR)/uglifyjs

NPM_DEV_TOOLS := $(ELM) $(ELM_FORMAT) $(UGLIFY)

all: backend ui

# Setup build environment

$(NPM_DEV_TOOLS):
	@cd $(CONSOLE_DIR) && npm install

$(STACK_WORK_DIR):
	@stack setup

setup: $(STACK_WORK_DIR) $(NPM_DEV_TOOLS)

# Clean environment

backend-clean:
	@stack clean

ui-clean:
	@rm -fr $(DIST_DIR) $(ELM_STUFF) $(CONSOLE_GEN_DIR)

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
	mkdir -p $(@D) && stack exec herd-node-codegen

$(DIST_DIR):
	@mkdir -p $(DIST_DIR)

$(CONSOLE_VIEW): $(DIST_DIR)
	@cp $(CONSOLE_DIR)/static/* $(DIST_DIR)/

$(CONSOLE_APP): $(NPM_DEV_TOOLS) $(CONSOLE_VIEW) $(REMOTE_API) $(CONSOLE_MAIN)
	@cd $(CONSOLE_DIR) && $(ELM) make $(CONSOLE_MAIN) --output=$(CONSOLE_APP)

ui: $(CONSOLE_APP)

uglify: $(CONSOLE_APP)
	$(UGLIFY) $(CONSOLE_APP) --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' --output=$(CONSOLE_APP) && $(UGLIFY) $(CONSOLE_APP) --mangle --output=$(CONSOLE_APP)

# Misc

test: all
	@stack test

install: test uglify
	@stack install

fmt:
	@./stylize.sh
	$(ELM_FORMAT) $(CONSOLE_SRC_DIR)/ --yes