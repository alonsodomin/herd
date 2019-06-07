PROJECT_NAME := herd

BASE_DIR := $(shell pwd)
ETC_DIR := $(BASE_DIR)/etc
BASE_DIST_DIR := $(BASE_DIR)/dist
DIST_DIR := $(BASE_DIST_DIR)/$(PROJECT_NAME)
DIST_BIN_FILE := $(BASE_DIST_DIR)/$(PROJECT_NAME)-bin.zip
DIST_CONSOLE_DIR := $(BASE_DIST_DIR)/$(PROJECT_NAME)/console

STACK_WORK_DIR := $(BASE_DIR)/.stack-work

CONSOLE_DIR := $(BASE_DIR)/purs-console
CONSOLE_DIST_DIR := $(CONSOLE_DIR)/dist
CONSOLE_GEN_DIR := $(CONSOLE_DIR)/gen
CONSOLE_VIEW := $(CONSOLE_DIST_DIR)/index.html
CONSOLE_APP := $(CONSOLE_DIST_DIR)/app.js

NPM_MODULES := $(CONSOLE_DIR)/node_modules
NPM_TOOLS_DIR := $(NPM_MODULES)/.bin
NPM := $(shell which npm)

REMOTE_API := $(CONSOLE_GEN_DIR)/Herd/Console/Remote.elm

UGLIFY := $(NPM_TOOLS_DIR)/uglifyjs

all: dist

# Setup build environment

$(NPM_MODULES): $(NPM)
	@cd $(CONSOLE_DIR) && npm install

$(UGLIFY):
	@cd $(CONSOLE_DIR) && npm install

$(STACK_WORK_DIR):
	@stack setup

setup: $(STACK_WORK_DIR) $(NPM)

# Clean environment

backend-clean:
	@stack clean

ui-clean: $(NPM_MODULES)
	@cd $(CONSOLE_DIR) && $(NPM) run clean

clean: backend-clean ui-clean

backend-cache-clean:
	@rm -fr $(STACK_WORK_DIR)

ui-cache-clean:
	@rm -fr $(NPM_MODULES)

clean-cache: backend-cache-clean ui-cache-clean

clean-all: clean clean-cache

# Build targets

backend: $(STACK_WORK_DIR)
	@stack build

$(REMOTE_API): backend
	@mkdir -p $(@D)
	stack exec herd-purs-codegen -- -d $(CONSOLE_GEN_DIR) -o "Herd.Console.Remote"

$(CONSOLE_DIST_DIR):
	mkdir -p $(CONSOLE_DIST_DIR)

$(CONSOLE_APP): $(NPM_MODULES) $(REMOTE_API) $(CONSOLE_DIST_DIR)
	@cd $(CONSOLE_DIR) && $(NPM) run dist

ui: $(CONSOLE_APP)

uglify: ui $(UGLIFY)
	$(UGLIFY) $(CONSOLE_APP) --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' --output=$(CONSOLE_APP)
	$(UGLIFY) $(CONSOLE_APP) --mangle --output=$(CONSOLE_APP)

# Testing tagets

backend-test: $(STACK_WORK_DIR)
	@stack test

ui-test: $(NPM_MODULES) ui
	@cd $(CONSOLE_DIR) && $(NPM) run test

test: backend-test ui

# Misc

$(DIST_DIR):
	@mkdir -p $(DIST_DIR)

$(DIST_BIN_FILE): test

$(DIST_DIR)/etc: $(DIST_DIR)
	@cp -r $(ETC_DIR) $(DIST_DIR)

$(DIST_CONSOLE_DIR)/app.js: $(DIST_DIR)
	@mkdir -p $(DIST_CONSOLE_DIR)
	cp -r $(CONSOLE_DIST_DIR)/* $(DIST_CONSOLE_DIR)/

dist: $(DIST_BIN_FILE) $(DIST_DIR)/etc $(DIST_CONSOLE_DIR)/app.js
	zip $(DIST_BIN_FILE) $(DIST_DIR)

install: dist
	@stack install

fmt: $(ELM_FORMAT)
	@./stylize.sh
	#@cd $(CONSOLE_DIR) && $(ELM_FORMAT) $(CONSOLE_SRC_DIR)/ --yes
	#@cd $(CONSOLE_DIR) && $(ELM_FORMAT) $(CONSOLE_TESTS_DIR)/ --yes
