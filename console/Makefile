BASE_DIR := $(shell pwd)
BUILD_FOLDER := $(BASE_DIR)/dist
GEN_FOLDER := $(BASE_DIR)/gen
ELM_STUFF := $(BASE_DIR)/elm-stuff
NPM_MODULES := $(BASE_DIR)/node_modules

DEV_TOOLS_DIR := $(NPM_MODULES)/.bin

ELM := $(DEV_TOOLS_DIR)/elm
ELM_FORMAT := $(DEV_TOOLS_DIR)/elm-format
UGLIFY := $(DEV_TOOLS_DIR)/uglifyjs
DEV_TOOLS := $(ELM) $(ELM_FORMAT) $(UGLIFY)

all: uglify

$(DEV_TOOLS):
	@npm install

setup: $(DEV_TOOLS)

$(BUILD_FOLDER):
	@mkdir -p $(BUILD_FOLDER)
	@cp $(BASE_DIR)/static/* $(BUILD_FOLDER)/

$(BUILD_FOLDER)/main.js: $(DEV_TOOLS) $(BUILD_FOLDER) $(GEN_FOLDER)/Herd/Console/Remote.elm
	$(ELM) make Main.elm --output=$(BUILD_FOLDER)/main.js

build: $(BUILD_FOLDER)/main.js

uglify: $(BUILD_FOLDER)/main.js
	$(UGLIFY) $(BUILD_FOLDER)/main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' --output=$(BUILD_FOLDER)/main.js && uglifyjs $(BUILD_FOLDER)/main.js --mangle --output=$(BUILD_FOLDER)/main.js

clean:
	@rm -fr $(BUILD_FOLDER) $(ELM_STUFF) $(GEN_FOLDER) $(NPM_MODULES)

fmt:
	$(ELM_FORMAT) src/ --yes