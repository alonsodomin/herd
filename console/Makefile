BASE_DIR := $(shell pwd)
BUILD_FOLDER := $(BASE_DIR)/build
GEN_FOLDER := $(BASE_DIR)/gen
ELM_STUFF := $(BASE_DIR)/elm-stuff

all: build

setup:
	@npm install -g elm@elm0.19.0 elm-format@elm0.19.0
	@npm install -g uglify-js

$(BUILD_FOLDER):
	@mkdir -p $(BUILD_FOLDER)
	@cp $(BASE_DIR)/static/* $(BUILD_FOLDER)/

$(BUILD_FOLDER)/main.js: $(BUILD_FOLDER) $(GEN_FOLDER)/Herd/Console/Remote.elm
	@elm make Main.elm --output=$(BUILD_FOLDER)/main.js

build: $(BUILD_FOLDER)/main.js

uglify: $(BUILD_FOLDER)/main.js
	@uglifyjs $(BUILD_FOLDER)/main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' --output=$(BUILD_FOLDER)/main.js && uglifyjs $(BUILD_FOLDER)/main.js --mangle --output=$(BUILD_FOLDER)/main.js

clean:
	@rm -fr $(BUILD_FOLDER) $(ELM_STUFF) $(GEN_FOLDER)