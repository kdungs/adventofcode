# Convenience makefile around Bazel and linting tools.

BUILD_FILES := $(wildcard **/BUILD)
CPP_FILES := $(wildcard **/*.h) $(wildcard **/*.cc)

build:
	bazel build ...

test:
	bazel test ...

format: buildifier clang-format

lint: buildifier-check clang-format-check cpplint-check

buildifier: $(BUILD_FILES)
	buildifier $^

buildifier-check: $(BUILD_FILES)
	buildifier -lint $^

clang-format: $(CPP_FILES)
	clang-format -i $^

clang-format-check: $(CPP_FILES)
	clang-format -Werror -n $^

cpplint-check: $(CPP_FILES)
	cpplint $^

compile_commands.json: build
	python tools/actions/generate_compile_commands_json.py
