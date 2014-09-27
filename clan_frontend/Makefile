.PHONY: run
run: deps
	rails server

deps: deps/bert deps/bertrpc
	mkdir -p deps
deps/bert:
	cd deps && git clone https://github.com/mojombo/bert.git
deps/bertrpc:
	cd deps && git clone https://github.com/mojombo/bertrpc.git
