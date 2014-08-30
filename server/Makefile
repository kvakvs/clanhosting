APPS := clanhosting libe
RUNCMD := erl -sname clanhosting@localhost -config ch.config -pa ebin apps/*/ebin deps/*/ebin -s ch_app
HERE := $(shell pwd)

.PHONY: all
all: deps compile

.PHONY: run
run: compile
	$(RUNCMD)

.PHONY: compile
compile:
	rebar compile skip_deps=true

.PHONY: deps
deps:
	rebar get-deps

.PHONY: clean
clean:
	rebar clean

.PHONY: test
test:
	rebar skip_deps=true eunit
#	rebar compile eunit

.PHONY: rel
rel: deps
	rebar compile generate -f

OTP_PLT   = $(HOME)/.ch_otp.plt
COMBO_PLT = $(HOME)/.ch_combo.plt
PLT_LIBS0 = $(wildcard apps/*/ebin) $(wildcard deps/*/ebin)
PLT_LIBS  = $(subst deps/riak_pb/ebin,,$(PLT_LIBS0))

DIALYZER_APPS = $(APPS)
DIALYZER_APPS_PATHS = $(addsuffix /ebin, $(addprefix apps/, $(DIALYZER_APPS)))

.PHONY: check_plt
check_plt: rel
	dialyzer --check_plt --plt $(COMBO_PLT) $(PLT_LIBS)

.PHONY: build_sysplt
build_sysplt: $(OTP_PLT)

$(OTP_PLT):
	dialyzer --output_plt $(OTP_PLT) --build_plt \
		--apps erts kernel stdlib mnesia compiler syntax_tools runtime_tools \
		crypto tools inets sasl ssh ssl public_key xmerl

.PHONY: build_plt
build_plt: compile build_sysplt $(COMBO_PLT)

$(COMBO_PLT):
	dialyzer --plt $(OTP_PLT) --output_plt $(COMBO_PLT) --add_to_plt $(PLT_LIBS)

.PHONY: dialyzer
dialyzer: compile check_plt
	dialyzer -Wno_return --fullpath --plt $(COMBO_PLT) $(DIALYZER_APPS_PATHS) \
	    | fgrep -v -f ./dialyzer.ignore-warnings | tee dialyzer.log -

.PHONY: cleanplt
cleanplt:
	rm $(COMBO_PLT)
