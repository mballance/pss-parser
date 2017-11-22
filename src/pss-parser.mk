
PSS_PARSER_SRC_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
PSS_PARSER_SRC_DIR := $(shell cd $(PSS_PARSER_SRC_DIR) ; pwd)
PSS_PARSER_SCRIPTS_DIR := $(PSS_PARSER_SRC_DIR)/../scripts

ifeq (,$(UNZIP))
UNZIP := unzip -o
endif

ifneq (1, $(RULES))

-include grammar/src.mk
-include antlr4-cpp-runtime/src.mk

ANTLR4_CPP_RUNTIME_DIR=antlr4-cpp-runtime/runtime/src

CXXFLAGS += -DANTLR4CPP_EXPORTS
SRC_DIRS += $(ANTLR4_CPP_RUNTIME_DIR) $(ANTLR4_CPP_RUNTIME_DIR)/atn 
SRC_DIRS += $(ANTLR4_CPP_RUNTIME_DIR)/dfa $(ANTLR4_CPP_RUNTIME_DIR)/support 
SRC_DIRS += $(ANTLR4_CPP_RUNTIME_DIR)/tree $(ANTLR4_CPP_RUNTIME_DIR)/misc
SRC_DIRS += $(ANTLR4_CPP_RUNTIME_DIR)/tree/pattern 
SRC_DIRS += $(ANTLR4_CPP_RUNTIME_DIR)/tree/xpath 
SRC_DIRS += grammar

LIB_TARGETS += libpss_parser.a libantlr_runtime.a

else # Rules

libpss_parser.a : $(PSS_GRAMMAR_SRC:.cpp=.o)
	$(Q)rm -f $@
	$(Q)$(AR) vcq $@ $^

libantlr_runtime.a : $(ANTLR_RT_SRC:.cpp=.o)
	rm -f $@
	$(AR) vcq $@ $^

runtime.unpack : $(PSS_PARSER_SCRIPTS_DIR)/antlr4-cpp-runtime-4.7-source.zip
	$(Q)rm -rf antlr4-cpp-runtime
	$(Q)mkdir antlr4-cpp-runtime
	$(Q)cd antlr4-cpp-runtime ; $(UNZIP) $^
	$(Q)touch $@

grammar.gen : $(PSS_PARSER_SRC_DIR)/PSS.g4
	$(Q)mkdir -p grammar
	$(Q)java -jar $(PSS_PARSER_SCRIPTS_DIR)/antlr-4.7-complete.jar -Dlanguage=Cpp -visitor -o grammar $^
	$(Q)touch $@

grammar/src.mk : grammar.gen
	$(Q)echo 'PSS_GRAMMAR_SRC += $$(notdir $$(wildcard grammar/*.cpp))' > $@
	
antlr4-cpp-runtime/src.mk : runtime.unpack
	$(Q)echo 'ANTLR_RT_SRC += $$(notdir $$(wildcard antlr4-cpp-runtime/runtime/src/*.cpp))' > $@
	$(Q)echo 'ANTLR_RT_SRC += $$(notdir $$(wildcard antlr4-cpp-runtime/runtime/src/atn/*.cpp))' >> $@
	$(Q)echo 'ANTLR_RT_SRC += $$(notdir $$(wildcard antlr4-cpp-runtime/runtime/src/dfa/*.cpp))' >> $@
	$(Q)echo 'ANTLR_RT_SRC += $$(notdir $$(wildcard antlr4-cpp-runtime/runtime/src/support/*.cpp))' >> $@
	$(Q)echo 'ANTLR_RT_SRC += $$(notdir $$(wildcard antlr4-cpp-runtime/runtime/src/tree/*.cpp))' >> $@
	$(Q)echo 'ANTLR_RT_SRC += $$(notdir $$(wildcard antlr4-cpp-runtime/runtime/src/tree/pattern/*.cpp))' >> $@
	$(Q)echo 'ANTLR_RT_SRC += $$(notdir $$(wildcard antlr4-cpp-runtime/runtime/src/tree/xpath/*.cpp))' >> $@
	$(Q)echo 'ANTLR_RT_SRC += $$(notdir $$(wildcard antlr4-cpp-runtime/runtime/src/misc/*.cpp))' >> $@


endif # End Rules

