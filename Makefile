NAME := ft_turing
SRC  := sources
ML   := $(SRC)/machine.ml $(SRC)/IO.ml $(SRC)/main.ml

BUILD_DIR := build
BIN_DIR   := $(BUILD_DIR)/bin
EXEC_OPT  := $(BUILD_DIR)/$(NAME)
EXEC_BYTE := $(BUILD_DIR)/$(NAME)_byte

OPAM_BIN    := $(shell opam var bin)
OPAM_PREFIX := $(shell opam var prefix)

OCAMLOPT := $(OPAM_BIN)/ocamlopt
OCAMLC   := $(OPAM_BIN)/ocamlc
YOJSON_DIR := $(OPAM_PREFIX)/lib/yojson

.PHONY: all check_deps clean fclean re run

all: check_deps $(EXEC_OPT) $(EXEC_BYTE)

check_deps:
	@which opam >/dev/null || (echo "OPAM not found. Please install OPAM." && exit 1)
	@opam list --installed yojson >/dev/null 2>&1 || (echo "Installing yojson..." && opam install -y yojson)

$(EXEC_OPT): | $(BIN_DIR)
	$(OCAMLOPT) -g -I $(SRC) -I $(YOJSON_DIR) yojson.cmxa $(ML) -o $(EXEC_OPT)
	mv $(SRC)/*.cm[iox] $(SRC)/*.o $(BIN_DIR) 2>/dev/null || true
	mv *.cm[iox] *.o $(BIN_DIR) 2>/dev/null || true

$(EXEC_BYTE): | $(BIN_DIR)
	$(OCAMLC) -g -I $(SRC) -I $(YOJSON_DIR) yojson.cma $(ML) -o $(EXEC_BYTE)
	mv $(SRC)/*.cmo $(SRC)/*.cmi $(SRC)/*.o $(BIN_DIR) 2>/dev/null || true
	mv *.cmo *.cmi *.o $(BIN_DIR) 2>/dev/null || true

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

run:
	@if [ -x "$(EXEC_OPT)" ]; then \
		echo "Running native binary..."; \
		$(EXEC_OPT); \
	elif [ -x "$(EXEC_BYTE)" ]; then \
		echo "Running bytecode binary..."; \
		$(EXEC_BYTE); \
	else \
		echo "No executable found. Please run 'make' first."; \
	fi

clean:
	rm -rf $(BIN_DIR)

fclean: clean
	rm -rf $(BUILD_DIR)

re: fclean all
