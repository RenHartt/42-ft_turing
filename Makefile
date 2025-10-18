# === ft_turing — build avec toolchain OPAM ===
NAME := ft_turing
SRC  := sources
ML   := $(SRC)/machine.ml $(SRC)/main.ml

OPAM_BIN    := $(shell opam var bin)
OPAM_PREFIX := $(shell opam var prefix)

OCAMLOPT := $(OPAM_BIN)/ocamlopt
YOJSON_DIR := $(OPAM_PREFIX)/lib/yojson

.PHONY: all clean fclean re

all: $(NAME)

$(NAME):
	$(OCAMLOPT) -g -I $(SRC) -I $(YOJSON_DIR) yojson.cmxa $(ML) -o $(NAME)

clean:
	rm -f $(SRC)/*.cm[iox] $(SRC)/*.o *.cm[iox] *.o

fclean: clean
	rm -f $(NAME)

re: fclean all
