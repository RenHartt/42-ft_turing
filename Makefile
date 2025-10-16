NAME      := ft_turing
SRC_DIR   := sources
MACHINES  := machines

MLIS := \
  $(SRC_DIR)/machine.mli \
  $(SRC_DIR)/engine.mli  \
  $(SRC_DIR)/tape.mli

MLS := \
  $(SRC_DIR)/machine.ml \
  $(SRC_DIR)/engine.ml  \
  $(SRC_DIR)/tape.ml    \
  $(SRC_DIR)/main.ml

OCAMLC   := ocamlfind ocamlc
OCAMLOPT := ocamlfind ocamlopt
PKGS     := -package yojson
LINKPKG  := -linkpkg
FLAGS    := -g

all: $(NAME)

$(NAME): $(MLIS) $(MLS)
	@echo "Building $(NAME)..."
	$(OCAMLOPT) $(FLAGS) $(PKGS) $(LINKPKG) -o $(NAME) $(MLS)
	@echo "Done."

byte:
	$(OCAMLC) $(FLAGS) $(PKGS) $(LINKPKG) -o $(NAME).byte $(MLS)

run: all
	./$(NAME) --help

clean:
	rm -f *.cm[iox] *.o $(SRC_DIR)/*.cm[iox] $(SRC_DIR)/*.o

fclean: clean
	rm -f $(NAME) $(NAME).byte

re: fclean all

.PHONY: all byte run clean fclean re