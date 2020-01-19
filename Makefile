.Phony: all re fclean clean
NAME1 = train
NAME2 = estimate
SOURCES1 =	parse.ml	\
			train.ml
SOURCES2 = parse.ml		\
		   estimate.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt

all: $(NAME1) $(NAME2)

OBJS1 = $(SOURCES1:.ml = cmo)
OBJS2 = $(SOURCES2:.ml = cmo)
OPTOBJS1 = $(SOURCES1:.ml = cmx)
OPTOBJS2 = $(SOURCES2:.ml = cmx)

$(NAME1): $(OPTOBJS1)
	$(CAMLOPT) unix.cmxa -o $(NAME1) $(OPTOBJS1)

$(NAME2): $(OPTOBJS2)
	$(CAMLOPT) unix.cmxa -o $(NAME2) $(OPTOBJS2)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

clean:
	rm -f *.cm[iiox] *~ .*~ *.o
	rm -f $(NAME1).o $(NAME2).o

fclean: clean
	rm -f $(NAME1) $(NAME2)
	rm -f $(NAME1).byt $(NAME2).byt
	rm -f $(NAME1).opt $(NAME2).opt


re: fclean all
