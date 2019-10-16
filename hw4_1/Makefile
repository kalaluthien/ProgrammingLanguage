HSC = ghc
TRG = kminus_parser
SRCS = main.hs Kminus.hs
MAIN = main.hs

$(TRG) : $(SRCS)
	$(HSC) $(MAIN) --make -o $@

.PHONY : run clean

run : $(TRG)
	./$(TRG)

clean :
	rm -f *.hi *.o $(TRG)
