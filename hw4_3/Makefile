HSC = ghc
TRG = present
SRCS = hw4_3.hs Present.hs
MAIN = hw4_3.hs

$(TRG) : $(SRCS)
	$(HSC) $(MAIN) --make -o $@

.PHONY : run clean

run : $(TRG)
	./$(TRG)

clean :
	rm -f *.hi *.o $(TRG)
