HSC = ghc
TRG = treasure_island
MAIN = hw4_2.hs

$(TRG) :
	$(HSC) $(MAIN) --make -o $@

.PHONY : run clean

run : $(TRG)
	./$(TRG)

clean :
	rm -f *.hi *.o $(TRG)
