HSC = ghc
TRG = treasure_island
SRCS = hw4_4.hs TreasureIsland.hs
MAIN = hw4_4.hs

$(TRG) : $(SRCS)
	$(HSC) $(MAIN) --make -o $@

.PHONY : run clean

run : $(TRG)
	./$(TRG)

clean :
	rm -f *.hi *.o $(TRG)
