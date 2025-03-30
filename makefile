# Name of the executable
EXEC = Scheduler

# The default target is to build the executable
all:
	ghc -o $(EXEC) Main.hs

# Clean rule to remove all .o and .hi files
clean:
	del /f *.o *.hi Scheduler.exe

# Phony targets
.PHONY: all clean
