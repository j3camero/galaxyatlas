Instructions to get running

Prerequisits:
- Install Haskell (ghc) and cabal package manager/build system
- Clone repository
- cd into local checkout

Commands to Set Up Environment (in {CHECKOUT}/server/ directory):

    cabal sandbox init # This makes installed packages local to this project
    cabal sandbox add-source ../stardata
    cabal install      # Installs all dependant packages and runtime data
    cabal configure    # Sets up whats needed for cabal to compile/run

Commands to Compile and Run:

    cabal build        # Compile everything
    cabal run          # Run the program
