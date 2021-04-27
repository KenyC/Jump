# Whether the program uses Haskell "GetOpts" library or the custom version of it
# Haskell "GetOpts" library specify short flags using "-f=VALUE"
# Custom  "GetOpts" library specify short flags using "-f VALUE"
# Example: if set to "yes", "jump bookmark -e=env" is a valid call, if "no", "jump bookmark -e env" is the valid call
ORIGINAL_GETOPTS=no

# Whether to compile Jump with dynamic or static linking
# Static: 4MB executable, does not need RTS to run, faster on loading executable
# Dynamic: 68KB executable, must have RTS to run, slower on loading executable
# (RTS = Haskell runtime system, the C library which all Haskell code must run with)
STATIC=no