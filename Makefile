
INSTALL_FOLDER=~/Documents/Utilities/jump/
CONFIG_FOLDER=~/.config/jump/
BASHRC=~/.bashrc
GET_OPTS_LIB = $(if $(ORIGINAL_GETOPTS),-DORIGINAL_GETOPTS,)
COMPILE_FLAGS=-dynamic -isrc -cpp $(GET_OPTS_LIB)

AUTOCOMPLETE_FILE=$(INSTALL_FOLDER)auto_complete.bash
BASHFUNCTION_FILE=$(INSTALL_FOLDER)jump_function.bash

all: src/Main
	mv src/Main .
	strip ./Main

src/Main: src/Main.hs
	ghc $(COMPILE_FLAGS) src/Main.hs

backup:
	@echo ">>> Backing up .bashrc and other crucial files..."
	mkdir -p /tmp/jump_bu/
	cp $(BASHRC) /tmp/jump_bu/

restore:
	cp -f /tmp/jump_bu/.bashrc $(BASHRC)

backup_profile:
	@echo ">>> Backing up profile..."
	mkdir -p /tmp/jump_bu/
	cp -r $(CONFIG_FOLDER)* /tmp/jump_bu/

restore_profile:
	@echo ">>> Restoring backed-up profile..."
	rm $(CONFIG_FOLDER)*
	cp -r /tmp/jump/* $(CONFIG_FOLDER)





.SILENT: install
	
install: all backup
	@echo ">>>>>>>>> Making configuration directory at "$(CONFIG_FOLDER)
	mkdir -p $(CONFIG_FOLDER)
	touch $(CONFIG_FOLDER)bookmarks.txt
	#
	@echo ">>>>>>>>> Making executable directory at "$(INSTALL_FOLDER)
	mkdir -p $(INSTALL_FOLDER)
	rm -rf $(INSTALL_FOLDER)*
	cp scripts/* $(INSTALL_FOLDER)
	cp Main $(INSTALL_FOLDER)jump
	#
	@echo ">>>>>>>>> Adding lines to .bashrc if not already there"
	grep -qxF 'source $(AUTOCOMPLETE_FILE)' $(BASHRC) || echo '\n\nsource $(AUTOCOMPLETE_FILE)' >> $(BASHRC)
	grep -qxF 'source $(BASHFUNCTION_FILE)' $(BASHRC) || echo 'source $(BASHFUNCTION_FILE)'     >> $(BASHRC)
	@echo ">>>>>>>>> Use \"make restore\" to restore the original .bashrc file"

uninstall:
	rm -r $(INSTALL_FOLDER)
	rm -r $(CONFIG_FOLDER)