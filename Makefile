

include config.mk

GET_OPTS_LIB=
ifeq ($(ORIGINAL_GETOPTS),yes)
GET_OPTS_LIB+=-DORIGINAL_GETOPTS
endif

DYNAMIC_FLAG=
ifeq ($(STATIC),no)
DYNAMIC_FLAG+=-dynamic
else
DYNAMIC_FLAG+=-static
endif

COMPILE_FLAGS=$(DYNAMIC_FLAG) -isrc -cpp $(GET_OPTS_LIB)

AUTOCOMPLETE_FILE=$(INSTALL_FOLDER)auto_complete.bash
BASHFUNCTION_FILE=$(INSTALL_FOLDER)jump_function.bash
TEMPORARY_BACKUP_FOLDER=/tmp/jump_bu/
TEMPORARY_SCRIPT_FOLDER=/tmp/jump_scripts/

all: build/jump
	strip $<

build/jump: src/Main.hs
	@mkdir -p build
	ghc $(COMPILE_FLAGS) $< -o $@

backup:
	@echo ">>> Backing up .bashrc and other crucial files..."
	mkdir -p $(TEMPORARY_BACKUP_FOLDER)
	cp $(BASHRC) $(TEMPORARY_BACKUP_FOLDER)

restore:
	cp -f $(TEMPORARY_BACKUP_FOLDER).bashrc $(BASHRC)

backup_profile:
	@echo ">>> Backing up profile..."
	mkdir -p $(TEMPORARY_BACKUP_FOLDER)
	mkdir -p $(CONFIG_FOLDER)
	cp -r $(CONFIG_FOLDER)* $(TEMPORARY_BACKUP_FOLDER)

restore_profile:
	@echo ">>> Restoring backed-up profile..."
	mkdir -p $(TEMPORARY_BACKUP_FOLDER)
	mkdir -p $(CONFIG_FOLDER)
	cp -rf $(TEMPORARY_BACKUP_FOLDER)* $(CONFIG_FOLDER)
# 	rm $(CONFIG_FOLDER)*





.SILENT: install
	
install: all backup
	@echo ">>>>>>>>> Making configuration directory at "$(CONFIG_FOLDER)
	mkdir -p $(CONFIG_FOLDER)
	touch $(CONFIG_FOLDER)bookmarks.txt
	#
	@echo ">>>>>>>>> Creating bash scripts"
	mkdir -p $(TEMPORARY_SCRIPT_FOLDER)
	sed "s|{{INSTALL_FOLDER}}|$(INSTALL_FOLDER)|" scripts/auto_complete.bash > $(TEMPORARY_SCRIPT_FOLDER)auto_complete.bash
	sed "s|{{INSTALL_FOLDER}}|$(INSTALL_FOLDER)|" scripts/jump_function.bash > $(TEMPORARY_SCRIPT_FOLDER)jump_function.bash
	#
	@echo ">>>>>>>>> Making executable directory at "$(INSTALL_FOLDER)
	mkdir -p $(INSTALL_FOLDER)
	rm -rf $(INSTALL_FOLDER)*
	cp $(TEMPORARY_SCRIPT_FOLDER)* $(INSTALL_FOLDER)
	cp build/jump $(INSTALL_FOLDER)jump
	#
	@echo ">>>>>>>>> Adding lines to .bashrc if not already there"
	grep -qxF 'source $(AUTOCOMPLETE_FILE)' $(BASHRC) || echo '\n\nsource $(AUTOCOMPLETE_FILE)' >> $(BASHRC)
	grep -qxF 'source $(BASHFUNCTION_FILE)' $(BASHRC) || echo 'source $(BASHFUNCTION_FILE)'     >> $(BASHRC)
	@echo ">>>>>>>>> Use \"make restore\" to restore the original .bashrc file"

uninstall:
	rm -r $(INSTALL_FOLDER)
	rm -r $(CONFIG_FOLDER)