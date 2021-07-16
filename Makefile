include config.mk

INSTALL_FOLDER_STRIPPED=$(strip $(INSTALL_FOLDER))
CONFIG_FOLDER_STRIPPED=$(strip $(CONFIG_FOLDER))
BASHRC_STRIPPED=$(strip $(BASHRC))


COMPILE_FLAGS=
ifeq ($(ORIGINAL_GETOPTS),yes)
COMPILE_FLAGS+=-DORIGINAL_GETOPTS
endif

COMPILE_FLAGS=
ifeq ($(STATIC),no)
COMPILE_FLAGS+=-dynamic
else
COMPILE_FLAGS+=-static
endif

COMPILE_FLAGS+=-isrc -cpp
# Expand tilde and remove spaces to pass Haskell
CONFIG_FOLDER_EXPANDED=$(strip $(CONFIG_FOLDER:~%="$$HOME"%))
COMPILE_FLAGS+=-DCONFIG_FOLDER=\"$(CONFIG_FOLDER_EXPANDED)\"

AUTOCOMPLETE_FILE=$(INSTALL_FOLDER_STRIPPED)auto_complete.bash
BASHFUNCTION_FILE=$(INSTALL_FOLDER_STRIPPED)jump_function.bash
TEMPORARY_BACKUP_FOLDER=/tmp/jump_bu/
TEMPORARY_SCRIPT_FOLDER=/tmp/jump_scripts/

# TEST=/usr
all: build/jump
	strip $<

build/jump: src/Main.hs
	@mkdir -p build
	ghc $(COMPILE_FLAGS) $< -o $@

backup:
	@echo ">>> Backing up .bashrc and other crucial files..."
	mkdir -p $(TEMPORARY_BACKUP_FOLDER)
	cp $(BASHRC_STRIPPED) $(TEMPORARY_BACKUP_FOLDER)

restore:
	cp -f $(TEMPORARY_BACKUP_FOLDER).bashrc $(BASHRC_STRIPPED)

backup_profile:
	@echo ">>> Backing up profile..."
	mkdir -p $(TEMPORARY_BACKUP_FOLDER)
	mkdir -p $(CONFIG_FOLDER_STRIPPED)
	cp -r $(CONFIG_FOLDER_STRIPPED)* $(TEMPORARY_BACKUP_FOLDER)

restore_profile:
	@echo ">>> Restoring backed-up profile..."
	mkdir -p $(TEMPORARY_BACKUP_FOLDER)
	mkdir -p $(CONFIG_FOLDER_STRIPPED)
	cp -rf $(TEMPORARY_BACKUP_FOLDER)* $(CONFIG_FOLDER_STRIPPED)
# 	rm $(CONFIG_FOLDER_STRIPPED)*





# .SILENT: install
	
install: all backup
	@echo ">>>>>>>>> Making configuration directory at "$(CONFIG_FOLDER_STRIPPED)
	mkdir -p $(CONFIG_FOLDER_STRIPPED)
	touch $(CONFIG_FOLDER_STRIPPED)bookmarks.txt
	#
	@echo ">>>>>>>>> Creating bash scripts"
	mkdir -p $(TEMPORARY_SCRIPT_FOLDER)
	sed "s|{{INSTALL_FOLDER}}|$(INSTALL_FOLDER_STRIPPED)|" scripts/jump_function.bash > $(TEMPORARY_SCRIPT_FOLDER)jump_function.bash
	sed "s|{{INSTALL_FOLDER}}|$(INSTALL_FOLDER_STRIPPED)|" scripts/auto_complete.bash > $(TEMPORARY_SCRIPT_FOLDER)auto_complete.bash
	#
	@echo ">>>>>>>>> Making executable directory at "$(INSTALL_FOLDER_STRIPPED)
	mkdir -p $(INSTALL_FOLDER_STRIPPED)
	rm -rf $(INSTALL_FOLDER_STRIPPED)*
	cp $(TEMPORARY_SCRIPT_FOLDER)* $(INSTALL_FOLDER_STRIPPED)
	cp build/jump $(INSTALL_FOLDER_STRIPPED)jump
	#
	@echo ">>>>>>>>> Adding lines to .bashrc if not already there"
	grep -qxF 'source $(BASHFUNCTION_FILE)' $(BASHRC_STRIPPED) || echo '\n\nsource $(BASHFUNCTION_FILE)'     >> $(BASHRC_STRIPPED)
	grep -qxF 'source $(AUTOCOMPLETE_FILE)' $(BASHRC_STRIPPED) || echo 'source $(AUTOCOMPLETE_FILE)' >> $(BASHRC_STRIPPED)
	@echo ">>>>>>>>> Use \"make restore\" to restore the original .bashrc file"

uninstall:
	rm -r $(INSTALL_FOLDER_STRIPPED)
	rm -r $(CONFIG_FOLDER_STRIPPED)