#!/bin/bash

function jump() {
	local command_path=/tmp/___verylongandarbitraryname
	/home/keny/Documents/Utilities/jump/jump "$@" > $command_path
	source $command_path
}

