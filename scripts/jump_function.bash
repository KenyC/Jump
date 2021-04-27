#!/bin/bash

function jump() {
	local command_path=/tmp/$(uuidgen)
	/home/keny/Documents/Utilities/jump/jump $command_path "$@"
	# echo $command_path "$@"
	source $command_path
}

