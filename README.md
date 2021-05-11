Jump
===================

Very lightweight tool to bookmark some directories and jump to them quickly

# Requirements

  - GHC
  - Linux

# Installation

Download this repository and run the command below. Install configuration options are available in ``config.mk``

```bash
make install
```


# Usage

## Creating a bookmark

Cd to directory you want to bookmark and give it a little name

```bash
cd ~/I/love/this/directory/so/much
jump --bookmark favorite_dir_ever
```

If you don't give it a name, the bookmark's name will default to the directory's name:

```bash
cd ~/I/love/this/directory/so/much
jump --bookmark
# stdout: Added a bookmark with name "much"
```


## Cd to bookmarked directory

Go to directory by name
```bash
jump favorite_dir_ever
```

## Remove bookmarked directory

```bash
jump --clear name_of_the_bookmark_that_has_fallen_out_of_grace
```

## Environments

If you want some commands to be executed after jump (e.g. rename the terminal, ``conda activate``, etc.), write down the commands to ``~/.config/jump/default_env.sh`` (a.k.a. *default environment file*) and execute:

```bash
jump favorite_dir_ever -e
# Alternatively
# jump favorite_dir_ever --env
```

You can create and use non-default environment files in ``~/.config/jump/``. If ``python_env.sh`` is the name of the file you created, call it using:

```bash
jump favorite_dir_ever -e python_env
```

(**Note:** The space between an option taking an optional argument like -e and its argument isn't kosher. Some (not me) would prefer to write ``-e=python_env``. If you wish Jump to have this behavior, install it using the following command:)

```bash
make install ORIGINAL_GETOPTS=1
```


# How it works

The difficulty is that a subprocess cannot modify characteristics of the parent process (in particular, its current working directory). To hack one's way around the problem, ``make install`` creates a bash function ``jump`` in your ``.bashrc``. This function ``jump`` itself calls a Haskell program which gives bash instructions to be sourced by the ``jump`` function. 