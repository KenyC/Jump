Jump
===================

Very lightweight tool to bookmark some directories and jump to them quickly

# Requirements

  - GHC
  - Linux

# Installation

Download this repo and run the following

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
# Added a bookmark with name "much"
```


## Cd to bookmarked directory

Go to directory by name
```bash
jump favorite_dir_ever
```

## Remove bookmarked directory

```bash
jump --clear name_of_the_bookmark_that_is_no_longer_in_favour
```

## Environments

If you want some commands to be executed after jump (e.g. rename the terminal, ``conda activate``, etc.), write down the commands to ``~/.config/jump/default_env`` (a.k.a. *default environment file*) and execute:

```bash
jump favorite_dir_ever -e
# Alternatively
# jump favorite_dir_ever --env
```

You can create and use non-default environment files in ``~/.config/jump/``. If ``python_env`` is the name of the file you created, call it using:

```bash
jump favorite_dir_ever -epython_env
```

(**Note:** Ideally, one would want a space between "-e" and ``python_env``. The Haskell GetOpt library used for parsing command line arguments does not seem to allow it...)

# How it works

The difficulty is that a subprocess cannot modify characteristic of the parent process (in particular, its current working directory). To hack one's way around the problem, ``make install`` thus creates a bash function ``jump`` in your ``.bashrc``. This function ``jump`` itself calls a Haskell program which gives bash instructions to be sourced by the ``jump`` function. 