Jump
===================

Very lightweight tool to bookmark some directories and jump to them quickly

# Usage

## Creating a bookmark

Change directory you want to bookmark and give it a little name

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
jump --clear out_of_favour_directory
```

# Installation

Download this repo and run the following

```bash
make install
```

# How it works

Creates a bash function ``jump`` in your ``.bashrc``. The function ``jump`` itself calls a Haskell program that locates the relevant folder. The output of this program tells ``jump`` where to jump to. 