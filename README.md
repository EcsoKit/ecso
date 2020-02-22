# ecso

# Code Example

Todo

# Build Instructions

Setup Haxe for development (see [Building Haxe from source](https://haxe.org/documentation/introduction/building-haxe.html)) 

Then setup `ecso` as follow:

```
$ cd path/to/dev/haxe/plugins/ecso
$ git clone https://gitlab.com/dpomier/ecso.git
$ haxelib dev ecso .
```

And finally, run the build command from the Haxe directory:

```
# On Unix
$ make PLUGIN=ecso plugin

# On Windows
$ make -f Makefile.win PLUGIN=ecso plugin
```
*(This will build for the current OS only.)*

To use `ecso` in a Haxe project, simply add `-lib ecso` to your compilation flags.