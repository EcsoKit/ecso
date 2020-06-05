ecso
----

`ecso` is an open-source and experimental [Entity Component System](https://en.wikipedia.org/wiki/Entity_component_system) plugin for Haxe, which enables compiler ECS oriented features and focuses on simplicity.

## Features

* **Framework agnostic**: focus exclusively on core ECS aspects.
* **Haxe-Powered**: integrates with built-in semantics and features (even macros).
* **Advanced code-analyzes**: structure for high-level ECS-oriented compiler optimizations.
* **Lightweight API**: minimalist and push clarity, readability and discoverability over conciseness.
* **Cross-platform**: enables target-specific optimizations.
* **Performance**: while speed is not the current focus, the nature of `ecso` should eventually offer a good level of customization to best fit specific needs, optimizing out branching, unboxing, etc... from systems.
* **Fast Compilation**: plugins are incredibly fast, plus zero-overhead for non-`ecso` projects.

## Code example

```haxe
import ecso.Entity;

typedef Position = {
    var x:Int;
    var y:Int;
}

typedef Velocity = {
    var vx:Int;
    var vy:Int;
}

function move (entity:Position & Velocity) {
    entity.x += entity.vx;
    entity.y += entity.vy;
}

function teleport (entity:Position, x:Int, y:Int) {
    entity.x = x;
    entity.y = y;
}

function main () {
    final entities = new EntityGroup();

    entities.createEntity({
        x: 10,
        y: 10,
        vx: 2,
        vy: 1
    });

    entities.foreachEntity( move );
    entities.foreachEntity( teleport.bind(_, 0, 0) );
    entities.foreachEntity( function (entity:{ y:Int, vy:Int }) {
        if (entity.y < 100)
            entity.vy -= 1;
    });
}
```

For more details about `ecso` please visit [the wiki page](https://github.com/EcsoKit/ecso/wiki).

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

To use `ecso` in a Haxe project, simply add `-L ecso` to your compilation flags.
