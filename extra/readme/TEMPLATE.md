#if github
ecso
----
<!--- DO NOT EDIT. Generated from /extra/readme -->
#end

`ecso` is an open-source and experimental [Entity Component System](https://en.wikipedia.org/wiki/Entity_component_system) plugin for Haxe, which enables ECS oriented compiler features and focuses on simplicity.

#if haxelib
The current alpha version requires Haxe 4.2.0 or 4.2.1. It is strongly advised to use `ecso` with the following targets: HashLink, JavaScript, C++, C#, or Eval (interp). Other targets will be tested progressively.
#end

If you have any suggestions or run into any problems, please [open an issue #if !github on GitHub #end](https://github.com/EcsoKit/ecso/issues).

#if github
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg?style=flat-square)](LICENSE.md)
[![Slack](https://img.shields.io/badge/chat-online-success?style=flat-square&logo=slack)](https://join.slack.com/t/ecsokit/shared_invite/zt-ex72k2bp-tDx6yJ~zvZc1swdFW6i8Eg)
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/EcsoKit/ecso/CI?style=flat-square)](https://github.com/EcsoKit/ecso/actions?query=workflow%3ACI)
#end

## Features

* **Framework agnostic**: focus exclusively on core ECS aspects.
* **Haxe-Powered**: integrates with built-in semantics and features (such as macros, type inference, closures, null safety, etc).
* **Advanced code-analyzes**: structure for high-level ECS-oriented compiler optimizations.
* **Super simple API**: lightweight with clarity, readability and discoverability in mind.
* **Cross-platform**: enables target-specific optimizations, and run on every Haxe target.
* **Performance**: while the current focus is on design and not on speed, the nature of `ecso` promises a good level of customization to best fit specific needs, optimizing out branching and unboxing from systems, etc.
* **Fast Compilation**: plugins are incredibly fast, with zero-overhead for non-`ecso` projects.

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

    entities.foreachEntity( move ); // runs the system `move`.
    entities.foreachEntity( teleport.bind(_, 0, 0) ); // runs `teleport` with arguments.
    entities.foreachEntity( function (entity:{ y:Int, vy:Int }) { // runs an anonymous system.
        if (entity.y < 100)
            entity.vy -= 1;
    });
}
```

For more details about `ecso` please visit [the wiki page](https://github.com/EcsoKit/ecso/wiki).

# How to install

Run `haxelib install ecso` and add `--library ecso` to your compilation flags.

To use `ecso` from source or use nightly builds, you can refer to the [installation](https://github.com/EcsoKit/ecso/wiki/Installation) page.

# Contribution

Any kind of contribution is welcome and appreciated. You can read more [here](https://github.com/EcsoKit/ecso/blob/master/CONTRIBUTING.md). Thank you!
