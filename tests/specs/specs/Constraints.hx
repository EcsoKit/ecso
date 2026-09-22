package specs;

import buddy.*;
import ecso.Entity;
using StringTools;
using buddy.Should;

// Define test abstract outside the class
abstract OtherAbstract(Dynamic) {}

class Constraints extends BuddySuite {

    function genericVanila<T:Null<Int>>(f:(T)->Void) f(cast 0);
    function genericVanilaAssign<T:Null<Int>>(ARGUMENT:T) {};

    function unifyError(have:String, want:String):String {
        static final template:String = CompilationShould.failFor({
            genericVanila((e:Array<String>) -> {});
        })
        .replace("Array<String>", "<HAVE>")
        .replace("Array<...>", "<HAVE_SHORT>")
        .replace("Null<Int>", "<WANT>")
        .replace("Null<...>", "<WANT_SHORT>");

        function shorter(s) return ~/<[\w:}{ ]+>/m.replace(s, "<...>");
        return template
        .replace('<HAVE>', have)
        .replace('<HAVE_SHORT>', shorter(have))
        .replace('<WANT>', want)
        .replace('<WANT_SHORT>', shorter(want));
    }

    function assignError(arg:String, have:String, want:String):String {
        static final template:String = CompilationShould.failFor({
            genericVanilaAssign([""]);
        })
        .replace("ARGUMENT", "<ARG>")
        .replace("Array<String>", "<HAVE>")
        .replace("Array<...>", "<HAVE_SHORT>")
        .replace("Null<Int>", "<WANT>")
        .replace("Null<...>", "<WANT_SHORT>");

        function shorter(s) return ~/<\w+>/m.replace(s, "<...>");
        return template
        .replace('<ARG>', arg)
        .replace('<HAVE>', have)
        .replace('<HAVE_SHORT>', shorter(have))
        .replace('<WANT>', want)
        .replace('<WANT_SHORT>', shorter(want));
    }
    
    public function new() {
        super();
        
        describe("Entity type constraint", {
            
            it('__', CompilationShould.failFor({
                genericVanila((e:String) -> {});
            }).should.be(unifyError('String', 'Null<Int>')));

            // it('should not match entity definitions', CompilationShould.failFor({
            //     new EntityGroup().createEntity(({ x: 3 }:Entity));
            // }).should.be(assignError('def', '{ x : Int }', 'ecso.Entity')));

            it('should constraint parameter types (string)', CompilationShould.failFor({
                genericForeach(new EntityGroup(), (e:String) -> {});
            }).should.be(unifyError('String', 'ecso.Entity')));

            it('should constraint parameter types (enum)', CompilationShould.failFor({
                genericForeach(new EntityGroup(), (e:haxe.ds.Option<String>) -> {});
            }).should.be(unifyError('haxe.ds.Option<String>', 'ecso.Entity')));

            it('should constraint parameter types (array)', CompilationShould.failFor({
                genericForeach(new EntityGroup(), (e:Array<Dynamic>) -> {});
            }).should.be(unifyError('Array<Dynamic>', 'ecso.Entity')));

            it('should constraint parameter types (class)', CompilationShould.failFor({
                genericForeach(new EntityGroup(), (e:haxe.Serializer) -> {});
            }).should.be(unifyError('haxe.Serializer', 'ecso.Entity')));

            it('should constraint generic substitutes', {
                // Just verify Entity type exists and can be referenced
                var g = new EntityGroup();
                g.createEntity({ x: 3 });
                g.createEntity({ x: 3 });
                g.createEntity({ y: 9 });
                var count = 0;

                genericForeach(g, (e:{ x:Int }) -> { // explicit type works
                // genericForeach(g, e -> {  // WIP HERE: implicit type fails
                    (e.x:Int).should.be(3);
                    count++;
                });
                count.should.be(2);
            });

            // it('should constraint parameter types', /* CompilationShould.failFor */({
            //     // Just verify Entity type exists and can be referenced
            //     var g = new EntityGroup();
            //     g.createEntity({ x: 3 });
            //     g.createEntity({ x: 3 });
            //     g.createEntity({ y: 9 });
            //     var count = 0;
            //     nonGenericForeach(g, (e:{ x:Int }) -> {
            //         e.x.should.be(3);
            //         count++;
            //     });
            //     count.should.be(2);
            // })/* .should.startWith('Aiey') */);

            // it('should constraint parameter types (delete)', /* CompilationShould.failFor */({
            //     // Just verify Entity type exists and can be referenced
            //     var g = new EntityGroup();
            //     g.createEntity({ x: 3 });
            //     g.createEntity({ x: 3 });
            //     g.createEntity({ y: 9 });
            //     var count = 0;
            //     nonGenericDelete(g, (e:{ x:Int }) -> {
            //         e.x.should.be(3);
            //         count++;
            //     });
            //     count.should.be(2);
            // })/* .should.startWith('Aiey') */);
        });
    }

    // generics

    @:generic
	function genericForeach<T:Entity>(g:EntityGroup, filter:(T)->Void) {
		g.foreachEntity((e:T) -> {
			filter(e);
		});
	}

	// function nonGenericForeach<T>(g:EntityGroup, filter:(T)->Void) {
	// 	g.foreachEntity((e:T) -> {
	// 		filter(e);
	// 	});
	// }

	// function nonGenericDelete<T:Entity>(g:EntityGroup, filter:(T)->Void) {
	// 	g.foreachEntity((e:T) -> {
	// 		filter(e);
    //         g.deleteEntity(e);
	// 	});
	// }
}

