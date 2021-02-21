package specs;

import buddy.*;
import ecso.Entity;
import specs.components.*;
import specs.systems.*;
import specs.units.*;
using buddy.Should;

class Main implements Buddy<[
    new CoreSpecification(),
    new Units(),
    new Groups()
]> {}

private class CoreSpecification extends BuddySuite {

    public static function movingY (e:PositionComponent & VelocityComponent):Void {
        e.y += e.vy;
    }
    
    public function new () {

        super();
        
        var entities:EntityGroup;

        var spy;

        function spying (e:PositionComponent & VelocityComponent) {
            spy.x = e.x;
            spy.y = e.y;
            spy.vx = e.vx;
            spy.vy = e.vy;
        }

        beforeEach({
            entities = new EntityGroup();
            spy = { x: -1, y: -1, vx: -1, vy: -1 }
        });

        describe('ecso', {

            describe('should create entities', {
                it('with anon structures', {
                    entities.createEntity({
                        foo: "bar"
                    });
                });
                it('with multiple components', {
                    entities.createEntity({
                        foo: "foo",
                        bar: new StringBuf()
                    });
                });
                it('with type constraints', {
                    entities.createEntity(({
                        foo: null
                    } : { foo:String }));
                });
                it('with prohibited component-retyping', buddy.CompilationShould.failFor({
                    TODO(post.typing.errors.skip.through);
                    entities.createEntity({
                        morph: 3
                    });
                    entities.createEntity({
                        morph: "string"
                    });
                }));
                it('with prohibited component-downcasts', buddy.CompilationShould.failFor({
                    TODO(post.typing.errors.skip.through);
                    entities.createEntity({
                        instance: new A()
                    });
                    entities.createEntity({
                        instance: new B()
                    });
                }));
                it('with prohibited component-quotes', buddy.CompilationShould.failFor({
                    TODO(post.typing.errors.skip.through);
                    entities.createEntity({
                        "quoted": 0
                    });
                }));
            });

            beforeEach({
                entities.createEntity({
                    x: 4,
                    y: 4,
                    vx: 10,
                    vy: 20,
                    spy: spy
                });
            });

            describe('should process systems', {
                it('from arraw functions', {
                    entities.foreachEntity(e -> {
                        e.x += 2;
                    });
                    spy.x.should.be(-1);
                    entities.foreachEntity(e -> {
                        e.spy.x = e.x;
                    });
                    spy.x.should.be(6);
                });
                it('from anon functions', {
                    entities.foreachEntity(function (e:PositionComponent) {
                        e.x += 2;
                    });
                    spy.x.should.be(-1);
                    entities.foreachEntity(function (e:{ x:Int, spy:{ x:Int } }) {
                        e.spy.x = e.x;
                    });
                    spy.x.should.be(6);
                });
                it('from local functions', {
                    function movingX (e:PositionComponent) {
                        e.x += 4;
                    }
                    entities.foreachEntity(movingX);
                    entities.foreachEntity(spying);
                    spy.x.should.be(8);
                    var e2 = { x: 0, y: 0 }
                    movingX(e2);
                    e2.x.should.be(4);
                });
                it('from hijacked functions', {
                    var movingX = null;
                    entities.foreachEntity(movingX = function (e:PositionComponent) {
                        e.x += 4;
                    });
                    var e2 = { x: 0, y: 0 }
                    movingX(e2);
                    entities.foreachEntity(spying);
                    spy.x.should.be(8);
                    e2.x.should.be(4);
                });
                it('from static functions', {
                    entities.foreachEntity(movingY);
                    entities.foreachEntity(spying);
                    spy.y.should.be(4 + spy.vy);
                });
                it('from module functions', {
                    pending("coming soon");
                });
                it('with correct matches only', {
                    entities.foreachEntity((e:{ foo:String }) -> {
                        fail('Entity $e should not match with { foo : String }');
                    });
                });
                it('with binding', {
                    entities.foreachEntity((function (e:{ x:Int, spy:{x:Int} }, shift:Int) {
                        e.x += shift;
                    }).bind(_, 50));
                    entities.foreachEntity(spying);
                    spy.x.should.be(54);
                });
                it('with multiple entries', {
                    entities.foreachEntity(movingY, spying);
                    spy.y.should.be(4 + spy.vy);
                });
                it('with optional components', {
                    var count = 0;
                    entities.foreachEntity((e:PositionComponent & { ?z:Int }) -> {
                        e.z.should.be(null);
                        e.z = 5;
                        count++;
                    });
                    entities.foreachEntity((e:PositionComponent & { ?z:Int }) -> {
                        e.z.should.be(5);
                        e.z = 10;
                        count++;
                    });
                    entities.foreachEntity((e:PositionComponent & { #if static ? #end z:Int }) -> {
                        e.z.should.be(10);
                        e.z = null;
                        count++;
                    });
                    entities.foreachEntity((e:PositionComponent & { z:Int }) -> {
                        fail();
                    });
                    count.should.be(3);
                });
                it('with nullable components', {
                    entities.createEntity({ nullable: "string" });
                    entities.foreachEntity((e:{ nullable:String }) -> {
                        @:nullSafety(Off) e.nullable = null;
                    });
                    entities.foreachEntity((e:{ nullable:String }) -> {
                        fail();
                    });
                });
                // it('with homonymous components', {
                //     var count = 0;
                //     entities.foreachEntity((e:PositionComponent & { ?z:Int }) -> {
                //         e.z.should.be(null);
                //         e.z = 3;
                //         count++;
                //     });
                //     entities.foreachEntity((e:PositionComponent & { ?z:String }) -> {
                //         e.z.should.be(null);
                //         e.z = "bar";
                //         count++;
                //     });
                //     entities.foreachEntity((e:{ z:Int }) -> {
                //         e.z.should.be(3);
                //         count++;
                //     });
                //     entities.foreachEntity((e:{ z:String }) -> {
                //         e.z.should.be("bar");
                //         count++;
                //     });
                //     count.should.be(4);
                // });
                it('with dynamic components', {
                    var count = 0;
                    entities.foreachEntity((e:{ x:Dynamic, vx:Dynamic }) -> {
                        e.x.should.be(4);
                        e.vx.should.be(10);
                        count++;
                    });
                    count.should.be(1);
                });
                it('with unspecified components', {
                    var count = 0;
                    entities.foreachEntity((e:{}) -> {
                        count++;
                    });
                    count.should.be(1);
                    entities.foreachEntity((e:Dynamic) -> {
                        count++;
                    });
                    count.should.be(2);
                    entities.foreachEntity((e:Any) -> {
                        count++;
                    });
                    count.should.be(3);
                    entities.foreachEntity(e -> {
                        count++;
                    });
                    count.should.be(4);
                });
                it('with inheritance', {
                    entities.createEntity({
                        object: new Parent()
                    });
                    var count = 0;
                    entities.foreachEntity((e:{ object:GrandParent }) -> {
                        count++;
                        e.object.should.beType(Parent);
                        buddy.CompilationShould.failFor({
                            TODO(post.typing.errors.skip.through);
                            e.object = new GrandParent();
                        });
                        e.object = new Child();
                    });
                    count.should.be(1);
                });
                it('with siblings', {
                    var count = 0;
                    entities.foreachEntity((e:PositionComponent, sibling:PositionComponent) -> {
                        e.should.be(sibling);
                        count++;
                    });
                    count.should.be(1);
                    entities.createEntity({ x: 0, y: 0, z: 1 });
                    var totalz = 0;
                    entities.foreachEntity((e:PositionComponent, sibling:{ z:Int }) -> {
                        totalz += sibling.z;
                    });
                    totalz.should.be(2);
                });
                it('with undirect function value', {
                    var positive = Math.random() > .5;
                    var sideEffect = false;
                    var system = if (positive) (e:PositionComponent) -> {
                        e.x = 12;
                        sideEffect = true;
                    } else (e:PositionComponent) -> {
                        e.x = -12;
                        sideEffect = true;
                    };
                    entities.foreachEntity(system, spying);
                    spy.x.should.be(positive ? 12 : -12);
                    if (sideEffect) {
                        system = function (e:PositionComponent) {
                            e.x = 42;
                            return false;
                        };
                    }
                    entities.foreachEntity(system, spying);
                    spy.x.should.be(42);
                });
            });

            describe('should add components', {
                it('with systems', {
                    var added = false;
                    entities.foreachEntity( function (e:{ addition:String }) {
                        added = true;
                    });
                    added.should.be(false);
                    entities.foreachEntity( function (e:{ x:Int, ?addition:String }) {
                        e.addition = "foobar";
                    });
                    entities.foreachEntity( function (e:{ x:Int, addition:String }) {
                        e.addition.should.be("foobar");
                        added = true;
                    });
                    added.should.be(true);
                });
            });

            describe('should remove components', {
                it('with explicit null', {
                    function remove (e:{ ?y:Int })
                        e.y = null;
                    entities.foreachEntity( spying, remove, movingY, spying );
                    spy.y.should.be(4);
                });
                #if !static
                it('with implicit null', {
                    function remove (e:{ y:Int })
                        e.y = null;
                    entities.foreachEntity( spying, remove, movingY, spying );
                    spy.y.should.be(4);
                });
                it('with nullable value', {
                    var n:Int = @:analyzer(no) (() -> null)();
                    function remove (e:{ y:Int })
                        e.y = n;
                    entities.foreachEntity( spying, remove, movingY, spying );
                    spy.y.should.be(4);
                });
                #end
            });

            describe('should delete entities', {
                it('using closures', {
                    function stoping (e:{ y:Int })
                        entities.deleteEntity(e);
                    entities.foreachEntity( movingY, spying, stoping );
                    spy.y.should.be(4 + spy.vy);
                    entities.foreachEntity( movingY, spying );
                    spy.y.should.be(4 + spy.vy);
                });
                it('using function binding', {
                    function stoping (e:{ y:Int }, group:EntityGroup)
                        group.deleteEntity(e);
                    entities.foreachEntity( movingY, spying, stoping.bind(_,entities) );
                    spy.y.should.be(4 + spy.vy);
                    entities.foreachEntity( movingY, spying );
                    spy.y.should.be(4 + spy.vy);
                });
            });
        });        
    }
}

class A { public function new () {} }
class B extends A {}

class GrandParent { public function new () {} }
class Parent extends GrandParent {}
class Child extends Parent {}