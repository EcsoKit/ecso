package tests;

import buddy.*;
import ecso.Entity;
import tests.components.*;
import tests.systems.*;
import tests.units.*;
using buddy.Should;

class Main implements Buddy<[
    new CoreSpecification(),
    new Units()
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

        describe('EntityGroup', {

            beforeEach({
                entities = new EntityGroup();
                spy = { x: -1, y: -1, vx: -1, vy: -1 }
            });

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
                    entities.createEntity({
                        morph: 3
                    });
                    entities.createEntity({
                        morph: "string"
                    });
                }));
                it('with prohibited component-downcasts', buddy.CompilationShould.failFor({
                    entities.createEntity({
                        instance: new A()
                    });
                    entities.createEntity({
                        instance: new B()
                    });
                }));
                it('with prohibited component-quotes', buddy.CompilationShould.failFor({
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

            describe('should process entities', {
                it('with arraw systems', {
                    entities.foreachEntity(e -> {
                        e.x += 2;
                    });
                    spy.x.should.be(-1);
                    entities.foreachEntity(e -> {
                        e.spy.x = e.x;
                    });
                    spy.x.should.be(6);
                });
                it('with anon systems', {
                    entities.foreachEntity(function (e:PositionComponent) {
                        e.x += 2;
                    });
                    spy.x.should.be(-1);
                    entities.foreachEntity(function (e:{ x:Int, spy:{ x:Int } }) {
                        e.spy.x = e.x;
                    });
                    spy.x.should.be(6);
                });
                it('with local systems', {
                    function movingX (e:PositionComponent) {
                        e.x += 4;
                    }
                    entities.foreachEntity(movingX);
                    entities.foreachEntity(spying);
                    spy.x.should.be(8);
                });
                it('with hijacked systems', {
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
                it('with static systems', {
                    entities.foreachEntity(movingY);
                    entities.foreachEntity(spying);
                    spy.y.should.be(4 + spy.vy);
                });
                it('with module-level systems', {
                    pending("coming soon");
                });
                it('with matching systems only', {
                    entities.foreachEntity((e:{ foo:String }) -> {
                        fail('Entity $e should not match with { foo : String }');
                    });
                });
                it('with system binding', {
                    entities.foreachEntity((function (e:{ x:Int, spy:{x:Int} }, shift:Int) {
                        e.x += shift;
                    }).bind(_, 50));
                    entities.foreachEntity(spying);
                    spy.x.should.be(54);
                });
                it('with multiple systems', {
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
                    entities.foreachEntity((e:PositionComponent & { z:Int }) -> {
                        e.z.should.be(10);
                        e.z = null;
                        count++;
                    });
                    entities.foreachEntity((e:PositionComponent & { z:Int }) -> {
                        fail();
                    });
                    count.should.be(3);
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
                    entities.createEntity({ z: 2 });
                    var totalz = 0;
                    entities.foreachEntity((e:PositionComponent, sibling:{ z:Int }) -> {
                        totalz += sibling.z;
                    });
                    totalz.should.be(4);
                });
                it('with undirect system values', {
                    var positive = Math.random() > .5;
                    var sideEffect = false;
                    var toto = if (positive) (e:PositionComponent) -> {
                        e.x = 12;
                        sideEffect = true;
                    } else (e:PositionComponent) -> {
                        e.x = -12;
                        sideEffect = true;
                    };
                    entities.foreachEntity(toto, spying);
                    spy.x.should.be(positive ? 12 : -12);
                    if (sideEffect) {
                        toto = function (e:PositionComponent) {
                            e.x = 42;
                            return false;
                        };
                    }
                    entities.foreachEntity(toto, spying);
                    spy.x.should.be(42);
                });
            });

            describe('should delete entities', {
                it('with closure', {
                    function stoping (e:{ y:Int })
                        entities.deleteEntity(e);
                    entities.foreachEntity( movingY, spying, stoping );
                    spy.y.should.be(4 + spy.vy);
                    entities.foreachEntity( movingY, spying );
                    spy.y.should.be(4 + spy.vy);
                });
                it('with binding', {
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