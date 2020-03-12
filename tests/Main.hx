package tests;

import buddy.*;
import ecso.Entity;
import tests.components.*;
import tests.systems.*;
using buddy.Should;

class Main implements Buddy<[
    new CoreSpecification(),
]> {}

private class CoreSpecification extends BuddySuite {
    
    static var INT_DEFAULT_VALUE:Int;
    static var FLOAT_DEFAULT_VALUE:Float;
    static var BOOL_DEFAULT_VALUE:Bool;

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
                        foo: 0
                    });
                });
                it('with multiple components', {
                    entities.createEntity({
                        foo: "ecso",
                        bar: new StringBuf()
                    });
                });
                it('with type constraints', {
                    entities.createEntity(({
                        foo: 0
                    } : { foo:Int }));
                });
                it('with down-casting constraints', {
                    entities.createEntity(({
                        foo: 2020,
                        bar: "ecso"
                    } : { foo:Float, bar:String }));
                });
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
                it('with anon systems', {
                    entities.process((e:PositionComponent) -> {
                        e.x += 2;
                    });
                    spy.x.should.be(-1);
                    entities.process((e:{ x:Int, spy:{ x:Int } }) -> {
                        e.spy.x = e.x;
                    });
                    spy.x.should.be(6);
                });
                it('with local systems', {
                    function movingX (e:PositionComponent) {
                        e.x += 4;
                    }
                    entities.process(movingX);
                    entities.process(spying);
                    spy.x.should.be(8);
                });
                it('with static systems', {
                    entities.process(movingY);
                    entities.process(spying);
                    spy.y.should.be(4 + spy.vy);
                });
                it('with module-level systems', {
                    pending("coming soon");
                });
                it('with matching systems only', {
                    entities.process((e:{ foo:Int }) -> {
                        fail('Entity $e should not match with { foo : Int }');
                    });
                });
                it('with system binding', {
                    entities.process((function (e:{ x:Int, spy:{x:Int} }, shift:Int) {
                        e.x += shift;
                    }).bind(_, 50));
                    entities.process(spying);
                    spy.x.should.be(54);
                });
                it('with multiple systems', {
                    entities.process(movingY, spying);
                    spy.y.should.be(4 + spy.vy);
                });
                it('with optional components', {
                    var count = 0;
                    entities.process((e:PositionComponent & { ?z:Int }) -> {
                        e.z.should.be(null);
                        e.z = 5;
                        count++;
                    });
                    entities.process((e:{ z:Int }) -> {
                        e.z.should.be(5);
                        count++;
                    });
                    count.should.be(2);
                });
                it('with siblings', {
                    entities.process((e:PositionComponent, sibling:PositionComponent) -> {
                        e.x.should.be(sibling.x);
                    });
                    entities.createEntity({ z: 2 });
                    var totalz = 0;
                    entities.process((e:PositionComponent, sibling:{ z:Int }) -> {
                        totalz += sibling.z;
                    });
                    totalz.should.be(2);
                });
            });

            describe('should delete entities', {
                it('with closure', {
                    function stoping (e:{ y:Int })
                        entities.deleteEntity(e);
                    entities.process( movingY, spying, stoping );
                    spy.y.should.be(4 + spy.vy);
                    entities.process( movingY, spying );
                    spy.y.should.be(4 + spy.vy);
                });
                it('with binding', {
                    function stoping (e:{ y:Int }, group:EntityGroup)
                        group.deleteEntity(e);
                    entities.process( movingY, spying, stoping.bind(_,entities) );
                    spy.y.should.be(4 + spy.vy);
                    entities.process( movingY, spying );
                    spy.y.should.be(4 + spy.vy);
                });
            });
        });        
    }
}