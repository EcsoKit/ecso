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

    public function new () {

        super();

        describe('EntityGroup', {

            var entities:EntityGroup;

            it('should have a constructor', {
                new EntityGroup();
            });

            beforeEach({
                entities = new EntityGroup();
            });
            
            it('should create entities', {

                // with anon structures
                entities.createEntity({
                    foo: 0
                });

                // with anon structures, two components
                entities.createEntity({
                    foo: "ecso",
                    bar: new StringBuf()
                });

                // with type constraints
                entities.createEntity(({
                    foo: 0
                } : { foo:Int }));

                // with type constraints, down-cast
                entities.createEntity(({
                    foo: 2020,
                    bar: "ecso"
                } : { foo:Float, bar:String }));
            });
            
            it('should process systems', {

                entities.process( MovementSystem.decelerate );

                entities.process( MovementSystem.move, MovementSystem.decelerate );

                var spy = { x: 0, y: 0, vx: 0, vy: 0 };
                
                entities.createEntity({
                    x: 2,
                    y: 4,
                    vx: 10,
                    vy: 20,
                    spy: spy
                });

                // test spy
                entities.process( MovementSystem.spy );
                spy.x.should.be(2);
                spy.y.should.be(4);
                spy.vx.should.be(10);
                spy.vy.should.be(20);

                // processed in order
                entities.process( MovementSystem.decelerate, MovementSystem.spy );
                spy.vx.should.be(9);
                spy.vy.should.be(19);

                // processed in order (with trailing)
                entities.process( MovementSystem.move, MovementSystem.spy, MovementSystem.move );
                spy.x.should.be(2 + 9);
                spy.y.should.be(4 + 19);
                entities.process( MovementSystem.spy );
                spy.x.should.be(2 + 9 * 2);
                spy.y.should.be(4 + 19 * 2);

                // anon system, processed once
                entities.process((e:PositionComponent) -> {
                    e.x -= spy.x;
                    e.y -= spy.y;
                }, MovementSystem.spy);
                spy.x.should.be(0);
                spy.y.should.be(0);

                // binded system, entity deletion, component down-cast (see `spy:PositionComponent`)
                entities.process( MovementSystem.moveAndStop.bind(_,entities) );
                spy.x.should.be(spy.vx);
                spy.y.should.be(spy.vy);
                entities.process( MovementSystem.move, MovementSystem.spy );
                spy.x.should.be(spy.vx);
                spy.y.should.be(spy.vy);

            });

        });
        
    }

}