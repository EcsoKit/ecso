package tests;

import tests.systems.OptionalComponentSystem.OptionalComponentSystem3;
import tests.systems.OptionalComponentSystem.OptionalComponentSystem2;
import tests.systems.OptionalComponentSystem.OptionalComponentSystem1;
import ecso.Component;
import buddy.*;
import ecso.Entity;
import ecso.EntityGroup;
import ecso.System;
import ecso.SystemGroup;
import tests.components.*;
import tests.systems.*;
import tests.sgroups.*;
using buddy.Should;

class Main implements Buddy<[
    new Tests()
]> {}

private class Tests extends BuddySuite {
    
    static var INT_DEFAULT_VALUE:Int;
    static var FLOAT_DEFAULT_VALUE:Float;
    static var BOOL_DEFAULT_VALUE:Bool;

    public function new () {

        super();

        describe('Ecso', {

            it('should be awesome', {
                true.should.be(true);
            });

        });

        describe('Component', {
            
            it('should have a unique id', {
                var emptyComponent = new EmptyComponent();
                emptyComponent.getComponentID().should.beType(Int);
                new PositionComponent().getComponentID().should.not.be(new PositionComponent().getComponentID());
            });

            it('should have members', {
                var positionComponent1 = new PositionComponent();
                var positionComponent2 = new PositionComponent();

                positionComponent1.x = 1;
                positionComponent2.x = 2;

                positionComponent1.x.should.be(1);
                positionComponent2.x.should.be(2);

                CompilationShould.failFor(new EmptyComponent().x);
            });

            it('should support members default value', {
                new PositionComponent().z.should.be(INT_DEFAULT_VALUE);
                new CollisionBox().depth.should.be(FLOAT_DEFAULT_VALUE);
                new CollisionBox().enabled.should.be(BOOL_DEFAULT_VALUE);
                new CollisionBox().collider.should.be(null);
            });
            
            it('should support custom members default value', {
                
                new PositionComponent().y.should.be(-1);
            });
        
            it('should have a unique type id', {

                new EmptyComponent().getComponentType().should.beType(Int);

                new PositionComponent().getComponentType().should.be(new PositionComponent().getComponentType());

                new PositionComponent().getComponentType().should.not.be(new EmptyComponent().getComponentType());

            });
        
        });
        
        describe('Entity', {
        
            var entity:Entity;
        
            it('should have a constructor', {

                new Entity();

            });
        
            beforeEach({
                entity = new Entity();
                entity.createComponent(EmptyComponent);
                entity.createComponent(VelocityComponent);
            });
        
            it('should have a unique id', {

                entity.id.should.beType(Int);

                new Entity().id.should.not.be(new Entity().id);

            });
        
            it('should have a getComponent method', {

                entity.getComponent(PositionComponent).should.be(null);

                entity.getComponent(EmptyComponent).should.beType(EmptyComponent);

            });

            it('should have a createComponent method', {

                entity.createComponent(CollisionBox);

                entity.createComponent(tests.components.CollisionBox);

                entity.createComponent(CollisionBox).with(0, 12, 0, false, null);

                var p:CollisionBox = entity.createComponent();

                var returned = entity.createComponent(CollisionBox);
                CompilationShould.failFor(returned.with(0, 12, 0, false, null));
                returned.collider = null;

                var returned = entity.createComponent(CollisionBox).with(0, 24, 0, false, null);
                returned.collider = null;

            });

            it('should have a deleteComponent method', {
                
                entity.deleteComponent(PositionComponent).should.be(false);

                entity.deleteComponent(EmptyComponent).should.be(true);

            });
        
            it('should preserve component identities', {

                entity.getComponent(PositionComponent).should.be(null);
                entity.createComponent(PositionComponent);
                var id0 = entity.getComponent(PositionComponent).getComponentID();
                entity.deleteComponent(PositionComponent).should.be(true);
                entity.getComponent(PositionComponent).should.be(null);
                entity.deleteComponent(PositionComponent).should.be(false);
                entity.createComponent(PositionComponent);
                #if !ecso_reuse_component_id
                entity.getComponent(PositionComponent).getComponentID().should.not.be(id0);
                #end
                entity.deleteComponent(PositionComponent).should.be(true);

                CompilationShould.failFor(entity.getComponent("EmptyComponent"));
                CompilationShould.failFor(entity.createComponent("PositionComponent"));
                CompilationShould.failFor(entity.deleteComponent("EmptyComponent"));

            });

        });

        describe('EntityGroup', {

            it('should have a constructor', {
                new EntityGroup();
            });

            var entities:EntityGroup;

            beforeEach({
                entities = new EntityGroup();
            });
            
            it('should have a method add', {
                entities.add(new Entity());
            });
            
            it('should have a method remove', {
                var e = new Entity();
                entities.remove(e);
                entities.add(e);
                entities.remove(e);
            });
            
            // it('should have a method foreach', {
            //     entities.forEach<PositionComponent, VelocityComponent>(function (e:Entity, p:PositionComponent, v:VelocityComponent) {
            //         // do somthing with e, p et v
            //     });
            // });
            
            // it('should have a method geteach', {
            //     var allMatchedEntities:Array<Entity> = entities.getEach<PositionComponent, VelocityComponent>();
            //     var allMatchedEntities:Array<Entity<PositionComponent, VelocityComponent>> = entities.getEach<PositionComponent, VelocityComponent>();
                
            //     var allMatchedAndFilteredEntities:Array<Entity> = entities.getEach<PositionComponent, VelocityComponent>(function (e:Entity, p:PositionComponent, v:VelocityComponent) {
            //         // filter e
            //         return Math.random() > 0.5 ? true : false;
            //     });
                
            //     var allMatchedAndFilteredEntities:Array<Entity<PositionComponent, VelocityComponent>> = entities.getEach<PositionComponent, VelocityComponent>(function (e:Entity, p:PositionComponent, v:VelocityComponent) {
            //         // filter e
            //         return Math.random() > 0.5 ? true : false;
            //     });

                // for (entity => components in entities.getEach<PositionComponent, VelocityComponent>) {
                    // components.positionComponent.x += 3;
                // }
            // });
            
            // it('should have a method createEntity', {
            //     var entity:Entity = entities.createEntity();
            //     var entityWithPosition:Entity = entities.createEntity([new PositionComponent()]);
            // });

        });

        describe('System', {

            it('should compute components', {

                var p = new PositionComponent();
                p.x = p.y = 0;
                var v = {
                    var e = new Entity();
                    e.createComponent(VelocityComponent);
                    e.getComponent(VelocityComponent);
                };
                v.x = v.y = 2;

                MovementSystem.compute(p, v);

                p.x.should.be(2);
                p.y.should.be(2);
            });

            it('should compute list of components', {

                var e1 = new Entity();
                var e2 = new Entity();
                var e3 = new Entity();

                var c1 = e1.createComponent(CollisionBox).with(1, 1, 1, false, null);
                var c2 = e2.createComponent(CollisionBox).with(2, 2, 2, false, null);
                var c3 = e3.createComponent(CollisionBox).with(3, 3, 3, false, null);

                e1.createComponent(PositionComponent).with(0, 0, 0);
                e2.createComponent(PositionComponent).with(5, 0, 7);
                e3.createComponent(PositionComponent).with(0, 0, 0);

                var v3 = e3.createComponent(VelocityComponent).with(0, 0, false);

                var entities = new EntityGroup();
                entities.add(e1);
                entities.add(e2);
                entities.add(e3);
                
                CollisionSystem.run(entities);
                ThreeCompSystem.run(entities);

                c1.collider.should.be(c3);
                c2.collider.should.be(null);
                c3.collider.should.be(c1);
                v3.x.should.be(3);
            });

            it('should run entities', {

                var e1 = new Entity();
                var p = e1.createComponent(PositionComponent).with(0, 0, 0);
                var entities = new EntityGroup();
                entities.add(e1);

                MovementSystem.run(entities);
                p.x.should.be(0);
                var v = e1.createComponent(VelocityComponent).with(4, 0, false);
                MovementSystem.run(entities);
                p.x.should.be(4);
                v.x += 6;
                MovementSystem.run(entities);
                p.x.should.be(14);
                e1.deleteComponent(VelocityComponent);
                MovementSystem.run(entities);
                p.x.should.be(14);

            });

            it('should allows optional components', {

                var entities = new EntityGroup();

                inline function runAll () {
                    OptionalComponentSystem.run(entities);
                    OptionalComponentSystem1.run(entities);
                    OptionalComponentSystem2.run(entities);
                    OptionalComponentSystem3.run(entities);
                }

                var e = new Entity();
                entities.add(e);

                runAll();
                
                var v = e.createComponent(VelocityComponent).with(0, 0, false);
                runAll();
                v.x.should.be(1111);
                v.y.should.be(0);

                var p = e.createComponent(PositionComponent);
                p.x = 0;
                runAll();
                v.x.should.be(2222);
                p.x.should.be(1110);
                v.y.should.be(0);

                e.createComponent(CollisionBox);
                runAll();
                v.x.should.be(3333);
                p.x.should.be(2220);
                v.y.should.be(100);

                var f = e.createComponent(FixedComponent).with(0);
                runAll();
                v.x.should.be(4444);
                p.x.should.be(3330);
                v.y.should.be(200);
                f.value.should.be(1000);
                runAll();
                v.x.should.be(5555);
                p.x.should.be(4440);
                v.y.should.be(300);
                f.value.should.be(2000);

                e.deleteComponent(CollisionBox);
                runAll();
                v.x.should.be(6666);
                p.x.should.be(5550);
                v.y.should.be(300);
                f.value.should.be(3000);

            });

        });
        
        describe('SystemGroup', {

            var entities:EntityGroup;
            var p0:PositionComponent;
            var v0:VelocityComponent;

            beforeEach({
                entities = new EntityGroup();

                var e0 = new Entity();
                p0 = e0.createComponent(PositionComponent).with(0, 0, 0);
                v0 = e0.createComponent(VelocityComponent).with(3, 0, false);

                var e1 = new Entity();
                e1.createComponent(PositionComponent).with(0, 0, 0);
                
                var e2 = new Entity();
                e2.createComponent(VelocityComponent).with(3, 0, false);

                entities.add(e0);
                entities.add(e1);
                entities.add(e2);
            });

            it('should have a constructor', {
                new SystemGroup(); // TODO
                new SystemGroup<MovementSystem>();
                new SystemGroup<TypedefOfSystems>();
                // new SystemGroup<Any>(); // TODO
                // new SystemGroup<InterfaceImplementedBySystems>(); // TODO
            });

            it('should add systems', {

                var systems = new SystemGroup();
                systems.add(MovementSystem);

            });

            // it('should remove systems', {

            //     var systems = new SystemGroup();
            //     systems.remove(MovementSystem);

            // });

            // it('should optimize systems', {

            //     var systems = new SystemGroup();
            //     systems.optimize();

            // });

            it('should run through EntityGroup', {

                p0.x.should.be(0);

                var systems1 = new SystemGroup<MovementSystem>();
                systems1.run(entities);

                p0.x.should.be(3);

                var systems2 = new SystemGroup<TypedefOfSystems>();
                systems2.run(entities);

                p0.x.should.be(0);

                var systems3 = new SystemGroup();
                systems3.add(MovementSystem);
                systems3.run(entities);

                p0.x.should.be(3);

                var systems4 = new SystemGroup<MovementSystem, TypedefOfSystems>();
                systems4.run(entities);

                p0.x.should.be(3);

                var systems5 = new SystemGroup<MovementSystem, TypedefOfSystems, BackwardMovementSystem>();
                systems5.run(entities);

                p0.x.should.be(0);

            });

        });
    }

}