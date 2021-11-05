package specs.units;

import utest.*;
import utest.ui.common.*;
import ecso.Entity;
import specs.systems.*;

private enum CallbackTracking {
    Added(from:Class<Dynamic>);
    Removed(from:Class<Dynamic>);
    AddedAbstract;
    RemovedAbstract;
}
private final tracking:Array<CallbackTracking> = [];
private function track(callback:CallbackTracking) {
    tracking.push(callback);
}
private function resetTracking() {
    tracking.resize(0);
}

class Components extends buddy.SingleSuite {

    var entities:EntityGroup;
    
    public function new () {

        describe('Metadata @:ecso.added invokes', {
            
            describe('on abstract\'s methods', {

                beforeEach({
                    resetTracking();
                    entities = new EntityGroup();
                });

                it('when initialized', {
                    entities.createEntity({ abstractComponent: (0:ComponentAbstract) });
                    Assert.same([AddedAbstract], tracking);
                });
                it('when assigned', {
                    entities.createEntity({});
                    Assert.same([], tracking);
                    entities.foreachEntity((e:{ ?abstractComponent:ComponentAbstract }) -> {
                        e.abstractComponent = (0:ComponentAbstract);
                    });
                    Assert.same([AddedAbstract], tracking);
                });
                
                it('when re-assigned', {
                    entities.createEntity({ abstractComponent: (0:ComponentAbstract) });
                    resetTracking();
                    entities.foreachEntity((e:{ abstractComponent:ComponentAbstract }) -> {
                        e.abstractComponent = e.abstractComponent;
                    });
                    trace(tracking);
                    Assert.same([AddedAbstract], tracking);
                });
                
                it('when not null', {
                    entities.createEntity({ });
                    entities.foreachEntity((e:{ ?abstractComponent:ComponentAbstract }) -> {
                        e.abstractComponent = null;
                    });
                    entities.createEntity({ abstractComponent: null });
                    Assert.same([], tracking);
                });
            });

        });
        
        describe('Metadata @:ecso.removed invokes', {
            
            beforeEach({
                entities = new EntityGroup();
            });

            describe('on abstract\'s methods', {

                beforeEach({
                    entities.createEntity({ abstractComponent: (0:ComponentAbstract) });
                    resetTracking();
                });

                it('when deleted', {
                    entities.foreachEntity(e -> {
                        entities.deleteEntity(e);
                    });
                    Assert.same([RemovedAbstract], tracking);
                });

                it('when unassigned', {
                    entities.foreachEntity(e -> {
                       e.abstractComponent = null;
                    });
                    Assert.same([RemovedAbstract], tracking);
                });
                
                it('when re-assigned', {
                    entities.foreachEntity((e:{ abstractComponent:ComponentAbstract }) -> {
                        e.abstractComponent = (5:ComponentAbstract);
                    });
                    Assert.same([RemovedAbstract,AddedAbstract], tracking);
                });
                
                it('when not null', {
                    entities.createEntity({ a: 0 });
                    entities.foreachEntity((e:{ a:Int, ?abstractComponent:ComponentAbstract }) -> {
                        e.abstractComponent = null;
                    });
                    Assert.same([], tracking);
                });
            
            });
        });
    }
}

private abstract ComponentAbstract(#if static Null<Int> #else Int #end) from Int {
    @:ecso.added
    function onAdded() {
        track(AddedAbstract);
    }
    @:ecso.removed
    function onRemoved() {
        track(RemovedAbstract);
    }
}

