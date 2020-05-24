package tests.units;

import utest.*;
import utest.ui.common.*;
import ecso.Entity;
import tests.systems.*;

class CustomGroup {
    public function new () {}
    @:ecso.create public extern function add <A> (v:A):Void;
    @:ecso.delete public extern function rem <B> (v:B);
    @:ecso.foreach public extern function run (systems : haxe.extern.Rest<Any>);
}

class Groups extends buddy.SingleSuite {

    var groupA:EntityGroup;
    var groupB:CustomGroup;
    
    public function new () {

        describe('Custom groups', {
            
            beforeEach({
                groupA = new EntityGroup();
                groupB = new CustomGroup();
            });

            it('should have independent component definitions', {
                groupA.createEntity({ x: 1, y: 2 });
                groupB.add({ x: 3, y: "high" });
                groupA.foreachEntity((e:{ x:Int, y:Int }) -> {
                    Assert.equals(1, e.x);
                    Assert.equals(2, e.y);
                });
                groupB.run((e:{ x:Int, y:String }) -> {
                    Assert.equals(3, e.x);
                    Assert.equals("high", e.y);
                });
            });

        });
    }
}