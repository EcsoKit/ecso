package specs.units;

import utest.*;
import utest.ui.common.*;
import ecso.Entity;
import specs.systems.*;

class CustomGroup {
    public function new () {}
    @:ecso.create public extern function add <A> (v:A):Void;
    @:ecso.delete public extern function rem <B> (v:B):Void;
    @:ecso.foreach public extern function run (systems : haxe.extern.Rest<Any>):Void;

    
    @:ecso.create public static extern function staticAdd <A> (v:A):Void;
    @:ecso.delete public static extern function staticRem <B> (v:B):Void;
    @:ecso.foreach public static extern function staticRun (systems : haxe.extern.Rest<Any>):Void;
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

            it('of class instance', {
                var count = 0;
                groupB.add({ kind: "instance" });
                groupB.run((e:{ kind:String }) -> {
                    Assert.equals("instance", e.kind);
                    count++;
                });
                Assert.equals(1,count);
            });

            it('of class type', {
                var count = 0;
                CustomGroup.staticAdd({ kind: "static" });
                CustomGroup.staticRun((e:{ kind:String }) -> {
                    Assert.equals("static", e.kind);
                    count++;
                });
                Assert.equals(1,count);
            });

            it('should have independent component definitions', {
                groupA.createEntity({ x: 1, y: 2 });
                groupB.add({ x: 3, y: "high" });
                CustomGroup.staticAdd({ x: "far", y: "high" });
                var count = 0;
                groupA.foreachEntity((e:{ x:Int, y:Int }) -> {
                    Assert.equals(1, e.x);
                    Assert.equals(2, e.y);
                    count++;
                });
                Assert.equals(1, count);
                groupB.run((e:{ x:Int, y:String }) -> {
                    Assert.equals(3, e.x);
                    Assert.equals("high", e.y);
                    count++;
                });
                Assert.equals(2, count);
                CustomGroup.staticRun((e:{ x:String, y:String }) -> {
                    Assert.equals("far", e.x);
                    Assert.equals("high", e.y);
                    count++;
                });
                Assert.equals(3, count);
            });

            it('should have independent data', {
                groupA.createEntity({ value: "default instance" });
                groupB.add({ value: "custom instance" });
                CustomGroup.staticAdd({ value: "custom static" });
                var count = 0;
                groupA.foreachEntity((e:{ value:String }) -> {
                    Assert.equals("default instance", e.value);
                    count++;
                });
                Assert.equals(1, count);
                groupB.run((e:{ value:String }) -> {
                    Assert.equals("custom instance", e.value);
                    count++;
                });
                Assert.equals(2, count);
                CustomGroup.staticRun((e:{ value:String }) -> {
                    Assert.equals("custom static", e.value);
                    count++;
                });
                Assert.equals(3, count);
            });

        });
    }
}