package units;

import utest.Assert;
import ecso.Entity;

class Constraints extends Test {
    
    function testEntityAsConstraint() {
        // Test that Entity can be used as a type parameter constraint
        function takesEntity<T:Entity>(e:T):Void {}
        
        // This should not throw
        takesEntity({});
        takesEntity({ x: 1, y: 2 });
    }
    
    function testEntityWithGeneric() {
        function identity<T:Entity>(e:T):T { return e; }
        
        var e1 = { a: 1 };
        var e2 = identity(e1);
        Assert.equals(e1, e2);
    }
    
    function testEntityInClass() {
        var c = new Container({ x: 1 });
        Assert.equals(c.entity.x, 1);
    }
    
    function testMultipleEntityConstraints() {
        function processTwo<T:Entity, U:Entity>(e1:T, e2:U):Void {}
        
        processTwo({ x: 1 }, { y: "test" });
    }
    
    // Note: Compilation failure tests cannot be easily done in utest
    // Those are handled in the specs (Buddy) tests
}

class Container<T:Entity> {
    public var entity:T;
    public function new(e:T) { this.entity = e; }
}