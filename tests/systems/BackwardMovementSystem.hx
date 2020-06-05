package tests.systems;

import ecso.System;
import tests.components.PositionComponent;
import tests.components.VelocityComponent;

@:ecso(tests.systems.BackwardMovementSystem.filterXXX)

class BackwardMovementSystem implements System {
    
    /**
     * - determine which component's fields are read-only or read-write
     */
    public static function compute (position:PositionComponent, velocity:VelocityComponent):Void {
        
        position.x -= velocity.x;
        position.y -= velocity.y;
    }

    /**
     * - inlined or not
     * - arguments should have the same names than in the function `compute` (the order and count doesn't matter)
     * - the types of the argument should be the same as `compute`, if there are missing, we infer them based on the types used in `compute`
     * - we can have several filterXXX functions (filterSpeed, filterVisibility, etc...)
     * - should always return a Bool value
     * - if true is returned, the components will be ran by the system, if false is returned, they will be ignored
     * - if several systems use the same filters, they will be in the same component-set, otherwise they will have separated component-sets
     *      > component-set lists: [actives components | filtered components]
     *      > idea 1: 
     *                  - add to the class Component a methode for each fields: `addFieldXChangeListener (c:ThisComponentType->Void)`
     *                  - when adding a component to a component-set, if the component-set have filters: we generate the code:
     *                        'comp.addFieldXChangeLister( __staticGeneratedFunctionToFilterTheCompInTheCurrentComponentSet )'
     *                  - all the systems that have a read-write computation over the field `X` will call at the end of their `compute` function the following:
     *                        'compInstanceWithTheFieldX.onFieldXChanged(compInstance)'
     *                  - be carefull that the current running system do not run infinitely in case it modifies the fields that is also filtered on this system
     * - determine which fields from wich components are being used to filter the components. 
     *   > the goal is that only the fields which are actualy being used anywhere in the code 
     *     generage the onFieldXChanged calls at the end of the `compute` functions of systems
     *     (we can call an inlined static function to make this job, and this function will be generated after we
     *      have all the data wrt filters and compute functions, to either generate an empty function or not (to be dce-friendly with the extra functions of 
     *      the class Component)).
     */
    public static function filterXXX (velocity):Bool {

        return velocity.x != 0 || velocity.y != 0;

    }
    
}