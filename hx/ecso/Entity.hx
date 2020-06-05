package ecso;

import haxe.macro.Expr;
import haxe.macro.MacroStringTools;
import haxe.macro.ExprTools;
import haxe.macro.TypeTools;
import haxe.macro.ComplexTypeTools;
#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
import ecso._core.Plugin;
#end
import haxe.extern.Rest;

final class Entity {

    public inline function new () {

    }

    @:noCompletion public macro function addComponent<T> (_:Expr, ?type:Expr):Expr
        return Context.fatalError( "ecso.Entity has no field addComponent (Suggestion: createComponent)", Context.currentPos() );

    @:noCompletion public macro function removeComponent<T> (_:Expr, ?type:Expr):Expr
        return Context.fatalError( "ecso.Entity has no field removeComponent (Suggestion: deleteComponent)", Context.currentPos() );

    public macro function createComponent (entity:Expr, def:Expr):Expr {
		
		var edef = def;

		do edef = switch edef.expr {
            case EParenthesis(e):
                e;
            case ECheckType(e, t):
                Plugin.registerComponent( Context.typeof(edef), e.pos );
                break;
            case _:
                Context.fatalError( "Expression `(_:T)` expected but has " + edef.expr.getName(), edef.pos );
                break;
        } while (true);

        return macro {};
    }
    
    public macro function deleteComponent (entity:Expr, type:Expr):ExprOf<Bool> {
        
        var stype = [];
        var etype = type;

		do etype = switch etype.expr {
            case EParenthesis(e):
                e;
            case EConst(CIdent(s)):
                stype.push( s );
                break;
            case EField(e, field):
                stype.push( field );
                e;
            case _:
                Context.fatalError( "Expression `(_:T)` expected but has " + etype.expr.getName(), etype.pos );
                break;
        } while (true);

        var ct:ComplexType = {
            var i = stype.length;
            var pack = [while( --i >= 0 ) stype[i]];
            TPath({pack: pack, name: pack.pop(), params: []});
        };

        Plugin.registerComponent( Context.typeof(macro (null:$ct)), type.pos );

        return macro false;
    }
    
    public macro function getComponent (entity:Expr, type:Expr):Expr {
        
        var stype = [];
        var etype = type;

		do etype = switch etype.expr {
            case EParenthesis(e):
                e;
            case EConst(CIdent(s)):
                stype.push( s );
                break;
            case EField(e, field):
                stype.push( field );
                e;
            case _:
                Context.fatalError( "Expression `(_:T)` expected but has " + etype.expr.getName(), etype.pos );
                break;
        } while (true);

        var ct:ComplexType = {
            var i = stype.length;
            var pack = [while( --i >= 0 ) stype[i]];
            TPath({pack: pack, name: pack.pop(), params: []});
        };

        Plugin.registerComponent( Context.typeof(macro (null:$ct)), type.pos );

        return macro null;
    }

} 

final class EntityGroup {

    public inline function new () {

    }

    public inline function add (e:Entity):Void {

    }

    public inline function remove (e:Entity):Bool {
        return false;
    }

    public extern function createEntity (def:Any):Void;

    public extern function deleteEntity (def:Any):Void;

    public extern function process (systems:Rest<Any>):Void;
    
    // {

        // var stype = [];
        // var esystems = systems;

		// do esystems = switch esystems.expr {
        //     case EParenthesis(e):
        //         e;
        //     case EConst(CIdent(_)) | EField(_, _):
        //         macro [$esystems];
        //     case EArrayDecl(values):
                
        //         // Parse each systems

        //         for( esystem in values ) {

        //             final tsystem = Context.typeof(esystem);

        //             switch tsystem {
        //                 case TFun(args, ret):

        //                     // Retrieve function's module

        //                     final stype = {
        //                         var stype = [];
        //                         var esystem = esystem;

        //                         do esystem = switch esystem.expr {
        //                             case EParenthesis(e):
        //                                 e;
        //                             case EConst(CIdent(s)):
        //                                 stype.push( s );
        //                                 break;
        //                             case EField(e, field):
        //                                 stype.push( field );
        //                                 e;
        //                             case _:
        //                                 Context.fatalError( "[ECSO] Expression `` expected but has " + esystem.expr.getName(), esystem.pos );
        //                                 break;
        //                         } while (true);
        //                         stype;
        //                     }

        //                     var functionIdentifier:String = stype[0];

        //                     var ct:ComplexType = {
        //                         var i = stype.length;
        //                         if( i == 1 ) {
        //                             // TODO
        //                             Context.fatalError( "[ECSO] Resolving system function's module is not yet supported without explicitely specifiying it, e.g. `SystemModule.systemFunction` is expected instead of `systemFunction` ", esystem.pos );
        //                         }
        //                         var pack = [while( --i >= 1 ) stype[i]]; // We skip the function identifier
        //                         TPath({pack: pack, name: pack.pop(), params: []});
        //                     };
                            
        //                     Plugin.registerSystem( Context.typeof(macro (null:$ct)), functionIdentifier, esystem.pos );

        //                 case _:
        //                     Context.fatalError( "Reference of function declaration expected but has " + tsystem.getName(), esystem.pos );
        //             }
        //         }

        //         break;
        //     case _:
        //         Context.fatalError( "Reference of function declaration expected but has " + esystems.expr.getName(), esystems.pos );
        //         break;
        // } while (true);

        // return macro {};
    // }

}