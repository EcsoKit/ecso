package;

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.io.Path;
import sys.io.File;
import sys.FileSystem;

using StringTools;
using haxe.macro.ExprTools;

function main() {
	processTemplate();
}

macro function processTemplate() {

	final DEFINES = Context.getDefines();
	final cwd = Sys.getCwd();
	final path = cwd + DEFINES.get("template");
	final dest = cwd + DEFINES.get("destination");
	
	Sys.println('Process template $path');

	final template = File.getContent(path);
	final pos = Context.makePosition({ min: 0, max: template.length, file: path	});

	final identifiers = [];
	final code = ~/(?<!\w|\.)#(if|elseif)\s+(\S+)(?!\w)/g.map(template, function(reg:EReg) {
		final matched = reg.matched(0);
		final eif = reg.matched(1);
		final cond = reg.matched(2);
		if(cond.split("(").length != cond.split(")").length) {
			final range = reg.matchedPos();
			final condIdx = matched.indexOf(cond, '#$eif '.length);
			final pos = {
				min: 1 + range.pos + condIdx,
				max: 1 + range.pos + condIdx + cond.length,
				file: path
			}
			Context.fatalError('Invalid conditional : spaces are not allowed', Context.makePosition(pos));
		}
		~/[A-Za-z_]+[A-Za-z0-9_]*/g.map(cond, function(reg:EReg) {
			final matched = reg.matched(0);
			identifiers.push(matched);
			return matched;
		});
		return shiftPositions(matched, '";' + (switch eif {
			case "if":
				'';
			case "elseif":
				'}else ';
			case _:
				throw null;
		}) + 'if($cond){v+=" ');
	});
	final code = ~/(?<!\w|\.)#(else|end)(?!\w)/g.map(code, function(reg:EReg) {
		final matched = reg.matched(0);
		final end = reg.matched(1);
		return shiftPositions(matched, '";}' + (switch end {
			case "else":
				'else{';
			case "end": 
				'';
			case _:
				throw null;
		}) + 'v+="');
	});
	final vars = identifiers.map(identifier -> {
		expr: EVars([{
			name: identifier,
			expr: macro $v{DEFINES.get(identifier)}
		}]),
		pos: (macro _).pos
	});
	final code = 'final v = { var v = "$code"; v; }';
	var content = Context.parseInlineString(code, pos);
	content = processEqualities(content);
	content.pos = pos;
	final expr = macro {
		@:mergeBlock $b{vars}
		$content;
		Sys.println('Write ' + $v{Path.normalize(dest)});
		FileSystem.createDirectory(Path.directory($v{dest}));
		File.saveContent( $v{dest}, v );
	}
	return expr;
}

#if macro
function processEqualities(expr : Expr) : Expr {
	return switch expr.expr {
		case EBinop(OpAssignOp(OpAdd), e1, e2): // ignore `v+=`
			{ expr: EBinop(OpAssignOp(OpAdd), e1, processEqualities(e2)), pos: expr.pos }
		case EBinop(op, e1, e2):
			switch [op, skip(e1).expr, skip(e2).expr] {
				case [OpBoolAnd | OpBoolOr, _, _]:
					{ expr: EBinop(op, processEqualities(e1), processEqualities(e2)), pos: expr.pos }
				case [OpEq | OpNotEq | OpGt | OpGte | OpLt | OpLte, _, _]:
					expr;
				case [_, EConst(CIdent(_)), EConst(_)]
				   | [_, EConst(_), EConst(CIdent(_))]:
					Context.fatalError('Illegal comparison : defines can only be compared with other defines', expr.pos);
				case _:
					{ expr: EBinop(op, processEqualities(e1), processEqualities(e2)), pos: expr.pos }
			}
		case EConst(CIdent(s)) if(s != 'v'):
			final e = macro $i{s} != null;
			e.pos = expr.pos;
			e;
		case _:
			expr.map(processEqualities);
	}
}
function skip(expr : Expr) : Expr {
	return switch expr.expr {
		case EParenthesis(e): skip(e);
		case _: expr;
	}
}
function shiftPositions(from : String, to : String) : String {
	final delta = from.length - to.length;
	if(delta <= 0) return to;
	final s = new StringBuf();
	s.add(to);
	for(i in 0...delta) {
		s.add(" ");
	}
	return s.toString();
}
#end