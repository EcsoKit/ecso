import js.html.CanvasRenderingContext2D;
import ecso.Entity;
import js.Browser.window;
import js.Browser.document;
import js.html.CanvasElement;

//----------------------
// Components
//----------------------

// Velocity archetype
typedef Velocity = {
	var vx : Float;
	var vy : Float;
}

// Position archetype
typedef Position = {
	var x : Float;
	var y : Float;
}

// Shape archetype
typedef Shape = {
	var primitive : Primitive;
}

enum abstract Primitive (Int) {
	final Box;
	final Circle;
}

// Define an archetype  of both "Velocity" and "Position"
typedef Moving = Velocity & Position


//----------------------
// Systems
//----------------------

function movableSystem(delta : Float, entity : Velocity & Position) {
	entity.x += entity.vx * delta;
	entity.y += entity.vy * delta;

	if (entity.x > canvasWidth + SHAPE_HALF_SIZE) entity.x = - SHAPE_HALF_SIZE;
	if (entity.x < - SHAPE_HALF_SIZE) entity.x = canvasWidth + SHAPE_HALF_SIZE;
	if (entity.y > canvasHeight + SHAPE_HALF_SIZE) entity.y = - SHAPE_HALF_SIZE;
	if (entity.y < - SHAPE_HALF_SIZE) entity.y = canvasHeight + SHAPE_HALF_SIZE;
}

function clearCanvas(ctx : CanvasRenderingContext2D) {
	ctx.globalAlpha = 1;
	ctx.fillStyle = "#ffffff";
	ctx.fillRect(0, 0, canvasWidth, canvasHeight);
	//ctx.globalAlpha = 0.6;
}

function renderSystem(ctx : CanvasRenderingContext2D, entity : Shape & Position) {
	switch entity.primitive {
		case Box:
			ctx.beginPath();
			ctx.rect(entity.x - SHAPE_HALF_SIZE, entity.y - SHAPE_HALF_SIZE, SHAPE_SIZE, SHAPE_SIZE);
			ctx.fillStyle= "#f28d89";
			ctx.fill();
			ctx.lineWidth = 1;
			ctx.strokeStyle = "#800904";
			ctx.stroke();
		case Circle:
			ctx.fillStyle = "#888";
			ctx.beginPath();
			ctx.arc(entity.x, entity.y, SHAPE_HALF_SIZE, 0, 2 * Math.PI, false);
			ctx.fill();
			ctx.lineWidth = 1;
			ctx.strokeStyle = "#222";
			ctx.stroke();
	}
}

//----------------------
// Create the world
//----------------------

inline var NUM_ELEMENTS = 600;
inline var SPEED_MULTIPLIER = 0.1;
inline var SHAPE_SIZE = 20;
inline var SHAPE_HALF_SIZE = SHAPE_SIZE / 2;

var canvasHeight = 0.0;
var canvasWidth = 0.0;

function createWorld() {

	// Some helper functions when creating the components
	inline function getRandomVelocity() {
		return SPEED_MULTIPLIER * (2 * Math.random() - 1);
	}

	inline function getRandomPosition(range : Float) {
		return Math.random() * range;
	}

	inline function getRandomShape() {
		return Math.random() >= 0.5 ? Circle : Box;
	}

	var entities = new EntityGroup();

	for (_ in 0...NUM_ELEMENTS) {
		entities.createEntity({
			x: getRandomPosition( canvasWidth ),
			y: getRandomPosition( canvasHeight ),
			vx: getRandomVelocity(), 
			vy: getRandomVelocity(),
			primitive: getRandomShape()
		});   
	}

	return entities;
}

//----------------------
// Run!
//----------------------

var ctx : CanvasRenderingContext2D;
var entities : EntityGroup;
var lastTime : Float;

function run(?_) {
	
	// Compute delta and elapsed time
	var time = window.performance.now();
	var delta = time - lastTime;
	
	// Reset canvas
	clearCanvas( ctx );

	// Run all the systems
	entities.foreachEntity(
		inline movableSystem.bind( delta ),
		inline renderSystem.bind( ctx )
	);

	lastTime = time;
	window.requestAnimationFrame( run );
}

//----------------------
// Entry point
//----------------------

var canvas : CanvasElement;

function main() {
	window.addEventListener( "load", init );
}

function init() {
	canvas = cast document.getElementById( "game" );
	
	resize();
	window.addEventListener( "resize", resize, false );
	
	// Start
	ctx = cast canvas.getContext( "2d" );
	entities = createWorld();
	lastTime = window.performance.now();
	run();
}

function resize() {
	canvasWidth = canvas.width = window.innerWidth;
	canvasHeight = canvas.height = window.innerHeight;
}