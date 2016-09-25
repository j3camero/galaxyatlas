// Constants
var distUpdateThreshold = 2;
var speed = 0.3;
var turnRate = 0.03;

var stars = [];
var starsUpdated = false;
var cameraPosition = new Vector(0, 0, 0);
var lastUpdatePosition = new Vector(0, 0, 0);
var lastRenderPosition = new Vector(0, 0, 0);
var cameraDirection = new Vector(0, 1, 0);
var lastRenderDirection = new Vector(0, 1, 0);
var upDirection = new Vector(0, 0, 1);
var keys = new Array();

var fovTestStars = [{"sid":-1,"x":1.0,"y":1.0,"z":0.0,"lum":1.0,"r":0,"g":255,"b":0},
                    {"sid":-2,"x":-1.0,"y":1.0,"z":0.0,"lum":1.0,"r":0,"g":255,"b":0},
                    {"sid":-3,"x":0.0,"y":1.0,"z":1.0,"lum":1.0,"r":0,"g":255,"b":0},
                    {"sid":-4,"x":0.0,"y":1.0,"z":-1.0,"lum":1.0,"r":0,"g":255,"b":0}]

function initKeys() {
    for (var i = 0; i < 256; ++i) {
	keys[i] = 0;
    }
}

window.onkeyup = function(event) {
    keys[event.keyCode] = 0;
};

window.onkeydown = function(event) {
    keys[event.keyCode] = 1;
};

function renderStar(context, screenX, screenY, area, color) {
    var radius = Math.sqrt(area / Math.PI);
    var alpha = 1;
    if (radius < 0.5) {
	radius = 0.5;
	alpha = 4 * area / Math.PI;
    }
    var gradient = context.createRadialGradient(screenX, screenY, 0,
						screenX, screenY, radius);
    var rgb = color['r'] + ',' + color['g'] + ',' + color['b'];
    gradient.addColorStop(0, 'rgba(255,255,255,' + alpha + ')');
    gradient.addColorStop(1, 'rgba(' + rgb + ',0)');
    context.fillStyle = gradient;
    context.beginPath();
    context.arc(screenX, screenY, radius, 0, 2 * Math.PI);
    context.fill();
}

function doOneFrame() {
    if (keys[87]) {
	cameraPosition.addInPlace(cameraDirection.multiplyScalar(speed));
    }
    if (keys[83]) {
	cameraPosition.addInPlace(cameraDirection.multiplyScalar(-speed));
    }
    var right = Vector.crossProduct(cameraDirection, upDirection);
    if (keys[65]) {
	cameraPosition.addInPlace(right.multiplyScalar(-speed));
    }
    if (keys[68]) {
	cameraPosition.addInPlace(right.multiplyScalar(speed));
    }
    if (keys[38]) {
	cameraDirection = cameraDirection.rotate(right, -turnRate);
	upDirection = upDirection.rotate(right, -turnRate);
    }
    if (keys[40]) {
	cameraDirection = cameraDirection.rotate(right, turnRate);
	upDirection = upDirection.rotate(right, turnRate);
    }
    if (keys[37]) {
	cameraDirection = cameraDirection.rotate(upDirection, turnRate);
    }
    if (keys[39]) {
	cameraDirection = cameraDirection.rotate(upDirection, -turnRate);
    }
    // Check if we need to re-render
    if (cameraDirection.isEqual(lastRenderDirection) &&
        cameraPosition.isEqual(lastRenderPosition) && !starsUpdated) {
        setTimeout(doOneFrame, 10);
        return;
    }
    lastRenderPosition.copyFrom(cameraPosition);
    lastRenderDirection.copyFrom(cameraDirection);
    starsUpdated = false;
    
    var canvas = document.getElementById('stars');
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    var context = canvas.getContext('2d');
    context.fillStyle = 'black';
    context.fillRect(0, 0, canvas.width, canvas.height);
    for (var i = 0; i < stars.length; ++i) {
	var star = stars[i];
	var position = new Vector(star.x, star.y, star.z);
	var translated = position.subtract(cameraPosition);
	var projected = translated.basisProjection(right,
						   cameraDirection,
						   upDirection);
	if (projected.y < 0.00001) {
	    continue;
	}
	var sx = (canvas.width / 2) +
            (canvas.width / 2) * projected.x / projected.y;
	var sy = (canvas.height / 2) +
            (canvas.height / 2) * projected.z / projected.y;
        if (sx < 0 || sx > canvas.width) continue;
        if (sy < 0 || sy > canvas.height) continue;
	var brightness = 255 * star.lum / projected.squaredLength();
	var color = {"r":star.r, "g":star.g, "b":star.b};
	renderStar(context, sx, sy, brightness, color);
    }
    setTimeout(doOneFrame, 10);
};

// Returns immediately, with callback later.
function updateStars(force) {
    if (force)
        console.log('Forcing star update')
    var distFromLastUpdate = Math.sqrt(
        Vector.subtract(lastUpdatePosition,
                        cameraPosition).squaredLength())
    if (distFromLastUpdate < distUpdateThreshold && !force)
        return;
    lastUpdatePosition.copyFrom(cameraPosition);
    var x = cameraPosition.x;
    var y = cameraPosition.y;
    var z = cameraPosition.z;
    getVisibleStarsMagic(0.004, 10, x, y, z, function(newStars) {
	stars = newStars;
        starsUpdated = true;
	console.log('Loaded ' + newStars.length + ' stars.');
    }, function(error) {
	console.log('Failure: ' + error);
    });
}

window.onload = function() {
    initKeys();
    updateStars(true);
    doOneFrame();
    setInterval(function() {
	updateStars(false);
    }, 500);
};
