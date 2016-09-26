// Constants
var distUpdateThreshold = 2;
var speed = 0.3;
var turnRate = 0.03;
var starGain = 1.0 * Math.PI * 4;
var dfrGain = 1.0 * Math.PI * 4;

// State variables for camera/controls
var stars = [];
var starsUpdated = false;
var cameraPosition = new Vector(0, 0, 0);
var lastUpdatePosition = new Vector(0, 0, 0);
var lastRenderPosition = new Vector(0, 0, 0);
var cameraDirection = new Vector(0, 1, 0);
var lastRenderDirection = new Vector(0, 1, 0);
var upDirection = new Vector(0, 0, 1);
var right = Vector.crossProduct(cameraDirection, upDirection);
var keys = new Array();

// Test data
var fovTestStars = [{"sid":-1,"x":1.0,"y":1.0,"z":0.0,"lum":1.0,"r":0,"g":255,"b":0},
                    {"sid":-2,"x":-1.0,"y":1.0,"z":0.0,"lum":1.0,"r":0,"g":255,"b":0},
                    {"sid":-3,"x":0.0,"y":1.0,"z":1.0,"lum":1.0,"r":0,"g":255,"b":0},
                    {"sid":-4,"x":0.0,"y":1.0,"z":-1.0,"lum":1.0,"r":0,"g":255,"b":0}]

var testDFRs = [{"dfr_id":0,
                 "dfr_x":-1.0,"dfr_y":3.0,"dfr_z":0.0,
                 "dfr_lum":30.0,
                 "dfr_r":0,"dfr_g":255,"dfr_b":0,
                 "dfr_rad":2.0},
                {"dfr_id":0,
                 "dfr_x":1.0,"dfr_y":3.0,"dfr_z":0.0,
                 "dfr_lum":30.0,
                 "dfr_r":0,"dfr_g":255,"dfr_b":0,
                 "dfr_rad":2.0}];

// Canvas element and context
var canvas = document.getElementById('stars');
var context = canvas.getContext('2d');

// "CCD" object for deep field
var dfCCD = null;
var df_image = null;

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

// Rendering functions
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

// Update camera position and rotattion from controls
function updateCamFromControls() {
    if (keys[87]) {
	cameraPosition.addInPlace(cameraDirection.multiplyScalar(speed));
    }
    if (keys[83]) {
	cameraPosition.addInPlace(cameraDirection.multiplyScalar(-speed));
    }
    right = Vector.crossProduct(cameraDirection, upDirection);
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
        return false;
    }
    // If we're going to render, reset the starsUpdated flag
    starsUpdated = false;
    lastRenderPosition.copyFrom(cameraPosition);
    lastRenderDirection.copyFrom(cameraDirection);
    return true;
}

// Execute one frame
function doOneFrame() {
    // If the camera hasn't moved/panned and the stars haven't changed
    // we don't need to re-render
    if (!updateCamFromControls()) {
        return;
    }

    // Clear screen
    context.fillStyle = 'black';
    context.fillRect(0, 0, canvas.width, canvas.height);
    dfCCD.clear();
    df_image = context.getImageData(0,0,canvas.width,canvas.height);
    for (var i = 0; i < testDFRs.length; ++i) {
        var dfr = testDFRs[i];
        var position = new Vector(dfr.dfr_x, dfr.dfr_y, dfr.dfr_z);
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
            (canvas.width / 2) * projected.z / projected.y;
        
        if (sx < 0 || sx > canvas.width) continue;
        if (sy < 0 || sy > canvas.height) continue;

        var dist = Math.sqrt(translated.squaredLength())
        var rad = (dfr.dfr_rad / (2 * dist)) * canvas.width
        var lum = 255 * dfr.dfr_lum /
            (4 * Math.PI * translated.squaredLength());
        //console.log(lum);
        lum *= dfrGain;
        var color = {"r":dfr.dfr_r, "g":dfr.dfr_g, "b":dfr.dfr_b};
        dfCCD.exposeCircle(Math.round(sx),Math.round(sy),
                           Math.round(rad),lum,color);
    }
    dfCCD.drawToImage(df_image);
    context.putImageData(df_image, 0, 0);
    
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
            (canvas.width / 2) * projected.z / projected.y;
        if (sx < 0 || sx > canvas.width) continue;
        if (sy < 0 || sy > canvas.height) continue;
	var brightness = 255 * star.lum /
            (4 * Math.PI * translated.squaredLength());
        brightness *= starGain;
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
    getVisibleStarsMagic(0.002, 10, x, y, z, function(newStars) {
	stars = newStars;
        starsUpdated = true;
	console.log('Loaded ' + newStars.length + ' stars.');
    }, function(error) {
	console.log('Failure: ' + error);
    });
}

window.onload = function() {
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    dfCCD = new CCD(canvas.width, canvas.height);
    initKeys();
    updateStars(true);
    doOneFrame();
    setInterval(function() {
	updateStars(false);
    }, 500);
};
