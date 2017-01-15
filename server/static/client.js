// Constants
var distUpdateThreshold = 2;
var speed = 0.3;
var turnRate = 0.03;

var visibleOctants = [];
var octantDict = {};

var starsUpdated = false;
var cameraPosition = new Vector(0, 0, 0);
var lastUpdatePosition = new Vector(0, 0, 0);
var lastRenderPosition = new Vector(0, 0, 0);
var cameraDirection = new Vector(0, 1, 0);
var lastRenderDirection = new Vector(0, 1, 0);
var upDirection = new Vector(0, 0, 1);
var keys = new Array();
var draggingCanvas = false;
var dragVector = {x: null, y: null};
var pressForward = false;
var activeDrag = false;
var totalStarDownloadCount = 0;

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
window.ontouchstart = function(event) {
	startDrag(event.touches[0].clientX,event.touches[0].clientY);
};
window.onmousedown = function(event) {
	startDrag(event.clientX,event.clientY);
};
window.ontouchmove = function(event) {
	dragCanvas(event.touches[0].clientX,event.touches[0].clientY);
};
window.onmousemove = function(event) {
	dragCanvas(event.clientX,event.clientY);
};
window.ontouchend = function(event) {
	endDrag();
};
window.onmouseup = function(event) {
	endDrag();
};

function startDrag(x,y) {
	draggingCanvas = true;
	pressForward = false;
	activeDrag = false;
	dragVector = {x: x, y: y};
    	setTimeout(function(){if(draggingCanvas && !activeDrag){pressForward = true;}}, 500);
};

function endDrag() {
	draggingCanvas = false;
	pressForward = false;
	activeDrag = false;
	dragVector = {x: null, y: null};
};

function dragCanvas(x,y) {
	if(dragVector.x == null || dragVector.y == null) {
		dragVector.x = x;
		dragVector.y = y;
	}
	if(draggingCanvas && Math.abs(x-dragVector.x) > 2 && Math.abs(y-dragVector.y) > 2) {
		pressForward = false;
		activeDrag = true;
		var right = Vector.crossProduct(cameraDirection, upDirection);
		if(y < dragVector.y) {
			cameraDirection = cameraDirection.rotate(right, turnRate*((dragVector.y-y)/10));
			upDirection = upDirection.rotate(right, turnRate*((dragVector.y-y)/10));

		} else if(y > dragVector.y) {
			cameraDirection = cameraDirection.rotate(right, -turnRate*((y-dragVector.y)/10));
			upDirection = upDirection.rotate(right, -turnRate*((y-dragVector.y)/10));
		}
		if(x < dragVector.x) {
			cameraDirection = cameraDirection.rotate(upDirection, -turnRate*((dragVector.x-x)/10));
		} else if(x > dragVector.x) {
			cameraDirection = cameraDirection.rotate(upDirection, turnRate*((x-dragVector.x)/10));
		}
		dragVector.x = x;
		dragVector.y = y;
	} else if(draggingCanvas) {
		activeDrag = false;
    		setTimeout(function(){if(draggingCanvas && !activeDrag){pressForward = true;}}, 1000);
	}
	
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
    //if (keys[87] ) {
    if (keys[87] || pressForward) {
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
    document.getElementById('controlsBody').innerHTML = "Position: "
	+cameraPosition.print()+"<br>Direction: "+cameraDirection.print();    
    starsUpdated = false;
    
    var canvas = document.getElementById('stars');
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    var context = canvas.getContext('2d');
    context.fillStyle = 'black';
    context.fillRect(0, 0, canvas.width, canvas.height);

    var starRenderCount = 0;
    var starSkipCount = 0;
    for (ix in visibleOctants) {
        if (typeof octantDict[visibleOctants[ix]] == 'undefined') {
            console.log("Undefined: " + visibleOctants[ix]);
            continue;
        }
        var stars = octantDict[visibleOctants[ix]];
        for (var i = 0; i < stars.length; ++i) {
	    var star = stars[i];

            var position = new Vector(star.x, star.y, star.z);
	    var translated = position.subtract(cameraPosition);
	    var brightness = star.lum /
                (4 * Math.PI * translated.squaredLength());
            var scaling = 255 * 64;
            brightness *= scaling;
            if (brightness < 0.001) {
                starSkipCount++;
                continue;
            }
            
	    var projected = translated.basisProjection(right,
						       cameraDirection,
						       upDirection);
	    if (projected.y < 0.00001) {
                starSkipCount++;
	        continue;
	    }
	    var fovMultiplier = 1.0 / 0.7;
	    var sx = (canvas.width / 2) +
                fovMultiplier * (canvas.width / 2) * projected.x / projected.y;
	    var sy = (canvas.height / 2) +
                fovMultiplier * (canvas.width / 2) * projected.z / projected.y;
            if (sx < 0 || sx > canvas.width) {
                starSkipCount++;
		continue;
	    }
            if (sy < 0 || sy > canvas.height) {
                starSkipCount++;
		continue;
	    }

	    var color = {"r":star.r, "g":star.g, "b":star.b};
	    renderStar(context, sx, sy, brightness, color);
            starRenderCount++;
        }
    }
    console.log('rendered: ' + starRenderCount + ' skipped: ' + starSkipCount);
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
    //console.log('x: ' + x + ' y: ' + y + ' z: ' + z);
    getVisibleOctants(0.0001, x, y, z, function(newOcts) {
        // Use a temporary so we don't update with this before we
        // actually have all thes tars.
	var tmpVisibleOctants = newOcts;
        // Create request for stars
        var octRequests = [];
        for (id in tmpVisibleOctants) {
            if (!(tmpVisibleOctants[id] in octantDict)) {
                octRequests.push(tmpVisibleOctants[id]);
            }
        }
        //console.log(octRequests);
        if (octRequests.length == 0) {
            starsUpdated = true;
            //console.log("Nothing new to download.");
            return;
        }
        
        getNodeStars(octRequests, function(newStars) {
            for (ix in octRequests) {
                if (!(typeof newStars[octRequests[ix]] !==
                      'undefined')) {
                    console.log("An octant we asked for is missing: " +
                                octRequests[ix]);
                }
                if (octRequests[ix] in octantDict) {
                    console.log("Aleady have that!" +
                                octRequests[ix]);
                }
                octantDict[octRequests[ix]] = newStars[octRequests[ix]];
		totalStarDownloadCount += newStars[octRequests[ix]].length;
		if (totalStarDownloadCount % 100 == 0) {
		    console.log('total stars loaded:', totalStarDownloadCount);
		}
            }
            //console.log("Nodes: " + Object.keys(octantDict).length);
            visibleOctants = tmpVisibleOctants;
            starsUpdated = true;
        }, function(error) {
            console.log('Failure: ' + error);
        });
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
