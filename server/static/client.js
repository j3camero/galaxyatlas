// Constants
var numStarsFetched = 0;
var minNearFieldLum = 0.005;
var distUpdateThreshold = 2;
var speed = 0.3;
var turnRate = 0.03;

var visibleOctants = [];
var octantsToRemove = [];

var octantDict = {};
var starObjs = {};
var starGroup = new THREE.Group();
var starsUpdated = false;

// Camera Position and Rotation
var initialCameraPosition = new THREE.Vector3(0, 0, 3);
var initialCameraRotation = new THREE.Quaternion(0, 0, 0, 1);

var lastUpdatePosition = new THREE.Vector3(0, 0, 0);

// Variables for keyboard/touchscreen controls
var keys = new Array();
var draggingCanvas = false;
var dragVector = {x: null, y: null};
var pressForward = false;
var activeDrag = false;

// Three.js global objects
var scene = null;
var camera = null;
var renderer = null;

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

/*
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
*/

function renderStar(context, screenX, screenY, area, color) {
    
}

// Direction vectors in camera local coordinates
var fwd = new THREE.Vector3(0, 0, -1);
var up = new THREE.Vector3(0, 1, 0);
var right = new THREE.Vector3(1, 0, 0);
function doOneFrame() {
    // Schedule this function
    requestAnimationFrame( doOneFrame );

    // Handle WASD for translation
    if (keys[87] || pressForward) {
	camera.translateOnAxis(fwd, speed);
    }
    if (keys[83]) {
	camera.translateOnAxis(fwd, -speed);
    }
    if (keys[65]) {
	camera.translateOnAxis(right, -speed);
    }
    if (keys[68]) {
	camera.translateOnAxis(right, speed);
    }
    if (keys[69]) {
        camera.translateOnAxis(up, speed);
    }
    if (keys[67]) {
        camera.translateOnAxis(up, -speed);
    }

    // Handle arrow keys for rotation
    if (keys[38]) {
        // Up
        camera.rotateOnAxis(right, -turnRate);
    }
    if (keys[40]) {
        // Down
	camera.rotateOnAxis(right, turnRate);
    }
    if (keys[37]) {
        // Left
        camera.rotateOnAxis(up, turnRate);
    }
    if (keys[39]) {
        // Right
        camera.rotateOnAxis(up, -turnRate);
    }
    camera.updateProjectionMatrix();

    // Adjust the secene graph
    if (starsUpdated) {
        // Remove whole octants we know are invisible
        var starsRemovedCount = 0;
        for (idx in octantsToRemove) {
            var octantId = octantsToRemove[idx];
            for (starIdx in octantDict[octantId]) {
                var star = starObjs[octantDict[octantId][starIdx]];
                scene.remove(star.mesh);
                starsRemovedCount++;
            }
        }
        //console.log("Stars removed as whole octants: " +
        //starsRemovedCount);
        octantsToRemove = [];
        
        // Figure out what all should actually be visible
        var visibleStarCount = 0;
        for (idx in visibleOctants) {
            var octantId = visibleOctants[idx];
            for (starIdx in octantDict[octantId]) {
                var star = starObjs[octantDict[octantId][starIdx]];
                if (star.isVisible(camera.position, 0.005)) {
                    // If the star is visible, check to see if it is
                    // already in the scene graph. If so, it's mesh
                    // refernce better match that stored in the star
                    // object. If not, add it.
                    visibleStarCount++;
                    var meshRef = scene.getObjectById(star.mesh.id);
                    if (typeof(meshRef) == 'undefined') {
                        scene.add(star.mesh);
                    } else {
                        if (meshRef != star.mesh)
                            throw "WTF!?";
                    }
                } else {
                    // If the star is not visible, check to see if it
                    // is still in the scene graph. If it is, remove it.
                    var meshRef = scene.getObjectById(star.mesh.id);
                    if (typeof(meshRef) != 'undefined') {
                        scene.remove(meshRef);
                    }
                }
            }
        }
        //console.log("Stars visible: " + visibleStarCount);

        // Reset this to false
        starsUpdated = false;
        //console.log("Num objects: " + scene.children.length);
    }

    // Render the scene
    renderer.render(scene, camera);

    // Update the stars if needed
    updateStars(false);
};

// Returns immediately, with callback later.
function updateStars(force) {
    if (force)
        console.log('Forcing star update')
    var distVect = new THREE.Vector3();
    distVect.copy(lastUpdatePosition);
    distVect.sub(camera.position);
    var distFromLastUpdate = distVect.length();
    if (distFromLastUpdate < distUpdateThreshold && !force)
        return;
    lastUpdatePosition.copy(camera.position);
    var x = camera.position.x;
    var y = camera.position.y;
    var z = camera.position.z;

    numStarsFetched = 0;
    getVisibleOctants(minNearFieldLum, x, y, z, function(newOcts) {
        // Use a temporary so we don't update with this before we
        // actually have all the stars.
	var tmpVisibleOctants = newOcts;

        // Look for things to be removed from the scene.
        for (ix in visibleOctants) {
            var found = false;
            var oldOctantId = visibleOctants[ix];
            for (jx in tmpVisibleOctants) {
                var newOctantId = tmpVisibleOctants[jx];
                if (newOctantId == oldOctantId) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                octantsToRemove.push(oldOctantId);
            }
        }
        
        // Create request for stars
        var octRequests = [];
        for (id in tmpVisibleOctants) {
            if (!(tmpVisibleOctants[id] in octantDict)) {
                octRequests.push(tmpVisibleOctants[id]);
            }
        }
        if (octRequests.length == 0) {
            // No update required
            starsUpdated = true;
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
                octantDict[octRequests[ix]] = [];

                // Create star objects
                for (starIndex in newStars[octRequests[ix]]) {
                    numStarsFetched++;
                    var star = newStars[octRequests[ix]][starIndex];
                    octantDict[octRequests[ix]].push(star.sid);
                    starObjs[star.sid] = new Star(star.sid,
                                                  star.x, star.y, star.z,
                                                  star.lum,
                                                  star.r, star.g, star.b);
                }
            }
            visibleOctants = tmpVisibleOctants;
            starsUpdated = true;
            //console.log('Number of stars fetched: ' + numStarsFetched);
        }, function(error) {
            console.log('Failure: ' + error);
        });
    }, function(error) {
	console.log('Failure: ' + error);
    });
}

function initSceneAndCamera() {
    scene = new THREE.Scene();
    camera = new THREE.PerspectiveCamera( 90,
                                          (window.innerWidth /
                                           window.innerHeight),
                                          0.1, 1000 );
    camera.position.copy(initialCameraPosition);
    camera.quaternion.copy(initialCameraRotation);
    camera.updateProjectionMatrix();
}

function initRenderer() {
    renderer = new THREE.WebGLRenderer();
    renderer.setSize(window.innerWidth, window.innerHeight);
    document.body.appendChild(renderer.domElement);
}

//var geometry = new THREE.BoxGeometry( 1, 1, 1 );
//var material = new THREE.MeshBasicMaterial( { color: 0x00ff00 } );
//var cube = new THREE.Mesh( geometry, material );

window.onload = function() {
    initSceneAndCamera();
    initRenderer();
    //scene.add( cube );
    
    initKeys();

    updateStars(true);
    doOneFrame();

    /*
    setInterval(function() {
	updateStars(false);
    }, 500);
    */
};
