// Star object

function Star(id, x, y, z, lum, r, g, b) {
    this.id = id;
    this.lum = lum;
    this.color = new THREE.Color( (r / 255.0),
                                  (g / 255.0),
                                  (b / 255.0) );
    console.log(this.color);
    // TODO(mthiffault): Fix this.
    var geom = new THREE.SphereGeometry( 0.1,
                                         10, // horiz segs
                                         10  // vert segs
                                       );
    var mat = new THREE.MeshBasicMaterial({color: this.color.getHex()});
    this.mesh = new THREE.Mesh(geom, mat);
    this.mesh.position.copy(new THREE.Vector3(x, y, z));
}

Star.prototype.position = function() {
    return this.mesh.position;
};

Star.prototype.isVisible = function(viewpos, lumthresh) {
    var distVec = new THREE.Vector3();
    distVec.copy(this.mesh.position);
    distVec.sub(viewpos);
    var dist = distVec.length();
    return (this.lum /
            (4 * Math.PI * dist * dist)) >= lumthresh;
}
