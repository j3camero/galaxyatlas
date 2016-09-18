// 3D vector

function Vector(x, y, z) {
    this.x = x;
    this.y = y;
    this.z = z;
}

Vector.prototype.copyFrom = function(v) {
    this.x = v.x;
    this.y = v.y;
    this.z = v.z;
};

Vector.prototype.isEqual = function(v) {
    return (this.x == v.x) &&
        (this.y == v.y) &&
        (this.z == v.z);
}

Vector.prototype.tensorProduct = function() {
    return new Matrix(this.x * this.x, this.x * this.y, this.x * this.z,
		      this.y * this.x, this.y * this.y, this.y * this.z,
		      this.z * this.x, this.z * this.y, this.z * this.z);
};

Vector.prototype.crossProductMatrix = function() {
    return new Matrix(0, -this.z, this.y,
		      this.z, 0, -this.x,
		      -this.y, this.x, 0);
};

Vector.crossProduct = function(a, b) {
    return new Vector(a.y * b.z - a.z * b.y,
		      a.z * b.x - a.x * b.z,
		      a.x * b.y - a.y * b.x);
};

Vector.dotProduct = function(a, b) {
    return a.x * b.x + a.y * b.y + a.z * b.z;
};

Vector.prototype.multiplyScalar = function(k) {
    return new Vector(k * this.x, k * this.y, k * this.z);
};

Vector.prototype.projectOnto = function(v) {
    var len = v.squaredLength();
    if (len < 0.0000001) {
	return new Vector(0, 0, 0);
    }
    var mult = Vector.dotProduct(this, v) / len;
    return v.multiplyScalar(mult);
};

// This function ONLY works if v is a unit vector!
Vector.prototype.projectOntoUnitVector = function(v) {
    return v.multiplyScalar(Vector.dotProduct(this, v));
};

// Performs a change of basis defined by a, b, and c. The arguments MUST be
// unit vectors or else the results are undefined!
Vector.prototype.basisProjection = function(a, b, c) {
    return new Vector(Vector.dotProduct(this, a),
		      Vector.dotProduct(this, b),
		      Vector.dotProduct(this, c));
};

// Implements Rodrigues' rotation formula. Yields a 3D rotation matrix that
// rotates some vector around this vector by the given angle.
Vector.prototype.rotationMatrix = function(angle) {
    var cos = Math.cos(angle);
    var sin = Math.sin(angle);
    var m = Matrix.identity(cos);
    var crossProductMatrix = this.crossProductMatrix();
    crossProductMatrix.multiplyScalarInPlace(sin);
    m.addInPlace(crossProductMatrix);
    var tensorProduct = this.tensorProduct();
    tensorProduct.multiplyScalarInPlace(1.0 - cos);
    m.addInPlace(tensorProduct);
    return m;
};

// Returns this vector rotated around a given axis by a given angle.
Vector.prototype.rotate = function(axis, angle) {
    var matrix = axis.rotationMatrix(angle);
    return matrix.vectorMultiply(this);
};

Vector.prototype.squaredLength = function() {
    return this.x * this.x + this.y * this.y + this.z * this.z;
};

Vector.subtract = function(a, b) {
    return new Vector(a.x - b.x, a.y - b.y, a.z - b.z);
};

Vector.prototype.subtract = function(v) {
    return Vector.subtract(this, v);
};

Vector.prototype.addInPlace = function(v) {
    this.x += v.x;
    this.y += v.y;
    this.z += v.z;
};

Vector.squaredDistance = function(a, b) {
    return Vector.subtract(a, b).squaredLength();
};
