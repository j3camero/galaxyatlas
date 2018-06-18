// Represents 3x3 matrix. Used for doing 3D calculations.

const Vector = require('./vector');

function Matrix(xx, xy, xz,
		yx, yy, yz,
		zx, zy, zz) {
    this.xx = xx;
    this.xy = xy;
    this.xz = xz;
    this.yx = yx;
    this.yy = yy;
    this.yz = yz;
    this.zx = zx;
    this.zy = zy;
    this.zz = zz;
}

Matrix.prototype.multiplyScalar = function(k) {
    return new Matrix(k * this.xx, k * this.xy, k * this.xz,
		      k * this.yx, k * this.yy, k * this.yz,
		      k * this.zx, k * this.zy, k * this.zz);
};

Matrix.prototype.multiplyScalarInPlace = function(k) {
    this.xx *= k;
    this.xy *= k;
    this.xz *= k;
    this.yx *= k;
    this.yy *= k;
    this.yz *= k;
    this.zx *= k;
    this.zy *= k;
    this.zz *= k;
};

Matrix.prototype.addInPlace = function(other) {
    this.xx += other.xx;
    this.xy += other.xy;
    this.xz += other.xz;
    this.yx += other.yx;
    this.yy += other.yy;
    this.yz += other.yz;
    this.zx += other.zx;
    this.zy += other.zy;
    this.zz += other.zz;
};

Matrix.prototype.absoluteMax = function() {
    return Math.max(this.xx, this.xy, this.xz,
		    this.yx, this.yy, this.yz,
		    this.zx, this.zy, this.zz,
		    -this.xx, -this.xy, -this.xz,
		    -this.yx, -this.yy, -this.yz,
		    -this.zx, -this.zy, -this.zz);
};

Matrix.add = function(a, b) {
    return new Matrix(a.xx + b.xx, a.xy + b.xy, a.xz + b.xz,
		      a.yx + b.yx, a.yy + b.yy, a.yz + b.yz,
		      a.zx + b.zx, a.zy + b.zy, a.zz + b.zz);
};

Matrix.subtract = function(a, b) {
    return new Matrix(a.xx - b.xx, a.xy - b.xy, a.xz - b.xz,
		      a.yx - b.yx, a.yy - b.yy, a.yz - b.yz,
		      a.zx - b.zx, a.zy - b.zy, a.zz - b.zz);
};

Matrix.maxDifference = function(a, b) {
    difference = Matrix.subtract(a, b);
    return difference.absoluteMax();
};

Matrix.identity = function(k) {
    if (k === undefined) {
	k = 1;
    }
    return new Matrix(k, 0, 0,
		      0, k, 0,
		      0, 0, k);
};

Matrix.prototype.vectorMultiply = function(v) {
    var r = new Vector(v.x * this.xx + v.y * this.xy + v.z * this.xz,
		       v.x * this.yx + v.y * this.yy + v.z * this.yz,
		       v.x * this.zx + v.y * this.zy + v.z * this.zz);
    return r;
};

module.exports = Matrix;
