// Functions and data types for deep field rendering

// A single pixel in our simulated CCD
function CCDPixel(x,y,r,g,b,a) {
    this.x = x;
    this.y = y;
    this.strikes = 0; // Number of objects accumulated
    this.r = r; // Red
    this.g = g; // Green
    this.b = b; // Blue
    this.a = a; // Alpha/Brightness
}

function blankCCDPixel(x,y) {
    return new CCDPixel(x,y,0.0,0.0,0.0,0.0);
}

CCDPixel.prototype.clear = function() {
    this.strikes = 0;
    this.r = 0.0;
    this.g = 0.0;
    this.b = 0.0;
    this.a = 0.0;
}

CCDPixel.prototype.accumulate = function(r,g,b,a) {
    var lastStrikes = this.strikes;
    this.strikes += 1;
    this.r = ((this.r * lastStrikes) + r) / this.strikes;
    this.g = ((this.g * lastStrikes) + g) / this.strikes;
    this.b = ((this.b * lastStrikes) + b) / this.strikes;
    this.a += a;
}

CCDPixel.prototype.distanceFrom = function(x,y) {
    var dx = (this.x - x);
    var dy = (this.y - y);
    return Math.sqrt( (dx * dx) + (dy * dy) );
}

// CCD object holds a 2d array of CCDPixels
function CCD(width, height) {
    this.width = width;
    this.height = height;

    this.array = [];
    for (var i = 0; i < width; ++i) {
        this.array[i] = [];
        for (var j = 0; j < height; ++j) {
            this.array[i][j] = blankCCDPixel(i,j);
        }
    }
}

CCD.prototype.clear = function() {
    for (var j = 0; j < this.height; ++j) {
        for (var i = 0; i < this.width; ++i) {
            this.array[i][j].clear();
        }
    }
}

CCD.prototype.exposeCircle = function(x,y,rad,lum,color) {
    var startX = (x - rad);
    var startY = (y - rad);
    startY = Math.max(startY, 0);
    var endX = x + rad;
    var endY = y + rad;
    endY = Math.min(endY, this.height);
    
    for (var j = startY; j < endY; ++j) {
        var dy = Math.abs(y - j);
        var jmp = 0.0;
        if (dy > 0.0) {
            jmp = Math.round(
                rad - Math.round(Math.sqrt((rad*rad) - (dy*dy)))
            );
        }
        var newStartX = Math.max(0, Math.min(startX + jmp, this.width));
        var newEndX = Math.max(0, Math.min(endX - jmp, this.width));
        for (var i = newStartX; i < newEndX; ++i) {
            this.array[i][j].accumulate(color.r,color.g,color.b,lum);
        }
    }
}

CCD.prototype.drawToImage = function(image) {
    for (var j = 0; j < this.height; ++j) {
        for (var i = 0; i < this.width; ++i) {
            var pxl = this.array[i][j];
            if (pxl.strikes == 0)
                continue;
            var rIx = ((j*(image.width*4)) + (i*4));
            var gIx = rIx + 1;
            var bIx = gIx + 1;
            var aIx = bIx + 1;
            //console.log(pxl.a);
            image.data[rIx] = Math.round(pxl.r);
            image.data[gIx] = Math.round(pxl.g);
            image.data[bIx] = Math.round(pxl.b);
            image.data[aIx] = Math.round(pxl.a);
        }
    }
}
