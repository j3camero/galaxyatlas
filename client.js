
function DrawStar(context, x, y, radius, alpha) {
    context.fillStyle = "rgba(255,255,128," + alpha + ")";
    context.beginPath();
    context.arc(x, y, radius, 0, 2 * Math.PI);
    context.fill();
}

setInterval(function() {
    var canvas = document.getElementById("stars");
    var context = canvas.getContext("2d");
    context.fillStyle = "black";
    context.fillRect(0, 0, canvas.width, canvas.height);
    for (var i = 1; i < 20; ++i) {
	for (var j = 1; j < 20; ++j) {
	    DrawStar(context,
		     10 * i + Math.random(),
		     10 * j + Math.random(),
		     0.05 * i,
		     0.05 * j);
	}
    }
}, 1000);

function MakeBlankCanvas(width, height, color) {
    var canvas = document.createElement("canvas");
    canvas.width = width;
    canvas.height = height;
    var context = canvas.getContext("2d");
    context.fillStyle = color;
    context.fillRect(0, 0, width, height);
    return canvas;
}

function TotalBrightness(canvas) {
    var context = canvas.getContext("2d");
    var imageData = context.getImageData(0, 0, canvas.width, canvas.height);
    for (var i = 0; i < imgageData.data.length; i += 4) {
	var brightness = Math.max(imageData.data[i],
				  imageData.data[i + 1],
				  imageData.data[i + 2]);
    }
}

console.log(threeParsecs.length);
