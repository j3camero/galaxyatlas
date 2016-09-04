
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
	    DrawStar(context, 10 * i, 10 * j, 0.1 * i, 0.1 * j);
	}
    }
}, 1000);
