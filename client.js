
function DrawStar(context, x, y, brightness) {
    context.fillStyle = "rgba(255,255,128,0.5)";
    context.beginPath();
    context.arc(x, y, brightness, 0, 2 * Math.PI);
    context.fill();
}

setTimeout(function() {
    var canvas = document.getElementById("stars");
    var context = canvas.getContext("2d");
    console.log(canvas.width + " " + canvas.height);
    context.fillStyle = "black";
    context.fillRect(0, 0, canvas.width, canvas.height);
    DrawStar(context, 10, 10, 7);
}, 3000);
