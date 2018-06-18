const Matrix = require('./matrix');
const Vector = require('./vector');

let canvas = document.getElementById('cubecanvas');
let context = canvas.getContext('2d');

let rotationMatrix = null;
let rotationDegrees = 0;
const orbitRadius = 10;

function drawCube(corner, corner2) {
  const centerX = 0.5 * canvas.width;
  const centerY = 0.5 * canvas.height;
  const scale = 1.4 * min(centerX, centerY);
  const v1 = corner1;
  const v2 = corner2;
  const v3 = new Vector(v1.x, v2.y, v1.z);
  const v4 = new Vector(v2.x, v2.y, v1.z);
  const v5 = new Vector(v2.x, v1.y, v1.z);
  const v6 = new Vector(v1.x, v2.y, v1.z);
  const v7 = new Vector(v2.x, v2.y, v1.z);
  const v8 = new Vector(v2.x, v1.y, v1.z);
  const vertices = [v1, v2, v3, v4, v5, v6, v7, v8];
  context.fillStyle = 'rgba(255,255,255,0.5)';
  for (let i = 0; i < vertices.length; ++i) {
    const vertex = vertices[i];
    let transformed = rotationMatrix.vectorMultiply(vertex);
    transformed.z += orbitRadius;
    if (transformed.z < 0.0000001) {
      continue;
    }
    const sx = centerX + scale * transformed.x / transformed.z;
    const sy = centerY + scale * transformed.y / transformed.z;
    context.beginPath();
    context.arc(sx, sy, 10, 0, 2 * Math.PI);
    context.fill();
  }
}

function doFrame() {
  context.fillStyle = 'black';
  context.fillRect(0, 0, canvas.width, canvas.height);
  const rotationAxis = new Vector(1, 0.5, 0);
  const rotationRadians = rotationDegrees * Math.PI / 180;
  rotationMatrix = rotationAxis.rotationMatrix(rotationRadians);
  rotationDegrees += 1;
  drawCube(new Vector(1, 1, 1), new Vector(2, 2, 2));
  setTimeout(doFrame, 30);
}

setTimeout(doFrame, 1);
