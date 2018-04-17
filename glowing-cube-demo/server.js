const express = require('express');

const port = 3000;

let app = express();
app.use(express.static('static'));
app.listen(port);
console.log(`Webserver listening on port :${port}`);
