"use strict";

const express = require("express");
const cors = require("cors");

// Constants
const PORT = 8080;
const HOST = "0.0.0.0";

// App
const app = express();
app.use(cors());

var bodyParser = require("body-parser");
app.use(bodyParser.json()); // support json encoded bodies

app.post("/compile", function(req, res) {
  console.log("compile", req.body.filename);

  res.send("{}");
});

app.listen(PORT, HOST);
console.log("Running on http://" + HOST + ":" + PORT);
