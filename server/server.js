"use strict";

const express = require("express");
const cors = require("cors");
const child_process = require("child_process");
const fs = require("fs");
const path = require("path");

// Constants
const PORT = 8080;
const HOST = "0.0.0.0";

// App
const app = express();
app.use(cors());

var bodyParser = require("body-parser");
app.use(bodyParser.json()); // support json encoded bodies

app.post("/compile", function(req, res) {
  const filename = req.body.filename;
  const output = req.body.output;
  console.log("compile", filename, output);

  child_process.execFile(
    "elm-make",
    ["--yes", filename, "--output", output],
    { cwd: "work" },
    function(err, stdout, stderr) {
      if (err) {
        console.log("Err", stderr);
        res.send({
          code: err.code,
          stdout: stdout,
          stderr: stderr
        });
      } else {
        console.log("OK");
        res.send({
          code: 0,
          stdout: stdout,
          stderr: stderr
        });
      }
    }
  );
});

app.post("/eval", function(req, res) {
    const filename = req.body.filename;
    console.log("eval", filename);

    child_process.execFile(
        "node",
        [filename],
        { cwd: "work" },
        function(err, stdout, stderr) {
            if (err) {
                console.log("Err", stderr);
                res.send({
                    code: err.code,
                    stdout: stdout,
                    stderr: stderr
                });
            } else {
                console.log("OK");
                res.send({
                    code: 0,
                    stdout: stdout,
                    stderr: stderr
                });
            }
        }
    );
});
app.post("/writeElmFile", function(req, res) {
  const filename = req.body.filename;
  const content = req.body.content;
  console.log("writeElmFile", filename);

  fs.writeFile(path.join("work", filename), content, function(err) {
    if (err) {
      console.log("Err", err);
      res.send({
        code: 1,
        stdout: "",
        stderr: err.toString()
      });
    } else {
      console.log("OK");
      res.send({
        code: 0,
        stdout: "",
        stderr: ""
      });
    }
  });
});

app.post("/reset", function(req, res) {
  console.log("reset");

  child_process.execFile("rm", ["-rf", "work"], { cwd: "." }, function(
    err,
    stdout1,
    stderr1
  ) {
    child_process.execFile("mkdir", ["-p", "work"], { cwd: "." }, function(
      err,
      stdout2,
      stderr2
    ) {
      if (err) {
        console.log("Err", stderr);
        res.send({
          code: err.code,
          stdout: stdout1 + "\n" + stdout2,
          stderr: stderr1 + "\n" + stderr2
        });
      } else {
        console.log("OK");
        res.send({
          code: 0,
          stdout: stdout1 + "\n" + stdout2,
          stderr: stderr1 + "\n" + stderr2
        });
      }
    });
  });
});

app.listen(PORT, HOST);
console.log("Running on http://" + HOST + ":" + PORT);
