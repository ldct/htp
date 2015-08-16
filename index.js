var express = require('express');
var app = express();
var bodyParser = require('body-parser');
var fs = require('fs');
var crypto = require('crypto');
var child_process = require('child_process');

var md5 = function(buf) {
  return crypto.createHash('md5').update(buf).digest("hex");
}

app.use(bodyParser.json({ 'limit': '10mb' }));

app.use(express.static('static'));

var interpreters = {};
var res_of = {};

app.post('/debug', function (req, res) {
  var source_code = req.body.source_code;
  var process_name = md5(source_code);
  var filename = process_name + '.np';
  fs.writeFileSync(filename, source_code);

  interpreters[process_name] = child_process.spawn('./main', ['d', filename]);
  interpreters[process_name].stdout.on('data', function (data) {
    console.log('stdout:', data, res_of[process_name]);
    if (res_of[process_name]) res_of[process_name].json({
      'data': data,
    });
  });

  res.json({
    'process_name': process_name,
    'source_code': source_code,
  });
});

app.post('/process/:process_name/f', function (req, res) {
  console.log('setting', req.params.process_name);
  res_of[req.params.process_name] = res;
  // console.log('res_of', res_of);
  interpreters[req.params.process_name].stdin.write('f\nio');
});

app.post('/process/:process_name/io', function (req, res) {
  console.log('setting', req.params.process_name);
  res_of[req.params.process_name] = res;
  // console.log('res_of', res_of);
  interpreters[req.params.process_name].stdin.write('io');
});

var port = +process.argv[2] || 3000;
var server = app.listen(port, function() {

  var host = server.address().address;
  var port = server.address().port;

  console.log('htp listening at http://%s:%s', host, port);

});