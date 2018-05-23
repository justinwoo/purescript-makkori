var express = require("express");

exports._makeApp = function() {
  return express();
};

exports._registerMethod = function(method, path, handler, app) {
  return app[method](path, handler);
};

exports._makeStaticMiddleware = function(root, options) {
  return express.static(root, options);
};

exports._makeJSONMiddleware = function(options) {
  return express.json(options);
};

exports._use = function(path, entity, app) {
  return app.use(path, entity);
};

exports._listen = function(port, cb, app) {
  return app.listen(port, cb);
};

exports._close = function(cb, server) {
  return server.close(cb);
};

exports._sendResponse = function(body, response) {
  return response.send(body);
};

exports._set = function(field, value, response) {
  return response.set(field, value);
};

exports._setStatus = function(status, response) {
  return response.status(status);
};

exports._getBody = function(request) {
  return request.body;
};

exports._getParams = function(request) {
  return request.params;
};
