var childProcess = require("child_process");

exports._fork = function(module) {
    return function(args) {
        return function(options) {
            return function () {
                return childProcess.fork(module, args, options);
            };
        };
    };
};

exports._unref = function(child) {
    return function() {
        child.unref();
    };
};

exports._disconnect = function(child) {
    return function() {
        child.disconnect();
    };
};
