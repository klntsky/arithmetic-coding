exports.saveAsPlainText = function (filename) {
    return function (contents) {
        var FileSaver = require('file-saver');
        var blob = new Blob([contents], {type: "text/plain;charset=utf-8"});
        FileSaver.saveAs(blob, filename);
    };
};
