window.keySendToApp = function() {
    var result = {};
    result.kdl = function(ev) {
        var appDiv = document.getElementById('ruler-container');
        var edit = document.getElementById('edit');
        var editClass = edit ? edit.getAttribute('class') : null;
        if (appDiv !== null && editClass === 'edit-hidden') {
            var repeat = new CustomEvent('keydown', { detail: { key: ev.key, keyCode: ev.keyCode } });
            ev.stopPropagation();
            ev.preventDefault();
            appDiv.dispatchEvent(repeat);
        }
    };
    window.addEventListener('keydown', result.kdl);

    result.kul = function(ev) {
        var appDiv = document.getElementById('ruler-container');
        var edit = document.getElementById('edit');
        var editClass = edit ? edit.getAttribute('class') : null;
        if (appDiv !== null && editClass === 'edit-hidden') {
            var repeat = new CustomEvent('keyup', { detail: { key: ev.key, keyCode: ev.keyCode } });
            ev.stopPropagation();
            ev.preventDefault();
            appDiv.dispatchEvent(repeat);
        }
    };

    window.addEventListener('keyup', result.kul);

    return result;
};

window.keyCancel = function(ksr) {
    window.removeEventListener('keydown', ksr.kdl);
    window.removeEventListener('keyup', ksr.kul);
};

window.getFileContent = function(ev) {
    var handleFile = function(file) {
        file.text().then((fdata) => {
            var appDiv = document.getElementById('mousecover');
            var event = new CustomEvent('file', { detail: { name: file.name, data: fdata } });
            console.log('file',file.name,'data',fdata);
            appDiv.dispatchEvent(event);
        });
    };
    if (ev.dataTransfer.items) {
        for (var i = 0; i < ev.dataTransfer.items.length; i++) {
            var item = ev.dataTransfer.items[i];
            if (item.kind === 'file') {
                var file = item.getAsFile();
                handleFile(file);
            }
        }
    } else {
        for (var i = 0; i < ev.dataTransfer.files.length; i++) {
            var item = ev.dataTransfer.files[i];
            handleFile(item);
        }
    }
    return null;
};

var app = require("./demo.bs").main(document.getElementById("app"));
