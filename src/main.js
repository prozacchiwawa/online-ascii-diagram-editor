window.keySendToApp = function() {
    var result = {};
    result.kdl = function(ev) {
        var appDiv = document.getElementById('drawing-cursor');
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
        var appDiv = document.getElementById('drawing-cursor');
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

var app = require("./demo.bs").main(document.getElementById("app"));
