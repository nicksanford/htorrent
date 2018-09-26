document.addEventListener("DOMContentLoaded", function () {
    var ws = new WebSocket('ws://localhost:9160/')

    ws.onopen = function(event) {
        console.log(event)
    };

    ws.onmessage = function(event) {
        console.log(event)
    };
});
