WS_URL = "ws://" + window.location.hostname + ":8000/status"

function log() {
    if (console && console.log) {
        console.log.apply(console.log, arguments);
    }
}

function AppViewModel() {
    var self = this;

    self.socket = new WebSocket(WS_URL, "status");
    self.socket.onopen = function() {
        log("websocket open");
    }
    self.socket.onerror = function(error) {
        log("websocket error: ", error)
    }
    self.socket.onclose = function(event) {
        log("websocket closed: ", event)
    }
    self.socket.onmessage = function(event) {
        log("message: ", event.data)
    }
}

document.addEvent("domready", function() {
    // Model is global.
    model = new AppViewModel();
    ko.applyBindings(model);
    log("document loaded");
});
