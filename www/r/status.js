WS_URL = "ws://" + window.location.hostname + ":8000/status"

function log() {
    if (console && console.log) {
        console.log.apply(console.log, arguments);
    }
}

function LogEntry(msg) {
    var self = this;

    var now = new Date();
    self.time = now.toISOString();
    self.prettyTime = now.toTimeString();
    if (msg.hasOwnProperty("GetEvent")) {
        self.operation = "get";
        self.target = msg["GetEvent"].eventTarget;
        model.countGets(model.countGets() + 1);
    } else if (msg.hasOwnProperty("SetEvent")) {
        self.operation = "set";
        self.target = msg["SetEvent"].eventTarget;
        model.countSets(model.countSets() + 1);
    } else {
        log("got unknown message: ", msg);
    }
}

function AppViewModel() {
    var self = this;

    self.log = ko.observableArray();
    self.countGets = ko.observable(0);
    self.countSets = ko.observable(0);

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
        self.log.push(new LogEntry(JSON.parse(event.data)));
        // Scroll to bottom
        $("log").scrollTop = $("log").scrollHeight;
    }
}

document.addEvent("domready", function() {
    // Model is global.
    model = new AppViewModel();
    ko.applyBindings(model);
    log("document loaded");
});
