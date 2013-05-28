WS_URL = "ws://" + window.location.hostname + ":8000/status"

function log() {
    if (console && console.log) {
        console.log.apply(console.log, arguments);
    }
}

function LogEntry(msg, handleEvent) {
    var self = this;

    var now = new Date();
    self.time = now.toISOString();
    self.prettyTime = now.toTimeString();
    if (msg.hasOwnProperty("GetEvent")) {
        self.operation = "get";
        self.target = msg["GetEvent"].eventTarget;
        handleEvent("get");
    } else if (msg.hasOwnProperty("SetEvent")) {
        self.operation = "set";
        self.target = msg["SetEvent"].eventTarget;
        handleEvent("set");
    } else {
        log("got unknown message: ", msg);
    }
}

function AppViewModel() {
    var self = this;

    self.log = ko.observableArray();

    self.countGets = ko.observable(0);
    self.countSets = ko.observable(0);
    self.eventsPerSec = [];
    self.eventsCurrentSec = {"get": 0, "set": 0};

    handleEvent = function(type) {
        if (type == "set") {
            self.countSets(self.countSets() + 1);
        } else if (type == "get") {
            self.countGets(self.countGets() + 1);
        }
        self.eventsCurrentSec[type] = self.eventsCurrentSec[type] + 1;
    }

    setInterval(function () {
        self.eventsPerSec.push(self.eventsCurrentSec);
        self.eventsCurrentSec = {"get": 0, "set": 0};
    }, 1000);

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
        self.log.push(new LogEntry(JSON.parse(event.data), handleEvent));
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
