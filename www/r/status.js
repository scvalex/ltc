WS_URL = "ws://" + window.location.hostname + ":" + window.location.port + "/status";
GRAPH_PERIOD = 1000;
MAX_PERIODS = 30;

// Palette:
//   - Mighty Slate     :: #556270
//   - Pacifica         :: #4ECDC4
//   - apple chic       :: #C7F464
//   - Cheery Pink      :: #FF6B6B
//   - grandma's pillow :: #C44D58

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
        self.keyDigest = msg["GetEvent"].keyDigest
        handleEvent("get");
    } else if (msg.hasOwnProperty("SetEvent")) {
        self.operation = "set";
        self.target = msg["SetEvent"].eventTarget;
        self.keyDigest = msg["SetEvent"].keyDigest
        self.valueDigest = msg["SetEvent"].valueDigest
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

    var eventsCurPeriod = {"get": 0, "set": 0};

    var x = 0;
    var getsPerPeriod = [];
    var setsPerPeriod = [];
    for (; x < MAX_PERIODS; x++) {
        getsPerPeriod.push({x: x, y: 0});
        setsPerPeriod.push({x: x, y: 0});
    }
    self.eventsGraph = new Rickshaw.Graph( {
        element: document.querySelector("#eventsGraph"),
        width: 500,
        height: 200,
        stroke: true,
        series: [{
            color: "#4ECDC4",
            data: getsPerPeriod
        }, {
            color: "#C7F464",
            data: setsPerPeriod
        }]
    });
    // new Rickshaw.Graph.Axis.Y({
    //     graph: self.eventsGraph,
    //     orientation: 'left',
    //     element: document.getElementById("eventsYAxis"),
    // });
    self.eventsGraph.render();

    handleEvent = function(type) {
        if (type == "set") {
            self.countSets(self.countSets() + 1);
        } else if (type == "get") {
            self.countGets(self.countGets() + 1);
        }
        eventsCurPeriod[type] = eventsCurPeriod[type] + 1;
    }

    setInterval(function () {
        // Add current counters to overall counters
        getsPerPeriod.push({x: x, y: eventsCurPeriod["get"]});
        setsPerPeriod.push({x: x, y: eventsCurPeriod["set"]});

        x = x + 1;
        // Only keep last 10 measurements
        getsPerPeriod.shift();
        setsPerPeriod.shift();

        // Render graph
        self.eventsGraph.render();

        // Reset event counters for next period
        eventsCurPeriod = {"get": 0, "set": 0};
    }, GRAPH_PERIOD);

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

function setupLayout() {
    var height = document.getSize().y - $$("header")[0].getSize().y - 20;
    var width = document.getSize().x;
    model.eventsGraph.configure({width: width});
}

document.addEventListener("DOMContentLoaded", function() {
    // Model is global.
    model = new AppViewModel();
    ko.applyBindings(model);
    log("document loaded");

    window.addEventListener("resize", setupLayout);
    setupLayout();
});
