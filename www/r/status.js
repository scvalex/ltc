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

    var eventsCurPeriod = {"get": 0, "set": 0};

    var getsPerPeriod = [{x: 0, y: 0}];
    var setsPerPeriod = [{x: 0, y: 0}];
    var eventsGraph = new Rickshaw.Graph( {
        element: document.querySelector("#eventsChart"),
        width: 500,
        height: 200,
        stroke: true,
        series: [{
            color: "blue",
            data: getsPerPeriod
        }, {
            color: "red",
            data: setsPerPeriod
        }]
    });
    // new Rickshaw.Graph.Axis.Y({
    //     graph: eventsGraph,
    //     orientation: 'left',
    //     element: document.getElementById("eventsYAxis"),
    // });
    eventsGraph.render();

    handleEvent = function(type) {
        if (type == "set") {
            self.countSets(self.countSets() + 1);
        } else if (type == "get") {
            self.countGets(self.countGets() + 1);
        }
        eventsCurPeriod[type] = eventsCurPeriod[type] + 1;
    }

    var x = 1;
    setInterval(function () {
        getsPerPeriod.push({x: x, y: eventsCurPeriod["get"]});
        setsPerPeriod.push({x: x, y: eventsCurPeriod["set"]});
        x = x + 1;
        if (x > 10) {
            // Only keep last 10 measurements
            getsPerPeriod.shift();
            setsPerPeriod.shift();
        }
        eventsGraph.render();
        eventsCurPeriod = {"get": 0, "set": 0};
    }, 5000);

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

document.addEventListener("DOMContentLoaded", function() {
    // Model is global.
    model = new AppViewModel();
    ko.applyBindings(model);
    log("document loaded");
});
