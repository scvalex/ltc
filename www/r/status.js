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

function Event(msg, handleEvent) {
    var self = this;

    var now = new Date();
    self.time = now.toISOString();
    self.prettyTime = now.toTimeString();
    if (msg.hasOwnProperty("GetEvent")) {
        self.operation = "get";
        self.key = msg["GetEvent"].eventTarget;
        self.keyDigest = msg["GetEvent"].keyDigest
        handleEvent("get", self);
    } else if (msg.hasOwnProperty("SetEvent")) {
        self.operation = "set";
        self.key = msg["SetEvent"].eventTarget;
        self.keyDigest = msg["SetEvent"].keyDigest
        self.valueDigest = msg["SetEvent"].valueDigest
        handleEvent("set", self);
    } else {
        log("got unknown message: ", msg);
    }
}

function KeyEntry(key, keyDigest, valueDigest) {
    var self = this;

    self.key = key;
    self.keyColour = model.colourFromDigest(keyDigest);
    self.valueColour = ko.observable(model.colourFromDigest(valueDigest));
}

function AppViewModel() {
    var self = this;

    self.log = ko.observableArray();

    self.countGets = ko.observable(0);
    self.countSets = ko.observable(0);

    // Tip boxes
    self.keys = {};
    self.tipBoxWidth = ko.observable(0);;
    self.tipBoxes = ko.observableArray();

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

    var handleEvent = function(type, event) {
        if (type == "set") {
            self.countSets(self.countSets() + 1);
            if (!(event.key in self.keys)) {
                // New key!
                self.keys[event.key] = new KeyEntry(event.key, event.keyDigest, event.valueDigest);
                self.keys[event.key].valueColour(self.colourFromDigest(event.valueDigest));
                var width = document.getSize().x - $("tipGraphLegend").getSize().x - 20;
                self.tipBoxWidth(Math.floor(width / Object.keys(self.keys).length));
                self.tipBoxes.removeAll();
                var i = 0;
                for (k in self.keys) {
                    self.tipBoxes.push({key : k,
                                        x : i * self.tipBoxWidth(),
                                        keyFill : self.keys[k].keyColour,
                                        valueFill : self.keys[k].valueColour
                                       });
                    i++;
                }
                self.tipBoxes.sort(function(x, y) {
                    return x.key == y.key ? 0 : (x.key < y.key ? -1 : 1)
                });
            } else {
                // Update existing key!
                self.keys[event.key].valueColour(self.colourFromDigest(event.valueDigest));
            }
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

    self.colourFromDigest = function(digest) {
        function pad(width, string) {
            return (width <= string.length) ? string : pad(width, "0" + string, "0");
        }

        var hex = (Math.abs(digest) % (1<<24)).toString(16);
        return ("#" + pad(6, hex));
    }

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
        self.log.push(new Event(JSON.parse(event.data), handleEvent));
        // Scroll to bottom
        $("log").scrollTop = $("log").scrollHeight;
    }
}

function setupLayout() {
    var height = document.getSize().y - $$("header")[0].getSize().y - 20;
    var width = document.getSize().x;
    model.eventsGraph.configure({width: width});
    $("tipGraph").style.width = width - $("tipGraphLegend").getSize().x - 20 + "px";
}

document.addEventListener("DOMContentLoaded", function() {
    // Model is global.
    model = new AppViewModel();
    ko.applyBindings(model);
    log("document loaded");

    window.addEventListener("resize", setupLayout);
    setupLayout();
});
