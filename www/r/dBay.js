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


function AppViewModel() {
    var self = this;

    self.bids = ko.observableArray();

    setInterval(function () {
        new Request({
            url: "/bids",
            onSuccess: function(data) {
                var bids = JSON.parse(data);
                log(bids);
            }
        }).get();
    }, 1000);
}

document.addEventListener("DOMContentLoaded", function() {
    // Model is global.
    model = new AppViewModel();
    ko.applyBindings(model);
    log("document loaded");
});
