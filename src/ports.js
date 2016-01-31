var saveKey = "space-wiz-save";
var initialState = {
    getStorage: localStorage.getItem(saveKey),
    didClearStorage: false
}
var main = Elm.fullscreen(Elm.Main, initialState);
main.ports.setStorage.subscribe(function(toSave) {
    if (toSave !== "") {
        localStorage.setItem(saveKey, toSave);
    }
});
main.ports.clearStorage.subscribe(function(shouldClear) {
    if (shouldClear) {
        var accepted = confirm("Really delete save?");
        main.ports.didClearStorage.send(accepted)
    }
});
