var saveKey = "space-wiz-save";
var initialState = {
    getStorage: localStorage.getItem(saveKey),
}
var main = Elm.fullscreen(Elm.Main, initialState);
main.ports.setStorage.subscribe(function(toSave) {
    if (toSave !== "") {
        localStorage.setItem(saveKey, toSave);
    }
});
main.ports.clearStorage.subscribe(function(unit) {
    localStorage.setItem(saveKey, null);
});
