var app = Elm.Main.fullscreen();

const storage = window.localStorage || {
  setItem(key, value) {
    this[key] = value;
  },
  getItem(key) {
    return this[key];
  }
};
var storageKey = "progrissStore";

app.ports.persistStore.subscribe(function(value) {
  localStorage.setItem(storageKey, value);
});

app.ports.triggerStoreLoad.subscribe(function() {
  if (localStorage.getItem(storageKey) != null) {
    app.ports.loadStore.send(localStorage.getItem(storageKey));
  };
});

