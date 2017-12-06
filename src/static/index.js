// pull in desired CSS/SASS files
require( './styles/style.scss' );

var search = new URLSearchParams(document.location.search);

// inject bundled Elm app into div#main
var Elm = require('../elm/Main');
var app = Elm.Main.embed(document.getElementById('main'), {
    request: search.get('request'),
    token: search.get('token')
});
app.ports.getLocalStorage.subscribe(function() {
    var res = []
    Object.keys(window.localStorage).forEach(function(key) {
        res.push([key, window.localStorage.getItem(key)]);
    });
    app.ports.localStorage.send(res)
});
