// pull in desired CSS/SASS files
require( './styles/style.scss' );

var search = new URLSearchParams(document.location.search);
console.log(search.get('request'), search.get('token'));

// inject bundled Elm app into div#main
var Elm = require('../elm/Main');
Elm.Main.embed(document.getElementById('main'), {
    request: search.get('request'),
    token: search.get('token')
});
