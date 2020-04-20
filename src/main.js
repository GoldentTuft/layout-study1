require('./style.scss');
const { Elm } = require('./Main.elm');

var app = Elm.Main.init({
  node: document.getElementById('elm')
});
