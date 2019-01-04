'use strict';

// require('./compare')
require('./index.html');
require('./elm-canvas.js')
const Elm = require('./Main.elm')
// const { reduce } = require('lodash/fp')

// Elm.Main.embed(document.getElementById('main'));
const app = Elm.Elm.Main.init({ node: document.querySelector('#main') })
