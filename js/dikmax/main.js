goog.provide('dikmax.main');

goog.require('dikmax.App');


/**
 * Main function.
 */
dikmax.main = function() {
  var app = new dikmax.App();
  app.init();
};

if (COMPILED) {
  dikmax.main();
}
