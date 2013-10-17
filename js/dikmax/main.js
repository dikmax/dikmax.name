goog.provide('dikmax.main');

goog.require('dikmax.App');
goog.require('goog.debug.ErrorHandler'); // To prevent compiler warnings
goog.require('goog.events.EventWrapper');


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
