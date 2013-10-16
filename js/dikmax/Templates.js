// This file was automatically generated from Templates.soy.
// Please don't edit this file by hand.

goog.provide('dikmax.Templates');

goog.require('soy');
goog.require('soydata');


/**
 * @param {Object.<string, *>=} opt_data
 * @param {(null|undefined)=} opt_ignored
 * @return {string}
 * @notypecheck
 */
dikmax.Templates.codeWrapper = function(opt_data, opt_ignored) {
  var output = '';
  var lineList3 = opt_data.lines;
  var lineListLen3 = lineList3.length;
  for (var lineIndex3 = 0; lineIndex3 < lineListLen3; lineIndex3++) {
    var lineData3 = lineList3[lineIndex3];
    output += ((! (lineIndex3 == 0)) ? '\n' : '') + '<span class="line" data-linenum="' + soy.$$escapeHtml(lineIndex3 + 1) + '">' + ((lineData3 == '') ? '&nbsp;' : soy.$$filterNoAutoescape(lineData3)) + '</span>';
  }
  return output;
};


/**
 * @param {Object.<string, *>=} opt_data
 * @param {(null|undefined)=} opt_ignored
 * @return {string}
 * @notypecheck
 */
dikmax.Templates.codeTooltip = function(opt_data, opt_ignored) {
  return '<div><div class="tooltip-arrow"></div><div class="tooltip-inner">Tooltip on left</div></div>';
};


/**
 * @param {Object.<string, *>=} opt_data
 * @param {(null|undefined)=} opt_ignored
 * @return {string}
 * @notypecheck
 */
dikmax.Templates.footnote = function(opt_data, opt_ignored) {
  return '<div><div class="arrow"></div><div class="popover-inner"><h3 class="popover-title"></h3><div class="popover-content"></div></div></div>';
};
