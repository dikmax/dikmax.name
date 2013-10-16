goog.provide('dikmax.App');

goog.require('dikmax.CodeSaxHandler');
goog.require('dikmax.CodeTooltip');
goog.require('dikmax.FootnotePopover');
goog.require('dikmax.Templates');
goog.require('goog.Timer');
goog.require('goog.array');
goog.require('goog.date');
goog.require('goog.dom');
goog.require('goog.dom.classlist');
goog.require('goog.dom.dataset');
goog.require('goog.events');
goog.require('goog.i18n.DateTimeFormat');
goog.require('goog.soy');
goog.require('goog.string.html.HtmlParser');
goog.require('goog.style');
goog.require('goog.style.transition');
goog.require('hljs');



/**
 * @constructor
 */
dikmax.App = function() {
};


/**
 * Inits application
 */
dikmax.App.prototype.init = function() {
  this.topNavBar_();
  this.fixTimeZones_();
  this.updateCommentsText_();
  this.updateCodeListings_();
  this.inlineFootnotes_();
  this.setupKeyboardNavigation_();
};


/**
 * @private
 */
dikmax.App.prototype.topNavBar_ = function() {
  var toggleButton = goog.dom.getElementByClass('navbar-toggle-button');
  var collapsibleBlock = goog.dom.getElementByClass('navbar-collapsible-block');

  var visible = false;
  var complete = function() {
    if (visible) {
      goog.dom.classlist.add(collapsibleBlock, 'in');
      goog.dom.classlist.remove(collapsibleBlock, 'collapse');
      goog.dom.classlist.remove(collapsibleBlock, 'collapsing');
    } else {
      goog.dom.classlist.add(collapsibleBlock, 'collapse');
      goog.dom.classlist.remove(collapsibleBlock, 'collapsing');
      goog.dom.classlist.remove(collapsibleBlock, 'in');
      goog.style.setHeight(collapsibleBlock, 1);
    }
  };

  goog.events.listen(toggleButton, goog.events.EventType.CLICK, function() {
    visible = !visible;

    if (visible) {
      goog.dom.classlist.add(collapsibleBlock, 'collapsing');
      goog.dom.classlist.remove(collapsibleBlock, 'collapse');
      goog.dom.classlist.remove(collapsibleBlock, 'in');
      var height = collapsibleBlock.scrollHeight + 1;
      goog.style.setHeight(collapsibleBlock, height);
    } else {
      goog.dom.classlist.remove(collapsibleBlock, 'collapse');
      goog.dom.classlist.remove(collapsibleBlock, 'in');
      goog.dom.classlist.add(collapsibleBlock, 'collapsing');
      goog.style.setHeight(collapsibleBlock, 0);
    }
    if (goog.style.transition.isSupported()) {
      setTimeout(complete, 350);
    } else {
      complete();
    }
  });
};


/**
 * Fixing timezone in post footer
 *
 * @private
 */
dikmax.App.prototype.fixTimeZones_ = function() {
  var doc = goog.dom.getDocument();
  var spans = doc.getElementsByTagName('span');

  var format = new goog.i18n.DateTimeFormat('cccc, d MMMM yyyy, HH:mm');

  goog.array.forEach(spans, function(el) {
    var postDate = goog.dom.dataset.get(el, 'postDate');
    if (!postDate) {
      return;
    }
    var date = goog.date.fromIsoString(postDate);
    if (!date) {
      return;
    }

    var dateString = format.format(date);
    goog.dom.setTextContent(el, dateString);
  });
};


/**
 * Comments processing.
 *
 * @private
 */
dikmax.App.prototype.updateCommentsText_ = function() {
  // Replace text in comments lint to proper Russian
  var elements = goog.dom.getElementsByTagNameAndClass('span', 'post-comments');
  if (!elements.length) {
    return;
  }
  this.commentsElements_ = elements;
  this.timer_ = new goog.Timer(100);
  goog.events.listen(this.timer_, goog.Timer.TICK,
      this.checkIsCommentsLoaded_, false, this);
  this.timer_.start();
};


/**
 * @private
 */
dikmax.App.prototype.checkIsCommentsLoaded_ = function() {
  if (goog.string.startsWith(goog.dom.getTextContent(
      this.commentsElements_[0]), 'Считаем')) {
    return;
  }
  this.timer_.stop();
  goog.array.forEach(this.commentsElements_, this.changeCommentText_, this);
};


/**
 * @param {Element} item Element to update.
 * @private
 */
dikmax.App.prototype.changeCommentText_ = function(item) {
  var links = goog.dom.getElementsByTagNameAndClass('a', null, item);
  if (goog.array.isEmpty(links)) {
    return;
  }
  var text = goog.dom.getTextContent(links[0]);
  var match = text.match(/^(\d+) комментариев/);
  if (match) {
    var count = Number(match[1]);
    if (Math.round(count % 100 / 10) != 1) {
      var count10 = count % 10;
      if (count10 == 1) {
        goog.dom.setTextContent(links[0], count + ' комментарий');
      } else if (count10 >= 2 && count10 <= 4) {
        goog.dom.setTextContent(links[0], count + ' комментария');
      }
    }
  }
};


/**
 * @private
 */
dikmax.App.prototype.updateCodeListings_ = function() {
  if (this.highlightBlocks_()) {
    new dikmax.CodeTooltip();
  }
};


/**
 * @private
 * @return {boolean} Is there any blocks on page?
 */
dikmax.App.prototype.highlightBlocks_ = function() {
  /** @type {{length: number}} */
  var blocks = goog.dom.getElementsByTagNameAndClass('code', 'sourceCode');
  blocks = goog.array.filter(blocks, function(item) {
    return item.parentNode instanceof HTMLPreElement;
  });
  if (blocks.length) {
    var parser = new goog.string.html.HtmlParser();
    var handler = new dikmax.CodeSaxHandler();
    goog.array.forEach(blocks, function(block) {
      hljs.highlightBlock(block);

      parser.parse(handler, block.innerHTML);
      goog.soy.renderElement(block, dikmax.Templates.codeWrapper,
          {lines: handler.getLines()});
      goog.dom.classlist.add(block, 'highlighted');
    });
  }

  return blocks.length > 0;
};


/**
 * @private
 */
dikmax.App.prototype.inlineFootnotes_ = function() {
  new dikmax.FootnotePopover();
};


/**
 * @private
 */
dikmax.App.prototype.setupKeyboardNavigation_ = function() {
  goog.events.listen(document, goog.events.EventType.KEYDOWN, function(e) {
    if (e.ctrlKey) {
      var link;
      if (e.keyCode === 37) {
        // Previous page link
        link = goog.dom.getElementByClass('previous');
      } else if (e.keyCode === 39) {
        // Next page link
        link = goog.dom.getElementByClass('next');
      }
      if (link && goog.dom.getAncestorByClass(link, 'pager')) {
        var anchor = goog.dom.getElementsByTagNameAndClass('a', null, link);
        if (anchor.length) {
          document.location = anchor[0].getAttribute('href');
        }
      }
    }
  });
};
