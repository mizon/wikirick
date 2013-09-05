$(function() {
  var View = Backbone.View.extend({
    initialize: function(navigation) {
      this._navigation = navigation;
    },

    render: function() {
      this._navigation.render();
    }
  });

  var Navigation = Backbone.View.extend({
    el: $('nav'),

    initialize: function() {
      this._buttons = {};
      ['Article', 'Edit', 'Source', 'History', 'View All'].forEach(function(name) {
        this._buttons[name] = NavigationButton.prototype.create(this, name);
      }, this);
      this._selected = NavigationButton.prototype.create(this, 'Dummy');
    },

    render: function() {
      this.$el.children().remove();
      var ul = $('<ul></ul>');
      ul.append($('<li id="article-title">FrontPage</li>'));
      for (var name in this._buttons) {
        this._buttons[name].render();
        ul.append(this._buttons[name].el);
      }
      this.$el.append(ul);
    },

    select: function(name) {
      var target = this._buttons[name];
      target.select();
      this._selected.unselect();
      this._selected = target;
    }
  });

  var NavigationButton = Backbone.View.extend({
    tagName: 'li',

    events: {
      'click': '_select'
    },

    create: function(navigation, name) {
      var button = new NavigationButton();
      button._navigation = navigation;
      button._name = name;
      button._selected = false;
      return button;
    },

    render: function() {
      this.$el.children().remove();
      var anchor = $('<a></a>');
      anchor.text(this._name);
      this.$el.append(anchor);
    },

    _select: function() {
      if (!this._selected)
        this._navigation.select(this._name);
    },

    select: function() {
      this._selected = true;
      this.$el.addClass('selected');
    },

    unselect: function() {
      this._selected = false;
      this.$el.removeClass('selected');
    }
  });

  var nav = new Navigation();
  var v = new View(nav);
  v.render();
  nav.select('Article');
});
