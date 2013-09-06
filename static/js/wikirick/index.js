$(function() {
  var AppRouter = Backbone.Router.extend({
    initialize: function() {
      Backbone.history.start({pushState: true});
    },

    routes: {
      ':name/': '_goArticle',
      ':name/edit': '_goEdit',
      ':name/source': '_goSource',
      ':name/history': '_goHistory'
    },

    _goArticle: function(name) {
      navigation.select('Article');
    },

    _goEdit: function(name) {
      navigation.select('Edit');
    },

    _goSource: function(name) {
      navigation.select('Source');
    },

    _goHistory: function(name) {
      navigation.select('History');
    }
  });

  var AppView = Backbone.View.extend({
    render: function() {
      navigation.render()
    }
  });

  var Navigation = Backbone.View.extend({
    el: $('nav'),

    create: function(articleName) {
      var navigation = new Navigation();
      navigation._articleName = articleName;
      navigation._buttons = {
        'Article': NavigationButton.prototype.create('Article', '', articleName),
        'Edit': NavigationButton.prototype.create('Edit', 'edit', articleName),
        'Source': NavigationButton.prototype.create('Source', 'source', articleName),
        'History': NavigationButton.prototype.create('History', 'history', articleName),
      };
      navigation._selected = NavigationButton.prototype.create('Dummy', '', articleName);
      return navigation;
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

    create: function(name, urlPath, articleName) {
      var button = new NavigationButton();
      button._name = name;
      button._urlPath = urlPath;
      button._selected = false;
      button._articleName = articleName;
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
        appRouter.navigate(this._articleName + '/' + this._urlPath, {trigger: true});
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

  var navigation = Navigation.prototype.create('FrontPage');
  var appRouter = new AppRouter();
  new AppView().render();
});
