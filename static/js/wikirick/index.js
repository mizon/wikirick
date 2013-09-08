$(function() {
  var AppRouter = Backbone.Router.extend({
    routes: {
      '': '_goArticle',
      ':action': '_runAction'
    },

    _goArticle: function() {
      this._runAction('');
    },

    _runAction: function(action) {
      navigation.select(action);
      ContentView.prototype.create(action).render();
    }
  });

  var AppView = Backbone.View.extend({
    render: function() {
      navigation.render();
    },

    changeContent: function(name) {
      ContentView.prototype.create(name, 'history').render();
    }
  });

  var Navigation = Backbone.View.extend({
    el: $('nav'),

    create: function() {
      var navigation = new Navigation();
      navigation._buttons = {
        '': NavigationButton.prototype.create('Article', ''),
        'edit': NavigationButton.prototype.create('Edit', 'edit'),
        'source': NavigationButton.prototype.create('Source', 'source'),
        'history': NavigationButton.prototype.create('History', 'history'),
      };
      navigation._selected = NavigationButton.prototype.create('Dummy', '');
      return navigation;
    },

    render: function() {
      this.$el.children().remove();
      var ul = $('<ul></ul>');
      ul.append($('<li class="title">FrontPage</li>'));
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

    create: function(name, actionName) {
      var button = new NavigationButton();
      button._name = name;
      button._actionName = actionName;
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
        appRouter.navigate(this._actionName, {trigger: true});
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

  var ContentView = Backbone.View.extend({
    el: $('#main'),

    create: function(actionName) {
      var view = new ContentView();
      view._actionName = actionName;
      return view;
    },

    render: function() {
      var $el = this.$el;
      $.get([rootPath, this._actionName].join('/')).done(function(body) {
        $el
          .children()
          .remove()
          .end()
          .append($(body));
      });
    }
  });

  var rootPath = /^(.*\/wiki\/.+?)(\/.*)?$/.exec(window.location.pathname)[1];
  Backbone.history.start({pushState: true, root: rootPath});

  var navigation = Navigation.prototype.create('FrontPage');
  var appRouter = new AppRouter();
  new AppView().render();
});
