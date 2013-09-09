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

    initialize: function() {
      this._buttons = {
        '': NavigationButton.prototype.create('Article', ''),
        'edit': NavigationButton.prototype.create('Edit', 'edit'),
        'source': NavigationButton.prototype.create('Source', 'source'),
        'history': NavigationButton.prototype.create('History', 'history'),
      };
      this._selected = NavigationButton.prototype.create('Dummy', '');
    },

    select: function(name) {
      var target = this._buttons[name];
      target.select();
      console.log(this._getSelected());
      this._selected.unselect();
      this._selected = target;
    },

    _getSelected: function() {
      return this.$el.find('li').hasClass('selected');
    }
  });

  var NavigationButton = Backbone.View.extend({
    events: {
      'click': '_select'
    },

    create: function(name, actionName) {
      return new NavigationButton({
        el: $('nav').find('li:contains(' + name + ')')[0],
        actionName: actionName,
        selected: false
      });
    },

    _select: function() {
      if (!this._selected)
        appRouter.navigate(this.options.actionName, {trigger: true});
    },

    select: function() {
      this.options.selected = true;
      this.$el.addClass('selected');
    },

    unselect: function() {
      this.options.selected = false;
      this.$el.removeClass('selected');
    }
  });

  var ContentView = Backbone.View.extend({
    el: $('#main'),

    create: function(actionName) {
      return new ContentView({
        actionName: actionName
      });
    },

    render: function() {
      var $el = this.$el;
      $.get([rootPath, this.options.actionName].join('/')).done(function(body) {
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

  var navigation = new Navigation();
  var appRouter = new AppRouter();
  new AppView().render();
});
