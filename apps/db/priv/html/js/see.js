var ENTER = 13;

var AlertView = Backbone.View.extend({
    el: '#alert-box',

    render: function(type, msg) {
        var templateBody = '';
        if (type == 'success')
            templateBody = $('#alert-success-template').html();
        else if (type == 'error')
            templateBody = $('#alert-error-template').html();

        this.$el.append(_.template(templateBody)({msg: msg}));
        return this;
    }
});

var MoreView = Backbone.View.extend({
    el: '#more-panel',

    events: {
        'keydown #url': 'keyDown',
        'click #add-url': 'addURL'
    },

    keyDown: function(e) {
        if (e.keyCode == ENTER)
            this.addURL();
    },

    addURL: function() {
        var url = this.$('#url').val();
        $.post('/add', {'url': url}).done(function(resp) {
            var alertView = new AlertView();
            if (resp.result == 'ok') {
                alertView.render('success', 'Added: ' + url);
            } else {
                alertView.render('error', 'Cannot add: ' + url);
            }
        });
    }
});

var SearchResultsView = Backbone.View.extend({
    el: '#search-results',
    template: _.template($('#search-result-template').html()),

    fetchResults: function(query) {
        var that = this;
        $.get('/search?query=' + encodeURIComponent(query), function(resp) {
            that.render(resp.results);
        });
    },

    render: function(results) {
        this.$el.html(this.template({results: results}));
        return this;
    }
});

var SearchBoxView = Backbone.View.extend({
    el: '#search-box',

    events: {
        'keydown #search-input': 'keyDown',
        'click #search-button': 'triggerSearch'
    },

    initialize: function(router) {
        this.router = router;
    },

    setQuery: function(query) {
        this.$('#search-input').val(query);
    },

    keyDown: function(e) {
        if (e.keyCode == ENTER)
            this.triggerSearch();
    },

    triggerSearch: function() {
        var query = this.$('#search-input').val();
        this.router.navigate('search/' + query, {trigger: true});
    }
});

var Router = Backbone.Router.extend({
    routes: {
        '': 'index',
        'search/*query': 'search'
    },

    initialize: function() {
        this.searchBoxView = new SearchBoxView(this);
        this.moreView = new MoreView();
        this.searchResultsView = new SearchResultsView();
    },

    index: function() {
    },

    search: function(query) {
        this.searchBoxView.setQuery(query);
        this.searchResultsView.fetchResults(query);
    }
});

var router = new Router();
Backbone.history.start();
