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
        $.post('/add', {'url': url})
            .success(function(resp) {
                var alertView = new AlertView();
                if (resp.result == 'ok') {
                    alertView.render('success', 'Added: ' + url);
                } else if (resp.result == 'filter_mismatch') {
                    alertView.render('error', 'Error: ' + url + ' does not match filter');
                }
            })
            .error(function() {
                var alertView = new AlertView();
                alertView.render('error', 'Unknown error');
            });
    }
});

var SearchResultsView = Backbone.View.extend({
    el: '#search-results',
    template: _.template($('#search-result-template').html()),

    fetchResults: function(query) {
        var that = this;
        $.get('/search?query=' + encodeURIComponent(query))
            .success(function(resp) {
                that.render(resp.results);
            })
            .error(function() {
                that.showError('Error');
            });
    },

    render: function(results) {
        this.$el.html(this.template({results: results, error: ''}));
        return this;
    },

    showError: function(errorMsg) {
        this.$el.html(this.template({results: [], error: errorMsg}));
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
