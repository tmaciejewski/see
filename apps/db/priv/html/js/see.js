var AlertView = Backbone.View.extend({
    el: '#alert-box',
    template: _.template($('#alert-box-template').html()),

    render: function(msg) {
        this.$el.append(this.template({msg: msg}));
        return this;
    }
});

var AdvancedView = Backbone.View.extend({
    el: '#advanced-panel',

    events: {
        'change #url': 'addURL'
    },

    addURL: function(e) {
        var url = 'http://' + e.target.value;
        $.post('/add', {'url': url}).done(function(resp) {
            var alertView = new AlertView();
            alertView.render('Added: ' + url);
        });
    }
});

var SearchResultsView = Backbone.View.extend({
    el: '#search-results',
    template: _.template($('#search-result-template').html()),

    render: function(results) {
        this.$el.html(this.template({results: results}));
        return this;
    }
});

var SearchBoxView = Backbone.View.extend({
    el: '#search-box',

    events: {
        'keydown #search-input': 'keyDown',
        'click #search-button': 'search'
    },

    initialize: function() {
        this.searchResultsView = new SearchResultsView();
    },

    keyDown: function(e) {
        var ENTER = 13;
        if (e.keyCode == ENTER)
            this.search();
    },

    search: function() {
        var that = this;
        var query = this.$('#search-input').val();
        $.get('/search?query=' + encodeURIComponent(query), function(resp) {
            that.searchResultsView.render(resp.results);
        });
    }
});

var searchBoxView = new SearchBoxView();
var advancedView = new AdvancedView();
