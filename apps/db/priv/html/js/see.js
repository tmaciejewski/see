var AlertView = Backbone.View.extend({
    tagName: 'div',
    template: _.template($('#alert-box').html()),

    render: function(msg) {
        this.$el.html(this.template({msg: msg}));
        return this;
    }
});

var AdvancedView = Backbone.View.extend({
    el: '#advanced-box',

    events: {
        'change #url': 'addURL'
    },

    addURL: function(e) {
        var that = this;
        var url = 'http://' + e.target.value;
        $.post('/add', {'url': url}).done(function(resp) {
            var alertView = new AlertView();
            that.$('#advanced-alert-box').append(alertView.render('Added: ' + url).el);
        });
    }
});

var UrlView = Backbone.View.extend({
    tagName: 'li',
    template: _.template($('#search-result-item').html()),

    render: function(url) {
        this.$el.html(this.template({url: url}));
        return this;
    }
});

var SearchResultsView = Backbone.View.extend({
    el: '#search-results',

    render: function(results) {
        var that = this;
        if (results.length > 0) {
            this.$el.html('');
            _.each(results, function(url) {
                var urlView = new UrlView();
                that.$el.append(urlView.render(url).el);
            });
        } else {
            this.$el.html('No results');
        }
    }
});

var SearchBoxView = Backbone.View.extend({
    el: '#search-box',

    events: {
        'change #search-input': 'search'
    },

    initialize: function() {
        this.searchResultsView = new SearchResultsView();
    },

    search: function() {
        var that = this;
        var query = this.$('#search-input').val();
        $.get('/search/' + query, function(resp) {
            that.searchResultsView.render(resp.results);
        });
    }
});

var searchBoxView = new SearchBoxView();
var advancedView = new AdvancedView();
