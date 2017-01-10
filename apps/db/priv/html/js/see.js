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

var AdvancedView = Backbone.View.extend({
    el: '#advanced-panel',

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
