var url = 'http://therapboard.com';
var page = new WebPage();
var fs = require('fs');

page.open(url, function (status) {
        just_wait();
});

function just_wait() {
    setTimeout(function() {
               fs.write('comments.html', page.content, 'w');
            phantom.exit();
    }, 2500);
}

