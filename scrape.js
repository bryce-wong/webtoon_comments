var url ='https://www.webtoons.com/en/challenge/tested/heck-of-a-start/viewer?title_no=231173&episode_no=1';
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

