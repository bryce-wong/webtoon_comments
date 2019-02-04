var url ='https://www.webtoons.com/en/challenge/tested/heck-of-a-start/viewer?title_no=231173&episode_no=1';
var webPage = require('webpage');
var page = webPage.create();
var fs = require('fs');

page.open(url, function (status) {
        page.includeJs('http://ajax.googleapis.com/ajax/libs/jquery/1.6.1/jquery.min.js', function(){
                page.evaluate(function(){
                  document.querySelector('u_cbox_btn_reply').click();
                });
        just_wait();
});

function just_wait() {
    setTimeout(function() {
            fs.write('comments.html', page.content, 'w');
            phantom.exit();
            });
    }, 2500);
}

