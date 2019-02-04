var url ='https://www.webtoons.com/en/challenge/tested/heck-of-a-start/viewer?title_no=231173&episode_no=1';
var webPage = require('webpage');
var page = webPage.create();
var fs = require('fs');

function click(el){
    var ev = document.createEvent('MouseEvent');
    ev.initMouseEvent(
        'click',
        true /* bubble */, true /* cancelable */,
        window, null,
        0, 0, 0, 0, /* coordinates */
        false, false, false, false, /* modifier keys */
        0 /*left*/, null
    );
    el.dispatchEvent(ev);
}

page.open(url, function (status) {
              page.includeJs('http://ajax.googleapis.com/ajax/libs/jquery/1.6.1/jquery.min.js', function(){
                page.evaluate(function(){
                  var replyboxes = document.getElementsByClassName('.u_cbox_btn_reply');
                  for(var i = 0, j = replyboxes.length; i < j; i++){
                    replyboxes[i].click();
                  };
                });
        });
        just_wait();
});

function just_wait() {
    setTimeout(function() {
            fs.write('comments.html', page.content, 'w');
            phantom.exit();
    }, 2500);
}

