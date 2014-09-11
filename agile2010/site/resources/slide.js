/*
 * copyleft OQube 2008
 */

var slideshow = slideshow || {};

jQuery(document).ready(
    function() {

        var stasis = {};

        for ( var item in window ) {
            stasis[item] = true;
        };

        function restoreState() {
            for ( var item in window ) {
                if ( !stasis[item] ) {
                    window[item] = undefined;
                    delete window[item];
                }
            }
        };

        slideshow.$ = jQuery;
        slideshow.position = 0;
        slideshow.title = $('title').text();

        var runcontext = {
            assert: function  (pass, msg){
                var type = pass ? "PASS" : "FAIL";
                jQuery("#results").append("<li class='" + type + "'><b>" + type + "</b> " + msg + "</li>");
            },

            error: function (msg){
                jQuery("#results").append("<li class='ERROR'><b>ERROR</b> " + msg + "</li>");
            },

            log: function (){
                var msg = "";
                for ( var i = 0; i < arguments.length; i++ ) {
                    msg += " " + arguments[i];
                }
                jQuery("#results").append("<li class='LOG'><b>LOG</b> " + msg + "</li>");
            }
        };

        function showTOC(headlines) {
            this.$('#content').html("<ol id=\"toc\"></ol>");
            headlines.each(function(index) {
                    var slideNumber = $("#slides h3").index(this);
                    var anchor = "<a href='#" + (slideNumber +1)+ "'>" + (index+1)+ ") " + $(this).text()+"</a>";
                    $('#content #toc').append("<li>"+ anchor + "</li>");
                    $('#content #toc li a').eq(index).click(function() {
                            slideshow.show(slideNumber +1);
                            return false;
                        });
                });
            this.$('#titre').text($('title').text()).removeClass("large");
        };

        function clearContent() {
            this.$("#content").empty();
            this.$("#pre").empty();
            this.$("#code").val('');
            this.$("#titre").empty();
            this.$("#results").empty();
        };

        function checkRunnable(source) {
            if ( source.match(/assert\(|log\(|error\(/) )
                this.$("#run").show();
            else
                this.$("#run").hide();
        };

        function resizeCode(code) {
	    if ( code[0].scrollHeight > 275 )
		code.add("#pre").height( code[0].scrollHeight + 5 );
        };

      function showCodeSlide(source,language) {
            var code = this.$("#code");
            this.$('#pre').show();
            this.$('#pre').attr("class",language);
            this.$('#code').show();
            this.$('#content').hide();
            this.$("#pre").html( source ).chili();
            this.$('#titre').removeClass("large");
            code.val( source.replace(/&amp;/g, "&").replace(/&lt;/g, "<").replace(/&gt;/g, ">") );
            checkRunnable(source);
            resizeCode(code);
        };

        function slideBody(slide) {
            var html = "";
            slide.contents().not('h3').each(function() {
                    html += $(this).clone().appendTo('<div/>').parent().html();
                });
            return html;
        };

        function showTextSlide(slide) {
            this.$('#content').show();
            this.$('#pre').hide();
            this.$('#code').hide();
            this.$('#titre').removeClass("large");
            this.$('#content').html(slideBody(slide));
            this.$("#run").hide();
        };

        function showTitleSlide(slide) {
            this.$('#content').hide();
            this.$('#pre').hide();
            this.$('#code').hide();
            this.$("#titre").addClass("large");
            this.$("#run").hide();
        };

        function atLastSlide(slideNumber) {
            return slideNumber == slideshow.$("#slides .slide").size();
        };

        function atFirstSlide(slideNumber) {
            return slideNumber == 1;
        };

        function isTitleSlide(slide) {
            return $(slide).html().match(/^\s*<h3>.*<\/h3>\s*$/);
        };

        function isCodeSlide(slide) {
            return getSourceFrom(slide) != "";
        };

        function getSourceFrom(slide) {
          return ($(slide).find("pre").text() || "")
		.replace(/(^|\n) /g, "$1").replace(/ ($|\n)/g, "$1");
        };

        function getLanguageFrom(slide) {
            return ($(slide).find("pre").attr("class") || "js");
        };

        function updateNavButtons(slideNumber) {
            this.$('div.buttons').show();
            if(atLastSlide(slideNumber)) {
                this.$('#prev').show();
                this.$('#next').hide();
            } else if(atFirstSlide(slideNumber)){
                this.$('#prev').hide();
                this.$('#next').show();
            } else {
                this.$('#prev').show();
                this.$('#next').show();
            }
        };

      function get(slideNumber) {
	return this.$("#slides .slide").eq(slideNumber -1);
      };

        function show(slideNumber) {
            clearContent();
            this.position = slideNumber;
            var theSlide = get(slideNumber);
            this.$('#titre').html(theSlide.children("h3").html());
            if(isCodeSlide(theSlide)) {
              showCodeSlide(getSourceFrom(theSlide),getLanguageFrom(theSlide));
            } else if(isTitleSlide(theSlide)){
                showTitleSlide(theSlide);
            }else
                showTextSlide(theSlide);
            updateNavButtons(slideNumber);
            window.location.hash = "#" + slideNumber;
        };

        function runCode(result,source){
            return function() {
                $(result).empty();

                try {
                    with(runcontext) {
                        eval($(source).val());
                    }
                } catch(e){
                    runcontext.error(e.message);
                }
                restoreState();
                return false;
            };
        };

        function run() {
            runCode("#results","#code")();
        };

        function next() {
            this.show(this.position + 1);
        };

        function previous() {
            this.show(this.position - 1);
        };

        function start() {
            clearContent();
            this.$('#titre').text(this.title).addClass("large");
            this.$('#slideshow div.buttons').hide();
        };

        function toc () {
            clearContent();
            var headlines = this.$('#slides div.slide')
                .filter(function() {
                        return isTitleSlide(this);
                    })
                .map(function() {
                        return $(this).find('h3');
                    });
            this.$('#content').show();
            this.$('#slideshow div.buttons').show();
            this.$('#next').show();
            this.$('#prev').hide();
            this.$('#pre').hide();
            this.$('#code').hide();
            this.showTOC(headlines);
            window.location.hash = "#toc";
        };

        function refresh(){
            if(window.location.hash.toString().match(/\#[0-9]+/)) {
                var num = parseInt(location.hash.substr(1));
                this.show(num);
            } else if(window.location.hash == "#toc")
                this.toc();
            else if (window.location.hash == "")
                this.start();
        };

        /*
         * Extension feature of jQuerey allows for incremental
         * extension of an object
         */
        $.extend(slideshow, {
                showTOC: showTOC,
                    show: show,
                    clearContent: clearContent,
                    next: next,
                    previous: previous,
                    run: run,
                    start: start,
                    toc: toc,
		   get: get,
                    refresh: refresh,
                    slideBody: slideBody,
                    isTitleSlide: isTitleSlide,
                   isCodeSlide: isCodeSlide,
		   getSourceFrom: getSourceFrom,
		   getLanguageFrom: getLanguageFrom
                    });

        /*
         * Link events and start show
         */
        $('#next').click(function() {
                slideshow.next();
            });

        $('#prev').click(function() {
                slideshow.previous();
            });

        $('#form').submit(runCode("#results","#code"));

        $('#titre').click(function() {
                slideshow.toc();
            });

        /*
         * following is copied verbatim and untested from
         * J.Resig's style
         */

        $("#pre").dblclick(
	    function(){
		$("#pre").hide();
		$("#code").focus();
	    });

	jQuery("#code").keydown(function(e){
                if ( this.setSelectionRange ) {
                    var start = this.selectionStart, val = this.value;
					if ( e.keyCode == 13 ) {
					    var match = val.substring(0, start).match(/(^|\n)([ \t]*)([^\n]*)$/);
					    if ( match ) {
						var spaces = match[2], length = spaces.length + 1;
						this.value = val.substring(0, start) + "\n" + spaces + val.substr(this.selectionEnd);
						this.setSelectionRange(start + length, start + length);
						this.focus();
						return false;
					    }
					} else if ( e.keyCode == 8 ) {
					    if ( val.substring(start - 2, start) == "  " ) {
						this.value = val.substring(0, start - 2) + val.substr(this.selectionEnd);
						this.setSelectionRange(start - 2, start - 2);
						this.focus();
						return false;
					    }
					} else if ( e.keyCode == 9 ) {
					    this.value = val.substring(0, start) + "  " + val.substr(this.selectionEnd);
					    this.setSelectionRange(start + 2, start + 2);
					    this.focus();
					    return false;
					}
				    }
				});

        slideshow.refresh();

      setInterval(function() {
		   if(window.location.hash.toString().match(/\#[0-9]+/)) {
                     var num = parseInt(location.hash.substr(1));
                     if(num != slideshow.position)
		       slideshow.show(num);
		   }
		 }, 1000);
    });