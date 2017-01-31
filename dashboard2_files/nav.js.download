$(document).ready(function(){
	//var $x=($(this).find("h2").html());
	$("#expandme").click(function() {
		   $('#expand_section').toggle();
	   });
	
	if (navigator.appVersion.indexOf("MSIE 7.") != -1)
    $('body').removeClass('responsive');

	var $classID='';

	$('#left nav ul>li:has(ul)').click(function(event){
		//alert("onclick="+$(this).attr('class'));
		$newul=$(this).find("ul:first");
		if($newul.css("display")=="none"){
			$newul=$(this).find("ul:first");
			if(($(this).find("h2").html()) || ($(this).hasClass("drop1"))) { $classID = '1'; } else { $classID = ''; }
			$newul.parent().removeClass("drop"+$classID);
			$newul.parent().addClass("opendrop"+$classID);
			$newul.show("fast");
			$newul.parent().removeClass("togopen"+$classID);
		} else {
			$newul=$(this).find("ul:first");
			//alert("ul:first = "+$newul.attr('id'));
			if(($(this).find("h2").html()) || ($(this).hasClass("opendrop1")))  { $classID = '1'; } else { $classID = ''; }
			$newul.parent().removeClass("opendrop"+$classID); 
			$newul.parent().addClass("drop"+$classID);
			$newul.hide("fast");
			$newul.parent().removeClass("togopen"+$classID);
		}
		event.stopPropagation();
	});
	($('li').has('ul')).each(function() {
		if(($(this).find("h2").html()) || ($(this).find(".linksheading").html())) { $classID = '1'; } else { $classID = ''; }
			$(this).addClass("drop"+$classID);
			$(".exnav>li:has(ul)>a").css("display","block");
			$(this).show("fast");
	});
});

/*
	Breakpoints.js
	version 1.0
	
	Creates handy events for your responsive design breakpoints
	
	Copyright 2011 XOXCO, Inc
	http://xoxco.com/

	Documentation for this plugin lives here:
	http://xoxco.com/projects/code/breakpoints
	
	Licensed under the MIT license:
	http://www.opensource.org/licenses/mit-license.php

*/
(function($) {

	var lastSize = 0;
	var interval = null;

	$.fn.resetBreakpoints = function() {
		$(window).unbind('resize');
		if (interval) {
			clearInterval(interval);
		}
		lastSize = 0;
	};

	$.fn.setBreakpoints = function(settings) {
		var options = jQuery.extend({
							distinct: true,
							breakpoints: new Array(0,320,480,584,768,1024)
				    	},settings);


		interval = setInterval(function() {

			var w = $(window).width();
			var done = false;

			for (var bp in options.breakpoints.sort(function(a,b) { return (b-a) })) {

				// fire onEnter when a browser expands into a new breakpoint
				// if in distinct mode, remove all other breakpoints first.
				if (!done && w >= options.breakpoints[bp] && lastSize < options.breakpoints[bp]) {
					if (options.distinct) {
						for (var x in options.breakpoints.sort(function(a,b) { return (b-a) })) {
							if ($('body').hasClass('breakpoint-' + options.breakpoints[x])) {
								$('body').removeClass('breakpoint-' + options.breakpoints[x]);
								$(window).trigger('exitBreakpoint' + options.breakpoints[x]);
							}
						}
						done = true;
					}
					$('body').addClass('breakpoint-' + options.breakpoints[bp]);
					$(window).trigger('enterBreakpoint' + options.breakpoints[bp]);

				}				

				// fire onExit when browser contracts out of a larger breakpoint
				if (w < options.breakpoints[bp] && lastSize >= options.breakpoints[bp]) {
					$('body').removeClass('breakpoint-' + options.breakpoints[bp]);
					$(window).trigger('exitBreakpoint' + options.breakpoints[bp]);

				}

				// if in distinct mode, fire onEnter when browser contracts into a smaller breakpoint
				if (
					options.distinct && // only one breakpoint at a time
					w >= options.breakpoints[bp] && // and we are in this one
					w < options.breakpoints[bp-1] && // and smaller than the bigger one
					lastSize > w && // and we contracted
					lastSize >0 &&  // and this is not the first time
					!$('body').hasClass('breakpoint-' + options.breakpoints[bp]) // and we aren't already in this breakpoint
					) {					
					$('body').addClass('breakpoint-' + options.breakpoints[bp]);
					$(window).trigger('enterBreakpoint' + options.breakpoints[bp]);
				}						
			}

			// set up for next call
			if (lastSize != w) {
				lastSize = w;
			}
		},250);
	};
})(jQuery);


/* NAV BAR */
$().ready(function(e) {
  
	if (($( window ).width()) < 584) {
		$('.responsive #sidebar').insertAfter('#maincontent');	
	}
	$('#sidebar').show();
	
	$(window).bind('exitBreakpoint320',function() { 
		$('.responsive .mainnav').prependTo('#left');		 
	});
	$(window).bind('exitBreakpoint480',function() { 
		$('.responsive .mainnav').prependTo('#left');	
	});
	$(window).bind('exitBreakpoint584',function() { //36.5em
		$('.responsive #sidebar').insertBefore('#maincontent');			
		$('.responsive .mainnav').prependTo('#left');	
	});		
	$(window).bind('enterBreakpoint480',function() {
		$('.responsive #sidebar').insertAfter('#maincontent');
		$('.responsive .mainnav').prependTo('#full');		 		 
	});
	$(window).bind('enterBreakpoint584',function() {
		$('.responsive #sidebar').insertBefore('#maincontent');	
		$('.responsive .mainnav').prependTo('#full');		 		 
	});
	$(window).bind('enterBreakpoint320',function() {
		$('.responsive #sidebar').insertAfter('#maincontent');	
		$('.responsive .mainnav').prependTo('#full');		 
	});
	$(window).bind('enterBreakpoint0',function() {
		$('.responsive #sidebar').insertAfter('#maincontent');	
		$('.responsive .mainnav').prependTo('#full');		 
	});
	$(window).setBreakpoints();
}); 
