/*
===============================================================================
Chili is the jQuery code highlighter plugin
...............................................................................
LICENSE: http://www.opensource.org/licenses/mit-license.php
WEBSITE: http://noteslog.com/chili/

Copyright 2008 / Andrea Ercolino
===============================================================================
*/

{
	  _name: "sh"
	, _case: true
	, _main: {
        	comment  : {
 		  _match: /#.*/
		  , _style: "color: red;"
		}
		, input  : {
         	  _match: /\n(>.*)\b/
		  , _style: "color: orange; font-style: italic; font-weight: bold;"
		}
		  , variable: {
			  _match: /\${\w+}/
			, _style: "color: #4040c2;"
		}

		, keyword: {
		  _match: /\b(?:for|do|done|if|fi|read|set|in|select|case|while|until)\b/
		  , _style: "color: navy; font-weight:bold;"
		}
	}
}
