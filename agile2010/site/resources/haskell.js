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
	  _name: "haskell"
	, _case: true
	, _main: {
        	mlcom  : {
		          _match: /{-[^-]+(?:[^{][^-]*-+)?-}/
			, _style: "color: red;"
		}
		, com  : {
         		  _match: /--.*/
			, _style: "color: red;"
		}
		, operator    : {
			  _match: /[!#$%&*+./<=>?@\\^|-]+/
			, _style: "color: purple;"
		}
		, ctor    : {
    		  _match: /\b(?:[A-Z]\w*)\b|\[\]|:[!#$%&*+./<=>?@\\^|-]+/
			, _style: "color: green;"
		}
		, stringOrChar : {
			  _match: /(?:\'[^\'\\\n](?:\\.[^\'\\\n]*)\')|(?:\"[^\"\\\n]*(?:\\.[^\"\\\n]*)*\")/
			, _style: "color: teal;"
		}
		, numberOrBool : {
			  _match: /(?:\b[+-]?(?:\d*\.?\d+|\d+\.?\d*)(?:[eE][+-]?\d+)?\b)|(?:0x[a-f0-9]+)\b|[^"]True[^"]|[^"]False[^"]/
			, _style: "color: teal;"
		}
		, keyword: {
			  _match: /\b(?:module|where|type|data|newtype|class|instance|default|deriving|as|import|let|in|if|then|else|case|of|do|infixl|infixr|infix)\b/
			, _style: "color: navy;"
		}
	}
}
