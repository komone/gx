%% Copyright 2010-2014 Steve Davis <steve@simulacity.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
% http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(gx_color).

-include("gx.hrl").

-export([encode/1, decode/1]).

encode(Bin) when byte_size(Bin) =:= 16 ->
	Bin;
encode({R, G, B, A}) ->
	<<R:32/unsigned-native, G:32/unsigned-native, B:32/unsigned-native, A:32/unsigned-native>>;
encode(Name) when is_atom(Name) ->
	encode(decode(Name)).
	
% color/1
decode(Value) when is_integer(Value) -> 
	{Value bsr 16, (Value bsr 8) band 16#ff, Value band 16#ff, 255};
decode([$#, R, G, B]) -> 
	decode(erlang:list_to_integer([R, R, G, G, B, B], 16));
decode([$#, R, R1, G, G1, B, B1]) ->
	decode(erlang:list_to_integer([R, R1, G, G1, B, B1], 16));
decode([R, G, B]) -> 
	{R, G, B, 255};
decode([R, G, B, A]) -> 
	{R, G, B, A};
%% ATTRIBUTE COLOR NAMES
%% HTML 4: aqua, black, blue, fuchsia, gray, green, lime, maroon, 
%%         navy, olive, purple, red, silver, teal, white, yellow
decode(aliceblue) -> decode(16#f0f8ff);
decode(antiquewhite) -> decode(16#faebd7);
decode(aqua) -> decode(16#00ffff);
decode(aquamarine) -> decode(16#7fffd4);
decode(azure) -> decode(16#f0ffff);
decode(beige) -> decode(16#f5f5dc);
decode(bisque) -> decode(16#ffe4c4);
decode(black) -> decode(16#000000);
decode(blanchedalmond) -> decode(16#ffebcd);
decode(blue) -> decode(16#0000ff);
decode(blueviolet) -> decode(16#8a2be2);
decode(brown) -> decode(16#a52a2a);
decode(burlywood) -> decode(16#deb887);
decode(cadetblue) -> decode(16#5f9ea0);
decode(chartreuse) -> decode(16#7fff00);
decode(chocolate) -> decode(16#d2691e);
decode(coral) -> decode(16#ff7f50);
decode(cornflowerblue) -> decode(16#6495ed);
decode(cornsilk) -> decode(16#fff8dc);
decode(crimson) -> decode(16#dc143c);
decode(cyan) -> decode(16#00ffff);
decode(darkblue) -> decode(16#00008b);
decode(darkcyan) -> decode(16#008b8b);
decode(darkgoldenrod) -> decode(16#b8860b);
decode(darkgray) -> decode(16#a9a9a9);
decode(darkgrey) -> decode(16#a9a9a9);
decode(darkgreen) -> decode(16#006400);
decode(darkkhaki) -> decode(16#bdb76b);
decode(darkmagenta) -> decode(16#8b008b);
decode(darkolivegreen) -> decode(16#556b2f);
decode(darkorange) -> decode(16#ff8c00);
decode(darkorchid) -> decode(16#9932cc);
decode(darkred) -> decode(16#8b0000);
decode(darksalmon) -> decode(16#e9967a);
decode(darkseagreen) -> decode(16#8fbc8f);
decode(darkslateblue) -> decode(16#483d8b);
decode(darkslategray) -> decode(16#2f4f4f);
decode(darkturquoise) -> decode(16#00ced1);
decode(darkviolet) -> decode(16#9400d3);
decode(deeppink) -> decode(16#ff1493);
decode(deepskyblue) -> decode(16#00bfff);
decode(dimgray) -> decode(16#696969);
decode(dodgerblue) -> decode(16#1e90ff);
decode(firebrick) -> decode(16#b22222);
decode(floralwhite) -> decode(16#fffaf0);
decode(forestgreen) -> decode(16#228b22);
decode(fuchsia) -> decode(16#ff00ff);
decode(gainsboro) -> decode(16#dcdcdc);
decode(ghostwhite) -> decode(16#f8f8ff);
decode(gold) -> decode(16#ffd700);
decode(goldenrod) -> decode(16#daa520);
decode(gray) -> decode(16#808080);
decode(grey) -> decode(16#808080);
decode(green) -> decode(16#008000);
decode(greenyellow) -> decode(16#adff2f);
decode(honeydew) -> decode(16#f0fff0);
decode(hotpink) -> decode(16#ff69b4);
decode(indianred ) -> decode(16#cd5c5c);
decode(indigo ) -> decode(16#4b0082);
decode(ivory) -> decode(16#fffff0);
decode(khaki) -> decode(16#f0e68c);
decode(lavender) -> decode(16#e6e6fa);
decode(lavenderblush) -> decode(16#fff0f5);
decode(lawngreen) -> decode(16#7cfc00);
decode(lemonchiffon) -> decode(16#fffacd);
decode(lightblue) -> decode(16#add8e6);
decode(lightcoral) -> decode(16#f08080);
decode(lightcyan) -> decode(16#e0ffff);
decode(lightgoldenrodyellow) -> decode(16#fafad2);
decode(lightgray) -> decode(16#d3d3d3);
decode(lightgrey) -> decode(16#d3d3d3);
decode(lightgreen) -> decode(16#90ee90);
decode(lightpink) -> decode(16#ffb6c1);
decode(lightsalmon) -> decode(16#ffa07a);
decode(lightseagreen) -> decode(16#20b2aa);
decode(lightskyblue) -> decode(16#87cefa);
decode(lightslategray) -> decode(16#778899);
decode(lightsteelblue) -> decode(16#b0c4de);
decode(lightyellow) -> decode(16#ffffe0);
decode(lime) -> decode(16#00ff00);
decode(limegreen) -> decode(16#32cd32);
decode(linen) -> decode(16#faf0e6);
decode(magenta) -> decode(16#ff00ff);
decode(maroon) -> decode(16#800000);
decode(mediumaquamarine) -> decode(16#66cdaa);
decode(mediumblue) -> decode(16#0000cd);
decode(mediumorchid) -> decode(16#ba55d3);
decode(mediumpurple) -> decode(16#9370d8);
decode(mediumseagreen) -> decode(16#3cb371);
decode(mediumslateblue) -> decode(16#7b68ee);
decode(mediumspringgreen) -> decode(16#00fa9a);
decode(mediumturquoise) -> decode(16#48d1cc);
decode(mediumvioletred) -> decode(16#c71585);
decode(midnightblue) -> decode(16#191970);
decode(mintcream) -> decode(16#f5fffa);
decode(mistyrose) -> decode(16#ffe4e1);
decode(moccasin) -> decode(16#ffe4b5);
decode(navajowhite) -> decode(16#ffdead);
decode(navy) -> decode(16#000080);
decode(oldlace) -> decode(16#fdf5e6);
decode(olive) -> decode(16#808000);
decode(olivedrab) -> decode(16#6b8e23);
decode(orange) -> decode(16#ffa500);
decode(orangered) -> decode(16#ff4500);
decode(orchid) -> decode(16#da70d6);
decode(palegoldenrod) -> decode(16#eee8aa);
decode(palegreen) -> decode(16#98fb98);
decode(paleturquoise) -> decode(16#afeeee);
decode(palevioletred) -> decode(16#d87093);
decode(papayawhip) -> decode(16#ffefd5);
decode(peachpuff) -> decode(16#ffdab9);
decode(peru) -> decode(16#cd853f);
decode(pink) -> decode(16#ffc0cb);
decode(plum) -> decode(16#dda0dd);
decode(powderblue) -> decode(16#b0e0e6);
decode(purple) -> decode(16#800080);
decode(red) -> decode(16#ff0000);
decode(rosybrown) -> decode(16#bc8f8f);
decode(royalblue) -> decode(16#4169e1);
decode(saddlebrown) -> decode(16#8b4513);
decode(salmon) -> decode(16#fa8072);
decode(sandybrown) -> decode(16#f4a460);
decode(seagreen) -> decode(16#2e8b57);
decode(seashell) -> decode(16#fff5ee);
decode(sienna) -> decode(16#a0522d);
decode(silver) -> decode(16#c0c0c0);
decode(skyblue) -> decode(16#87ceeb);
decode(slateblue) -> decode(16#6a5acd);
decode(slategray) -> decode(16#708090);
decode(slategrey) -> decode(16#708090);
decode(snow) -> decode(16#fffafa);
decode(springgreen) -> decode(16#00ff7f);
decode(steelblue) -> decode(16#4682b4);
decode(tan) -> decode(16#d2b48c);
decode(teal) -> decode(16#008080);
decode(thistle) -> decode(16#d8bfd8);
decode(tomato) -> decode(16#ff6347);
decode(turquoise) -> decode(16#40e0d0);
decode(violet) -> decode(16#ee82ee);
decode(wheat) -> decode(16#f5deb3);
decode(white) -> decode(16#ffffff);
decode(whitesmoke) -> decode(16#f5f5f5);
decode(yellow) -> decode(16#ffff00);
decode(yellowgreen) -> decode(16#9acd32).

