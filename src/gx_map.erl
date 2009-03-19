-module(gx_map).

-compile(export_all).

%% Map GX to WX types
type(frame) -> wxFrame;
type(dialog) -> wxDialog;
type(window) -> wxWindow;
type(_) -> undefined.

%% NOTE: I am not at all sure about this approach....
%% The basic map function
map(Key, _, [{Key, Value}|_]) -> Value;
map(Key, Default, [{_, _}|T]) -> map(Key, Default, T);
map(_, Default, []) -> Default.

% a first go at getter/setters for properties, rather than "undefined" try returning ancestor...
get(wxStaticLine, Key) -> map(Key, wxControl, [{vertical, {isVertical, 1}}, {default, {getDefaultSize, 0}}]);
get(wxStaticText, Key) -> map(Key, wxControl, [{label, {getLabel, 1}}]);
get(_, _) -> undefined.

set(wxStaticLine, Key) -> map(Key, wxControl, [{destroy, {destroy, 1}}]);
set(wxStaticText, Key) -> map(Key, wxControl, [{label, {setLabel, 2}}, {wrap, {wrap, 2}}]);
set(_, _) -> undefined.


%% ATTRIBUTE COLOR NAMES
%% HTML 4: aqua, black, blue, fuchsia, gray, green, lime, maroon, 
%%         navy, olive, purple, red, silver, teal, white, yellow
% Names (could optimize later)
color(aliceblue) -> color(16#f0f8ff);
color(antiquewhite) -> color(16#faebd7);
color(aqua) -> color(16#00ffff);
color(aquamarine) -> color(16#7fffd4);
color(azure) -> color(16#f0ffff);
color(beige) -> color(16#f5f5dc);
color(bisque) -> color(16#ffe4c4);
color(black) -> color(16#000000);
color(blanchedalmond) -> color(16#ffebcd);
color(blue) -> color(16#0000ff);
color(blueviolet) -> color(16#8a2be2);
color(brown) -> color(16#a52a2a);
color(burlywood) -> color(16#deb887);
color(cadetblue) -> color(16#5f9ea0);
color(chartreuse) -> color(16#7fff00);
color(chocolate) -> color(16#d2691e);
color(coral) -> color(16#ff7f50);
color(cornflowerblue) -> color(16#6495ed);
color(cornsilk) -> color(16#fff8dc);
color(crimson) -> color(16#dc143c);
color(cyan) -> color(16#00ffff);
color(darkblue) -> color(16#00008b);
color(darkcyan) -> color(16#008b8b);
color(darkgoldenrod) -> color(16#b8860b);
color(darkgray) -> color(16#a9a9a9);
color(darkgrey) -> color(16#a9a9a9);
color(darkgreen) -> color(16#006400);
color(darkkhaki) -> color(16#bdb76b);
color(darkmagenta) -> color(16#8b008b);
color(darkolivegreen) -> color(16#556b2f);
color(darkorange) -> color(16#ff8c00);
color(darkorchid) -> color(16#9932cc);
color(darkred) -> color(16#8b0000);
color(darksalmon) -> color(16#e9967a);
color(darkseagreen) -> color(16#8fbc8f);
color(darkslateblue) -> color(16#483d8b);
color(darkslategray) -> color(16#2f4f4f);
color(darkturquoise) -> color(16#00ced1);
color(darkviolet) -> color(16#9400d3);
color(deeppink) -> color(16#ff1493);
color(deepskyblue) -> color(16#00bfff);
color(dimgray) -> color(16#696969);
color(dodgerblue) -> color(16#1e90ff);
color(firebrick) -> color(16#b22222);
color(floralwhite) -> color(16#fffaf0);
color(forestgreen) -> color(16#228b22);
color(fuchsia) -> color(16#ff00ff);
color(gainsboro) -> color(16#dcdcdc);
color(ghostwhite) -> color(16#f8f8ff);
color(gold) -> color(16#ffd700);
color(goldenrod) -> color(16#daa520);
color(gray) -> color(16#808080);
color(grey) -> color(16#808080);
color(green) -> color(16#008000);
color(greenyellow) -> color(16#adff2f);
color(honeydew) -> color(16#f0fff0);
color(hotpink) -> color(16#ff69b4);
color(indianred ) -> color(16#cd5c5c);
color(indigo ) -> color(16#4b0082);
color(ivory) -> color(16#fffff0);
color(khaki) -> color(16#f0e68c);
color(lavender) -> color(16#e6e6fa);
color(lavenderblush) -> color(16#fff0f5);
color(lawngreen) -> color(16#7cfc00);
color(lemonchiffon) -> color(16#fffacd);
color(lightblue) -> color(16#add8e6);
color(lightcoral) -> color(16#f08080);
color(lightcyan) -> color(16#e0ffff);
color(lightgoldenrodyellow) -> color(16#fafad2);
color(lightgray) -> color(16#d3d3d3);
color(lightgrey) -> color(16#d3d3d3);
color(lightgreen) -> color(16#90ee90);
color(lightpink) -> color(16#ffb6c1);
color(lightsalmon) -> color(16#ffa07a);
color(lightseagreen) -> color(16#20b2aa);
color(lightskyblue) -> color(16#87cefa);
color(lightslategray) -> color(16#778899);
color(lightsteelblue) -> color(16#b0c4de);
color(lightyellow) -> color(16#ffffe0);
color(lime) -> color(16#00ff00);
color(limegreen) -> color(16#32cd32);
color(linen) -> color(16#faf0e6);
color(magenta) -> color(16#ff00ff);
color(maroon) -> color(16#800000);
color(mediumaquamarine) -> color(16#66cdaa);
color(mediumblue) -> color(16#0000cd);
color(mediumorchid) -> color(16#ba55d3);
color(mediumpurple) -> color(16#9370d8);
color(mediumseagreen) -> color(16#3cb371);
color(mediumslateblue) -> color(16#7b68ee);
color(mediumspringgreen) -> color(16#00fa9a);
color(mediumturquoise) -> color(16#48d1cc);
color(mediumvioletred) -> color(16#c71585);
color(midnightblue) -> color(16#191970);
color(mintcream) -> color(16#f5fffa);
color(mistyrose) -> color(16#ffe4e1);
color(moccasin) -> color(16#ffe4b5);
color(navajowhite) -> color(16#ffdead);
color(navy) -> color(16#000080);
color(oldlace) -> color(16#fdf5e6);
color(olive) -> color(16#808000);
color(olivedrab) -> color(16#6b8e23);
color(orange) -> color(16#ffa500);
color(orangered) -> color(16#ff4500);
color(orchid) -> color(16#da70d6);
color(palegoldenrod) -> color(16#eee8aa);
color(palegreen) -> color(16#98fb98);
color(paleturquoise) -> color(16#afeeee);
color(palevioletred) -> color(16#d87093);
color(papayawhip) -> color(16#ffefd5);
color(peachpuff) -> color(16#ffdab9);
color(peru) -> color(16#cd853f);
color(pink) -> color(16#ffc0cb);
color(plum) -> color(16#dda0dd);
color(powderblue) -> color(16#b0e0e6);
color(purple) -> color(16#800080);
color(red) -> color(16#ff0000);
color(rosybrown) -> color(16#bc8f8f);
color(royalblue) -> color(16#4169e1);
color(saddlebrown) -> color(16#8b4513);
color(salmon) -> color(16#fa8072);
color(sandybrown) -> color(16#f4a460);
color(seagreen) -> color(16#2e8b57);
color(seashell) -> color(16#fff5ee);
color(sienna) -> color(16#a0522d);
color(silver) -> color(16#c0c0c0);
color(skyblue) -> color(16#87ceeb);
color(slateblue) -> color(16#6a5acd);
color(slategray) -> color(16#708090);
color(slategrey) -> color(16#708090);
color(snow) -> color(16#fffafa);
color(springgreen) -> color(16#00ff7f);
color(steelblue) -> color(16#4682b4);
color(tan) -> color(16#d2b48c);
color(teal) -> color(16#008080);
color(thistle) -> color(16#d8bfd8);
color(tomato) -> color(16#ff6347);
color(turquoise) -> color(16#40e0d0);
color(violet) -> color(16#ee82ee);
color(wheat) -> color(16#f5deb3);
color(white) -> color(16#ffffff);
color(whitesmoke) -> color(16#f5f5f5);
color(yellow) -> color(16#ffff00);
color(yellowgreen) -> color(16#9acd32);
% TODO: could improve filters...
% color/1
color(Value) when is_atom(Value) -> undefined;
color(Value) when is_integer(Value) -> 
	{Value bsr 16, (Value bsr 8) band 16#ff, Value band 16#ff, 255};
color([R, G, B]) -> color(R, G, B);
color([R, G, B, A]) -> {R, G, B, A}.
% color/2
color(Name, Alpha) when is_atom(Name), Alpha >= 0, Alpha < 256 ->
	{R, G, B} = color(Name), 
	{R, G, B, Alpha};
color(_, _) -> undefined.
% color/3
color(R, G, B) when R >= 0, R < 256, G >= 0, G < 256, B >= 0, B < 256 -> 
	{R, G, B, 255};
color(_, _, _) -> undefined.

