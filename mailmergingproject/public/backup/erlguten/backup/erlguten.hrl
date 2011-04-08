-record(afm2, {baseFont,    % FontName
	       firstChar,   % first char in widths table
	       lastChar,    % last char in Widths table
	       widths,      % the widths table (0=undefined)
	       kernPairs,   % [{{C1,C2},K}]
	       ascender,    % num
	       capHeight,   % num
	       descender,   % num
	       flags,       % num
	       fontBBox,    % {int,int,int,int}
	       italicAngle, % num 
	       stemV,       % num
	       xHeight}).   % num
 
-record(info, {creator,creationDate, producer, author, title, subject,
	       keywords}).

-record(pdfContext, {
	  info,
	  fonts,
	  images=[], 
	  currentpage,
	  pages=[],
	  mediabox
	 }).

-record(paraBox,{x,y,measure,ptSize,leading,nl,fontMap}).

%% x       = X coordinate of start of line in points
%% y       = Y coordinate of start of line in points
%% measure = Width of colum in picas 
%%           (a 5" column = 30 picas)
%% ptSize  = Typface size in points (72 points=1")
%% leading = Distance between two lines in points
%% nl      = Nl = number of lines in the bounding box for the text
%%
%% fontmap = [{Tag,Int,Bool,FontName}]
%%           Tag = code | em | raw => One of the tags that can
%%           occure in the XML (raw is implicit)
%%           Int = a sequence number 1 2 3 
%%           These will turn into F1 F2 F3 in the PDF
%%           FontName = name of font (these are standard names in Acrobat)

-record(box, {continue,  % str()  = name of the continuation frame 
	      free=1,    % int()  = first free line in the box
	      grid,      % bool   = show a grid
	      bg,        % no | {yes,R,G,B} background color
	      pointSize, % int()  = size in points of the main text in
	                 %          the box
              fontSize,  % str() "pt/leading" Added by Yuan, for table and list
	      leading,   % int()  = gap between lines in points
	      maxLines,  % int()  = max lines in the box
	      measure,   % int()  = width of box in picos (1 pico=12 points)
	      name,      % str()  = name of box
	      objs,      % [#obj] = objects
	      x,         % int()  = X coord of top left hand corner of box
	      y,         % int()  = Y coord of top left hand corner of box
              width,     % int()  added by Yuan, to fix the bg problem
              height,    % int()  added by Yuan, to fix the bg problem
              class}).   % str() Added by Yuan, to determine a table or list frame  

-record(obj,{
	  name,          % atom() = tag name
	  paraIndent,    % {int,int} = {first,line,...}
	  tags}).        % [#tag]









