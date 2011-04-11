/*
 * MMGGraphicTextFrame.j
 * Text frame
 *
 * YuanZhiqian
 * 
 */

@import "MMGGraphic.j"


@implementation MMGTextFrame : MMGGraphic
{
    CPString _name;
    BOOL     _hasGrid;
    CPString _bg;
    CPString _textFont;
    CPString _fontSize;
    CPString _paraIndent;
    CPString _maxLines;
    CPString _hasContinue;
    CPString _ifBreak;
    
    CPString _content;
    CPColor  _textColor;
    CPFont _fontInfo;
}

-(void) test
{
    [self setName: "Frame"];
    [self setHasGrid: YES];
    [self setBg: "0.9,0,0"];
    [self setTextFont: "Times-Roman"];
    [self setFontSize: "32/32"];
    [self setParaIndent: "0"];        
    [self setMaxLines: "6"];
    [self setHasContinue: "none"];
    [self setIfBreak: "true"];
    [self setContent: "Hello #name, you need to pay #price."]
    [self setTextColor: [CPColor blackColor]];
}

- (id)init {
    self = [super init];
    if (self) {
      [self test];
    }
    return self;
}

//getters and setters

- (CPString)name {
    return _name;
}

- (void)setName:(CPString)name {
    _name = name;
}

- (BOOL)hasGrid {
    return _hasGrid;
}

- (void)setHasGrid:(BOOL)hasGrid {
    _hasGrid = hasGrid;
}

- (CPString)bg {
    return _bg;
}

- (void)setBg:(CPString)bg {
    _bg = bg;
}

- (CPString)textFont {
    return _textFont;
}

- (void)setTextFont:(CPString)textFont {
    _textFont = textFont;
}

- (CPString)fontSize {
    return _fontSize;
}

- (void)setFontSize:(CPString)fontSize {
    _fontSize = fontSize;
}

- (CPString)paraIndent {
    return _paraIndent;
}

- (void)setParaIndent:(CPString)paraIndent {
    _paraIndent = paraIndent;
}

- (CPString)maxLines {
    return _maxLines;
}

- (void)setMaxLines:(CPString)maxLines {
    _maxLines = maxLines;
}

- (CPString)hasContinue {
    return _hasContinue;
}

- (void)setHasContinue:(CPString)hasContinue {
    _hasContinue = hasContinue;
}

- (CPString)ifBreak {
    return _ifBreak;
}

- (void)setIfBreak:(CPString)ifBreak {
    _ifBreak = ifBreak;
}

- (CPString)content {
    return _content;
}

- (void)setContent:(CPString)content {
    _content = content;
}

- (CPColor)textColor {
    return _textColor;
}

- (void)setTextColor:(CPColor)textColor {
    _textColor = textColor;
}

//override the method of parent class

- (CGPath)bezierPathForDrawing 
{
	var path = CGPathCreateMutable();
	var rect = [self bounds];

	CGPathAddRect(path, nil, rect);
	CGPathCloseSubpath(path);

    return path;
}


@end
