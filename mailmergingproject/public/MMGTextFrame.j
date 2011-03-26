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
    CPColor  _textColor;
    CPFont   _textFont;
    BOOL     _hasGrid;
    CPNumber _maxLines;
    CPString _content;
}

-(void) test
{
    [self setName: "First_Frame"];
    [self setTextColor: [CPColor blackColor]];
    [self setTextFont: [CPFont systemFontOfSize: 12]];
    [self setHasGrid: YES];    
    [self setMaxLines: [CPNumber numberWithInt: 6]];
    [self setContent: ""]
}

- (id)init {
    self = [super init];
    if (self) {
      [self test];
    }
    return self;
}

//getters and setters

- (CPString)content {
    return _content;
}

- (void)setContent:(CPString)content {
    _content = content;
}

- (CPString)name {
    return _name;
}

- (void)setName:(CPString)name {
    _name = name;
}

- (CPColor)textColor {
    return _textColor;
}

- (void)setTextColor:(CPColor)textColor {
    _textColor = textColor;
}

- (CPFont)textFont {
    return _textFont;
}

- (void)setTextFont:(CPFont)textFont {
    _textFont = textFont;
}

- (BOOL)hasGrid {
    return _hasGrid;
}

- (void)setHasGrid:(BOOL)hasGrid {
    _hasGrid = hasGrid;
}

- (CPNumber)maxLines {
    return _maxLines;
}

- (void)setMaxLines:(CPNumber)maxLines {
    _maxLines = maxLines;
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
