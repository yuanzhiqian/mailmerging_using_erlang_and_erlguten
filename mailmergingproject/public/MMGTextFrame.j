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
    
}

-(void) settings
{
    [self setName: "Frame"];
    [self setClass: "text"];
    [self setHasGrid: YES];
    [self setBg: "default"];
    [self setTextFont: "Times-Roman"];
    [self setFontSize: "14/24"];
    [self setParaIndent: "0"];        
    [self setMaxLines: "2"];
    [self setHasContinue: "none"];
    [self setIfBreak: "true"];
    [self setContent: ""]
    [self setTextColor: [CPColor blackColor]];
}

- (id)init {
    self = [super init];
    if (self) {
      [self settings];
    }
    return self;
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
