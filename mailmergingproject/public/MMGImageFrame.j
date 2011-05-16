/*
 * MMGImageFrame.j
 * Image frame
 *
 * YuanZhiqian
 * 
 */

@import "MMGGraphic.j"


@implementation MMGImageFrame : MMGGraphic
{
    
}

-(void) settings
{
    [self setName: "Frame"];
    [self setClass: "img"];
    [self setHasGrid: YES];
    [self setBg: "default"];
    [self setTextFont: "N/A"];
    [self setFontSize: "N/A"];
    [self setParaIndent: "N/A"];        
    [self setMaxLines: "N/A"];
    [self setHasContinue: "N/A"];
    [self setIfBreak: "N/A"];
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


- (CGPath)bezierPathForDrawing 
{
	var path = CGPathCreateMutable();
	var rect = [self bounds];

	CGPathAddRect(path, nil, rect);
	CGPathCloseSubpath(path);

    return path;
}


@end
