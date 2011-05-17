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

        var minx = CPRectGetMinX(rect);
        var miny = CPRectGetMinY(rect);
        var maxx = CPRectGetMaxX(rect);
        var maxy = CPRectGetMaxY(rect);

	CGPathAddRect(path, nil, rect);
        CGPathMoveToPoint(path, nil, minx, miny);
        CGPathAddLineToPoint(path, nil, maxx, maxy);
        CGPathMoveToPoint(path, nil, maxx, miny);
        CGPathAddLineToPoint(path, nil, minx, maxy);
	CGPathCloseSubpath(path);

    return path;
}


@end
