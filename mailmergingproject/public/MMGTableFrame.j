/*
 * MMGTableFrame.j
 * Table frame
 *
 * YuanZhiqian
 * 
 */

@import "MMGGraphic.j"


@implementation MMGTableFrame : MMGGraphic
{
    
}

-(void) settings
{
    [self setName: "shoppingtable"];
    [self setClass: "table"];
    [self setHasGrid: YES];
    [self setBg: "default"];
    [self setTextFont: "Times-Roman"];
    [self setFontSize: "14/24"];
    [self setParaIndent: "0"];        
    [self setMaxLines: "2"];
    [self setHasContinue: "shoppingtable2"];
    [self setIfBreak: "true"];
    [self setContent: "{table columns = {goods, count, price, place}}{tr}{th}#goods{/th}{th}count_col?{/th}{th}$price?{/th}{th}place{/th}{/tr}{/table}"]
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
        var height = CPRectGetHeight(rect);
        var width = CPRectGetWidth(rect);

	CGPathAddRect(path, nil, rect);

        var degree = 4;
        var i = 1;
        var j = 1;
        for(;i<degree;i++)
        {
          CGPathMoveToPoint(path, nil, minx, miny + height*i/degree);
          CGPathAddLineToPoint(path, nil, maxx, miny + height*i/degree);
        }
        for(;j<degree;j++)
        {
          CGPathMoveToPoint(path, nil, minx + width*j/degree, miny);
          CGPathAddLineToPoint(path, nil, minx + width*j/degree, maxy);
        }

	CGPathCloseSubpath(path);

    return path;
}


@end
