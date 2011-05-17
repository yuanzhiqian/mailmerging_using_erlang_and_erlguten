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

	CGPathAddRect(path, nil, rect);
	CGPathCloseSubpath(path);

    return path;
}


@end
