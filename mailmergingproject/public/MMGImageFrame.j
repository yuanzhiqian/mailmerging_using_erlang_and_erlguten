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

- (id)init {
    self = [super init];
    if (self) {
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
