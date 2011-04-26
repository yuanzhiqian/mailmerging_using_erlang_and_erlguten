/*
 * MMGGraphic.j
 * parent class of text frame and imgaes, modified from tutorial-drawing-part2
 * drawContentsInView function is replaced by another function which can make use of context from layers 
 *
 * YuanZhiqian
 * 
 */

@import <Foundation/CPObject.j>
@import <AppKit/CPView.j>
@import "EditingView.j"

//The tutorial provides a perfect base class of graphic and I simply reuse the code

SKTGraphicNoHandle = 0;

SKTGraphicUpperLeftHandle = 1,
SKTGraphicUpperMiddleHandle = 2,
SKTGraphicUpperRightHandle = 3,
SKTGraphicMiddleLeftHandle = 4,
SKTGraphicMiddleRightHandle = 5,
SKTGraphicLowerLeftHandle = 6,
SKTGraphicLowerMiddleHandle = 7,
SKTGraphicLowerRightHandle = 8;

// The handles that graphics draw on themselves are 6 point by 6 point rectangles.
SKTGraphicHandleWidth = 6.0;
SKTGraphicHandleHalfWidth = 6.0 / 2.0;

var horizontalFlippings;
var verticalFlippings;

@implementation MMGGraphic : CPObject
{
    CPRect _bounds;
    BOOL _isDrawingFill;
    CPColor _fillColor;
    BOOL _isDrawingStroke;
    CPColor _strokeColor;
    float _strokeWidth;

    CPString _name;
    CPString _class;
}


+ (void)initialize
{
    if (self != [MMGGraphic class])
        return;

	horizontalFlippings = new Array();
	horizontalFlippings[SKTGraphicUpperLeftHandle] = SKTGraphicUpperRightHandle;
        horizontalFlippings[SKTGraphicUpperMiddleHandle] = SKTGraphicUpperMiddleHandle;
    	horizontalFlippings[SKTGraphicUpperRightHandle] = SKTGraphicUpperLeftHandle;
    	horizontalFlippings[SKTGraphicMiddleLeftHandle] = SKTGraphicMiddleRightHandle;
    	horizontalFlippings[SKTGraphicMiddleRightHandle] = SKTGraphicMiddleLeftHandle;
    	horizontalFlippings[SKTGraphicLowerLeftHandle] = SKTGraphicLowerRightHandle;
    	horizontalFlippings[SKTGraphicLowerMiddleHandle] = SKTGraphicLowerMiddleHandle;
    	horizontalFlippings[SKTGraphicLowerRightHandle] = SKTGraphicLowerLeftHandle;

	verticalFlippings = new Array();
    	verticalFlippings[SKTGraphicUpperLeftHandle] = SKTGraphicLowerLeftHandle;
    	verticalFlippings[SKTGraphicUpperMiddleHandle] = SKTGraphicLowerMiddleHandle;
    	verticalFlippings[SKTGraphicUpperRightHandle] = SKTGraphicLowerRightHandle;
    	verticalFlippings[SKTGraphicMiddleLeftHandle] = SKTGraphicMiddleLeftHandle;
    	verticalFlippings[SKTGraphicMiddleRightHandle] = SKTGraphicMiddleRightHandle;
    	verticalFlippings[SKTGraphicLowerLeftHandle] = SKTGraphicUpperLeftHandle;
    	verticalFlippings[SKTGraphicLowerMiddleHandle] = SKTGraphicUpperMiddleHandle;
    	verticalFlippings[SKTGraphicLowerRightHandle] = SKTGraphicUpperRightHandle;
}


// An override of the superclass' designated initializer.
- (id)init {
    self = [super init];
    if (self) {
		// Set up decent defaults for a new graphic.
		_bounds = CPRectMakeZero()
		_isDrawingFill = NO;
		_fillColor = [CPColor whiteColor];
		_isDrawingStroke = YES;
		_strokeColor = [CPColor blackColor];
		_strokeWidth = 1.0;
    }
    return self;
}


// Accessors
- (CPString)name {
    return _name;
}

- (void)setName:(CPString)name {
    _name = name;
}

- (CPString)class {
    return _class;
}

- (void)setClass:(CPString)class {
    _class = class;
}

- (CPRect)bounds {
    return _bounds;
}

- (void)setBounds:(CPRect)bounds {
    _bounds = bounds;
}

- (BOOL)isDrawingFill {
    return _isDrawingFill;
}

- (void)setIsDrawingFill:(BOOL)isDrawingFill {
    _isDrawingFill = isDrawingFill;
}

- (CPColor)fillColor {
    return _fillColor;
}

- (void)setFillColor:(CPColor)fillColor {
    _fillColor = fillColor;
}

- (BOOL)isDrawingStroke {
    return _isDrawingStroke;
}

- (BOOL)setIsDrawingStroke:(BOOL)isDrawingStroke {
    _isDrawingStroke = isDrawingStroke;
}

- (CPColor)strokeColor {
    return _strokeColor;
}

- (void)setStrokeColor:(CPColor)strokeColor {
    _strokeColor = strokeColor;
}

- (float)strokeWidth {
    return _strokeWidth;
}

- (float)setStrokeWidth:(float)strokeWidth {
    _strokeWidth = strokeWidth;
}



- (void)setColor:(CPColor)color 
{
    // Can we fill the graphic?
    if ([self canSetDrawingFill]) 
	{
		// Are we filling it? If not, start, using the new color.
		if (![self isDrawingFill]) {
		    [self setIsDrawingFill:YES];
		}
		
		[self setFillColor:color];
    }
}




// Pretty simple.
- (float)xPosition {
    return [self bounds].origin.x;
}

- (float)yPosition {
    return [self bounds].origin.y;
}

- (float)width {
    return [self bounds].size.width;
}

- (float)height {
    return [self bounds].size.height;
}

- (void)setXPosition:(float)xPosition {
    var bounds = [self bounds];
    bounds.origin.x = xPosition;
    [self setBounds:bounds];
}

- (void)setYPosition:(float)yPosition {
    var bounds = [self bounds];
    bounds.origin.y = yPosition;
    [self setBounds:bounds];
}

- (void)setWidth:(float)width {
    var bounds = [self bounds];
    bounds.size.width = width;
    [self setBounds:bounds];
}

- (void)setHeight:(float)height {
    var bounds = [self bounds];
    bounds.size.height = height;
    [self setBounds:bounds];
}




+ (CPRect)boundsOfGraphics:(CPArray)graphics
{
	var index;
	
    // The bounds of an array of graphics is the union of all of their bounds.
    var bounds = CGRectMakeZero();
    var graphicCount = [graphics count];
    if (graphicCount > 0) 
	{
		bounds = [[graphics objectAtIndex:0] bounds];
		for (index = 1; index<graphicCount; index++) 
		{
	    	bounds = CGRectUnion(bounds, [[graphics objectAtIndex:index] bounds]);
		}
    }
    return bounds;
}


+ (CPRect)drawingBoundsOfGraphics:(CPArray)graphics 
{
	var index;
	
    // The drawing bounds of an array of graphics is the union of all of their drawing bounds.
    var drawingBounds = CGRectMakeZero();
    var graphicCount = [graphics count];
    if (graphicCount > 0)
	{
		drawingBounds = [[graphics objectAtIndex:0] drawingBounds];
		for (index = 1; index<graphicCount; index++) 
		{
	    	drawingBounds = CGRectUnion(drawingBounds, [[graphics objectAtIndex:index] drawingBounds]);
		}
    }
    return drawingBounds;
}

+ (void)translateGraphics:(CPArray)graphics byX:(float)deltaX y:(float)deltaY
{
	var index;
	
    // Pretty simple.
    var graphicCount = [graphics count];
    for (index = 0; index<graphicCount; index++) 
	{
		var graphic = [graphics objectAtIndex:index];
		[graphic setBounds:CGRectOffset([graphic bounds], deltaX, deltaY)];
    }
}

- (CPRect)drawingBounds
{
    // Assume that -[SKTGraphic drawContentsInView:] and -[SKTGraphic drawHandlesInView:] will be doing the drawing. 
    // Start with the plain bounds of the graphic, then take drawing of handles at the corners of the bounds into account, then optional stroke drawing.
    var outset = SKTGraphicHandleHalfWidth;
    if ([self isDrawingStroke]) 
	{
		var strokeOutset = [self strokeWidth] / 2.0;
		if (strokeOutset>outset)
		{
		    outset = strokeOutset;
		}
    }
    var inset = 0.0 - outset;
    var drawingBounds = CGRectInset([self bounds], inset, inset);
    
    // -drawHandleInView:atPoint: draws a one-unit drop shadow too.
    drawingBounds.size.width += 1.0;
    drawingBounds.size.height += 1.0;

    return drawingBounds;
}

//replace the drawContentsInView:isBeingCreateOrEdited: function when using layer to display graphics  -- Yuan Zhiqian
- (void)drawContentsInLayer:(CALayer)layer withContext:(CGContext)context isBeingCreatedOrEdited:(BOOL)isBeingCreatedOrEditing 
{
    var path = [self bezierPathForDrawing];
    if (path)
    {
	CGContextBeginPath(context);
	CGContextAddPath(context, path);
	CGContextClosePath(context);
		
	if ([self isDrawingFill]) 
	{
	    CGContextSetFillColor(context, _fillColor);
            CGContextFillPath(context);
	}
		
	if ([self isDrawingStroke]) 
	{
	    CGContextSetStrokeColor(context, _strokeColor);
	    CGContextStrokePath(context);
	}
    }
}

- (CGPath)bezierPathForDrawing 
{    
    // Live to be overriden.
    [CPException raise:CPInternalInconsistencyException reason:@"Neither -drawContentsInView: nor -bezierPathForDrawing has been overridden."];
    return nil;   
}

//replace the drawContentsInView:isBeingCreateOrEdited: function when using layer to display graphics -- YuanZhiqian
- (void)drawHandlesInLayer:(CALayer)layer withContext:(CGContext)context
{
    // Draw handles at the corners and on the sides.
    var bounds = [self bounds];

    [self drawHandleInLayer:layer withContext:context atPoint:CGPointMake(CGRectGetMinX(bounds), CGRectGetMinY(bounds))];
    [self drawHandleInLayer:layer withContext:context atPoint:CGPointMake(CGRectGetMidX(bounds), CGRectGetMinY(bounds))];
    [self drawHandleInLayer:layer withContext:context atPoint:CGPointMake(CGRectGetMaxX(bounds), CGRectGetMinY(bounds))];
    [self drawHandleInLayer:layer withContext:context atPoint:CGPointMake(CGRectGetMinX(bounds), CGRectGetMidY(bounds))];
    [self drawHandleInLayer:layer withContext:context atPoint:CGPointMake(CGRectGetMaxX(bounds), CGRectGetMidY(bounds))];
    [self drawHandleInLayer:layer withContext:context atPoint:CGPointMake(CGRectGetMinX(bounds), CGRectGetMaxY(bounds))];
    [self drawHandleInLayer:layer withContext:context atPoint:CGPointMake(CGRectGetMidX(bounds), CGRectGetMaxY(bounds))];
    [self drawHandleInLayer:layer withContext:context atPoint:CGPointMake(CGRectGetMaxX(bounds), CGRectGetMaxY(bounds))];
}

- (void)drawHandleInLayer:(CALayer)Layer withContext:(CGContext)context atPoint:(CPPoint)point 
{	
    // Figure out a rectangle that's centered on the point but lined up with device pixels.
    var x = point.x - SKTGraphicHandleHalfWidth;
    var y = point.y - SKTGraphicHandleHalfWidth;
    var width = SKTGraphicHandleWidth;
    var height = SKTGraphicHandleWidth;
	var handleBounds = CGRectMake(x, y, width, height);
    
    // Draw the shadow of the handle.
    var handleShadowBounds = CGRectOffset(handleBounds, 1.0, 1.0);
    CGContextSetFillColor(context, [CPColor shadowColor]);
    CGContextFillRect(context, handleShadowBounds);

    // Draw the handle itself.
    CGContextSetFillColor(context, [CPColor darkGrayColor]);
    CGContextFillRect(context, handleBounds);
}

+ (int)creationSizingHandle 
{
    // Return the number of the handle for the lower-right corner. 
    // If the user drags it so that it's no longer in the lower-right, -resizeByMovingHandle:toPoint: will deal with it.
    return SKTGraphicLowerRightHandle;
}


- (BOOL)canSetDrawingFill 
{
    // The default implementation of -drawContentsInView: can draw fills.
    return YES;
}

- (BOOL)canSetDrawingStroke 
{
    // The default implementation of -drawContentsInView: can draw strokes.
    return YES;
}


- (BOOL)canMakeNaturalSize 
{
    // Only return YES if -makeNaturalSize would actually do something.
    var bounds = [self bounds];
    return bounds.size.width != bounds.size.height;
}


- (BOOL)isContentsUnderPoint:(CPPoint)point
{
    // Just check against the graphic's bounds.
    return CGRectContainsPoint([self bounds], point);
}


- (int)handleUnderPoint:(CPPoint)point 
{
    // Check handles at the corners and on the sides.
    var handle = SKTGraphicNoHandle;
    var bounds = [self bounds];

    if ([self isHandleAtPoint:CGPointMake(CGRectGetMinX(bounds), CGRectGetMinY(bounds)) underPoint:point]) {
		handle = SKTGraphicUpperLeftHandle;
    } else if ([self isHandleAtPoint:CGPointMake(CGRectGetMidX(bounds), CGRectGetMinY(bounds)) underPoint:point]) {
		handle = SKTGraphicUpperMiddleHandle;
    } else if ([self isHandleAtPoint:CGPointMake(CGRectGetMaxX(bounds), CGRectGetMinY(bounds)) underPoint:point]) {
		handle = SKTGraphicUpperRightHandle;
    } else if ([self isHandleAtPoint:CGPointMake(CGRectGetMinX(bounds), CGRectGetMidY(bounds)) underPoint:point]) {
		handle = SKTGraphicMiddleLeftHandle;
    } else if ([self isHandleAtPoint:CGPointMake(CGRectGetMaxX(bounds), CGRectGetMidY(bounds)) underPoint:point]) {
		handle = SKTGraphicMiddleRightHandle;
    } else if ([self isHandleAtPoint:CGPointMake(CGRectGetMinX(bounds), CGRectGetMaxY(bounds)) underPoint:point]) {
		handle = SKTGraphicLowerLeftHandle;
    } else if ([self isHandleAtPoint:CGPointMake(CGRectGetMidX(bounds), CGRectGetMaxY(bounds)) underPoint:point]) {
		handle = SKTGraphicLowerMiddleHandle;
    } else if ([self isHandleAtPoint:CGPointMake(CGRectGetMaxX(bounds), CGRectGetMaxY(bounds)) underPoint:point]) {
		handle = SKTGraphicLowerRightHandle;
    }

    return handle;
}

- (BOOL)isHandleAtPoint:(CPPoint)handlePoint underPoint:(NSPoint)point 
{
    // Check a handle-sized rectangle that's centered on the handle point.
    var x = handlePoint.x - SKTGraphicHandleHalfWidth;
    var y = handlePoint.y - SKTGraphicHandleHalfWidth;
    var width = SKTGraphicHandleWidth;
    var height = SKTGraphicHandleWidth;
    var handleBounds = CGRectMake(x, y, width, height);

    return CGRectContainsPoint(handleBounds, point);
}

- (int)resizeByMovingHandle:(int)handle toPoint:(CPPoint)point {

    // Start with the original bounds.
    var bounds = [self bounds];

    // Is the user changing the width of the graphic?
    if (handle==SKTGraphicUpperLeftHandle || handle==SKTGraphicMiddleLeftHandle || handle==SKTGraphicLowerLeftHandle) 
	{
		// Change the left edge of the graphic.
        bounds.size.width = CGRectGetMaxX(bounds) - point.x;
        bounds.origin.x = point.x;
    } 
    else if (handle==SKTGraphicUpperRightHandle || handle==SKTGraphicMiddleRightHandle || handle==SKTGraphicLowerRightHandle) 
	{
		// Change the right edge of the graphic.
        bounds.size.width = point.x - bounds.origin.x;
    }

    // Did the user actually flip the graphic over?
    if (bounds.size.width < 0.0) 
	{
	    // The handle is now playing a different role relative to the graphic.
	    handle = horizontalFlippings[handle];

	    // Make the graphic's width positive again.
	    bounds.size.width = 0.0 - bounds.size.width;
	    bounds.origin.x -= bounds.size.width;

	    // Tell interested subclass code what just happened.
	    [self flipHorizontally];
    }
    
    // Is the user changing the height of the graphic?
    if (handle==SKTGraphicUpperLeftHandle || handle==SKTGraphicUpperMiddleHandle || handle==SKTGraphicUpperRightHandle) 
	{
		// Change the top edge of the graphic.
        bounds.size.height = CGRectGetMaxY(bounds) - point.y;
        bounds.origin.y = point.y;
    } 
	else if (handle==SKTGraphicLowerLeftHandle || handle==SKTGraphicLowerMiddleHandle || handle==SKTGraphicLowerRightHandle) 
	{
		// Change the bottom edge of the graphic.
		bounds.size.height = point.y - bounds.origin.y;
    }

    // Did the user actually flip the graphic upside down?
    if (bounds.size.height < 0.0) 
	{
		// The handle is now playing a different role relative to the graphic.
        handle = verticalFlippings[handle];
	
		// Make the graphic's height positive again.
        bounds.size.height = 0.0 - bounds.size.height;
        bounds.origin.y -= bounds.size.height;

		// Tell interested subclass code what just happened.
        [self flipVertically];
    }

    // Done.
    [self setBounds:bounds];

    return handle;
}


- (void)flipHorizontally 
{
    // Live to be overridden.
}


- (void)flipVertically 
{
    // Live to be overridden.
}

- (void)makeNaturalSize 
{
    // Just make the graphic square.
    var bounds = [self bounds];

    if (bounds.size.width < bounds.size.height) {
        bounds.size.height = bounds.size.width;
        [self setBounds:bounds];
    } 
    else if (bounds.size.width > bounds.size.height) {
        bounds.size.width = bounds.size.height;
        [self setBounds:bounds];
    }
}

- (CPView)newEditingViewWithSuperviewBounds:(CPRect)superviewBounds
{    
    // Live to be overridden.
    var tempView = [[EditingView alloc] initWithFrame:CGRectMake(0,0,200,200)];
    [tempView setNeedsDisplay: YES];
    return nil;
}


- (void)finalizeEditingView:(CPView)editingView
{    
    // Live to be overridden.   
}


@end
