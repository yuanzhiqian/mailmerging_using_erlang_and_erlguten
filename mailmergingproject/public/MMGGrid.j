/*
 * MMGGrid.j
 * Adopted from tutorial-drawing-part2
 *
 * YuanZhiqian
 * 
 */

@import <Foundation/CPObject.j>

// The number of seconds that we wait after temporarily showing the grid before we hide it again. This number has never been reviewed by an actual user interface designer, but it seems nice to at least one engineer at Apple. 
MMGGridTemporaryShowingTime = 1.0;

@implementation MMGGrid : CPObject
{
    CPColor _color;
    float _spacing;
    BOOL _isAlwaysShown;
    BOOL _isConstraining;

    // Sometimes we temporarily show the grid to provide feedback for user changes to the grid spacing. When we do that we use a timer to turn it off again.
    CPTimer _hidingTimer;  
}

- (id)init
{    
    self = [super init];
    if (self)
    {
	// Establish reasonable defaults. 9 points is an eighth of an inch, which is a reasonable default.
	_color = [CPColor lightGrayColor];
	_spacing = 9.0;
    }

    return self;
}

- (void)stopShowingGridForTimer:(CPTimer)timer
{    
    // The timer is now invalid and will be releasing itself.
    _hidingTimer = nil;
    
    // Tell observing views to redraw. By the way, it is virtually always a mistake to put willChange/didChange 
	// invocations together with nothing in between. Doing so can result in bugs that are hard to track down. 
	// You should always invoke -willChangeValueForKey:theKey before the result of -valueForKey:theKey would change, 
	// and then invoke -didChangeValueForKey:theKey after the result of -valueForKey:theKey would have changed. 
	// We can get away with this here because there is no value for the "any" key.
}

- (void)setSpacing:(float)spacing
{    
    // Weed out redundant invocations.
    if (spacing != _spacing)
    {
        _spacing = spacing;

	// If the grid is drawable, make sure the user gets visual feedback of the change. 
	// We don't have to do anything special if the grid is being shown right now.  
	// Observers of "any" will get notified of this change because of what we did in +initialize. They're expected to invoke -drawRect:inView:. 
	if (_spacing > 0 && ! _isAlwaysShown)
	{
    	    // Are we already showing the grid temporarily?
	    if (_hidingTimer)
	    {
		// Yes, and now the user's changed the grid spacing again, so put off the hiding of the grid.
		[_hidingTimer setFireDate:[CPDate dateWithTimeIntervalSinceNow:MMGGridTemporaryShowingTime]];
    	    }
	    else
	    {
		// No, so show it the next time -drawRect:inView: is invoked, and then hide it again in one second.
		_hidingTimer = [CPTimer scheduledTimerWithTimeInterval:MMGGridTemporaryShowingTime target:self selector:@selector(stopShowingGridForTimer:) userInfo:nil repeats:NO];
	
		// Don't bother with a separate _showsGridTemporarily instance variable. -drawRect: can just check to see if _hidingTimer is non-nil.
    	    }
	}
    }
}

- (BOOL)canSetColor
{    
    // Don't let the user change the color of the grid when that would be useless.
    return _isAlwaysShown && [self isUsable];
}

- (BOOL)canSetSpacing
{
    // Don't let the user change the spacing of the grid when that would be useless.
    return _isAlwaysShown || _isConstraining;
}

- (BOOL)isAlwaysShown
{
    return _isAlwaysShown;
}

- (BOOL)isConstraining 
{
    return _isConstraining;
}

- (void)setConstraining:(BOOL)isConstraining
{
    _isConstraining = isConstraining;
}

- (BOOL)isUsable
{
    // The grid isn't usable if the spacing is set to zero. The header comments explain why we don't validate away zero spacing.
    return _spacing > 0;
}

- (void)setAlwaysShown:(BOOL)isAlwaysShown
{
    // Weed out redundant invocations.
    if (isAlwaysShown != _isAlwaysShown)
    {
	_isAlwaysShown = isAlwaysShown;

	// If we're temporarily showing the grid then there's a timer that's going to hide it. If we're supposed to show the grid right now then we don't want the timer to undo that. If we're supposed to hide the grid right now then the hiding that the timer would do is redundant.
	if (_hidingTimer) 
	{
    	    [_hidingTimer invalidate];
	    _hidingTimer = nil;
	}
    }
}

- (CPPoint)constrainedPoint:(CPPoint)point
{
    // The grid might not be usable right now, or constraining might be turned off.
    if ([self isUsable] && _isConstraining)
    {
	var x = Math.floor((point.x / _spacing) + 0.5) * _spacing;
	var y = Math.floor((point.y / _spacing) + 0.5) * _spacing;
		
	point = CPMakePoint(x, y);
    }

    return point;
}

- (BOOL)canAlign
{
    // You can invoke alignedRect: any time the spacing is valid.
    return [self isUsable];
}

- (CPRect)alignedRect:(CPRect)rect
{
    // Aligning is done even when constraining is not.
    var upperRight = CPMakePoint(CGRectGetMaxX(rect), CGRectGetMaxY(rect));
    rect.origin.x = Math.floor((rect.origin.x / _spacing) + 0.5) * _spacing;
    rect.origin.y = Math.floor((rect.origin.y / _spacing) + 0.5) * _spacing;
    upperRight.x = Math.floor((upperRight.x / _spacing) + 0.5) * _spacing;
    upperRight.y = Math.floor((upperRight.y / _spacing) + 0.5) * _spacing;
    rect.size.width = upperRight.x - rect.origin.x;
    rect.size.height = upperRight.y - rect.origin.y;

    return rect;
}

-(void)drawRect:(CPRect)rect inLayer:(CALayer)layer withContext:(CGContext)context
{
    // The grid might not be usable right now. It might be shown, but only temporarily.
    if ([self isUsable] && (_isAlwaysShown || _hidingTimer))
    {	
	// Figure out a big bezier path that corresponds to the entire grid. It will consist of the vertical lines and then the horizontal lines.
	CGContextBeginPath(context);

	var lastVerticalLineNumber = Math.floor(CGRectGetMaxX(rect) / _spacing);
	for (var lineNumber = Math.ceil(CGRectGetMinX(rect) / _spacing); lineNumber<=lastVerticalLineNumber; lineNumber++)
	{
	    CGContextMoveToPoint(context, (lineNumber * _spacing), CGRectGetMinY(rect));
	    CGContextAddLineToPoint(context, (lineNumber * _spacing), CGRectGetMaxY(rect));
	}
	var lastHorizontalLineNumber = Math.floor(CGRectGetMaxY(rect) / _spacing);
	for (var lineNumber = Math.ceil(CGRectGetMinY(rect) / _spacing); lineNumber<=lastHorizontalLineNumber; lineNumber++)
	{
	    CGContextMoveToPoint(context, CGRectGetMinX(rect), (lineNumber * _spacing));
	    CGContextAddLineToPoint(context, CGRectGetMaxX(rect), (lineNumber * _spacing));
	}
	
	CGContextClosePath(context);
	
	// Draw the grid as one-pixel-wide lines of a specific color.
	CGContextSetLineWidth(context, 0.0);
	CGContextSetStrokeColor(context, _color);
        CGContextStrokePath(context);
    }	
}

@end
