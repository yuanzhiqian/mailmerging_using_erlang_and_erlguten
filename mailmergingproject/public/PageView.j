/*
 * PageView.j
 * Contain PaintLayer -- not anymore
 * I reused some codes to implement the graphic effects
 * 
 * YuanZhiqian
 * 
 */

@import <AppKit/CPView.j>
@import <AppKit/CPColor.j>
@import <AppKit/CPGraphicsContext.j>
@import "MMGGraphic.j"
@import "MMGToolPaletteController.j"
@import "MMGGrid.j"

SKTGraphicViewDefaultPasteCascadeDelta = 10.0;

@implementation PageView:CPView
{    
    MMGGrid _grid;

    MMGGraphic _creatingGraphic;
    MMGraphic  _editingGraphic;

    // The bounds of the marquee selection, if marquee selection is being done right now, NSZeroRect otherwise.
    CPRect _marqueeSelectionBounds;

    // Whether or not selection handles are being hidden while the user moves graphics.
    BOOL _isHidingHandles;

    // Sometimes we temporarily hide the selection handles when the user moves graphics using the keyboard. When we do that this is the timer to start showing them again.
    CPTimer _handleShowingTimer;

    CPView _editingView;
    CPRect _editingViewFrame;

    // The bounds of the marquee selection, if marquee selection is being done right now, NSZeroRect otherwise.
    CPRect _marqueeSelectionBounds;

    // For mouse tracking
    MMGGraphic _resizedGraphic;
    int _resizedHandle;
    BOOL _didMove;
    BOOL _isMoving;
    CPPoint _lastPoint;
    CPPoint _selOriginOffset;
    CPArray _selGraphics;
	
    // Marquee tracking
    CPPoint _originalMouseLocation;
    CPIndexSet _oldSelectionIndexes;

    // The state of the cascading of graphics that we do during repeated pastes.
    int _pasteboardChangeCount;
    int _pasteCascadeNumber;
    CPPoint _pasteCascadeDelta;
}

- (id)initWithFrame:(CGRect)aFrame
{
    self = [super initWithFrame:aFrame];
    
    if (self)
    {
       _marqueeSelectionBounds = CPRectMakeZero();
       _pasteboardChangeCount = -1;
       _pasteCascadeNumber = 0;
       _pasteCascadeDelta = CPMakePoint(SKTGraphicViewDefaultPasteCascadeDelta, SKTGraphicViewDefaultPasteCascadeDelta);
    }
    
    return self;

}

- (CPArray)graphics 
{    
    var graphics = [[[self window] windowController] graphics]; 
    if (!graphics) {
	graphics = [CPArray array];
    }
    return graphics;
}

- (CPArray)mutableGraphics 
{    
    return [self graphics];   
}

- (CPIndexSet)selectionIndexes 
{
    var selectionIndexes = [[[self window] windowController] selectionIndexes];
    if (!selectionIndexes) {
	selectionIndexes = [CPIndexSet indexSet];
    }
    return selectionIndexes;
}

- (void)changeSelectionIndexes:(CPIndexSet)indexes 
{
    [[[self window] windowController] setSelectionIndexes:indexes];
}

//magic, determining graphics being selected
- (CPArray)selectedGraphics
{
    // Simple, because we made sure -graphics and -selectionIndexes never return nil.
    return [[self graphics] objectsAtIndexes:[self selectionIndexes]];
}

// An override of the NSView method.
- (void)drawRect:(CPRect)rect 
{
    var context = [[CPGraphicsContext currentContext] graphicsPort];

    // Draw the background background.
    CGContextSetFillColor(context, [CPColor whiteColor]);
    CGContextFillRect(context, rect);

    // Draw the grid.
    [_grid drawRect:rect inView:self];

    // Draw every graphic that intersects the rectangle to be drawn. The frontmost graphics have the lowest indexes.	
    var graphics = [self graphics];
    var selectionIndexes = [self selectionIndexes];

    var graphicCount = [graphics count];
    for (var index = graphicCount - 1; index>=0; index--) 
	{
        var graphic = [graphics objectAtIndex:index];
        var graphicDrawingBounds = [graphic drawingBounds];
        if (CPRectIntersectsRect(rect, graphicDrawingBounds)) 
		{
	    	// Figure out whether or not to draw selection handles on the graphic. Selection handles are drawn for all selected objects except:
		    // - While the selected objects are being moved.
		    // - For the object actually being created or edited, if there is one.
		    var drawSelectionHandles = NO;
	        if (!_isHidingHandles && graphic!=_creatingGraphic && graphic!=_editingGraphic) {
				drawSelectionHandles = [selectionIndexes containsIndex:index];
	        }

		    // Draw the graphic, possibly with selection handles.
			CGContextSaveGState(context);
	
		    // [NSBezierPath clipRect:graphicDrawingBounds];
		    [graphic drawContentsInView:self isBeingCreateOrEdited:(graphic==_creatingGraphic || graphic==_editingGraphic)];
		    if (drawSelectionHandles) {
				[graphic drawHandlesInView:self];
		    }
	    
			CGContextRestoreGState(context);
        }
    }

    // If the user is in the middle of selecting draw the selection rectangle.
    if (!CPRectIsEmpty(_marqueeSelectionBounds)) 
	{
		CGContextSetStrokeColor(context, [CPColor lightGrayColor]);
		CGContextStrokeRect(context, _marqueeSelectionBounds);
		//CGContextStrokeRectWithWidth(context, _marqueeSelectionBounds, 1.0);
    }
}

- (CPDictionary)graphicUnderPoint:(CPPoint)point
{
    var graphicToReturn = nil;
    var outIndex = 0;
    var outIsSelected = NO;
    var outHandle = SKTGraphicNoHandle;

    // Search through all of the graphics, front to back, looking for one that claims that the point is on a selection handle 
	// (if it's selected) or in the contents of the graphic itself.
    var graphics = [self graphics];
    var selectionIndexes = [self selectionIndexes];
    var graphicCount = [graphics count];
    for (var index = 0; index<graphicCount; index++) 
    {
	var graphic = [graphics objectAtIndex:index];

	// Do a quick check to weed out graphics that aren't even in the neighborhood.
	if (CPRectContainsPoint([graphic drawingBounds], point)) 
	{
	    // Check the graphic's selection handles first, because they take precedence when they overlap the graphic's contents.
	    var graphicIsSelected = [selectionIndexes containsIndex:index];
	    if (graphicIsSelected)
	    {
		// If the graphic is selected, we can see it's handles and try to select one with the mouse
		var handle = [graphic handleUnderPoint:point];
		if (handle != SKTGraphicNoHandle) 
		{
		    // The user clicked on a handle of a selected graphic.
		    graphicToReturn = graphic;
		    outHandle = handle;
		}
	    }
		
	    if (! graphicToReturn) 
	    {
		var clickedOnGraphicContents = [graphic isContentsUnderPoint:point];
		if (clickedOnGraphicContents) 
		{
		    // The user clicked on the contents of a graphic.
		    graphicToReturn = graphic;
		    outHandle = SKTGraphicNoHandle;
		}
	    }
		
	    if (graphicToReturn)
	    {
		// Return values and stop looking.
		outIndex = index;
		outIsSelected = graphicIsSelected;
		break;
	    }
	}
    }

    // PLL : Be aware, we can not use nil for graphicToReturn but [CPNull null]
    if (! graphicToReturn)
    {
	graphicToReturn = [CPNull null]
    }
	
    var objects = [CPArray arrayWithObjects:graphicToReturn, [CPNumber numberWithInt:outIndex], 
					[CPNumber numberWithBool:outIsSelected], [CPNumber numberWithInt:outHandle], nil];
    var keys = [CPArray arrayWithObjects:@"graphic", @"index", @"isSelected", @"handle", nil];	
    return [CPDictionary dictionaryWithObjects:objects forKeys:keys];
}

//adopted from the tutorial, however its funcionality is not fully implemented, it is just a bland left for other people to fill according to their needs -- Yuan Zhiqian

- (void)setNeedsDisplayForEditingViewFrameChangeNotification:(CPNotification)viewFrameDidChangeNotification 
{
    // If the editing view got smaller we have to redraw where it was or cruft will be left on the screen. 
	// If the editing view got larger we might be doing some redundant invalidation (not a big deal), 
	// but we're not doing any redundant drawing (which might be a big deal). 
	// If the editing view actually moved then we might be doing substantial redundant drawing, but so far that wouldn't happen in Sketch.
    // In Sketch this prevents cruft being left on the screen when the user 
	// 1) creates a great big text area and fills it up with text, 
	// 2) sizes the text area so not all of the text fits, 
	// 3) starts editing the text area but doesn't actually change it, so the text area hasn't been automatically 
	// resized and the text editing view is actually bigger than the text area, 
	// and 4) deletes so much text in one motion (Select All, then Cut) that the text editing view suddenly becomes smaller than the text area. 
	// In every other text editing situation the text editing view's invalidation or the fact that the SKTText's "drawingBounds" 
	// changes is enough to cause the proper redrawing.
    var newEditingViewFrame = [[viewFrameDidChangeNotification object] frame];
    [self setNeedsDisplayInRect:CPRectUnion(_editingViewFrame, newEditingViewFrame)];
    _editingViewFrame = newEditingViewFrame;
}


- (void)startEditingGraphic:(PLGraphic)graphic
{
    // It's the responsibility of invokers to not invoke this method when editing has already been started.
    // CPAssert((!_editingGraphic && !_editingView), @"-[SKTGraphicView startEditingGraphic:] is being mis-invoked.");

    // Can the graphic even provide an editing view?
    _editingView = [graphic newEditingViewWithSuperviewBounds:[graphic bounds]];
    if (_editingView) 
    {
	// Keep a pointer to the graphic around so we can ask it to draw its "being edited" look, and eventually send it a -finalizeEditingView: message.
	_editingGraphic = graphic;

	// If the editing view adds a ruler accessory view we're going to remove it when editing is done, so we have to remember the old reserved accessory view thickness so we can restore it. Otherwise there will be a big blank space in the ruler.
	// _oldReservedThicknessForRulerAccessoryView = [[[self enclosingScrollView] horizontalRulerView] reservedThicknessForAccessoryView];

	// Make the editing view a subview of this one. It was the graphic's job to make sure that it was created with the right frame and bounds.
	[self addSubview:_editingView];

	// Make the editing view the first responder so it takes key events and relevant menu item commands.
	[[self window] makeFirstResponder:_editingView];

	// Get notified if the editing view's frame gets smaller, because we may have to force redrawing when that happens. Record the view's frame because it won't be available when we get the notification.
	[[CPNotificationCenter defaultCenter] addObserver:self selector:@selector(setNeedsDisplayForEditingViewFrameChangeNotification:) name:CPViewFrameDidChangeNotification object:_editingView];
	_editingViewFrame = [_editingView frame];

	// Give the graphic being edited a chance to draw one more time. In Sketch, SKTText draws a focus ring.
	[self setNeedsDisplayInRect:[_editingGraphic drawingBounds]];
    }

}


- (void)stopEditing
{
    // Make it harmless to invoke this method unnecessarily.
    if (_editingView)
    {

	// Undo what we did in -startEditingGraphic:.
	[[CPNotificationCenter defaultCenter] removeObserver:self name:CPViewFrameDidChangeNotification object:_editingView];
        
        // Pull the editing view out of this one. When editing is being stopped because the user has clicked in this view, outside of the editing view, NSWindow will have already made this view the window's first responder, and that's good. However, when editing is being stopped because the edited graphic is being removed (by undoing or scripting, for example), the invocation of -[NSView removeFromSuperview] we do here will leave the window as its own first responder, and that would be bad, so also fix the window's first responder if appropriate. It wouldn't be appropriate to steal first-respondership from sibling views here.
	var makeSelfFirstResponder = [[self window] firstResponder] == _editingView ? YES : NO;
	[_editingView removeFromSuperview];
	if (makeSelfFirstResponder) {
	    [[self window] makeFirstResponder:self];
	}

	// Give the graphic that created the editing view a chance to tear down their relationships and then forget about them both.
	[_editingGraphic finalizeEditingView:_editingView];
	_editingGraphic = nil;
	_editingView = nil;

    }
}
////////The functions above is partly implemented  -- Yuan Zhiqian

- (void)createGraphicOfClass:(Class)graphicClass withEvent:(CPEvent)event 
{    
    // Clear the selection.
    [self changeSelectionIndexes:[CPIndexSet indexSet]];

    // Where is the mouse pointer as graphic creation is starting? Should the location be constrained to the grid?
    var graphicOrigin = [self convertPoint:[event locationInWindow] fromView:nil];
    if (_grid) {
	graphicOrigin = [_grid constrainedPoint:graphicOrigin];
    }

    // Create the new graphic and set what little we know of its location.
    _creatingGraphic = [[graphicClass alloc] init];
    [_creatingGraphic setBounds:CPMakeRect(graphicOrigin.x, graphicOrigin.y, 0.0, 0.0)];

    // Add it to the set of graphics right away so that it will show up in other views of the same array of graphics as the user sizes it.
    var mutableGraphics = [self mutableGraphics];
    [mutableGraphics insertObject:_creatingGraphic atIndex:0]; //alert([mutableGraphics count]);

    // Let the user size the new graphic until they let go of the mouse. Because different kinds of graphics have different kinds of handles, 
    // first ask the graphic class what handle the user is dragging during this initial sizing.
    [self resizeGraphic:_creatingGraphic usingHandle:[graphicClass creationSizingHandle] withEvent:event];
}

- (void)resizeGraphic:(MGGraphic)graphic usingHandle:(int)handle withEvent:(CPEvent)event
{
//	debugger;
    _resizedGraphic = graphic;
    _resizedHandle = handle;

    [self resizeGraphic:event];
	
//	debugger;
}

-(void) resizeGraphic:(CPEvent)event
{
    var type = [event type];

    //if (type == CPLeftMouseDown) alert("mouse down");
    if (type == CPLeftMouseUp)
    {
	// PLL : We use resizeGraphic: to resize the newly created object or to resize ususal objects
	if (_creatingGraphic)
	{			
	    // Did we really create a graphic? Don't check with !NSIsEmptyRect(createdGraphicBounds) because the bounds of 
	    // a perfectly horizontal or vertical line is "empty" but of course we want to let people create those.
	    var createdGraphicBounds = [_creatingGraphic bounds];
	    if (CPRectGetWidth(createdGraphicBounds) != 0.0 || CPRectGetHeight(createdGraphicBounds) != 0.0)
	    {
		// Select it.
		[self changeSelectionIndexes:[CPIndexSet indexSetWithIndex:0]];

		// The graphic wasn't sized to nothing during mouse tracking. Present its editing interface it if it's that kind of graphic (like Sketch's SKTTexts). 
		// Invokers of the method we're in right now should have already cleared out _editingView.
		[self startEditingGraphic:_creatingGraphic];		
	    }

	    [self setNeedsDisplay:YES];            
		
	    // Done.
	    _creatingGraphic = nil;
	}
	
        return;
    }
    
    if (type == CPLeftMouseDragged)
    {
	[self autoscroll:event];  //I don't understand, what's the purpose of this line?
		
        var handleLocation = [self convertPoint:[event locationInWindow] fromView:nil];
	
	if (_grid) {
   	  handleLocation = [_grid constrainedPoint:handleLocation];
	}
    
	_resizedHandle = [_resizedGraphic resizeByMovingHandle:_resizedHandle toPoint:handleLocation];
	
	[self setNeedsDisplay:YES];
    }

    //determining the function to response the next mouse dragged and mouse up event -- Yuan Zhiqian 
    [CPApp setTarget:self selector:@selector(resizeGraphic:) forNextEventMatchingMask:CPLeftMouseDraggedMask | CPLeftMouseUpMask untilDate:nil inMode:nil dequeue:YES];
}

//////////////
- (void)moveSelectedGraphicsWithEvent:(CPEvent)event
{
    var type = [event type];
    
    if (type == CPLeftMouseUp)
    {        
        if (_isMoving) 
	{
	    _isHidingHandles = NO;
	    [self setNeedsDisplayInRect:[MMGGraphic drawingBoundsOfGraphics:_selGraphics]];
	    if (_didMove) 
	    {
	        // Only if we really moved.
	        // [[self undoManager] setActionName:NSLocalizedStringFromTable(@"Move", @"UndoStrings", @"Action name for moves.")];
	    }
	}

        return;
    }

    //I don't understand -- Yuan Zhiqian
    //Anyway, it seems that because we didn't dequeue event from the calller, so that in this function, CPLeftMouseDown event is still being processed, even if other events comes into the queue
    if (type == CPLeftMouseDown)
    {
        _selGraphics = [self selectedGraphics];
	var c = [_selGraphics count];
	
	var selBounds = [[MMGGraphic self] boundsOfGraphics:_selGraphics];

	_didMove = NO;
	_isMoving = NO;

	_lastPoint = [self convertPoint:[event locationInWindow] fromView:nil];
	_selOriginOffset = CPMakePoint((_lastPoint.x - selBounds.origin.x), (_lastPoint.y - selBounds.origin.y));
	
    }
    else if (type == CPLeftMouseDragged)
    {
	[self autoscroll:event];
        var curPoint = [self convertPoint:[event locationInWindow] fromView:nil];
        //If the status is not moving and the move is significant enough to trigger a translantion operation
        if (!_isMoving && ((Math.abs(curPoint.x - _lastPoint.x) >= 2.0) || (Math.abs(curPoint.y - _lastPoint.y) >= 2.0))) 
	{
            _isMoving = YES;
            _isHidingHandles = YES;
        }

        if (_isMoving) 
	{
            if (_grid) 
	    {
		var boundsOrigin = CPMakePoint((curPoint.x - _selOriginOffset.x), (curPoint.y - _selOriginOffset.y));
		boundsOrigin  = [_grid constrainedPoint:boundsOrigin];
                curPoint.x = (boundsOrigin.x + _selOriginOffset.x);
                curPoint.y = (boundsOrigin.y + _selOriginOffset.y);
            }

            if (! CPPointEqualToPoint(_lastPoint, curPoint))
	    {
		[[MMGGraphic class] translateGraphics:_selGraphics byX:(curPoint.x - _lastPoint.x) y:(curPoint.y - _lastPoint.y)];
		_didMove = YES;
	
		[self setNeedsDisplay:YES];				
			
                //What for?
                _pasteCascadeDelta.x += (curPoint.x - _lastPoint.x);
                _pasteCascadeDelta.y += (curPoint.y - _lastPoint.y);
            }

            _lastPoint = curPoint;
        }
    }
    
//Indicate that it will accept and deal with the next mouse drag and mouse up event -- Yuan Zhiqian
    [CPApp setTarget:self selector:@selector(moveSelectedGraphicsWithEvent:) forNextEventMatchingMask:CPLeftMouseDraggedMask | CPLeftMouseUpMask untilDate:nil inMode:nil dequeue:YES];
}

- (CPIndexSet)indexesOfGraphicsIntersectingRect:(CPRect)rect
{
    var indexSetToReturn = [CPMutableIndexSet indexSet];
    var graphics = [self graphics];
    var graphicCount = [graphics count];
    for (var index = 0; index<graphicCount; index++) 
    {
	var graphic = [graphics objectAtIndex:index];
        if (CPRectIntersectsRect(rect, [graphic drawingBounds]))
	{
            [indexSetToReturn addIndex:index];
        }
    }
    return indexSetToReturn;
}

- (void)marqueeSelectWithEvent:(CPEvent)event 
{
    var type = [event type];
    
    if (type == CPLeftMouseUp)
    {        
	//debugger;
	 
	[self setNeedsDisplay:YES];

	// Make it not there.
	_marqueeSelectionBounds = CPRectMakeZero();

	_oldSelectionIndexes = nil;
        return;
    }
    
    if (type == CPLeftMouseDown)
    {
        _oldSelectionIndexes = [self selectionIndexes];
	_originalMouseLocation = [self convertPoint:[event locationInWindow] fromView:nil];
    }
    else if (type == CPLeftMouseDragged)
    {
	[self autoscroll:event];
	var currentMouseLocation = [self convertPoint:[event locationInWindow] fromView:nil];
		
	// Figure out a new a selection rectangle based on the mouse location.
	var newMarqueeSelectionBounds = CPMakeRect(Math.min(_originalMouseLocation.x, currentMouseLocation.x), 
							Math.min(_originalMouseLocation.y, currentMouseLocation.y), 
							Math.abs(currentMouseLocation.x - _originalMouseLocation.x), 
							Math.abs(currentMouseLocation.y - _originalMouseLocation.y));
	if (! CPRectEqualToRect(newMarqueeSelectionBounds, _marqueeSelectionBounds))
	{
	    // Erase the old selection rectangle and draw the new one.

	    [self setNeedsDisplayInRect:_marqueeSelectionBounds];
	    _marqueeSelectionBounds = newMarqueeSelectionBounds;

	    [self setNeedsDisplayInRect:_marqueeSelectionBounds];

	    // Either select or deselect all of the graphics that intersect the selection rectangle.
	    var indexesOfGraphicsInRubberBand = [self indexesOfGraphicsIntersectingRect:_marqueeSelectionBounds];
	    var newSelectionIndexes = [_oldSelectionIndexes mutableCopy];
	    for (var index = [indexesOfGraphicsInRubberBand firstIndex]; index!=CPNotFound; index = [indexesOfGraphicsInRubberBand indexGreaterThanIndex:index]) 
	    {
		if ([newSelectionIndexes containsIndex:index])
		{
		    [newSelectionIndexes removeIndex:index];
		} 
		else 
		{
		    [newSelectionIndexes addIndex:index];
		}
	    }
            [self changeSelectionIndexes:newSelectionIndexes];

  	    [self setNeedsDisplay:YES];
	}
    }
    
    [CPApp setTarget:self selector:@selector(marqueeSelectWithEvent:) forNextEventMatchingMask:CPLeftMouseDraggedMask | CPLeftMouseUpMask untilDate:nil inMode:nil dequeue:YES];
}
//////////////

//swallow the mouse events
- (void)swallowMouseEvents:(CPEvent)event
{
    var type = [event type];
    
    if (type == CPLeftMouseUp)
    {        
	console.log("swallowMouseEvents");
        return;
    }
        
    [CPApp setTarget:self selector:@selector(swallowMouseEvents:) forNextEventMatchingMask:CPLeftMouseDraggedMask | CPLeftMouseUpMask untilDate:nil inMode:nil dequeue:YES];
}

- (void)selectAndTrackMouseWithEvent:(CPEvent)event 
{
    // Are we changing the existing selection instead of setting a new one?
    var modifyingExistingSelection = ([event modifierFlags] & CPShiftKeyMask) ? YES : NO;  //Detect if shift key is pressed, if so it allow multiselection -- Yuan Zhiqian

    // Has the user clicked on a graphic?
    var mouseLocation = [self convertPoint:[event locationInWindow] fromView:nil];
    var dict = [self graphicUnderPoint:mouseLocation];
    var clickedGraphic = [dict objectForKey:@"graphic"];
    var clickedGraphicIndex = [[dict objectForKey:@"index"] intValue];
    var clickedGraphicIsSelected = [[dict objectForKey:@"isSelected"] boolValue];
    var clickedGraphicHandle = [[dict objectForKey:@"handle"] intValue];
	
    if (clickedGraphic != [CPNull null])
    {
	// Clicking on a graphic knob takes precedence.
	if (clickedGraphicHandle != SKTGraphicNoHandle)
	{
	    // The user clicked on a graphic's handle. Let the user drag it around.
	    [self resizeGraphic:clickedGraphic usingHandle:clickedGraphicHandle withEvent:event];
	} 
	else
	{
   	    //debugger;
	
	    // The user clicked on a graphic's contents. Update the selection.
	    if (modifyingExistingSelection) 
	    {
		if (clickedGraphicIsSelected)
		{		    
		    // Remove the graphic from the selection.
		    var newSelectionIndexes = [[self selectionIndexes] mutableCopy];
		    [newSelectionIndexes removeIndex:clickedGraphicIndex];
		    [self changeSelectionIndexes:newSelectionIndexes];
		    clickedGraphicIsSelected = NO;
		    
		} 
		else
		{		    
		    // Add the graphic to the selection.
		    var newSelectionIndexes = [[self selectionIndexes] mutableCopy];
		    [newSelectionIndexes addIndex:clickedGraphicIndex];
		    [self changeSelectionIndexes:newSelectionIndexes];
		    clickedGraphicIsSelected = YES;
		    
		}
				
		[self setNeedsDisplay:YES];
	    } 
	    else 
	    {
                [self setNeedsDisplayInRect:_marqueeSelectionBounds];

		// If the graphic wasn't selected before then it is now, and none of the rest are.
		if (!clickedGraphicIsSelected) {
		    [self changeSelectionIndexes:[CPIndexSet indexSetWithIndex:clickedGraphicIndex]];
		    clickedGraphicIsSelected = YES;
				
	            [self setNeedsDisplay:YES];
		}
		
	    }
	    
	    // Is the graphic that the user has clicked on now selected?
	    if (clickedGraphicIsSelected) 
	    {
		// Yes. Let the user move all of the selected objects.
		[self moveSelectedGraphicsWithEvent:event];
	    } 
	    else 
	    {
		// No. Just swallow mouse events until the user lets go of the mouse button. We don't even bother autoscrolling here.
		[self swallowMouseEvents:event];
	    }
	}
    }
    else 
    {        	
	// The user clicked somewhere other than on a graphic. Clear the selection, unless the user is holding down the shift key.
        // This is a very clever solution of marquee selection with/without shift key operation. The programme detect the shift key first, then perform the marquee selection, so that it won't need to check shift key again and again each time it select/deselect a graphic -- Yuan Zhiqian 
	if (! modifyingExistingSelection) {
   	    [self changeSelectionIndexes:[CPIndexSet indexSet]];
	}
	
	// The user clicked on a point where there is no graphic. Select and deselect graphics until the user lets go of the mouse button.
        [self marqueeSelectWithEvent:event];
    }
    
}

- (void)mouseDown:(CPEvent)event
{
    //console.log("here");
    [self stopEditing];
    var graphicClassToInstantiate = [[MMGToolPaletteController sharedToolPaletteController] currentGraphicClass];
    if (graphicClassToInstantiate) // When false, it means Selection tool is selected, otherwise it means some graphic creation tool is selected -- Yuan Zhiqian
    {
	// Create a new graphic and then track to size it.
        // This is the creation part -- Yuan Zhiqian
	[self createGraphicOfClass:graphicClassToInstantiate withEvent:event]; //alert('yes');
    } 
    else 
    {
	// Double-clicking with the selection tool always means "start editing," or "do nothing" if no editable graphic is double-clicked on.
        // Editing part is what we will add in the future
	var doubleClickedGraphic = nil;
	if ([event clickCount] > 1) 
	{
	    var mouseLocation = [self convertPoint:[event locationInWindow] fromView:nil];
	    var dict = [self graphicUnderPoint:mouseLocation];
            doubleClickedGraphic = [dict objectForKey:@"graphic"];
	    if (doubleClickedGraphic != [CPNull null]) 
	    {
		[self startEditingGraphic:doubleClickedGraphic];
	    }
	}
	
        // Here, this short line is the core of performing translation, deselection, resizing and even rotation operations  -- Yuan Zhiqian
	if (!doubleClickedGraphic) 
	{
	    // Update the selection and/or move graphics or resize graphics.
	    [self selectAndTrackMouseWithEvent:event];
	}

    }
}

@end
