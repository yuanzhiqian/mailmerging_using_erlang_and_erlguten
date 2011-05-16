/*
 * MMGWindowController.j
 * Manipulate windows and initiate pageview
 *
 * YuanZhiqian
 * 
 */

@import <AppKit/CPWindowController.j>

@import "PageView.j"

@implementation MMGWindowController : CPWindowController
{
    CPScrollView _scrollView;
    PageView _pageView;
}

- (id)init
{
    var theWindow = [[CPWindow alloc] initWithContentRect:CGRectMake(300.0, 50.0, 610.0, 500.0) 
						styleMask: CPTitledWindowMask | CPClosableWindowMask];
    var contentView = [theWindow contentView];
    var bounds = [contentView bounds];

    // Make the background black.
    [contentView setBackgroundColor:[CPColor blackColor]];

    // Create and Center our Container View
    _scrollView = [[CPScrollView alloc] initWithFrame:CGRectMake(0, 0, CGRectGetWidth(bounds), CGRectGetHeight(bounds))];
    [_scrollView setBackgroundColor:[CPColor blackColor]];
    [_scrollView setAutohidesScrollers:YES];
    [_scrollView setAutoresizingMask:CPViewWidthSizable | CPViewHeightSizable];

    _pageView = [[PageView alloc] initWithFrame:CGRectMake(0,0,595,842)];
    [_pageView setBackgroundColor:[CPColor whiteColor]];
    [_scrollView setDocumentView:_pageView];

    [contentView addSubview:_scrollView];
    //[theWindow setTitle: "Template design"]; // not working 
    [theWindow makeFirstResponder:_scrollView];	
	
    self = [super initWithWindow:theWindow];
    if (self)
    {
    }
    
    return self;
}

-(PageView)pageView
{
    return _pageView;
}

-(void)setpageView:(PageView)pageView
{
    _pageView = pageView;
}

- (CPArray)graphics 
{    
    var graphics = [[self document] graphics];
    if (!graphics) {
	graphics = [CPArray array];
    }
    return graphics;
}

- (CPIndexSet)selectionIndexes 
{
    return [[self document] selectionIndexes];
}

- (void)setSelectionIndexes:(CPIndexSet)selectionIndexes
{
    [[self document] setSelectionIndexes:selectionIndexes];
}

@end
