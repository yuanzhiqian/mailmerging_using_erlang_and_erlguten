/*
 * MMGDocument.j
 * Initiate MMGWindowController and store graphic objects
 *
 * YuanZhiqian
 * 
 */

@import <Foundation/CPObject.j>

@import <AppKit/CPDocument.j>
@import <AppKit/CPWindowController.j>
@import <AppKit/CPWindow.j>

@import "MMGWindowController.j"
@import "MMGGraphic.j"
@import "MMGTextFrame.j"

@implementation MMGDocument : CPDocument
{
    CPArray _graphics;
    CPIndexSet _selectionIndexes;
}

- (id)init
{
    self = [super init];
    if (self)
    {
	_graphics = [CPArray array];
        _selectionIndexes = [CPIndexSet indexSet];       
    }
    
    return self;
}

- (CPArray)graphics 
{    
    return _graphics;
}

- (CPIndexSet)selectionIndexes 
{
    return _selectionIndexes;
}

- (void)setSelectionIndexes:(CPIndexSet)selectionIndexes
{
    _selectionIndexes = selectionIndexes;
}

- (void)makeWindowControllers
{
    // debugger;
    var controller = [[MMGWindowController alloc] init];
    [self addWindowController:controller];
}

//override dataofType to make the save operation work
-(CPData) dataOfType:(CPString) aType error:({CPError}) anError
{
  var data = [CPData dataWithString: @"test"];
  return data;
}	 

@end
