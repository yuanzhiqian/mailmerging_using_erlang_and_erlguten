/*
 * MMGToolPaletteController.j
 * Adapted and modified from tutorial-drawing-part2
 *
 * YuanZhiqian
 * 
 */

@import <Foundation/CPObject.j>

@import <AppKit/CPWindowController.j>

@import "MMGTextFrame.j"
@import "MMGImageFrame.j"

@import "ImageCell.j"

MMGArrowToolRow = 0;
MMGTextToolRow = 1;
MMGImgToolRow = 2;

MMGSelectedToolDidChangeNotification = @"MMGSelectedToolDidChange";

var sharedToolPaletteController = nil;

@implementation MMGToolPaletteController : CPWindowController
{
    CPCollectionView _toolsCollectionView;
}

+ (id)sharedToolPaletteController 
{
    if (!sharedToolPaletteController) 
    {
        sharedToolPaletteController = [[MMGToolPaletteController alloc] init];
    }

    return sharedToolPaletteController;
}

- (id)init
{
    var theWindow = [[CPPanel alloc] initWithContentRect:CGRectMake(100, 100, 40, 160) styleMask: CPClosableWindowMask];
    self = [super initWithWindow:theWindow];
    if (self)
    {
	[theWindow setTitle:@"Tools"];
	[theWindow setLevel:CPFloatingWindowLevel];

	var contentView = [theWindow contentView];
	var bounds = [contentView bounds];
	
	var toolCollectionViewItem = [[CPCollectionViewItem alloc] init];
	[toolCollectionViewItem setView:[[ImageCell alloc] initWithFrame:CGRectMake(0, 0, 36, 36)]];

	_toolsCollectionView = [[CPCollectionView alloc] initWithFrame:CGRectMake(0, 0, CGRectGetWidth(bounds), CGRectGetHeight(bounds))];
	[_toolsCollectionView setMaxNumberOfColumns:1];
	[_toolsCollectionView setMaxNumberOfRows:4];
	[_toolsCollectionView setVerticalMargin:0.0];
	[_toolsCollectionView setDelegate:self];
	[_toolsCollectionView setItemPrototype:toolCollectionViewItem];
	[_toolsCollectionView setMinItemSize:CGSizeMake(36, 36)];
	[_toolsCollectionView setMaxItemSize:CGSizeMake(48, 48)];
	[_toolsCollectionView setAutoresizingMask:CPViewHeightSizable | CPViewWidthSizable];
	[_toolsCollectionView setAllowsMultipleSelection:NO];

	// Associate the content array with the collection view
	[_toolsCollectionView setContent:[self imagePathArray]];

	[contentView addSubview:_toolsCollectionView];    
    }
    
    return self;
}


-(CPArray) imagePathArray
{
    var mainBundle = [CPBundle mainBundle];
    var path1 = [mainBundle pathForResource:@"Arrow.png"];        // the return of pathForResource is still a CPString object
    var path2 = [mainBundle pathForResource:@"TextGraphic.gif"];
    var path3 = [mainBundle pathForResource:@"ImgGraphic.gif"]
	
    // Pas de texte pour le moment
    return [[CPArray alloc] initWithObjects:path1, path2, path3, nil];
}


- (IBAction)selectToolAction:(id)sender
{
    [[CPNotificationCenter defaultCenter] postNotificationName:MMGSelectedToolDidChangeNotification object:self];
}

- (Class)currentGraphicClass
{
    var row = [[_toolsCollectionView selectionIndexes] firstIndex];
    var theClass = nil;

    // debugger;
	
    if (row == MMGTextToolRow) {
        theClass = [MMGTextFrame class];
    }
    else if (row == MMGImgToolRow){
        theClass = [MMGImageFrame class];
    }

    return theClass;
}

- (void)selectArrowTool {
//    [toolButtons selectCellAtRow:PLArrowToolRow column:0];
    [[CPNotificationCenter defaultCenter] postNotificationName:MMGSelectedToolDidChangeNotification object:self];
}


@end
