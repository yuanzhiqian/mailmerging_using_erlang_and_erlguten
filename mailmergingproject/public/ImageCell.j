/*
 * ImageCell.j
 * Adapted and modified from tutorial-drawing-part2
 *
 * YuanZhiqian
 * 
 */

@import <Foundation/CPObject.j>

ImageCellDragType = @"ImageCellDragType";

@implementation ImageCell : CPView
{
    CPImage         image;
    CPImageView     imageView;
    CPView          highlightView;
    
    CPString        imagePath @accessors;
}

// anObject is a CPString containing the imagePath
- (void)setRepresentedObject:(CPObject)anObject
{
    if(!imageView)
    {
        imageView = [[CPImageView alloc] initWithFrame:CGRectMakeCopy([self bounds])];
        [imageView setAutoresizingMask:CPViewWidthSizable | CPViewHeightSizable];
        [imageView setImageScaling:CPScaleProportionally];
        [imageView setHasShadow:YES];
        [self addSubview:imageView];
    }

    
    imagePath = anObject;
    
    [image setDelegate:nil];
    
    image = [[CPImage alloc] initWithContentsOfFile:imagePath];
    [image setDelegate:self];
    
    if([image loadStatus] == CPImageLoadStatusCompleted)
        [imageView setImage:image];
    else
        [imageView setImage:nil];
}

- (void)imageDidLoad:(CPImage)anImage
{
    [imageView setImage:anImage];
}

- (void)setSelected:(BOOL)flag
{
    // This is a lazy creation : we postpone the creation of the highlight view since
    // the user may never want to select the picture (this allows to create less views)
    if(!highlightView)
    {
        highlightView = [[CPView alloc] initWithFrame:CGRectCreateCopy([self bounds])];
	// This will allow the highlight to be scaled as well (if displayed)
        [highlightView setAutoresizingMask:CPViewWidthSizable | CPViewHeightSizable];
        [highlightView setBackgroundColor:[CPColor colorWithCalibratedWhite:0.8 alpha:0.6]];
    }
    else
    {
	// Since we allow the photo cell to be scaled after creation
	// we have to be sure to update the size (it will not be autoresized when removed from superview)
	[highlightView setFrame:CGRectCreateCopy([self bounds])]
    }

    if(flag)
    {
        // [highlightView setFrame:CGRectInset([imageView imageRect], -10.0, -10.0)];
        [self addSubview:highlightView positioned:CPWindowBelow relativeTo:imageView];
    }
    else
    {
	// We remove the hightlight from the view hierarchy
        [highlightView removeFromSuperview];
    }
}


@end
