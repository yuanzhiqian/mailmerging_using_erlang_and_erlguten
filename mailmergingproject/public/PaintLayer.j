/*
 * PaintLayer.j
 * Display graphics
 *
 * YuanZhiqian
 * 
 */

@import <AppKit/CPView.j>
@import <AppKit/CPColor.j>
@import <AppKit/CPGraphicsContext.j>
@import "MMGGraphic.j"

@implementation PaintLayer:CALayer
{
    PageView    _pageView;
}

- (id)initWithPageView:(PageView)aPageView
{
    self = [super init];
    
    if (self)
    {
        _pageView = aPageView;
        
    }
    
    return self;
}

- (PageView)pageView
{
    return _pageView;
}

@end
