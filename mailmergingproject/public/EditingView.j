@import <AppKit/CPView.j>

@implementation EditingView:CPView
{
    
}

- (id)initWithFrame:(CGRect)aFrame
{
    self = [super initWithFrame:aFrame];
    
    if (self)
    {
      [self setBackgroundColor:[CPColor blackColor]];
    }
    return self;
}

@end
