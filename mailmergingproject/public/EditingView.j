@import <AppKit/CPView.j>

@implementation EditingView:CPTextField
{
    
}

- (id)initWithFrame:(CGRect)aFrame
{
    self = [super initWithFrame:aFrame];
    
    if (self)
    {
      [self setBackgroundColor:[CPColor yellowColor]]
      [self setEditable: YES]
      [self setBordered: YES]
    }
    return self;
}

@end
