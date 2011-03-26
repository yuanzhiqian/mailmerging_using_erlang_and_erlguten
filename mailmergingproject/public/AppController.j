/*
 * AppController.j
 * Modified from the template
 *
 * YuanZhiqian
 * 
 */

@import <Foundation/CPObject.j>

@import "PageView.j"
@import "MMGDocument.j"
@import "MMGToolPaletteController.j"

@implementation AppController : CPObject
{
}

- (void)applicationDidFinishLaunching:(CPNotification)aNotification
{
    var theWindow = [[CPWindow alloc] initWithContentRect:CGRectMakeZero() styleMask:CPBorderlessBridgeWindowMask];
    var contentView = [theWindow contentView];
    var bounds = [contentView bounds];

    var button = [[CPButton alloc] initWithFrame:CGRectMake(10, 10, 200, 24)];
    [button setTitle:@"Create Document"];
    [button setAction:@selector(createDocument:)];
    [button setTarget:self];
    [button setAutoresizingMask:CPViewMinXMargin | CPViewMaxXMargin | CPViewMinYMargin | CPViewMaxYMargin];
    [contentView addSubview:button];

    // debugger;
    button = [[CPButton alloc] initWithFrame:CGRectMake(10, 40, 200, 24)];
    [button setTitle:@"Show Tools"];
    [button setAction:@selector(showToolsWindow:)];
    [button setTarget:self];
    [button setAutoresizingMask:CPViewMinXMargin | CPViewMaxXMargin | CPViewMinYMargin | CPViewMaxYMargin];
    [contentView addSubview:button];

    button = [[CPButton alloc] initWithFrame:CGRectMake(10, 70, 200, 24)];
    [button setTitle:@"Hide Tools"];
    [button setAction:@selector(hideToolsWindow:)];
    [button setTarget:self];
    [button setAutoresizingMask:CPViewMinXMargin | CPViewMaxXMargin | CPViewMinYMargin | CPViewMaxYMargin];
    [contentView addSubview:button];

    button = [[CPButton alloc] initWithFrame:CGRectMake(10, 110, 200, 24)];
    [button setTitle:@"Generate Template"];
    [button setAction:@selector(generateTemplate:)];
    [button setTarget:self];
    [button setAutoresizingMask:CPViewMinXMargin | CPViewMaxXMargin | CPViewMinYMargin | CPViewMaxYMargin];
    [contentView addSubview:button];

    [theWindow makeKeyAndOrderFront:self];
    
    // Uncomment the following line to turn on the standard menu bar.
    //var menu = [[CPMenu alloc] init];
    //[[CPApplication sharedApplication] setMenu:menu]; 
    //[CPMenu setMenuBarVisible:YES];
 
}

-(IBAction)showToolsWindow:(id)sender
{
    var sharedToolPaletteController = [MMGToolPaletteController sharedToolPaletteController];
    [sharedToolPaletteController showWindow:sender];
}

-(IBAction)hideToolsWindow:(id)sender
{
    var sharedToolPaletteController = [MMGToolPaletteController sharedToolPaletteController];
    [[sharedToolPaletteController window] orderOut:sender];
}

-(IBAction)createDocument:(id)sender
{
    var sharedDocumentController = [CPDocumentController sharedDocumentController];
	
    var documents = [sharedDocumentController documents];
    var defaultType = [sharedDocumentController defaultType];
	
    [sharedDocumentController newDocument:self];
    // [sharedDocumentController openUntitledDocumentOfType:@"MyBundle" display:YES];
}

-(IBAction)generateTemplate:(id)sender
{
    var sharedDocumentController = [CPDocumentController sharedDocumentController];
    var theDocuments = [sharedDocumentController documents];
    
    //We assume that we only have one document at present
    //console.log([documents count]);
    var theDocument = [theDocuments objectAtIndex: 0];
    var graphics = [theDocument graphics];
    //console.log([graphics count]);
    var graphicCount = [graphics count];
    var xmlString = '<?xml version="1.0" ?><paper>';
    for (var index = 0; index<graphicCount; index++) 
    {
        var graphic = [graphics objectAtIndex:index]
        var name = [graphic name];
        var bounds = [graphic bounds];
        var x = CGRectGetMinX(bounds);
        var y = CGRectGetMinY(bounds);
        var width = CGRectGetWidth(bounds);
        var height = CGRectGetHeight(bounds);
        var grid = [graphic hasGrid]?"true":"false";
        var font = "Times-Roman";  //hard code
        var fontsize = "32/32";    //hard code
        var maxlines = [graphic maxLines];
        var content = [graphic content];
        xmlString += '<frame'+ ' name = ' + '"' + name + '"' + ' x = ' + '"' + x + '"' + ' y = ' + '"' + y + '"' + ' width = ' + '"' + width + '"' + ' height = ' + '"' + height + '"' + ' grid= ' + '"' + grid + '"' + ' font = ' + '"' + font + '"' + ' fontsize = ' + '"' + fontsize + '"' + ' maxlines = ' + '"' + maxlines + '"' +'>'+ content +'</frame>'; 
        
    }    
    xmlString += '</paper>';
    var httpBody = xmlString;
    var urlString = @"http://localhost:3000/template";
    var request = [CPURLRequest requestWithURL: urlString];
    [request setHTTPMethod: @"POST"];
    [request setHTTPBody: httpBody];
    //[request setValue:[httpBody length] forHTTPHeaderField:@"Content-Length"];
    [request setValue:"application/x-www-form-urlencoded" forHTTPHeaderField:@"Content-Type"];
    [CPURLConnection connectionWithRequest:request delegate:self];   
}

- (void)connection:(CPURLConnection) connection didReceiveData:(CPString)data
{
    //This method is called when a connection receives a response. in a
    //multi-part request, this method will (eventually) be called multiple times,
    //once for each part in the response.
}

- (void)connection:(CPURLConnection)connection didFailWithError:(CPString)error
{
    //This method is called if the request fails for any reason.
}

@end
