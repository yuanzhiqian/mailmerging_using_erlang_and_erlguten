/*
 * AppController.j
 * Modified from the template
 *
 * YuanZhiqian
 * --------------------------
 * Note!!! The frames can't be too small, otherwise it will arouse undetectable error when processed in erlguten
 */

@import <Foundation/CPObject.j>

@import "PageView.j"
@import "MMGDocument.j"
@import "MMGToolPaletteController.j"

//The xml packing function
function packXml(xmlDoc, paperNode, graphic)
{ 
  //Set attributes
  var attr_name = xmlDoc.createAttribute("name");
  var attr_class = xmlDoc.createAttribute("class");
  var attr_x = xmlDoc.createAttribute("x");
  var attr_y = xmlDoc.createAttribute("y");
  var attr_width = xmlDoc.createAttribute("width");
  var attr_height = xmlDoc.createAttribute("height");
  var attr_grid = xmlDoc.createAttribute("grid");
  var attr_bg = xmlDoc.createAttribute("bg");
  var attr_font = xmlDoc.createAttribute("font");
  var attr_fontsize = xmlDoc.createAttribute("fontsize");
  var attr_paraIndent = xmlDoc.createAttribute("paraIndent");
  var attr_maxlines = xmlDoc.createAttribute("maxlines");
  var attr_continue = xmlDoc.createAttribute("continue");
  var attr_break = xmlDoc.createAttribute("break");

  attr_name.nodeValue = [graphic name];
  var bounds = [graphic bounds];
  attr_class.nodeValue = [graphic class];
  attr_x.nodeValue = CGRectGetMinX(bounds);
  attr_y.nodeValue = CGRectGetMinY(bounds);
  attr_width.nodeValue = CGRectGetWidth(bounds);
  attr_height.nodeValue = CGRectGetHeight(bounds);
  attr_grid.nodeValue = [graphic hasGrid]?"true":"false";
  attr_bg.nodeValue = [graphic bg];
  attr_font.nodeValue = [graphic textFont];
  attr_fontsize.nodeValue = [graphic fontSize];
  attr_paraIndent.nodeValue = [graphic paraIndent];
  attr_maxlines.nodeValue = [graphic maxLines];
  attr_continue.nodeValue = [graphic hasContinue];
  attr_break.nodeValue = [graphic ifBreak];
 
  //Create the frame node
  var frameNode = xmlDoc.createElement("frame");  

  //Add Attributes to frame
  frameNode.setAttributeNode(attr_name);
  frameNode.setAttributeNode(attr_class);
  frameNode.setAttributeNode(attr_x);
  frameNode.setAttributeNode(attr_y);
  frameNode.setAttributeNode(attr_width);
  frameNode.setAttributeNode(attr_height);
  frameNode.setAttributeNode(attr_grid);
  frameNode.setAttributeNode(attr_bg);
  frameNode.setAttributeNode(attr_font);
  frameNode.setAttributeNode(attr_fontsize);
  frameNode.setAttributeNode(attr_paraIndent);
  frameNode.setAttributeNode(attr_maxlines);
  frameNode.setAttributeNode(attr_continue);
  frameNode.setAttributeNode(attr_break);
  frameNode.setAttributeNode(attr_name);
  frameNode.setAttributeNode(attr_name);

  //Add text nodes to frame  
  var content = [graphic content];
  var textNode = xmlDoc.createTextNode(content);
  frameNode.appendChild(textNode);

  //Add frame nodes
  paperNode.appendChild(frameNode);

  return xmlDoc;
}

//The objective-j part
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
    [button setTitle:@"Save Templates"];
    [button setAction:@selector(saveTemplates:)];
    [button setTarget:self];
    [button setAutoresizingMask:CPViewMinXMargin | CPViewMaxXMargin | CPViewMinYMargin | CPViewMaxYMargin];
    [contentView addSubview:button];

    button = [[CPButton alloc] initWithFrame:CGRectMake(10, 140, 200, 24)];
    [button setTitle:@"Generate pdf"];
    [button setAction:@selector(generatePdf:)];
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

-(IBAction)generatePdf:(id)sender
{
    var httpBody = "";
    var filename = "template_1";
    var urlString = @"http://localhost:3000/genPdf/" + filename;
    var request = [CPURLRequest requestWithURL: urlString];
    [request setHTTPMethod: @"POST"];
    [request setHTTPBody: httpBody];
    //[request setValue:[httpBody length] forHTTPHeaderField:@"Content-Length"];
    [request setValue:"application/x-www-form-urlencoded" forHTTPHeaderField:@"Content-Type"];
    [CPURLConnection connectionWithRequest:request delegate:self];   
}

-(IBAction)saveTemplates:(id)sender
{
    var xmlString = '<?xml version="1.0" ?><template></template>';
    // Create DOM
    if (window.DOMParser)
    {
      var parser = new DOMParser();
      var xmlDoc = parser.parseFromString(xmlString, "text/xml");
    }
    else //Internet Explorer
    {
      var xmlDoc = new ActiveXObject("Microsoft.XMLDOM");
      xmlDoc.async = "false";
      xmlDoc.loadXML(txt);
    }

    // Set template attr
    var sharedDocumentController = [CPDocumentController sharedDocumentController];
    var theDocuments = [sharedDocumentController documents];
    DocCount = [theDocuments count];
    Alt2 = "template_2.xml";
    Alt3 = "template_3.xml";

    var t = xmlDoc.getElementsByTagName("template")[0];
    var attr_count = xmlDoc.createAttribute("count");
    attr_count.nodeValue = DocCount;
    var attr_alt2 = xmlDoc.createAttribute("alt2");
    attr_alt2.nodeValue = Alt2;
    var attr_alt3 = xmlDoc.createAttribute("alt3");
    attr_alt3.nodeValue = Alt3;
     
    t.setAttributeNode(attr_count);
    t.setAttributeNode(attr_alt2);
    t.setAttributeNode(attr_alt3);

    // Add papers

    for (var index = 0; index<DocCount; index++)
    {
        var theDocument = [theDocuments objectAtIndex: index];
        [self generatePaper: theDocument forXml: xmlDoc];
    }

    // Convert xmlDom to xmlString

    xmlString = (new XMLSerializer()).serializeToString(xmlDoc);

    //Add header
    xmlString = '<?xml version="1.0" ?>' + xmlString;
    console.log(xmlString); 
    //Sending xml back to the server
    var httpBody = xmlString;
    var filename = "template_1";
    var urlString = @"http://localhost:3000/saveXml/" + filename;
    var request = [CPURLRequest requestWithURL: urlString];
    [request setHTTPMethod: @"POST"];
    [request setHTTPBody: httpBody];
    //[request setValue:[httpBody length] forHTTPHeaderField:@"Content-Length"];
    [request setValue:"application/x-www-form-urlencoded" forHTTPHeaderField:@"Content-Type"];
    [CPURLConnection connectionWithRequest:request delegate:self];   
}

-(IBAction)generatePaper:(CPDocument)theDocument forXml:xmlDoc
{
    var paperNode = xmlDoc.createElement("paper"); 
    var attr_name = xmlDoc.createAttribute("name");
    attr_name.nodeValue = "first";
    var attr_class = xmlDoc.createAttribute("class");
    attr_class.nodeValue = "front";

    paperNode.setAttributeNode(attr_name);
    paperNode.setAttributeNode(attr_class);

    var x = xmlDoc.getElementsByTagName("template")[0];
    x.appendChild(paperNode);

    var graphics = [theDocument graphics];
    //console.log([graphics count]);
    var graphicCount = [graphics count];
    
    //Modify the DOM
    for (var index = 0; index<graphicCount; index++) 
    {
        var graphic = [graphics objectAtIndex:index]
        xmlDoc = packXml(xmlDoc, paperNode, graphic);
        //console.log(xmlDoc);        
    }    
}

- (void)connection:(CPURLConnection) connection didReceiveData:(CPString)data
{
    //This method is called when a connection receives a response. in a
    //multi-part request, this method will (eventually) be called multiple times,
    //once for each part in the response.
    console.log("Success!\n"+data);
}

- (void)connection:(CPURLConnection)connection didFailWithError:(CPString)error
{
    //This method is called if the request fails for any reason.
}

@end
