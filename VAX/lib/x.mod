IMPLEMENTATION MODULE x;
 
 
PROCEDURE @INLINE   ConnectionNumber (display:DisplayPtr): Integer;
BEGIN
    RETURN display^.fd;
END ConnectionNumber;
 
 
PROCEDURE @INLINE   RootWindow (display:DisplayPtr; scr: Integer):Window;
BEGIN
    RETURN display^.screens^[scr].root;
END RootWindow;
 
 
PROCEDURE @INLINE DefaultScreen (display:DisplayPtr):Integer;
BEGIN
    RETURN display^.default_screen;
END DefaultScreen;


PROCEDURE @INLINE   DefaultVisual (display:DisplayPtr; scr: Integer): VisualPtr;
BEGIN
    RETURN display^.screens^[scr].root_visual;
END DefaultVisual;
 
 
PROCEDURE @INLINE   DefaultGC (display:DisplayPtr; scr: Integer):GC;
BEGIN
    RETURN display^.screens^[scr].default_gc;
END DefaultGC;
 
 
PROCEDURE @INLINE   BlackPixel (display:DisplayPtr; scr: Integer): Integer;
BEGIN
    RETURN display^.screens^[scr].black_pixel;
END BlackPixel;
 
 
PROCEDURE @INLINE   WhitePixel (display:DisplayPtr; scr: Integer): Integer;
BEGIN
    RETURN display^.screens^[scr].white_pixel;
END WhitePixel;
 
 
PROCEDURE @INLINE   AllPlanes (display:DisplayPtr): PlaneMask;
BEGIN
    RETURN PlaneMask (-1);
END AllPlanes;
 
 
PROCEDURE @INLINE   QLength (display:DisplayPtr): Integer;
BEGIN
    RETURN display^.qlen;
END QLength;
 
 
PROCEDURE @INLINE   DisplayWidth (display:DisplayPtr; scr: Integer): Integer;
BEGIN
    RETURN display^.screens^[scr].width;
END DisplayWidth;
 
 
PROCEDURE @INLINE   DisplayHeight (display:DisplayPtr; scr: Integer): Integer;
BEGIN
    RETURN display^.screens^[scr].height;
END DisplayHeight;
 
 
PROCEDURE @INLINE   DisplayWidthMM (display:DisplayPtr; scr: Integer): Integer;
BEGIN
    RETURN display^.screens^[scr].mwidth;
END DisplayWidthMM;
 
 
PROCEDURE @INLINE   DisplayHeightMM (display:DisplayPtr; scr: Integer): Integer;
BEGIN
    RETURN display^.screens^[scr].mheight;
END DisplayHeightMM;
 
 
PROCEDURE @INLINE   DisplayCells (display:DisplayPtr; scr: Integer): Integer;
BEGIN
    RETURN display^.screens^[scr].root_visual^.map_entries;
END DisplayCells;
 
 
PROCEDURE @INLINE   DisplayPlanes (display:DisplayPtr; scr: Integer): Integer;
BEGIN
    RETURN display^.screens^[scr].root_depth;
END DisplayPlanes;

  
PROCEDURE @INLINE   ServerVendor (display:DisplayPtr): CharArrayPtr;
BEGIN
    RETURN display^.vendor;
END ServerVendor;
 
 
PROCEDURE @INLINE   ProtocolVersion (display:DisplayPtr): Integer;
BEGIN
    RETURN display^.proto_major_version;
END ProtocolVersion;
 
 
PROCEDURE @INLINE   ProtocolRevision (display:DisplayPtr): Integer;
BEGIN
    RETURN display^.proto_minor_version;
END ProtocolRevision;
 
 
PROCEDURE @INLINE   VendorRelease (display:DisplayPtr): Integer;
BEGIN
    RETURN display^.vnumber;
END VendorRelease;
 
 
PROCEDURE @INLINE   DisplayString (display:DisplayPtr):CharArrayPtr;
BEGIN
    RETURN display^.display_name;
END DisplayString;
 
 
PROCEDURE @INLINE   DefaultDepth (display:DisplayPtr; scr: Integer):Integer;
BEGIN
    RETURN display^.screens^[scr].root_depth;
END DefaultDepth;
 
 
PROCEDURE @INLINE   DefaultColormap (display:DisplayPtr; scr: Integer):Colormap;
BEGIN
    RETURN display^.screens^[scr].cmap;
END DefaultColormap;
 
 
PROCEDURE @INLINE   BitmapUnit (display:DisplayPtr): Integer;
BEGIN
    RETURN display^.bitmap_unit;
END BitmapUnit;
 
 
PROCEDURE @INLINE   BitmapBitOrder (display:DisplayPtr): Integer;
BEGIN
    RETURN display^.bitmap_bit_order;
END BitmapBitOrder;
 
 
PROCEDURE @INLINE   BitmapPad (display:DisplayPtr): Integer;
BEGIN
    RETURN display^.bitmap_pad;
END BitmapPad;
 
 
PROCEDURE @INLINE  ImageByteOrder (display:DisplayPtr): Integer;
BEGIN
    RETURN display^.byte_order;
END ImageByteOrder;
 
 
PROCEDURE @INLINE XUniqueContext (): XContext;
BEGIN
    RETURN XContext (XrmUniqueQuark ());
END XUniqueContext;
 
END x.
