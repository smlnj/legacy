(* debug-reply.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 *)

structure DebugXReply =
  struct

    open XReply

    fun debug (f, s) x = (f x) handle ex => (
	  CIO.print(implode[
	      "XReply.", s, ": uncaught exception ", System.exn_name ex, "\n"
	    ]);
	  raise ex)

    val decodeAllocColorCellsReply = debug (decodeAllocColorCellsReply, "decodeAllocColorCellsReply")
    val decodeAllocColorPlanesReply = debug (decodeAllocColorPlanesReply, "decodeAllocColorPlanesReply")
    val decodeAllocColorReply = debug (decodeAllocColorReply, "decodeAllocColorReply")
    val decodeAllocNamedColorReply = debug (decodeAllocNamedColorReply, "decodeAllocNamedColorReply")
    val decodeConnectReqReply = debug (decodeConnectReqReply, "decodeConnectReqReply")
    val decodeError = debug (decodeError, "decodeError")
    val decodeGetAtomNameReply = debug (decodeGetAtomNameReply, "decodeGetAtomNameReply")
    val decodeGetFontPathReply = debug (decodeGetFontPathReply, "decodeGetFontPathReply")
    val decodeGetGeometryReply = debug (decodeGetGeometryReply, "decodeGetGeometryReply")
    val decodeGetImageReply = debug (decodeGetImageReply, "decodeGetImageReply")
    val decodeGetInputFocusReply = debug (decodeGetInputFocusReply, "decodeGetInputFocusReply")
    val decodeGetKeyboardControlReply = debug (decodeGetKeyboardControlReply, "decodeGetKeyboardControlReply")
    val decodeGetKeyboardMappingReply = debug (decodeGetKeyboardMappingReply, "decodeGetKeyboardMappingReply")
    val decodeGetModifierMappingReply = debug (decodeGetModifierMappingReply, "decodeGetModifierMappingReply")
    val decodeGetMotionEventsReply = debug (decodeGetMotionEventsReply, "decodeGetMotionEventsReply")
    val decodeGetPointerControlReply = debug (decodeGetPointerControlReply, "decodeGetPointerControlReply")
    val decodeGetPointerMappingReply = debug (decodeGetPointerMappingReply, "decodeGetPointerMappingReply")
    val decodeGetPropertyReply = debug (decodeGetPropertyReply, "decodeGetPropertyReply")
    val decodeGetScreenSaverReply = debug (decodeGetScreenSaverReply, "decodeGetScreenSaverReply")
    val decodeGetSelectionOwnerReply = debug (decodeGetSelectionOwnerReply, "decodeGetSelectionOwnerReply")
    val decodeGetWindowAttributesReply = debug (decodeGetWindowAttributesReply, "decodeGetWindowAttributesReply")
    val decodeGrabKeyboardReply = debug (decodeGrabKeyboardReply, "decodeGrabKeyboardReply")
    val decodeGrabPointerReply = debug (decodeGrabPointerReply, "decodeGrabPointerReply")
    val decodeGraphicsExpose = debug (decodeGraphicsExpose, "decodeGraphicsExpose")
    val decodeInternAtomReply = debug (decodeInternAtomReply, "decodeInternAtomReply")
    val decodeListExtensionsReply = debug (decodeListExtensionsReply, "decodeListExtensionsReply")
    val decodeListFontsReply = debug (decodeListFontsReply, "decodeListFontsReply")
    val decodeListHostsReply = debug (decodeListHostsReply, "decodeListHostsReply")
    val decodeListInstalledColormapsReply = debug (decodeListInstalledColormapsReply, "decodeListInstalledColormapsReply")
    val decodeListPropertiesReply = debug (decodeListPropertiesReply, "decodeListPropertiesReply")
    val decodeLookupColorReply = debug (decodeLookupColorReply, "decodeLookupColorReply")
    val decodeNoExpose = debug (decodeNoExpose, "decodeNoExpose")
    val decodeQueryBestSizeReply = debug (decodeQueryBestSizeReply, "decodeQueryBestSizeReply")
    val decodeQueryColorsReply = debug (decodeQueryColorsReply, "decodeQueryColorsReply")
    val decodeQueryExtensionReply = debug (decodeQueryExtensionReply, "decodeQueryExtensionReply")
    val decodeQueryFontReply = debug (decodeQueryFontReply, "decodeQueryFontReply")
    val decodeQueryKeymapReply = debug (decodeQueryKeymapReply, "decodeQueryKeymapReply")
    val decodeQueryPointerReply = debug (decodeQueryPointerReply, "decodeQueryPointerReply")
    val decodeQueryTextExtentsReply = debug (decodeQueryTextExtentsReply, "decodeQueryTextExtentsReply")
    val decodeQueryTreeReply = debug (decodeQueryTreeReply, "decodeQueryTreeReply")
    val decodeSetModifierMappingReply = debug (decodeSetModifierMappingReply, "decodeSetModifierMappingReply")
    val decodeSetPointerMappingReply = debug (decodeSetPointerMappingReply, "decodeSetPointerMappingReply")
    val decodeTranslateCoordsReply = debug (decodeTranslateCoordsReply, "decodeTranslateCoordsReply")
    val decodeXEvent = debug (decodeXEvent, "decodeXEvent")

  end (* DebugXReply *)

structure XReply = DebugXReply

