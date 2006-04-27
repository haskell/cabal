<!-- ======================================================================= -->
<!-- SMIL 2.0 Media Objects Modules ======================================== -->
<!-- file: SMIL-media.mod

     This is SMIL 2.0.
     Copyright 2000 W3C (MIT, INRIA, Keio), All Rights Reserved.

     Author:     Rob Lanphier, Jacco van Ossenbruggen
     Revision:   $Id: SMIL-media.mod,v 1.1.1.1 2002/03/19 12:29:24 malcolm Exp $

     This DTD module is identified by the PUBLIC and SYSTEM identifiers:

     PUBLIC "-//W3C//ELEMENTS SMIL 2.0 Media Objects//EN"
     SYSTEM "SMIL-media.mod"

     ======================================================================= -->

<!-- ================== Profiling Entities ================================= -->

<!ENTITY % BasicMedia.module "INCLUDE">
<![%BasicMedia.module;[
  <!ENTITY % media-object.content "EMPTY">
  <!ENTITY % media-object.attrib "">

  <!-- ================ Media Objects Entities ============================= -->

  <!ENTITY % mo-attributes-BasicMedia "
        abstract        CDATA   #IMPLIED
        alt             CDATA   #IMPLIED
        author          CDATA   #IMPLIED
        copyright       CDATA   #IMPLIED
        longdesc        CDATA   #IMPLIED
        src             CDATA   #IMPLIED
        type            CDATA   #IMPLIED
  ">
]]>
<!ENTITY % mo-attributes-BasicMedia "">


<!ENTITY % MediaClipping.module "IGNORE">
<![%MediaClipping.module;[
  <!ENTITY % mo-attributes-MediaClipping "
        clipBegin      CDATA   #IMPLIED
        clipEnd        CDATA   #IMPLIED
  ">
]]>
<!ENTITY % mo-attributes-MediaClipping "">

<!ENTITY % MediaClipping.deprecated.module "IGNORE">
<![%MediaClipping.module;[
  <!ENTITY % mo-attributes-MediaClipping-deprecated "
       clip-begin      CDATA   #IMPLIED
        clip-end        CDATA   #IMPLIED
  ">
  ]]>
<!ENTITY % mo-attributes-MediaClipping-deprecated "">

<!ENTITY % MediaParam.module "IGNORE">
<![%MediaParam.module;[
  <!ENTITY % mo-attributes-MediaParam "
        erase        (whenDone|never)	'whenDone'
        mediaRepeat  (preserve|strip)	'preserve'
  ">
]]>
<!ENTITY % mo-attributes-MediaParam "">

<!ENTITY % MediaAccessibility.module "IGNORE">
<![%MediaAccessibility.module;[
  <!ENTITY % mo-attributes-MediaAccessibility "
        readIndex    CDATA           #IMPLIED
  ">
]]>
<!ENTITY % mo-attributes-MediaAccessibility "">


<!ENTITY % mo-attributes "
        %Core.attrib;
        %I18n.attrib;
        %mo-attributes-BasicMedia;
        %mo-attributes-MediaParam;
        %mo-attributes-MediaAccessibility;
        %media-object.attrib;
">

<!--
     Most info is in the attributes, media objects are empty or
     have children defined at the language integration level:
-->

<!ENTITY % mo-content "%media-object.content;">

<!-- ================== Media Objects Elements ============================= -->
<!-- BasicMedia -->
<!ENTITY % ref.qname        "ref">
<!ENTITY % audio.qname      "audio">
<!ENTITY % img.qname        "img">
<!ENTITY % video.qname      "video">
<!ENTITY % text.qname       "text">
<!ENTITY % textstream.qname "textstream">
<!ENTITY % animation.qname  "animation">

<!ENTITY % ref.content        "%mo-content;">
<!ENTITY % audio.content      "%mo-content;">
<!ENTITY % img.content        "%mo-content;">
<!ENTITY % video.content      "%mo-content;">
<!ENTITY % text.content       "%mo-content;">
<!ENTITY % textstream.content "%mo-content;">
<!ENTITY % animation.content  "%mo-content;">

<!ELEMENT %ref.qname;           %ref.content;>
<!ELEMENT %audio.qname;         %audio.content;>
<!ELEMENT %img.qname;           %img.content;>
<!ELEMENT %video.qname;         %video.content;>
<!ELEMENT %text.qname;          %text.content;>
<!ELEMENT %textstream.qname;    %textstream.content;>
<!ELEMENT %animation.qname;     %animation.content;>

<!ATTLIST %img.qname;           
	%mo-attributes;
>
<!ATTLIST %text.qname;          
	%mo-attributes;
>
<!ATTLIST %ref.qname;           
        %mo-attributes-MediaClipping;
        %mo-attributes-MediaClipping-deprecated;
	%mo-attributes;
>
<!ATTLIST %audio.qname;         
        %mo-attributes-MediaClipping;
        %mo-attributes-MediaClipping-deprecated;
	%mo-attributes;
>
<!ATTLIST %video.qname;         
        %mo-attributes-MediaClipping;
        %mo-attributes-MediaClipping-deprecated;
	%mo-attributes;
>
<!ATTLIST %textstream.qname;    
        %mo-attributes-MediaClipping;
        %mo-attributes-MediaClipping-deprecated;
	%mo-attributes;
>
<!ATTLIST %animation.qname;     
        %mo-attributes-MediaClipping;
        %mo-attributes-MediaClipping-deprecated;
	%mo-attributes;
>

<!-- MediaParam -->
<![%MediaParam.module;[

  <!ENTITY % param.qname "param">
  <!ELEMENT %param.qname; EMPTY>

  <!ATTLIST %param.qname;
    %Core.attrib;
    %I18n.attrib;
    name        CDATA          #IMPLIED
    value       CDATA          #IMPLIED
    valuetype   (data|ref|object) "data"
    type        %ContentType;  #IMPLIED
  >
]]>

<!-- BrushMedia -->
<!ENTITY % BrushMedia.module "IGNORE">
<![%BrushMedia.module;[
  <!ENTITY % brush.attrib "">
  <!ENTITY % brush.content "%mo-content;">
  <!ENTITY % brush.qname "brush">
  <!ELEMENT %brush.qname; %brush.content;>
  <!ATTLIST %brush.qname; %brush.attrib; 
	%mo-attributes; 
        color        CDATA           #IMPLIED
  >
]]>

<!-- end of SMIL-media.mod -->
