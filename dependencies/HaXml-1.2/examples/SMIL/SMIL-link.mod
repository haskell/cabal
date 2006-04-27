<!-- ======================================================================= -->
<!-- SMIL Linking Module  ================================================== -->
<!-- file: SMIL-link.mod

     This is SMIL 2.0.
     Copyright 2000 W3C (MIT, INRIA, Keio), All Rights Reserved.

        Author:     Jacco van Ossenbruggen, Lloyd Rutledge, Aaron Cohen
        Revision:   $Id: SMIL-link.mod,v 1.1.1.1 2002/03/19 12:29:24 malcolm Exp $

     This DTD module is identified by the PUBLIC and SYSTEM identifiers:

     PUBLIC "-//W3C//ELEMENTS SMIL 2.0 Linking//EN"
     SYSTEM "SMIL-link.mod"

     ======================================================================= -->

<!-- ======================== LinkingAttributes Entities =================== -->
<!ENTITY % linking-attrs "
	sourceLevel             CDATA               '100&#37;'
	destinationLevel        CDATA               '100&#37;'
	sourcePlaystate         (play|pause|stop)   #IMPLIED
	destinationPlaystate    (play|pause|stop)   'play'
	show                    (new|pause|replace) 'replace'
	accesskey               CDATA               #IMPLIED
	tabindex                CDATA               #IMPLIED
	target                  CDATA               #IMPLIED
	external                (true|false)        'false'
	actuate                 (onRequest|onLoad)  'onRequest'
">



<!-- ========================= BasicLinking Elements ======================= -->
<!ENTITY % BasicLinking.module "IGNORE">
<![%BasicLinking.module;[

  <!-- ======================= BasicLinking Entities ======================= -->
  <!ENTITY % Shape "(rect|circle|poly|default)">
  <!ENTITY % Coords "CDATA">
    <!-- comma separated list of lengths -->

  <!ENTITY % a.attrib  "">
  <!ENTITY % a.content "EMPTY">
  <!ENTITY % a.qname   "a">
  <!ELEMENT %a.qname; %a.content;>
  <!ATTLIST %a.qname; %a.attrib;
    %linking-attrs;
    href                      %URI;               #IMPLIED
    %Core.attrib;
    %I18n.attrib;
  >

  <!ENTITY % area.attrib  "">
  <!ENTITY % area.content "EMPTY">
  <!ENTITY % area.qname   "area">
  <!ELEMENT %area.qname; %area.content;>
  <!ATTLIST %area.qname; %area.attrib;
    %linking-attrs;
    shape                     %Shape;             'rect'
    coords                    %Coords;            #IMPLIED
    href                      %URI;               #IMPLIED
    nohref                    (nohref)            #IMPLIED
    alt                       %Text;              #IMPLIED
    %Core.attrib;
    %I18n.attrib;
  >

  <!ENTITY % anchor.attrib  "">
  <!ENTITY % anchor.content "EMPTY">
  <!ENTITY % anchor.qname  "anchor">
  <!ELEMENT %anchor.qname; %anchor.content;>
  <!ATTLIST %anchor.qname; %area.attrib;
    %linking-attrs;
    shape                     %Shape;             'rect'
    coords                    %Coords;            #IMPLIED
    href                      %URI;               #IMPLIED
    nohref                    (nohref)            #IMPLIED
    alt                       %Text;              #IMPLIED
    %Core.attrib;
    %I18n.attrib;
  >
]]> <!-- end of BasicLinking -->

<!-- ======================== ObjectLinking ================================ -->
<!ENTITY % ObjectLinking.module "IGNORE">
<![%ObjectLinking.module;[

  <!ENTITY % Fragment "
    fragment                  CDATA               #IMPLIED
  ">

  <!-- ====================== ObjectLinking Elements ======================= -->
  <!-- add fragment attribute to area, and anchor elements -->
  <!ATTLIST %area.qname;
      %Fragment;
  >

  <!ATTLIST %anchor.qname;
      %Fragment;
  >
]]>
<!-- ======================== End ObjectLinking ============================ -->

<!-- end of SMIL-link.mod -->
