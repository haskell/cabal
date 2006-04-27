<!-- ================================================================= -->
<!-- SMIL Timing and Synchronization Modules ========================= -->
<!-- file: SMIL-timing.mod

     This is SMIL 2.0.
     Copyright 2000 W3C (MIT, INRIA, Keio), All Rights Reserved.

     Author:     Jacco van Ossenbruggen.
     Revision:   $Id: SMIL-timing.mod,v 1.1.1.1 2002/03/19 12:29:24 malcolm Exp $

     This DTD module is identified by the PUBLIC and SYSTEM identifiers:

     PUBLIC "-//W3C//ELEMENTS SMIL 2.0 Timing//EN"
     SYSTEM "SMIL-timing.mod"

     ================================================================= -->


<!-- ================== Timing Elements ============================== -->

<!ENTITY % BasicTimeContainers.module "IGNORE">
<![%BasicTimeContainers.module;[
  <!ENTITY % par.content "EMPTY">
  <!ENTITY % seq.content "EMPTY">
  <!ENTITY % par.attrib  "">
  <!ENTITY % seq.attrib  "">
  <!ENTITY % seq.qname   "seq">
  <!ENTITY % par.qname   "par">

  <!ENTITY % description.attrib "
        abstract        CDATA   #IMPLIED
        author          CDATA   #IMPLIED
        copyright       CDATA   #IMPLIED
  ">

  <!ELEMENT %seq.qname; %seq.content;>
  <!ATTLIST %seq.qname; %seq.attrib;
   %Core.attrib;
   %I18n.attrib;
   %description.attrib;
  >

  <!ELEMENT %par.qname; %par.content;>
  <!ATTLIST %par.qname; %par.attrib;
   %Core.attrib;
   %I18n.attrib;
   %description.attrib;
  >
]]>  <!-- End of BasicTimeContainers.module -->


<!ENTITY % ExclTimeContainers.module "IGNORE">
<![%ExclTimeContainers.module;[
  <!ENTITY % excl.content          "EMPTY">
  <!ENTITY % priorityClass.content "EMPTY">
  <!ENTITY % excl.attrib           "">
  <!ENTITY % priorityClass.attrib  "">
  <!ENTITY % excl.qname            "excl">
  <!ENTITY % priorityClass.qname   "priorityClass">

  <!ELEMENT %excl.qname; %excl.content;>
  <!ATTLIST %excl.qname; %excl.attrib;
   %Core.attrib;
   %I18n.attrib;
   %description.attrib;
  >

  <!ELEMENT %priorityClass.qname; %priorityClass.content;>
  <!ATTLIST %priorityClass.qname; %priorityClass.attrib;
    peers	(stop|pause|defer|never) "stop"
    higher      (stop|pause)             "pause" 
    lower       (defer|never)            "defer"
    pauseDisplay (disable|hide|show )    "show"
    %description.attrib;
    %Core.attrib;
    %I18n.attrib;
  >
]]>  <!-- End of ExclTimeContainers.module -->

<!-- end of SMIL-timing.mod -->
