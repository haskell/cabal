<!-- ================================================================ -->
<!-- SMIL Metainformation Module  =================================== -->
<!-- file: SMIL-metainformation.mod

     This is SMIL 2.0.
     Copyright 2000 W3C (MIT, INRIA, Keio), All Rights Reserved.

     This module declares the meta and metadata elements types and 
     its attributes, used to provide declarative document metainformation.

     Author: Thierry Michel, Jacco van Ossenbruggen
     Revision: $Id: SMIL-metainformation.mod,v 1.1.1.1 2002/03/19 12:29:24 malcolm Exp $
   
     This DTD module is identified by the PUBLIC and SYSTEM identifiers:

     PUBLIC "-//W3C//ELEMENTS SMIL 2.0 Document Metadata//EN"
     SYSTEM "SMIL-metainformation.mod"

     ================================================================ -->


<!-- ================== Profiling Entities ========================== -->

<!ENTITY % meta.content     "EMPTY">
<!ENTITY % meta.attrib      "">
<!ENTITY % meta.qname       "meta">

<!ENTITY % metadata.content "EMPTY">
<!ENTITY % metadata.attrib  "">
<!ENTITY % metadata.qname   "metadata">

<!-- ================== meta element ================================ -->

<!ELEMENT %meta.qname; %meta.content;>
<!ATTLIST %meta.qname; %meta.attrib;
  content CDATA #IMPLIED
  name CDATA #REQUIRED        
  >

<!-- ================== metadata element ============================ -->

<!ELEMENT %metadata.qname; %metadata.content;>
<!ATTLIST %metadata.qname; %metadata.attrib;
  %Core.attrib;
  %I18n.attrib;
>

<!-- end of SMIL-metadata.mod -->
