<!-- ====================================================================== -->
<!-- SMIL Structure Module  =============================================== -->
<!-- file: SMIL-struct.mod

     This is SMIL 2.0.
     Copyright 2000 W3C (MIT, INRIA, Keio), All Rights Reserved.

     This DTD module is identified by the PUBLIC and SYSTEM identifiers:

     PUBLIC "-//W3C//ELEMENTS SMIL 2.0 Document Structure//EN"
     SYSTEM "SMIL-struct.mod"

     Author: Warner ten Kate, Jacco van Ossenbruggen
     Revision: $Id: SMIL-struct.mod,v 1.1.1.1 2002/03/19 12:29:24 malcolm Exp $

     ===================================================================== -->

<!-- ================== SMIL Document Root =============================== -->
<!ENTITY % smil.attrib  "" >
<!ENTITY % smil.content "EMPTY" >
<!ENTITY % smil.qname   "smil" >

<!ELEMENT %smil.qname; %smil.content;>
<!ATTLIST %smil.qname; %smil.attrib;
        %Core.attrib;
        %I18n.attrib;
        xmlns %URI; #FIXED %SMIL.ns;
>

<!-- ================== The Document Head ================================ -->
<!ENTITY % head.content "EMPTY" >
<!ENTITY % head.attrib  "" >
<!ENTITY % head.qname   "head" >

<!ELEMENT %head.qname; %head.content;>
<!ATTLIST %head.qname; %head.attrib;
        %Core.attrib;
        %I18n.attrib;
>

<!--=================== The Document Body - Timing Root ================== -->
<!ENTITY % body.content "EMPTY" >
<!ENTITY % body.attrib  "" >
<!ENTITY % body.qname   "body" >

<!ELEMENT %body.qname; %body.content;>
<!ATTLIST %body.qname; %body.attrib;
        %Core.attrib;
        %I18n.attrib;
>
<!-- end of SMIL-struct.mod -->
