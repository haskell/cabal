<!-- ================================================================= -->
<!-- SMIL Content Control Module  ==================================== -->
<!-- file: SMIL-control.mod

     This is SMIL 2.0.
     Copyright 2000 W3C (MIT, INRIA, Keio), All Rights Reserved.

     Author:     Jacco van Ossenbruggen, Aaron Cohen
     Revision:   $Id: SMIL-control.mod,v 1.1.1.1 2002/03/19 12:29:23 malcolm Exp $

     This DTD module is identified by the PUBLIC and SYSTEM identifiers:

     PUBLIC "-//W3C//ELEMENTS SMIL 2.0 Content Control//EN"
     SYSTEM "SMIL-control.mod"

     ================================================================= -->

<!ENTITY %  BasicContentControl.module "INCLUDE">
<![%BasicContentControl.module;[
  <!ENTITY % switch.attrib "">
  <!ENTITY % switch.content "EMPTY">
  <!ENTITY % switch.qname "switch">
  
  <!ELEMENT %switch.qname; %switch.content;>
  <!ATTLIST %switch.qname; %switch.attrib;
        %Core.attrib;
        %I18n.attrib;
  >
]]>

<!-- ========================= CustomTest Elements ========================= -->
<!ENTITY %  CustomTestAttributes.module "IGNORE">
<![%CustomTestAttributes.module;[

  <!ENTITY % customTest.attrib "">
  <!ENTITY % customTest.qname "customTest">
  <!ENTITY % customTest.content "EMPTY">
  <!ELEMENT %customTest.qname; %customTest.content;>
  <!ATTLIST %customTest.qname; %customTest.attrib;
	defaultState		(true|false)	      'false'
	override		(allowed|not-allowed) 'not-allowed'
	uid			%URI;                 #IMPLIED
        %Core.attrib;
        %I18n.attrib;
  >
  <!ENTITY % customAttributes.attrib "">
  <!ENTITY % customAttributes.qname "customAttributes">
  <!ENTITY % customAttributes.content "EMPTY">
  <!ELEMENT %customAttributes.qname; %customAttributes.content;>
  <!ATTLIST %customAttributes.qname; %customAttributes.attrib;
        %Core.attrib;
        %I18n.attrib;
  >

]]> <!-- end of CustomTestAttributes -->

<!-- ========================= PrefetchControl Elements ==================== -->
<!ENTITY % PrefetchControl.module "IGNORE">
<![%PrefetchControl.module;[
  <!ENTITY % prefetch.attrib "">
  <!ENTITY % prefetch.qname "prefetch">
  <!ENTITY % prefetch.content "EMPTY">
  <!ELEMENT %prefetch.qname; %prefetch.content;>
  <!ATTLIST %prefetch.qname; %prefetch.attrib;
	mediaSize		CDATA		#IMPLIED
	mediaTime		CDATA		#IMPLIED
	bandwidth		CDATA		#IMPLIED
        %Core.attrib;
        %I18n.attrib;
  >
]]>
