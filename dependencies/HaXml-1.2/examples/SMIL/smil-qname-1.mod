<!-- ....................................................................... -->
<!-- SMIL Qualified Names Module  .......................................... -->
<!-- file: smil-qname-1.mod

     This is SMIL.
     Copyright 1998-2000 W3C (MIT, INRIA, Keio), All Rights Reserved.
     Revision: $Id: smil-qname-1.mod,v 1.1.1.1 2002/03/19 12:29:24 malcolm Exp $ SMI

     This DTD module is identified by the PUBLIC and SYSTEM identifiers:

       PUBLIC "-//W3C//ENTITIES SMIL Qualified Names 1.0//EN"
       SYSTEM "smil-qname-1.mod"

     ....................................................................... -->

<!-- SMIL Qualified Names

     This module is contained in two parts, labeled Section 'A' and 'B':

       Section A declares parameter entities to support namespace-
       qualified names, namespace declarations, and name prefixing 
       for SMIL and extensions.
    
       Section B declares parameter entities used to provide
       namespace-qualified names for all SMIL element types:

         %animation.qname; the xmlns-qualified name for <animation>
         %video.qname;     the xmlns-qualified name for <video>
         ...

     SMIL extensions would create a module similar to this one, 
     using the '%smil-qname-extra.mod;' parameter entity to insert 
     it within Section A.  A template module suitable for this purpose 
     ('template-qname-1.mod') is included in the XHTML distribution.
-->

<!-- Section A: SMIL XML Namespace Framework :::::::::::::::::::: -->

<!-- 1. Declare the two parameter entities used to support XLink,
        first the parameter entity container for the URI used to
        identify the XLink namespace:
-->
<!ENTITY % XLINK.xmlns "http://www.w3.org/1999/xlink" >

<!-- This contains the XLink namespace declaration attribute.
-->
<!ENTITY % XLINK.xmlns.attrib
     "xmlns:xlink  %URI.datatype;           #FIXED '%XLINK.xmlns;'"
>

<!-- 2. Declare parameter entities (eg., %SMIL.xmlns;) containing 
        the namespace URI for the SMIL namespace, and any namespaces
        included by SMIL:
-->

<!ENTITY % SMIL.xmlns  "http://www.w3.org/TR/REC-smil/SMIL20" >

<!-- 3. Declare parameter entities (eg., %SMIL.prefix;) containing
        the default namespace prefix string(s) to use when prefixing 
        is enabled. This may be overridden in the DTD driver or the
        internal subset of an document instance.

     NOTE: As specified in [XMLNAMES], the namespace prefix serves 
     as a proxy for the URI reference, and is not in itself significant.
-->
<!ENTITY % SMIL.prefix  "" >

<!-- 4. Declare a %SMIL.prefixed; conditional section keyword, used
        to activate namespace prefixing. The default value should 
        inherit '%NS.prefixed;' from the DTD driver, so that unless 
        overridden, the default behaviour follows the overall DTD 
        prefixing scheme.
-->
<!ENTITY % NS.prefixed "IGNORE" >
<!ENTITY % SMIL.prefixed "%NS.prefixed;" >

<!-- 5. Declare parameter entities (eg., %SMIL.pfx;) containing the 
        colonized prefix(es) (eg., '%SMIL.prefix;:') used when 
        prefixing is active, an empty string when it is not.
-->
<![%SMIL.prefixed;[
<!ENTITY % SMIL.pfx  "%SMIL.prefix;:" >
]]>
<!ENTITY % SMIL.pfx  "" >


<!-- declare qualified name extensions here -->
<!ENTITY % smil-qname-extra.mod "" >
%smil-qname-extra.mod;

<!-- 6. The parameter entity %SMIL.xmlns.extra.attrib; may be
        redeclared to contain any non-SMIL namespace declaration 
        attributes for namespaces embedded in SMIL. The default 
        is an empty string.  XLink should be included here if used 
        in the DTD and not already included by a previously-declared 
        %*.xmlns.extra.attrib;.
-->
<!ENTITY % SMIL.xmlns.extra.attrib "" >

<!-- 7. The parameter entity %NS.prefixed.attrib; is defined to be
        the prefix for SMIL elements if any and whatever is in
		SMIL.xmlns.extra.attrib.
-->
<![%SMIL.prefixed;[
<!ENTITY % NS.prefixed.attrib
	"xmlns:%SMIL.prefix;	%URI.datatype;	#FIXED	'%SMIL.xmlns;'
	 %SMIL.xmlns.extra.attrib; " >
]]>
<!ENTITY % NS.prefixed.attrib "%SMIL.xmlns.extra.attrib;" >


<!-- Section B: SMIL Qualified Names ::::::::::::::::::::::::::::: -->

<!-- This section declares parameter entities used to provide
     namespace-qualified names for all SMIL element types.
-->

<!ENTITY % animate.qname "%SMIL.pfx;animate" >
<!ENTITY % set.qname "%SMIL.pfx;set" >
<!ENTITY % animateMotion.qname "%SMIL.pfx;animateMotion" >
<!ENTITY % animateColor.qname "%SMIL.pfx;animateColor" >

<!ENTITY % switch.qname "%SMIL.pfx;switch" >
<!ENTITY % customTest.qname "%SMIL.pfx;customTest" >
<!ENTITY % customAttributes.qname "%SMIL.pfx;customAttributes" >
<!ENTITY % prefetch.qname "%SMIL.pfx;prefetch" >

<!ENTITY % layout.qname "%SMIL.pfx;layout" >
<!ENTITY % region.qname "%SMIL.pfx;region" >
<!ENTITY % root-layout.qname "%SMIL.pfx;root-layout" >
<!ENTITY % viewport.qname "%SMIL.pfx;viewport" >
<!ENTITY % regPoint.qname "%SMIL.pfx;regPoint" >

<!ENTITY % a.qname "%SMIL.pfx;a" >
<!ENTITY % area.qname "%SMIL.pfx;area" >
<!ENTITY % anchor.qname "%SMIL.pfx;anchor" >

<!ENTITY % ref.qname "%SMIL.pfx;ref" >
<!ENTITY % audio.qname "%SMIL.pfx;audio" >
<!ENTITY % img.qname "%SMIL.pfx;img" >
<!ENTITY % video.qname "%SMIL.pfx;video" >
<!ENTITY % text.qname "%SMIL.pfx;text" >
<!ENTITY % textstream.qname "%SMIL.pfx;textstream" >
<!ENTITY % animation.qname "%SMIL.pfx;animation" >
<!ENTITY % param.qname "%SMIL.pfx;param" >
<!ENTITY % brush.qname "%SMIL.pfx;brush" >

<!ENTITY % meta.qname "%SMIL.pfx;meta" >
<!ENTITY % metadata.qname "%SMIL.pfx;metadata" >

<!ENTITY % rtpmap.qname "%SMIL.pfx;rtpmap" >

<!ENTITY % smil.qname "%SMIL.pfx;smil" >
<!ENTITY % head.qname "%SMIL.pfx;head" >
<!ENTITY % body.qname "%SMIL.pfx;body" >

<!ENTITY % seq.qname "%SMIL.pfx;seq" >
<!ENTITY % par.qname "%SMIL.pfx;par" >
<!ENTITY % excl.qname "%SMIL.pfx;excl" >

<!ENTITY % transition.qname "%SMIL.pfx;transition" >
<!ENTITY % transitionFilter.qname "%SMIL.pfx;transitionFilter" >

<!-- end of smil-qname-1.mod -->
