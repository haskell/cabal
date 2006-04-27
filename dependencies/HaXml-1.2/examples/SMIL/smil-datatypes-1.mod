<!-- ...................................................................... -->
<!-- SMIL 2.0 Datatypes Module  ........................................... -->
<!-- file: smil-datatypes-1.mod

     This is SMIL 2.0.
     Copyright 1998-2000 W3C (MIT, INRIA, Keio), All Rights Reserved.
     Revision: $Id: smil-datatypes-1.mod,v 1.1.1.1 2002/03/19 12:29:24 malcolm Exp $

     This DTD module is identified by the PUBLIC and SYSTEM identifiers:

     PUBLIC "-//W3C//ENTITIES SMIL 2.0 Datatypes 1.0//EN"
     SYSTEM "smil-datatypes-1.mod"

     ....................................................................... -->

<!-- Datatypes

     defines containers for the following datatypes, many of
     these imported from other specifications and standards.
-->

<!ENTITY % Character "CDATA">
    <!-- a single character from [ISO10646] -->
<!ENTITY % ContentType "CDATA">
    <!-- media type, as per [RFC2045] -->
<!ENTITY % LanguageCode "NMTOKEN">
    <!-- a language code, as per [RFC1766] -->
<!ENTITY % LanguageCodes "CDATA">
    <!-- comma-separated list of language codes, as per [RFC1766] -->
<!ENTITY % Number "CDATA">
    <!-- one or more digits -->
<!ENTITY % Script "CDATA">
    <!-- script expression -->
<!ENTITY % Text "CDATA">
    <!-- used for titles etc. -->
<!ENTITY % TimeValue "CDATA">
    <!-- a Number, possibly with its dimension, or a reserved 
         word like 'indefinite' -->
<!ENTITY % URI.datatype "CDATA" >
<!ENTITY % URI "CDATA" >
    <!-- used for URI references -->
