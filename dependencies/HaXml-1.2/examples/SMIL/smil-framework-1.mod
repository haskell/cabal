<!-- ...................................................................... -->
<!-- SMIL 2.0 Modular Framework Module  ................................... -->
<!-- file: smil-framework-1.mod

     This is SMIL 2.0.
     Copyright 1998-2000 W3C (MIT, INRIA, Keio), All Rights Reserved.

     This DTD module is identified by the PUBLIC and SYSTEM identifiers:

     PUBLIC "-//W3C//ENTITIES SMIL 2.0 Modular Framework 1.0//EN"
     SYSTEM "smil-framework-1.mod"

     Revision: $Id: smil-framework-1.mod,v 1.1.1.1 2002/03/19 12:29:24 malcolm Exp $
     ....................................................................... -->

<!-- Modular Framework

     This required module instantiates the modules needed
     to support the SMIL 2.0 modularization model, including:

        +  datatypes
        +  namespace-qualified names
        +  common attributes
        +  document model

     The Intrinsic Events module is ignored by default but
     occurs in this module because it must be instantiated
     prior to Attributes but after Datatypes.
-->

<!-- The (still to be determined) SMIL namespace: -->
<!ENTITY % SMIL.ns       "'http://www.w3.org/TR/REC-smil/SMIL20'">

<!ENTITY % smil-datatypes.module "INCLUDE" >
<![%smil-datatypes.module;[
<!ENTITY % smil-datatypes.mod
     PUBLIC "-//W3C//ENTITIES SMIL 2.0 Datatypes 1.0//EN"
            "smil-datatypes-1.mod" >
%smil-datatypes.mod;]]>

<!ENTITY % smil-qname.module "INCLUDE" >
<![%smil-qname.module;[
<!ENTITY % smil-qname.mod
     PUBLIC "-//W3C//ENTITIES SMIL 2.0 Qualified Names 1.0//EN"
            "smil-qname-1.mod" >
%smil-qname.mod;]]>

<!ENTITY % smil-events.module "IGNORE" >
<![%smil-events.module;[
<!ENTITY % smil-events.mod
     PUBLIC "-//W3C//ENTITIES SMIL 2.0 Intrinsic Events 1.0//EN"
            "smil-events-1.mod" >
%smil-events.mod;]]>

<!ENTITY % smil-attribs.module "INCLUDE" >
<![%smil-attribs.module;[
<!ENTITY % smil-attribs.mod
     PUBLIC "-//W3C//ENTITIES SMIL 2.0 Common Attributes 1.0//EN"
            "smil-attribs-1.mod" >
%smil-attribs.mod;]]>

<!ENTITY % smil-model.module "INCLUDE" >
<![%smil-model.module;[
<!-- A content model MUST be defined by the driver file -->
%smil-model.mod;]]>

<!-- end of smil-framework-1.mod -->
