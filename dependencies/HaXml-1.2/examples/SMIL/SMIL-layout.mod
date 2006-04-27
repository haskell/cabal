<!-- ======================================================================= -->
<!-- SMIL 2.0 Layout Modules =============================================== -->
<!-- file: SMIL-layout.mod

        This is SMIL 2.0.
        Copyright 2000 W3C (MIT, INRIA, Keio), All Rights Reserved.

        Authors:  Jacco van Ossenbruggen, Aaron Cohen
        Revision: $Id: SMIL-layout.mod,v 1.1.1.1 2002/03/19 12:29:24 malcolm Exp $

        This DTD module is identified by the PUBLIC and SYSTEM identifiers:

        PUBLIC "-//W3C//ELEMENTS SMIL 2.0 Layout//EN"
        SYSTEM "SMIL-layout.mod"

        ==================================================================== -->

<!-- ================== BasicLayout ======================================== -->
<!-- ================== BasicLayout Profiling Entities ===================== -->
<!ENTITY % layout.attrib       "">
<!ENTITY % region.attrib       "">
<!ENTITY % rootlayout.attrib   "">
<!ENTITY % layout.content     "EMPTY">
<!ENTITY % region.content     "EMPTY">
<!ENTITY % rootlayout.content "EMPTY">

<!-- ================== BasicLayout Entities =============================== -->
<!ENTITY % viewport-attrs "
        height              CDATA    'auto'
        width               CDATA    'auto'
	close               (never|whenNotActive) 'never'
	open                (always|whenActive)   'always'
        %BackgroundColor.attrib;
        %BackgroundColor-deprecated.attrib;
">

<!ENTITY % region-attrs "
        bottom              CDATA    'auto'
        left                CDATA    'auto'
        right               CDATA    'auto'
        top                 CDATA    'auto'
        z-index             CDATA    #IMPLIED
	showBackground      (always|whenActive) 'always'
	%Fit.attrib;
">

<!-- ================== BasicLayout Elements =============================== -->
<!--
     Layout contains the region and root-layout elements defined by
     smil-basic-layout or other elements defined an external layout
     mechanism.
-->

<!ENTITY % layout.qname "layout">
<!ELEMENT %layout.qname; %layout.content;>
<!ATTLIST %layout.qname; %layout.attrib;
	%Core.attrib;
	%I18n.attrib;
        type CDATA 'text/smil-basic-layout'
>

<!-- ================== Region Element ======================================-->
<!ENTITY % region.qname "region">
<!ELEMENT %region.qname; %region.content;>
<!ATTLIST %region.qname; %region.attrib;
	%Core.attrib;
	%I18n.attrib;
        %viewport-attrs;
        %region-attrs;
>

<!-- ================== Root-layout Element =================================-->
<!ENTITY % root-layout.qname "root-layout">
<!ELEMENT %root-layout.qname; %rootlayout.content; >
<!ATTLIST %root-layout.qname; %rootlayout.attrib;
	%Core.attrib;
	%I18n.attrib;
        %viewport-attrs;
>


<!-- ================== AudioLayout ======================================== -->
<!ENTITY % AudioLayout.module "IGNORE">
<![%AudioLayout.module;[
  <!-- ================== AudioLayout Entities ============================= -->
  <!ENTITY % audio-attrs "
        soundLevel                        CDATA    '100&#37;'
  ">

  <!-- ================ AudioLayout Elements =============================== -->
  <!-- ================ Add soundLevel to region element =================== -->
  <!ATTLIST %region.qname; %audio-attrs;>
]]> <!-- end AudioLayout.module -->


<!-- ================ MultiWindowLayout ==================================== -->
<!ENTITY % MultiWindowLayout.module "IGNORE">
<![%MultiWindowLayout.module;[
  <!-- ============== MultiWindowLayout Profiling Entities ================= -->
  <!ENTITY % viewport.attrib    "">
  <!ENTITY % viewport.content   "EMPTY">
  
  <!-- ============== MultiWindowLayout Elements =========================== -->
  <!--================= viewport element =================================== -->
  <!ENTITY % viewport.qname "viewport">
  <!ELEMENT %viewport.qname; %viewport.content;>
  <!ATTLIST %viewport.qname; %viewport.attrib;
	%Core.attrib;
	%I18n.attrib;
        %viewport-attrs;
  >
]]> <!-- end MultiWindowLayout.module -->


<!-- ====================== HierarchicalLayout ============================= -->
<!ENTITY % HierarchicalLayout.module "IGNORE">
<![%HierarchicalLayout.module;[
  <!-- ========== HierarchicalLayout Profiling Entities ==================== -->
  <!ENTITY % regPoint.attrib     "">
  <!ENTITY % regPoint.content   "EMPTY">

  <!-- ============ HierarchicalLayout Elements ============================ -->
  <!ENTITY % regPoint.qname "regPoint">
  <!ELEMENT %regPoint.qname; %regPoint.content;>
  <!ATTLIST %regPoint.qname; %regPoint.attrib;
	%Core.attrib;
	%I18n.attrib;
        %Sub-region.attrib;
        %RegistrationPoint.attrib;
  >
]]> <!-- end HierarchicalLayout.module -->


<!-- end of SMIL-layout.mod -->
