<!-- ======================================================================= -->
<!-- SMIL Animation Module  ================================================ -->
<!-- file: SMIL-anim.mod

     This is SMIL 2.0.
     Copyright 2000 W3C (MIT, INRIA, Keio), All Rights Reserved.

        Author:     Patrick Schmitz, Ken Day, Jacco van Ossenbruggen
        Revision:   $Id: SMIL-anim.mod,v 1.1.1.1 2002/03/19 12:29:23 malcolm Exp $

     This DTD module is identified by the PUBLIC and SYSTEM identifiers:

     PUBLIC "-//W3C//ELEMENTS SMIL 2.0 Animation//EN"
     SYSTEM "SMIL-anim.mod"

     ======================================================================= -->


<!-- ============================= Dependencies ============================ -->
<!-- The integrating profile is expected to define the following entities,
     Unless the defaults provided are sufficient.
 -->

<!-- SplineAnimation.module entity:  Define as "INCLUDE" if the integrating
     profile includes the SMIL 2.0 SplineAnimation Module, "IGNORE" if not.
     The default is "IGNORE", i.e. by default SplineAnimation is not included
     in the integrating language profile.
 -->
<!ENTITY % SplineAnimation.module "IGNORE">

<!-- Animation depends on SMIL Timing, importing the attributes listed
     in the AnimationTime.attrib entity.  If the integrating profile does 
     include the MinMaxTiming.module, its default value includes the 
     attributes defined in BasicInlineTiming.attrib and inMaxTiming.attrib.
     Otherwise, it is defaulted to BasicInlineTiming.attrib, which is the 
     minimum requirement.
     
     Note that the profile can override these defaults by redefining 
     AnimationTime.attrib.  The profile is also expected to define 
     Fill.attrib.
 -->
<!ENTITY % MinMaxTiming.module "IGNORE">
<![%MinMaxTiming.module;[
  <!ENTITY % AnimationTime.attrib "
	%BasicInlineTiming.attrib;
	%MinMaxTiming.attrib;
  ">
]]>
<!ENTITY % AnimationTime.attrib "%BasicInlineTiming.attrib;">
<!ENTITY % Fill.attrib "">

<!ENTITY % animTimingAttrs "
  %AnimationTime.attrib;             
  %Fill.attrib;            
">

<!-- Language Designer chooses to integrate targetElement or xlink attributes.
     To integrate the targetElement attribute, define the entity
     animation-targetElement as "INCLUDE"; to integrate the XLink attributes,
     define animation-XLinkTarget as "INCLUDE".
     
     One or the other MUST be defined.  It is strongly recommended that only one
     of the two be defined.
-->

<!ENTITY % animation-targetElement "IGNORE">
<![%animation-targetElement;[
  <!ENTITY % animTargetElementAttr
   "targetElement  IDREF  #IMPLIED"
  >
]]>
<!ENTITY % animTargetElementAttr "">

<!ENTITY % animation-XLinkTarget "IGNORE">
<![%animation-XLinkTarget;[
  <!ENTITY % animTargetElementXLink "
    actuate        (onRequest|onLoad)                  'onLoad' 
    href           %URI;                               #IMPLIED
    show           (new | embed | replace)             #FIXED 'embed'
    type           (simple | extended | locator | arc) #FIXED 'simple'
">
]]>
<!ENTITY % animTargetElementXLink "">


<!-- ========================== Attribute Groups =========================== -->

<!-- All animation elements include these attributes -->
<!ENTITY % animAttrsCommon
 "%Core.attrib;
  %I18n.attrib;
  %System.attrib;
  %animTimingAttrs;
  %animTargetElementAttr;
  %animTargetElementXLink;"
>

<!-- All except animateMotion need an identified target attribute -->
<!ENTITY % animAttrsNamedTarget
 "%animAttrsCommon;
  attributeName  CDATA  #REQUIRED
  attributeType  CDATA  #IMPLIED"
>

<!-- All except set support the full animation-function specification,
     additive and cumulative animation.
     SplineAnimation adds the attributes keyTimes, keySplines and path,
	 and the calcMode value "spline", to those of BasicAnimation.
 -->
<![%SplineAnimation.module;[
  <!ENTITY % splineAnimCalcModeValues "| spline">
  <!ENTITY % splineAnimValueAttrs
   "keyTimes CDATA #IMPLIED
    keySplines CDATA #IMPLIED"
  >
  <!ENTITY % splineAnimPathAttr
   "path CDATA #IMPLIED"
  >
]]>
<!ENTITY % splineAnimCalcModeValues "">
<!ENTITY % splineAnimValueAttrs "">
<!ENTITY % splineAnimPathAttr "">

<!ENTITY % animValueAttrs "
  %BasicAnimation.attrib;
  calcMode   (discrete|linear|paced %splineAnimCalcModeValues;) 'linear'
  %splineAnimValueAttrs;
  additive   (replace | sum) 'replace'
  accumulate (none | sum) 'none'"
>


<!-- ========================== Animation Elements ========================= -->

<!ENTITY % animate.attrib  "">
<!ENTITY % animate.content "EMPTY">
<!ENTITY % animate.qname   "animate">
<!ELEMENT %animate.qname; %animate.content;>
<!ATTLIST %animate.qname; %animate.attrib;
  %animAttrsNamedTarget;
  %animValueAttrs;
>

<!ENTITY % set.attrib  "">
<!ENTITY % set.content "EMPTY">
<!ENTITY % set.qname   "set">
<!ELEMENT %set.qname; %set.content;>
<!ATTLIST %set.qname; %set.attrib;
  %animAttrsNamedTarget;
  to  CDATA  #IMPLIED
>

<!ENTITY % animateMotion.attrib  "">
<!ENTITY % animateMotion.content "EMPTY">
<!ENTITY % animateMotion.qname   "animateMotion">
<!ELEMENT %animateMotion.qname; %animateMotion.content;>
<!ATTLIST %animateMotion.qname; %animateMotion.attrib;
  %animAttrsCommon;
  %animValueAttrs;
  %splineAnimPathAttr;
  origin  (default)  "default"
>


<!ENTITY % animateColor.attrib  "">
<!ENTITY % animateColor.content "EMPTY">
<!ENTITY % animateColor.qname   "animateColor">
<!ELEMENT %animateColor.qname; %animateColor.content;>
<!ATTLIST %animateColor.qname; %animateColor.attrib;
  %animAttrsNamedTarget;
  %animValueAttrs;
>

<!-- ========================== End Animation ============================= -->
<!-- end of SMIL-anim.mod -->
