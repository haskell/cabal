<!-- .................................................................... -->
<!-- SMIL 2.0 Document Model Module ..................................... -->
<!-- file: smil-model-1.mod

     This is SMIL 2.0.
     Copyright 1998-2000 W3C (MIT, INRIA, Keio), All Rights Reserved.

     This DTD module is identified by the PUBLIC and SYSTEM identifiers:

     PUBLIC "-//W3C//ENTITIES SMIL 2.0 Document Model 1.0//EN"
     SYSTEM "smil-model-1.mod"

     Author: Warner ten Kate, Jacco van Ossenbruggen, Aaron Cohen
     Revision: $Id: smil-model-1.mod,v 1.1.1.1 2002/03/19 12:29:24 malcolm Exp $
     ....................................................................... -->

<!--
        This file defines the SMIL 2.0 Language Document Model.
        All attributes and content models are defined in the second
        half of this file.  We first start with some utility definitions.
        These are mainly used to simplify the use of Modules in the
        second part of the file.

-->
<!-- ================== Util: Head ========================================= -->
<!ENTITY % head-meta.content       "metadata?,meta*">
<!ENTITY % head-layout.content     "layout|switch">
<!ENTITY % head-control.content    "customAttributes">
<!ENTITY % head-transition.content "transition+, meta*">

<!--=================== Util: Body - Content Control ======================= -->
<!ENTITY % content-control "switch|prefetch">

<!--=================== Util: Body - Animation ========================= -->
<!ENTITY % animation.elements "animate|set|animateMotion|animateColor">

<!--=================== Util: Body - Media ========================= -->

<!ENTITY % media-object "audio|video|animation|text|img|textstream|ref|brush
                               |%animation.elements;">

<!--=================== Util: Body - Timing ================================ -->
<!ENTITY % BasicTimeContainers.class "par|seq">
<!ENTITY % ExclTimeContainers.class "excl">
<!ENTITY % timecontainer.class
 "%BasicTimeContainers.class;|%ExclTimeContainers.class;">
<!ENTITY % timecontainer.content
 "%timecontainer.class;|%media-object;|%content-control;|a">

<!ENTITY % smil-time.attrib "
 %BasicInlineTiming.attrib;
 %MinMaxTiming.attrib;
 %RestartTiming.attrib;
 %SyncBehavior.attrib;
 %SyncBehaviorDefault.attrib;
 %BasicInlineTiming-deprecated.attrib;
 %Fill.attrib;
">

<!ENTITY % timecontainer.attrib "
 %BasicInlineTiming.attrib;
 %MinMaxTiming.attrib;
 %BasicTimeContainers.attrib;
 %RestartTiming.attrib;
 %SyncBehavior.attrib;
 %SyncBehaviorDefault.attrib;
 %BasicInlineTiming-deprecated.attrib;
 %System.attrib;
">

<!-- ====================================================================== -->
<!-- ====================================================================== -->
<!-- ====================================================================== -->

<!-- 
     The actual content model and attribute definitions for each module 
     sections follow below.
-->

<!-- ================== Content Control =================================== -->
<!ENTITY % BasicContentControl.module  "INCLUDE">
<!ENTITY % CustomTestAttributes.module "INCLUDE">
<!ENTITY % PrefetchControl.module      "INCLUDE">
<!ENTITY % SkipContentControl.module   "INCLUDE">

<!ENTITY % switch.content "(layout|%timecontainer.class;|%media-object;|
			   %content-control;|a)*">
<!ENTITY % prefetch.content "%switch.content;">
<!ENTITY % customAttributes.content "(customTest)+">

<!ENTITY % switch.attrib            "%System.attrib; %skipContent.attrib;">
<!ENTITY % prefetch.attrib          "%timecontainer.attrib; %skipContent.attrib; ">
<!ENTITY % customAttributes.attrib  "%skipContent.attrib;">
<!ENTITY % customTest.attrib        "%skipContent.attrib;">

<!-- ================== Animation ========================================= -->
<!ENTITY % BasicAnimation.module "INCLUDE">

<!-- choose targetElement or XLink: -->
<!ENTITY % animation-targetElement "INCLUDE">
<!ENTITY % animation-XLinkTarget   "IGNORE">

<!ENTITY % animate.content "EMPTY">
<!ENTITY % animateColor.content "EMPTY">
<!ENTITY % animateMotion.content "EMPTY">
<!ENTITY % set.content "EMPTY">

<!ENTITY % animate.attrib        "%skipContent.attrib;">
<!ENTITY % animateColor.attrib   "%skipContent.attrib;">
<!ENTITY % animateMotion.attrib  "%skipContent.attrib;">
<!ENTITY % set.attrib            "%skipContent.attrib;">

<!-- ================== Layout ============================================ -->
<!ENTITY % BasicLayout.module        "INCLUDE">
<!ENTITY % AudioLayout.module        "INCLUDE">
<!ENTITY % MultiWindowLayout.module  "INCLUDE">
<!ENTITY % HierarchicalLayout.module "INCLUDE">

<!ENTITY % layout.content "(region|viewport|root-layout|regPoint)*">
<!ENTITY % region.content "(region)*">
<!ENTITY % rootlayout.content "(region)*">
<!ENTITY % viewport.content "(region)*">
<!ENTITY % regPoint.content "EMPTY">

<!ENTITY % rootlayout.attrib      "%skipContent.attrib;">
<!ENTITY % viewport.attrib        "%skipContent.attrib;">
<!ENTITY % region.attrib          "%skipContent.attrib;">
<!ENTITY % regPoint.attrib        "%skipContent.attrib;">

<!-- ================== Linking =========================================== -->
<!ENTITY % LinkingAttributes.module "INCLUDE">
<!ENTITY % BasicLinking.module      "INCLUDE">
<!ENTITY % ObjectLinking.module   "INCLUDE">

<!ENTITY % a.content      "(%timecontainer.class;|%media-object;|
                            %content-control;)*">
<!ENTITY % area.content	  "EMPTY">
<!ENTITY % anchor.content "EMPTY">

<!ENTITY % a.attrib      "%smil-time.attrib;">
<!ENTITY % area.attrib   "%smil-time.attrib; %skipContent.attrib;"> 
<!ENTITY % anchor.attrib "%smil-time.attrib; %skipContent.attrib;"> 

<!-- ================== Media  ============================================ -->
<!ENTITY % BasicMedia.module                     "INCLUDE">
<!ENTITY % MediaClipping.module                  "INCLUDE">
<!ENTITY % MediaClipping.deperecated.module      "INCLUDE">
<!ENTITY % MediaClipMarkers.module               "INCLUDE">
<!ENTITY % MediaParam.module                     "INCLUDE">
<!ENTITY % BrushMedia.module                     "INCLUDE">
<!ENTITY % MediaAccessibility.module             "INCLUDE">

<!ENTITY % media-object.content "(%animation.elements;|anchor|area
                                  |transitionFilter|param)*">
<!ENTITY % media-object.attrib "
  %smil-time.attrib;
  %System.attrib;
  %Region.attrib;
  %Transition.attrib;
  %BackgroundColor.attrib;
  %BackgroundColor-deprecated.attrib;
  %Sub-region.attrib;
  %RegistrationPoint.attrib;
  %Fit.attrib;
">

<!ENTITY % brush.attrib        "%skipContent.attrib;">

<!-- ================== Metadata ========================================== -->
<!ENTITY % meta.content     "EMPTY">
<!ENTITY % meta.attrib      "%skipContent.attrib;">

<!ENTITY % metadata.content "EMPTY">
<!ENTITY % metadata.attrib  "%skipContent.attrib;">

<!-- ================== Structure ========================================= -->
<!ENTITY % Structure.module "INCLUDE">
<!ENTITY % smil.content "(head?,body?)">
<!ENTITY % head.content "
	((%head-meta.content;)?,
	((%head-layout.content;),meta*)?,
	 (%head-transition.content;)?,
	((%head-control.content;),meta*)?)">
<!ENTITY % body.content "(%timecontainer.class;|%media-object;|
                          %content-control;|a)*">

<!ENTITY % body.attrib "%timecontainer.attrib; %Region.attrib;">

<!-- ================== Transitions ======================================= -->
<!ENTITY % BasicTransitions.module        "INCLUDE">
<!ENTITY % MultiElementTransitions.module "INCLUDE">

<!ENTITY % transition.content "(transitionFilter*)">
<!ENTITY % transition.attrib "%skipContent.attrib;">
<!ENTITY % transitionFilter.attrib "%skipContent.attrib;">

<!-- ================== Timing ============================================ -->
<!ENTITY % BasicInlineTiming.module      "INCLUDE">
<!ENTITY % SyncbaseTiming.module         "INCLUDE">
<!ENTITY % EventTiming.module            "INCLUDE">
<!ENTITY % WallclockTiming.module        "INCLUDE">
<!ENTITY % MultiSyncArcTiming.module     "INCLUDE">
<!ENTITY % MediaMarkerTiming.module      "INCLUDE">
<!ENTITY % MinMaxTiming.module           "INCLUDE">
<!ENTITY % BasicTimeContainers.module    "INCLUDE">
<!ENTITY % ExclTimeContainers.module     "INCLUDE">
<!ENTITY % PrevTiming.module             "INCLUDE">
<!ENTITY % RestartTiming.module          "INCLUDE">
<!ENTITY % SyncBehavior.module           "INCLUDE">
<!ENTITY % SyncBehaviorDefault.module    "INCLUDE">
<!ENTITY % RestartDefault.module         "INCLUDE">
<!ENTITY % FillDefault.module            "INCLUDE">

<!ENTITY %  par.attrib "%timecontainer.attrib; %Region.attrib;">
<!ENTITY %  seq.attrib "%timecontainer.attrib; %Region.attrib;">
<!ENTITY % excl.attrib "%timecontainer.attrib; %Region.attrib; %skipContent.attrib;">

<!ENTITY %  par.content "(%timecontainer.content;)*">
<!ENTITY %  seq.content "(%timecontainer.content;)*">
<!ENTITY % excl.content "((%timecontainer.content;)*|priorityClass+)">

<!ENTITY % priorityClass.attrib  "%skipContent.attrib;">
<!ENTITY % priorityClass.content "((%timecontainer.content;)*|priorityClass+)">
