<!-- ...................................................................... -->
<!-- SMIL 2.0 Common Attributes Module  ................................... -->
<!-- file: smil-attribs-1.mod

     This is SMIL 2.0.
     Copyright 1998-2000 W3C (MIT, INRIA, Keio), All Rights Reserved.
     Revision: $Id: smil-attribs-1.mod,v 1.1.1.1 2002/03/19 12:29:24 malcolm Exp $

     This DTD module is identified by the PUBLIC and SYSTEM identifiers:

     PUBLIC "-//W3C//ENTITIES SMIL 2.0 Common Attributes 1.0//EN"
     SYSTEM "smil-attribs-1.mod"

     ...................................................................... -->

<!-- Common Attributes

     This module declares the common attributes for the SMIL DTD Modules.
-->

<!ENTITY % SMIL.pfx "">

<!ENTITY % Id.attrib
 "%SMIL.pfx;id           ID                       #IMPLIED"
>

<!ENTITY % Class.attrib
 "%SMIL.pfx;class        CDATA                    #IMPLIED"
>

<!ENTITY % Title.attrib
 "%SMIL.pfx;title        %Text;                   #IMPLIED"
>

<!ENTITY % Core.extra.attrib "" >

<!ENTITY % Core.attrib
 "%Id.attrib;
  %Class.attrib;
  %Title.attrib;
  %Core.extra.attrib;"
>

<!ENTITY % I18n.extra.attrib "" >
<!ENTITY % I18n.attrib "
  xml:lang %LanguageCode; #IMPLIED
  %I18n.extra.attrib;"
>

<!-- ================== BasicLayout ======================================= -->
<!ENTITY % Region.attrib "
 %SMIL.pfx;region         CDATA #IMPLIED
">

<!ENTITY % Fill.attrib "
 %SMIL.pfx;fill         (remove|freeze|hold|transition) #IMPLIED
">

<!-- ================== HierarchicalLayout ======================================= -->
<!ENTITY % BackgroundColor.attrib "
 %SMIL.pfx;backgroundColor     CDATA    #IMPLIED
">
<!ENTITY % BackgroundColor-deprecated.attrib "
 %SMIL.pfx;background-color     CDATA    #IMPLIED
">

<!ENTITY % Sub-region.attrib "
 %SMIL.pfx;top     CDATA    'auto'
 %SMIL.pfx;bottom  CDATA    'auto'
 %SMIL.pfx;left    CDATA    'auto'
 %SMIL.pfx;right   CDATA    'auto'
">

<!ENTITY % Fit.attrib "
 %SMIL.pfx;fit            (hidden|fill|meet|scroll|slice)    'hidden'
">

<!-- ================ Registration Point attribute for media elements ============ -->
<!-- integrating language using HierarchicalLayout must include regPoint   -->
<!-- attribute on media elements for regPoint elements to be useful        -->

<!ENTITY % RegistrationPoint.attrib "
 %SMIL.pfx;regPoint  CDATA    #IMPLIED
 %SMIL.pfx;regAlign  (topLeft|topMid|topRight|midLeft|center|
                     midRight|bottomLeft|bottomMid|bottomRight) #IMPLIED
">

<!--=================== Content Control =======================-->
<!-- customTest Attribute -->
<!ENTITY % CustomTest.attrib "
        %SMIL.pfx;customTest              IDREF           #IMPLIED
">

<!-- ========================= SkipContentControl Module ========================= -->
<!ENTITY % skipContent.attrib "
	%SMIL.pfx;skip-content		(true|false)	'true'
">

<!-- Switch Parameter Attributes --> 

<!ENTITY % System.attrib "
        %CustomTest.attrib;
        %SMIL.pfx;systemBitrate                	CDATA		#IMPLIED
	%SMIL.pfx;systemCaptions		(on|off)	#IMPLIED
	%SMIL.pfx;systemLanguage		CDATA		#IMPLIED
	%SMIL.pfx;systemOverdubOrSubtitle	(overdub|subtitle) #IMPLIED
	%SMIL.pfx;systemRequired		NMTOKEN		#IMPLIED
	%SMIL.pfx;systemScreenSize		CDATA		#IMPLIED
	%SMIL.pfx;systemScreenDepth		CDATA		#IMPLIED
	%SMIL.pfx;systemAudioDesc		(on|off)	#IMPLIED
	%SMIL.pfx;systemOperatingSystem		NMTOKEN		#IMPLIED
	%SMIL.pfx;systemCPU			NMTOKEN		#IMPLIED
	%SMIL.pfx;systemComponent		CDATA		#IMPLIED

	%SMIL.pfx;system-bitrate		CDATA		#IMPLIED
	%SMIL.pfx;system-captions		(on|off)	#IMPLIED
	%SMIL.pfx;system-language		CDATA		#IMPLIED
	%SMIL.pfx;system-overdub-or-caption	(overdub|caption) #IMPLIED
	%SMIL.pfx;system-required		NMTOKEN		#IMPLIED
	%SMIL.pfx;system-screen-size		CDATA		#IMPLIED
	%SMIL.pfx;system-screen-depth		CDATA		#IMPLIED
">

<!-- SMIL Animation Module  ================================================ -->
<!ENTITY % BasicAnimation.attrib "
  %SMIL.pfx;values     CDATA #IMPLIED
  %SMIL.pfx;from       CDATA #IMPLIED
  %SMIL.pfx;to         CDATA #IMPLIED
  %SMIL.pfx;by         CDATA #IMPLIED
">

<!-- SMIL Timing Module  =================================================== -->
<!ENTITY % BasicInlineTiming.attrib "
  %SMIL.pfx;dur                       %TimeValue;                  #IMPLIED
  %SMIL.pfx;repeatCount               %TimeValue;                  #IMPLIED
  %SMIL.pfx;repeatDur                 %TimeValue;                  #IMPLIED
  %SMIL.pfx;begin                     %TimeValue;                  #IMPLIED
  %SMIL.pfx;end                       %TimeValue;                  #IMPLIED
">

<!ENTITY % MinMaxTiming.attrib "
  %SMIL.pfx;min                       %TimeValue;                  #IMPLIED
  %SMIL.pfx;max                       %TimeValue;                  #IMPLIED
">

<!ENTITY % BasicInlineTiming-deprecated.attrib "
  %SMIL.pfx;repeat                   %TimeValue;                   #IMPLIED
">

<!ENTITY % BasicTimeContainers.attrib "
  %SMIL.pfx;endsync                  (first|last|all|IDREF)        'last'
  %Fill.attrib;
">

<!ENTITY % TimeContainerAttributes.attrib "
  %SMIL.pfx;timeAction            CDATA                        #IMPLIED
  %SMIL.pfx;timeContainer         CDATA                        #IMPLIED
">

<!ENTITY % RestartTiming.attrib "
  %SMIL.pfx;restart               (always|whenNotActive|never) 'always'
">

<!ENTITY % RestartDefaultTiming.attrib "
  %SMIL.pfx;restartDefault        (inherit|always|never|whenNotActive) 'always'
">

<!ENTITY % SyncBehavior.attrib "
  %SMIL.pfx;syncBehavior  (canSlip|locked|independent) #IMPLIED
  %SMIL.pfx;syncTolerence %TimeValue;                  #IMPLIED
">

<!ENTITY % SyncBehaviorDefault.attrib "
  %SMIL.pfx;syncBehaviorDefault   (canSlip|locked|independent) #IMPLIED
  %SMIL.pfx;syncToleranceDefault  %TimeValue;                  #IMPLIED
">

<!ENTITY % SyncMaster.attrib "
  %SMIL.pfx;syncMaster    (true|false)                 'false'
">

<!-- ================== Time Manipulations ================================= -->
<!ENTITY % TimeManipulations.attrib "
  %SMIL.pfx;accelerate		%Number;	'0'
  %SMIL.pfx;decelerate		%Number;	'0'
  %SMIL.pfx;autoReverse         (true|false)    'false'
  %SMIL.pfx;speed		%Number;	'1.0'
">

<!-- ================== Streaming Media ==================================== -->
<!ENTITY % Streaming-media.attrib "
  %SMIL.pfx;port                  CDATA   #IMPLIED
  %SMIL.pfx;rtpformat             CDATA   #IMPLIED
  %SMIL.pfx;transport             CDATA   #IMPLIED
">

<!ENTITY % Streaming-timecontainer.attrib "
  %SMIL.pfx;control               CDATA   #IMPLIED
">

<!-- ================== Transitions Media ================================== -->
<!ENTITY % Transition.attrib "
 %SMIL.pfx;transIn                IDREF        #IMPLIED
 %SMIL.pfx;transOut               IDREF        #IMPLIED
">
