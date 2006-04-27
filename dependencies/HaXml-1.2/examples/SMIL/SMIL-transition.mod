<!-- ====================================================================== -->
<!-- SMIL Transition Module  ============================================== -->
<!-- file: SMIL-transition.mod

     This is SMIL 2.0
     Copyright 2000 W3C (MIT, INRIA, Keio), All Rights Reserved.

     Revision:   $Id: SMIL-transition.mod,v 1.1.1.1 2002/03/19 12:29:24 malcolm Exp $

     This DTD module is identified by the PUBLIC and SYSTEM identifiers:

     PUBLIC "-//W3C//ELEMENTS SMIL 2.0 Transition//EN"
     SYSTEM "SMIL-transition.mod"

    

     ====================================================================== -->

<!ENTITY % transition-types "(barWipe|boxWipe|fourBoxWipe|barnDoorWipe|
diagonalWipe|bowTieWipe|miscDiagonalWipe|veeWipe|barnVeeWipe|zigZagWipe|
barnZigZagWipe|miscShapeWipe|triangleWipe|arrowHeadWipe|pentagonWipe|
hexagonWipe|ellipseWipe|eyeWipe|roundRectWipe|starWipe|clockWipe|
pinWheelWipe|singleSweepWipe|fanWipe|doubleFanWipe|doubleSweepWipe|
saloonDoorWipe|windshieldWipe|snakeWipe|spiralWipe|parallelSnakesWipe|
boxSnakesWipe|waterfallWipe|pushWipe|slideWipe|fade)"
>

<!ENTITY % transition-subtypes "(bottom
|bottomCenter|bottomLeft|bottomLeftClockwise|bottomLeftCounterClockwise|
bottomLeftDiagonal|bottomRight|bottomRightClockwise|
bottomRightCounterClockwise|bottomRightDiagonal|centerRight|centerTop|
circle|clockwiseBottom|clockwiseBottomRight|clockwiseLeft|clockwiseNine|
clockwiseRight|clockwiseSix|clockwiseThree|clockwiseTop|clockwiseTopLeft|
clockwiseTwelve|cornersIn|cornersOut|counterClockwiseBottomLeft|
counterClockwiseTopRight|crossfade|diagonalBottomLeft|
diagonalBottomLeftOpposite|diagonalTopLeft|diagonalTopLeftOpposite|
diamond|doubleBarnDoor|doubleDiamond|down|fadeFromColor|fadeToColor|
fanInHorizontal|fanInVertical|fanOutHorizontal|fanOutVertical|fivePoint|
fourBlade|fourBoxHorizontal|fourBoxVertical|fourPoint|fromBottom|fromLeft|
fromRight|fromTop|heart|horizontal|horizontalLeft|horizontalLeftSame|
horizontalRight|horizontalRightSame|horizontalTopLeftOpposite|
horizontalTopRightOpposite|keyhole|left|leftCenter|leftToRight|
oppositeHorizontal|oppositeVertical|parallelDiagonal|
parallelDiagonalBottomLeft|parallelDiagonalTopLeft|
parallelVertical|rectangle|right|rightCenter|sixPoint|top|topCenter|
topLeft|topLeftClockwise|topLeftCounterClockwise|topLeftDiagonal|
topLeftHorizontal|topLeftVertical|topRight|topRightClockwise|
topRightCounterClockwise|topRightDiagonal|topToBottom|twoBladeHorizontal|
twoBladeVertical|twoBoxBottom|twoBoxLeft|twoBoxRight|twoBoxTop|up|
vertical|verticalBottomLeftOpposite|verticalBottomSame|verticalLeft|
verticalRight|verticalTopLeftOpposite|verticalTopSame)"
>


<!ENTITY  % transition-attrs '
    type         %transition-types;     #IMPLIED
    subtype      %transition-subtypes;  #IMPLIED
    horzRepeat   CDATA                  "0"
    vertRepeat   CDATA                  "0"
    borderWidth  CDATA                  "0"
    borderColor  CDATA                  "black"
    fadeColor	 CDATA		        "black"
    coordinated  (true|false)           "false" 
    clibBoundary (parent|children)      "children" 
'>

<!ENTITY % transition.attrib  "">
<!ENTITY % transition.content "EMPTY">
<!ENTITY % transition.qname   "transition">
<!ELEMENT %transition.qname; %transition.content;>
<!ATTLIST %transition.qname; %transition.attrib;
    %Core.attrib;
    %I18n.attrib;
    %transition-attrs;
    dur           CDATA              #IMPLIED
    startProgress CDATA              "0.0"
    endProgress   CDATA              "1.0"
    direction     (forward|reverse)  "forward"
>

<!ENTITY % transitionFilter.attrib  "">
<!ENTITY % transitionFilter.content "EMPTY">
<!ENTITY % transitionFilter.qname   "transitionFilter">
<!ELEMENT %transitionFilter.qname; %transitionFilter.content;>
<!ATTLIST %transitionFilter.qname; %transitionFilter.attrib;
    %Core.attrib;
    %I18n.attrib;
    %transition-attrs;
    %BasicInlineTiming.attrib;
    %BasicAnimation.attrib;
    calcMode   (discrete|linear|paced) 'linear'
>
    
<!-- end of SMIL-transition.mod -->
