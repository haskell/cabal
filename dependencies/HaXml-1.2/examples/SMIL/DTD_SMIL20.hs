module DTD_SMIL20 where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN


{-Type decls-}

data Smil = Smil
    { smilId :: (Maybe String)
    , smilClass :: (Maybe String)
    , smilTitle :: (Maybe String)
    , smilXml'lang :: (Maybe String)
    , smilXmlns :: (Defaultable String)
    } deriving (Eq,Show)
data Head = Head
    { headId :: (Maybe String)
    , headClass :: (Maybe String)
    , headTitle :: (Maybe String)
    , headXml'lang :: (Maybe String)
    } deriving (Eq,Show)
data Body = Body
    { bodyId :: (Maybe String)
    , bodyClass :: (Maybe String)
    , bodyTitle :: (Maybe String)
    , bodyXml'lang :: (Maybe String)
    } deriving (Eq,Show)
data Animate = Animate
    { animateId :: (Maybe String)
    , animateClass :: (Maybe String)
    , animateTitle :: (Maybe String)
    , animateXml'lang :: (Maybe String)
    , animateCustomTest :: (Maybe String)
    , animateSystemBitrate :: (Maybe String)
    , animateSystemCaptions :: (Maybe Animate_SystemCaptions)
    , animateSystemLanguage :: (Maybe String)
    , animateSystemOverdubOrSubtitle :: (Maybe Animate_SystemOverdubOrSubtitle)
    , animateSystemRequired :: (Maybe String)
    , animateSystemScreenSize :: (Maybe String)
    , animateSystemScreenDepth :: (Maybe String)
    , animateSystemAudioDesc :: (Maybe Animate_SystemAudioDesc)
    , animateSystemOperatingSystem :: (Maybe String)
    , animateSystemCPU :: (Maybe String)
    , animateSystemComponent :: (Maybe String)
    , animateSystem_bitrate :: (Maybe String)
    , animateSystem_captions :: (Maybe Animate_System_captions)
    , animateSystem_language :: (Maybe String)
    , animateSystem_overdub_or_caption :: (Maybe Animate_System_overdub_or_caption)
    , animateSystem_required :: (Maybe String)
    , animateSystem_screen_size :: (Maybe String)
    , animateSystem_screen_depth :: (Maybe String)
    , animateDur :: (Maybe String)
    , animateRepeatCount :: (Maybe String)
    , animateRepeatDur :: (Maybe String)
    , animateBegin :: (Maybe String)
    , animateEnd :: (Maybe String)
    , animateAttributeName :: String
    , animateAttributeType :: (Maybe String)
    , animateValues :: (Maybe String)
    , animateFrom :: (Maybe String)
    , animateTo :: (Maybe String)
    , animateBy :: (Maybe String)
    , animateCalcMode :: (Defaultable Animate_CalcMode)
    , animateAdditive :: (Defaultable Animate_Additive)
    , animateAccumulate :: (Defaultable Animate_Accumulate)
    } deriving (Eq,Show)
data Animate_SystemCaptions = Animate_SystemCaptions_On  | 
			      Animate_SystemCaptions_Off
			    deriving (Eq,Show)
data Animate_SystemOverdubOrSubtitle = Animate_SystemOverdubOrSubtitle_Overdub
				        |  Animate_SystemOverdubOrSubtitle_Subtitle
				     deriving (Eq,Show)
data Animate_SystemAudioDesc = Animate_SystemAudioDesc_On  | 
			       Animate_SystemAudioDesc_Off
			     deriving (Eq,Show)
data Animate_System_captions = Animate_System_captions_On  | 
			       Animate_System_captions_Off
			     deriving (Eq,Show)
data Animate_System_overdub_or_caption = Animate_System_overdub_or_caption_Overdub
					  |  Animate_System_overdub_or_caption_Caption
				       deriving (Eq,Show)
data Animate_CalcMode = Animate_CalcMode_Discrete  | 
			Animate_CalcMode_Linear  |  Animate_CalcMode_Paced
		      deriving (Eq,Show)
data Animate_Additive = Animate_Additive_Replace  | 
			Animate_Additive_Sum
		      deriving (Eq,Show)
data Animate_Accumulate = Animate_Accumulate_None  | 
			  Animate_Accumulate_Sum
			deriving (Eq,Show)
data Set = Set
    { setId :: (Maybe String)
    , setClass :: (Maybe String)
    , setTitle :: (Maybe String)
    , setXml'lang :: (Maybe String)
    , setCustomTest :: (Maybe String)
    , setSystemBitrate :: (Maybe String)
    , setSystemCaptions :: (Maybe Set_SystemCaptions)
    , setSystemLanguage :: (Maybe String)
    , setSystemOverdubOrSubtitle :: (Maybe Set_SystemOverdubOrSubtitle)
    , setSystemRequired :: (Maybe String)
    , setSystemScreenSize :: (Maybe String)
    , setSystemScreenDepth :: (Maybe String)
    , setSystemAudioDesc :: (Maybe Set_SystemAudioDesc)
    , setSystemOperatingSystem :: (Maybe String)
    , setSystemCPU :: (Maybe String)
    , setSystemComponent :: (Maybe String)
    , setSystem_bitrate :: (Maybe String)
    , setSystem_captions :: (Maybe Set_System_captions)
    , setSystem_language :: (Maybe String)
    , setSystem_overdub_or_caption :: (Maybe Set_System_overdub_or_caption)
    , setSystem_required :: (Maybe String)
    , setSystem_screen_size :: (Maybe String)
    , setSystem_screen_depth :: (Maybe String)
    , setDur :: (Maybe String)
    , setRepeatCount :: (Maybe String)
    , setRepeatDur :: (Maybe String)
    , setBegin :: (Maybe String)
    , setEnd :: (Maybe String)
    , setAttributeName :: String
    , setAttributeType :: (Maybe String)
    , setTo :: (Maybe String)
    } deriving (Eq,Show)
data Set_SystemCaptions = Set_SystemCaptions_On  | 
			  Set_SystemCaptions_Off
			deriving (Eq,Show)
data Set_SystemOverdubOrSubtitle = Set_SystemOverdubOrSubtitle_Overdub
				    |  Set_SystemOverdubOrSubtitle_Subtitle
				 deriving (Eq,Show)
data Set_SystemAudioDesc = Set_SystemAudioDesc_On  | 
			   Set_SystemAudioDesc_Off
			 deriving (Eq,Show)
data Set_System_captions = Set_System_captions_On  | 
			   Set_System_captions_Off
			 deriving (Eq,Show)
data Set_System_overdub_or_caption = Set_System_overdub_or_caption_Overdub
				      |  Set_System_overdub_or_caption_Caption
				   deriving (Eq,Show)
data AnimateMotion = AnimateMotion
    { animateMotionId :: (Maybe String)
    , animateMotionClass :: (Maybe String)
    , animateMotionTitle :: (Maybe String)
    , animateMotionXml'lang :: (Maybe String)
    , animateMotionCustomTest :: (Maybe String)
    , animateMotionSystemBitrate :: (Maybe String)
    , animateMotionSystemCaptions :: (Maybe AnimateMotion_SystemCaptions)
    , animateMotionSystemLanguage :: (Maybe String)
    , animateMotionSystemOverdubOrSubtitle :: (Maybe AnimateMotion_SystemOverdubOrSubtitle)
    , animateMotionSystemRequired :: (Maybe String)
    , animateMotionSystemScreenSize :: (Maybe String)
    , animateMotionSystemScreenDepth :: (Maybe String)
    , animateMotionSystemAudioDesc :: (Maybe AnimateMotion_SystemAudioDesc)
    , animateMotionSystemOperatingSystem :: (Maybe String)
    , animateMotionSystemCPU :: (Maybe String)
    , animateMotionSystemComponent :: (Maybe String)
    , animateMotionSystem_bitrate :: (Maybe String)
    , animateMotionSystem_captions :: (Maybe AnimateMotion_System_captions)
    , animateMotionSystem_language :: (Maybe String)
    , animateMotionSystem_overdub_or_caption :: (Maybe AnimateMotion_System_overdub_or_caption)
    , animateMotionSystem_required :: (Maybe String)
    , animateMotionSystem_screen_size :: (Maybe String)
    , animateMotionSystem_screen_depth :: (Maybe String)
    , animateMotionDur :: (Maybe String)
    , animateMotionRepeatCount :: (Maybe String)
    , animateMotionRepeatDur :: (Maybe String)
    , animateMotionBegin :: (Maybe String)
    , animateMotionEnd :: (Maybe String)
    , animateMotionValues :: (Maybe String)
    , animateMotionFrom :: (Maybe String)
    , animateMotionTo :: (Maybe String)
    , animateMotionBy :: (Maybe String)
    , animateMotionCalcMode :: (Defaultable AnimateMotion_CalcMode)
    , animateMotionAdditive :: (Defaultable AnimateMotion_Additive)
    , animateMotionAccumulate :: (Defaultable AnimateMotion_Accumulate)
    , animateMotionOrigin :: (Defaultable AnimateMotion_Origin)
    } deriving (Eq,Show)
data AnimateMotion_SystemCaptions = AnimateMotion_SystemCaptions_On
				     |  AnimateMotion_SystemCaptions_Off
				  deriving (Eq,Show)
data AnimateMotion_SystemOverdubOrSubtitle = AnimateMotion_SystemOverdubOrSubtitle_Overdub
					      |  AnimateMotion_SystemOverdubOrSubtitle_Subtitle
					   deriving (Eq,Show)
data AnimateMotion_SystemAudioDesc = AnimateMotion_SystemAudioDesc_On
				      |  AnimateMotion_SystemAudioDesc_Off
				   deriving (Eq,Show)
data AnimateMotion_System_captions = AnimateMotion_System_captions_On
				      |  AnimateMotion_System_captions_Off
				   deriving (Eq,Show)
data AnimateMotion_System_overdub_or_caption = AnimateMotion_System_overdub_or_caption_Overdub
					        |  AnimateMotion_System_overdub_or_caption_Caption
					     deriving (Eq,Show)
data AnimateMotion_CalcMode = AnimateMotion_CalcMode_Discrete  | 
			      AnimateMotion_CalcMode_Linear  |  AnimateMotion_CalcMode_Paced
			    deriving (Eq,Show)
data AnimateMotion_Additive = AnimateMotion_Additive_Replace  | 
			      AnimateMotion_Additive_Sum
			    deriving (Eq,Show)
data AnimateMotion_Accumulate = AnimateMotion_Accumulate_None  | 
				AnimateMotion_Accumulate_Sum
			      deriving (Eq,Show)
data AnimateMotion_Origin = AnimateMotion_Origin_Default
			  deriving (Eq,Show)
data AnimateColor = AnimateColor
    { animateColorId :: (Maybe String)
    , animateColorClass :: (Maybe String)
    , animateColorTitle :: (Maybe String)
    , animateColorXml'lang :: (Maybe String)
    , animateColorCustomTest :: (Maybe String)
    , animateColorSystemBitrate :: (Maybe String)
    , animateColorSystemCaptions :: (Maybe AnimateColor_SystemCaptions)
    , animateColorSystemLanguage :: (Maybe String)
    , animateColorSystemOverdubOrSubtitle :: (Maybe AnimateColor_SystemOverdubOrSubtitle)
    , animateColorSystemRequired :: (Maybe String)
    , animateColorSystemScreenSize :: (Maybe String)
    , animateColorSystemScreenDepth :: (Maybe String)
    , animateColorSystemAudioDesc :: (Maybe AnimateColor_SystemAudioDesc)
    , animateColorSystemOperatingSystem :: (Maybe String)
    , animateColorSystemCPU :: (Maybe String)
    , animateColorSystemComponent :: (Maybe String)
    , animateColorSystem_bitrate :: (Maybe String)
    , animateColorSystem_captions :: (Maybe AnimateColor_System_captions)
    , animateColorSystem_language :: (Maybe String)
    , animateColorSystem_overdub_or_caption :: (Maybe AnimateColor_System_overdub_or_caption)
    , animateColorSystem_required :: (Maybe String)
    , animateColorSystem_screen_size :: (Maybe String)
    , animateColorSystem_screen_depth :: (Maybe String)
    , animateColorDur :: (Maybe String)
    , animateColorRepeatCount :: (Maybe String)
    , animateColorRepeatDur :: (Maybe String)
    , animateColorBegin :: (Maybe String)
    , animateColorEnd :: (Maybe String)
    , animateColorAttributeName :: String
    , animateColorAttributeType :: (Maybe String)
    , animateColorValues :: (Maybe String)
    , animateColorFrom :: (Maybe String)
    , animateColorTo :: (Maybe String)
    , animateColorBy :: (Maybe String)
    , animateColorCalcMode :: (Defaultable AnimateColor_CalcMode)
    , animateColorAdditive :: (Defaultable AnimateColor_Additive)
    , animateColorAccumulate :: (Defaultable AnimateColor_Accumulate)
    } deriving (Eq,Show)
data AnimateColor_SystemCaptions = AnimateColor_SystemCaptions_On
				    |  AnimateColor_SystemCaptions_Off
				 deriving (Eq,Show)
data AnimateColor_SystemOverdubOrSubtitle = AnimateColor_SystemOverdubOrSubtitle_Overdub
					     |  AnimateColor_SystemOverdubOrSubtitle_Subtitle
					  deriving (Eq,Show)
data AnimateColor_SystemAudioDesc = AnimateColor_SystemAudioDesc_On
				     |  AnimateColor_SystemAudioDesc_Off
				  deriving (Eq,Show)
data AnimateColor_System_captions = AnimateColor_System_captions_On
				     |  AnimateColor_System_captions_Off
				  deriving (Eq,Show)
data AnimateColor_System_overdub_or_caption = AnimateColor_System_overdub_or_caption_Overdub
					       |  AnimateColor_System_overdub_or_caption_Caption
					    deriving (Eq,Show)
data AnimateColor_CalcMode = AnimateColor_CalcMode_Discrete  | 
			     AnimateColor_CalcMode_Linear  |  AnimateColor_CalcMode_Paced
			   deriving (Eq,Show)
data AnimateColor_Additive = AnimateColor_Additive_Replace  | 
			     AnimateColor_Additive_Sum
			   deriving (Eq,Show)
data AnimateColor_Accumulate = AnimateColor_Accumulate_None  | 
			       AnimateColor_Accumulate_Sum
			     deriving (Eq,Show)
data Switch = Switch
    { switchId :: (Maybe String)
    , switchClass :: (Maybe String)
    , switchTitle :: (Maybe String)
    , switchXml'lang :: (Maybe String)
    } deriving (Eq,Show)
data Meta = Meta
    { metaContent :: (Maybe String)
    , metaName :: String
    } deriving (Eq,Show)
data Metadata = Metadata
    { metadataId :: (Maybe String)
    , metadataClass :: (Maybe String)
    , metadataTitle :: (Maybe String)
    , metadataXml'lang :: (Maybe String)
    } deriving (Eq,Show)
data Layout = Layout
    { layoutId :: (Maybe String)
    , layoutClass :: (Maybe String)
    , layoutTitle :: (Maybe String)
    , layoutXml'lang :: (Maybe String)
    , layoutType :: (Defaultable String)
    } deriving (Eq,Show)
data Region = Region
    { regionId :: (Maybe String)
    , regionClass :: (Maybe String)
    , regionTitle :: (Maybe String)
    , regionXml'lang :: (Maybe String)
    , regionHeight :: (Defaultable String)
    , regionWidth :: (Defaultable String)
    , regionClose :: (Defaultable Region_Close)
    , regionOpen :: (Defaultable Region_Open)
    , regionBackgroundColor :: (Maybe String)
    , regionBackground_color :: (Maybe String)
    , regionBottom :: (Defaultable String)
    , regionLeft :: (Defaultable String)
    , regionRight :: (Defaultable String)
    , regionTop :: (Defaultable String)
    , regionZ_index :: (Maybe String)
    , regionShowBackground :: (Defaultable Region_ShowBackground)
    , regionFit :: (Defaultable Region_Fit)
    } deriving (Eq,Show)
data Region_Close = Region_Close_Never  | 
		    Region_Close_WhenNotActive
		  deriving (Eq,Show)
data Region_Open = Region_Open_Always  |  Region_Open_WhenActive
		 deriving (Eq,Show)
data Region_ShowBackground = Region_ShowBackground_Always  | 
			     Region_ShowBackground_WhenActive
			   deriving (Eq,Show)
data Region_Fit = Region_Fit_Hidden  |  Region_Fit_Fill  | 
		  Region_Fit_Meet  |  Region_Fit_Scroll  |  Region_Fit_Slice
		deriving (Eq,Show)
data Root_layout = Root_layout
    { root_layoutId :: (Maybe String)
    , root_layoutClass :: (Maybe String)
    , root_layoutTitle :: (Maybe String)
    , root_layoutXml'lang :: (Maybe String)
    , root_layoutHeight :: (Defaultable String)
    , root_layoutWidth :: (Defaultable String)
    , root_layoutClose :: (Defaultable Root_layout_Close)
    , root_layoutOpen :: (Defaultable Root_layout_Open)
    , root_layoutBackgroundColor :: (Maybe String)
    , root_layoutBackground_color :: (Maybe String)
    } deriving (Eq,Show)
data Root_layout_Close = Root_layout_Close_Never  | 
			 Root_layout_Close_WhenNotActive
		       deriving (Eq,Show)
data Root_layout_Open = Root_layout_Open_Always  | 
			Root_layout_Open_WhenActive
		      deriving (Eq,Show)
data Ref = Ref
    { refId :: (Maybe String)
    , refClass :: (Maybe String)
    , refTitle :: (Maybe String)
    , refXml'lang :: (Maybe String)
    } deriving (Eq,Show)
data Audio = Audio
    { audioId :: (Maybe String)
    , audioClass :: (Maybe String)
    , audioTitle :: (Maybe String)
    , audioXml'lang :: (Maybe String)
    } deriving (Eq,Show)
data Img = Img
    { imgId :: (Maybe String)
    , imgClass :: (Maybe String)
    , imgTitle :: (Maybe String)
    , imgXml'lang :: (Maybe String)
    } deriving (Eq,Show)
data Video = Video
    { videoId :: (Maybe String)
    , videoClass :: (Maybe String)
    , videoTitle :: (Maybe String)
    , videoXml'lang :: (Maybe String)
    } deriving (Eq,Show)
data Text = Text
    { textId :: (Maybe String)
    , textClass :: (Maybe String)
    , textTitle :: (Maybe String)
    , textXml'lang :: (Maybe String)
    } deriving (Eq,Show)
data Textstream = Textstream
    { textstreamId :: (Maybe String)
    , textstreamClass :: (Maybe String)
    , textstreamTitle :: (Maybe String)
    , textstreamXml'lang :: (Maybe String)
    } deriving (Eq,Show)
data Animation = Animation
    { animationId :: (Maybe String)
    , animationClass :: (Maybe String)
    , animationTitle :: (Maybe String)
    , animationXml'lang :: (Maybe String)
    } deriving (Eq,Show)
data Transition = Transition
    { transitionId :: (Maybe String)
    , transitionClass :: (Maybe String)
    , transitionTitle :: (Maybe String)
    , transitionXml'lang :: (Maybe String)
    , transitionType :: (Maybe Transition_Type)
    , transitionSubtype :: (Maybe Transition_Subtype)
    , transitionHorzRepeat :: (Defaultable String)
    , transitionVertRepeat :: (Defaultable String)
    , transitionBorderWidth :: (Defaultable String)
    , transitionBorderColor :: (Defaultable String)
    , transitionFadeColor :: (Defaultable String)
    , transitionCoordinated :: (Defaultable Transition_Coordinated)
    , transitionClibBoundary :: (Defaultable Transition_ClibBoundary)
    , transitionDur :: (Maybe String)
    , transitionStartProgress :: (Defaultable String)
    , transitionEndProgress :: (Defaultable String)
    , transitionDirection :: (Defaultable Transition_Direction)
    } deriving (Eq,Show)
data Transition_Type = Transition_Type_BarWipe  | 
		       Transition_Type_BoxWipe  |  Transition_Type_FourBoxWipe  | 
		       Transition_Type_BarnDoorWipe  |  Transition_Type_DiagonalWipe  | 
		       Transition_Type_BowTieWipe  |  Transition_Type_MiscDiagonalWipe  | 
		       Transition_Type_VeeWipe  |  Transition_Type_BarnVeeWipe  | 
		       Transition_Type_ZigZagWipe  |  Transition_Type_BarnZigZagWipe  | 
		       Transition_Type_MiscShapeWipe  |  Transition_Type_TriangleWipe  | 
		       Transition_Type_ArrowHeadWipe  |  Transition_Type_PentagonWipe  | 
		       Transition_Type_HexagonWipe  |  Transition_Type_EllipseWipe  | 
		       Transition_Type_EyeWipe  |  Transition_Type_RoundRectWipe  | 
		       Transition_Type_StarWipe  |  Transition_Type_ClockWipe  | 
		       Transition_Type_PinWheelWipe  |  Transition_Type_SingleSweepWipe
		        |  Transition_Type_FanWipe  |  Transition_Type_DoubleFanWipe  | 
		       Transition_Type_DoubleSweepWipe  |  Transition_Type_SaloonDoorWipe
		        |  Transition_Type_WindshieldWipe  |  Transition_Type_SnakeWipe
		        |  Transition_Type_SpiralWipe  | 
		       Transition_Type_ParallelSnakesWipe  | 
		       Transition_Type_BoxSnakesWipe  |  Transition_Type_WaterfallWipe  | 
		       Transition_Type_PushWipe  |  Transition_Type_SlideWipe  | 
		       Transition_Type_Fade
		     deriving (Eq,Show)
data Transition_Subtype = Transition_Subtype_Bottom  | 
			  Transition_Subtype_BottomCenter  |  Transition_Subtype_BottomLeft
			   |  Transition_Subtype_BottomLeftClockwise  | 
			  Transition_Subtype_BottomLeftCounterClockwise  | 
			  Transition_Subtype_BottomLeftDiagonal  | 
			  Transition_Subtype_BottomRight  | 
			  Transition_Subtype_BottomRightClockwise  | 
			  Transition_Subtype_BottomRightCounterClockwise  | 
			  Transition_Subtype_BottomRightDiagonal  | 
			  Transition_Subtype_CenterRight  |  Transition_Subtype_CenterTop  | 
			  Transition_Subtype_Circle  |  Transition_Subtype_ClockwiseBottom
			   |  Transition_Subtype_ClockwiseBottomRight  | 
			  Transition_Subtype_ClockwiseLeft  | 
			  Transition_Subtype_ClockwiseNine  | 
			  Transition_Subtype_ClockwiseRight  | 
			  Transition_Subtype_ClockwiseSix  | 
			  Transition_Subtype_ClockwiseThree  | 
			  Transition_Subtype_ClockwiseTop  | 
			  Transition_Subtype_ClockwiseTopLeft  | 
			  Transition_Subtype_ClockwiseTwelve  |  Transition_Subtype_CornersIn
			   |  Transition_Subtype_CornersOut  | 
			  Transition_Subtype_CounterClockwiseBottomLeft  | 
			  Transition_Subtype_CounterClockwiseTopRight  | 
			  Transition_Subtype_Crossfade  | 
			  Transition_Subtype_DiagonalBottomLeft  | 
			  Transition_Subtype_DiagonalBottomLeftOpposite  | 
			  Transition_Subtype_DiagonalTopLeft  | 
			  Transition_Subtype_DiagonalTopLeftOpposite  | 
			  Transition_Subtype_Diamond  |  Transition_Subtype_DoubleBarnDoor
			   |  Transition_Subtype_DoubleDiamond  |  Transition_Subtype_Down
			   |  Transition_Subtype_FadeFromColor  | 
			  Transition_Subtype_FadeToColor  | 
			  Transition_Subtype_FanInHorizontal  | 
			  Transition_Subtype_FanInVertical  | 
			  Transition_Subtype_FanOutHorizontal  | 
			  Transition_Subtype_FanOutVertical  |  Transition_Subtype_FivePoint
			   |  Transition_Subtype_FourBlade  | 
			  Transition_Subtype_FourBoxHorizontal  | 
			  Transition_Subtype_FourBoxVertical  |  Transition_Subtype_FourPoint
			   |  Transition_Subtype_FromBottom  |  Transition_Subtype_FromLeft
			   |  Transition_Subtype_FromRight  |  Transition_Subtype_FromTop  | 
			  Transition_Subtype_Heart  |  Transition_Subtype_Horizontal  | 
			  Transition_Subtype_HorizontalLeft  | 
			  Transition_Subtype_HorizontalLeftSame  | 
			  Transition_Subtype_HorizontalRight  | 
			  Transition_Subtype_HorizontalRightSame  | 
			  Transition_Subtype_HorizontalTopLeftOpposite  | 
			  Transition_Subtype_HorizontalTopRightOpposite  | 
			  Transition_Subtype_Keyhole  |  Transition_Subtype_Left  | 
			  Transition_Subtype_LeftCenter  |  Transition_Subtype_LeftToRight
			   |  Transition_Subtype_OppositeHorizontal  | 
			  Transition_Subtype_OppositeVertical  | 
			  Transition_Subtype_ParallelDiagonal  | 
			  Transition_Subtype_ParallelDiagonalBottomLeft  | 
			  Transition_Subtype_ParallelDiagonalTopLeft  | 
			  Transition_Subtype_ParallelVertical  | 
			  Transition_Subtype_Rectangle  |  Transition_Subtype_Right  | 
			  Transition_Subtype_RightCenter  |  Transition_Subtype_SixPoint  | 
			  Transition_Subtype_Top  |  Transition_Subtype_TopCenter  | 
			  Transition_Subtype_TopLeft  |  Transition_Subtype_TopLeftClockwise
			   |  Transition_Subtype_TopLeftCounterClockwise  | 
			  Transition_Subtype_TopLeftDiagonal  | 
			  Transition_Subtype_TopLeftHorizontal  | 
			  Transition_Subtype_TopLeftVertical  |  Transition_Subtype_TopRight
			   |  Transition_Subtype_TopRightClockwise  | 
			  Transition_Subtype_TopRightCounterClockwise  | 
			  Transition_Subtype_TopRightDiagonal  | 
			  Transition_Subtype_TopToBottom  | 
			  Transition_Subtype_TwoBladeHorizontal  | 
			  Transition_Subtype_TwoBladeVertical  | 
			  Transition_Subtype_TwoBoxBottom  |  Transition_Subtype_TwoBoxLeft
			   |  Transition_Subtype_TwoBoxRight  |  Transition_Subtype_TwoBoxTop
			   |  Transition_Subtype_Up  |  Transition_Subtype_Vertical  | 
			  Transition_Subtype_VerticalBottomLeftOpposite  | 
			  Transition_Subtype_VerticalBottomSame  | 
			  Transition_Subtype_VerticalLeft  | 
			  Transition_Subtype_VerticalRight  | 
			  Transition_Subtype_VerticalTopLeftOpposite  | 
			  Transition_Subtype_VerticalTopSame
			deriving (Eq,Show)
data Transition_Coordinated = Transition_Coordinated_True  | 
			      Transition_Coordinated_False
			    deriving (Eq,Show)
data Transition_ClibBoundary = Transition_ClibBoundary_Parent  | 
			       Transition_ClibBoundary_Children
			     deriving (Eq,Show)
data Transition_Direction = Transition_Direction_Forward  | 
			    Transition_Direction_Reverse
			  deriving (Eq,Show)
data TransitionFilter = TransitionFilter
    { transitionFilterId :: (Maybe String)
    , transitionFilterClass :: (Maybe String)
    , transitionFilterTitle :: (Maybe String)
    , transitionFilterXml'lang :: (Maybe String)
    , transitionFilterType :: (Maybe TransitionFilter_Type)
    , transitionFilterSubtype :: (Maybe TransitionFilter_Subtype)
    , transitionFilterHorzRepeat :: (Defaultable String)
    , transitionFilterVertRepeat :: (Defaultable String)
    , transitionFilterBorderWidth :: (Defaultable String)
    , transitionFilterBorderColor :: (Defaultable String)
    , transitionFilterFadeColor :: (Defaultable String)
    , transitionFilterCoordinated :: (Defaultable TransitionFilter_Coordinated)
    , transitionFilterClibBoundary :: (Defaultable TransitionFilter_ClibBoundary)
    , transitionFilterDur :: (Maybe String)
    , transitionFilterRepeatCount :: (Maybe String)
    , transitionFilterRepeatDur :: (Maybe String)
    , transitionFilterBegin :: (Maybe String)
    , transitionFilterEnd :: (Maybe String)
    , transitionFilterValues :: (Maybe String)
    , transitionFilterFrom :: (Maybe String)
    , transitionFilterTo :: (Maybe String)
    , transitionFilterBy :: (Maybe String)
    , transitionFilterCalcMode :: (Defaultable TransitionFilter_CalcMode)
    } deriving (Eq,Show)
data TransitionFilter_Type = TransitionFilter_Type_BarWipe  | 
			     TransitionFilter_Type_BoxWipe  |  TransitionFilter_Type_FourBoxWipe
			      |  TransitionFilter_Type_BarnDoorWipe  | 
			     TransitionFilter_Type_DiagonalWipe  | 
			     TransitionFilter_Type_BowTieWipe  | 
			     TransitionFilter_Type_MiscDiagonalWipe  | 
			     TransitionFilter_Type_VeeWipe  |  TransitionFilter_Type_BarnVeeWipe
			      |  TransitionFilter_Type_ZigZagWipe  | 
			     TransitionFilter_Type_BarnZigZagWipe  | 
			     TransitionFilter_Type_MiscShapeWipe  | 
			     TransitionFilter_Type_TriangleWipe  | 
			     TransitionFilter_Type_ArrowHeadWipe  | 
			     TransitionFilter_Type_PentagonWipe  | 
			     TransitionFilter_Type_HexagonWipe  | 
			     TransitionFilter_Type_EllipseWipe  |  TransitionFilter_Type_EyeWipe
			      |  TransitionFilter_Type_RoundRectWipe  | 
			     TransitionFilter_Type_StarWipe  |  TransitionFilter_Type_ClockWipe
			      |  TransitionFilter_Type_PinWheelWipe  | 
			     TransitionFilter_Type_SingleSweepWipe  | 
			     TransitionFilter_Type_FanWipe  | 
			     TransitionFilter_Type_DoubleFanWipe  | 
			     TransitionFilter_Type_DoubleSweepWipe  | 
			     TransitionFilter_Type_SaloonDoorWipe  | 
			     TransitionFilter_Type_WindshieldWipe  | 
			     TransitionFilter_Type_SnakeWipe  | 
			     TransitionFilter_Type_SpiralWipe  | 
			     TransitionFilter_Type_ParallelSnakesWipe  | 
			     TransitionFilter_Type_BoxSnakesWipe  | 
			     TransitionFilter_Type_WaterfallWipe  | 
			     TransitionFilter_Type_PushWipe  |  TransitionFilter_Type_SlideWipe
			      |  TransitionFilter_Type_Fade
			   deriving (Eq,Show)
data TransitionFilter_Subtype = TransitionFilter_Subtype_Bottom  | 
				TransitionFilter_Subtype_BottomCenter  | 
				TransitionFilter_Subtype_BottomLeft  | 
				TransitionFilter_Subtype_BottomLeftClockwise  | 
				TransitionFilter_Subtype_BottomLeftCounterClockwise  | 
				TransitionFilter_Subtype_BottomLeftDiagonal  | 
				TransitionFilter_Subtype_BottomRight  | 
				TransitionFilter_Subtype_BottomRightClockwise  | 
				TransitionFilter_Subtype_BottomRightCounterClockwise  | 
				TransitionFilter_Subtype_BottomRightDiagonal  | 
				TransitionFilter_Subtype_CenterRight  | 
				TransitionFilter_Subtype_CenterTop  | 
				TransitionFilter_Subtype_Circle  | 
				TransitionFilter_Subtype_ClockwiseBottom  | 
				TransitionFilter_Subtype_ClockwiseBottomRight  | 
				TransitionFilter_Subtype_ClockwiseLeft  | 
				TransitionFilter_Subtype_ClockwiseNine  | 
				TransitionFilter_Subtype_ClockwiseRight  | 
				TransitionFilter_Subtype_ClockwiseSix  | 
				TransitionFilter_Subtype_ClockwiseThree  | 
				TransitionFilter_Subtype_ClockwiseTop  | 
				TransitionFilter_Subtype_ClockwiseTopLeft  | 
				TransitionFilter_Subtype_ClockwiseTwelve  | 
				TransitionFilter_Subtype_CornersIn  | 
				TransitionFilter_Subtype_CornersOut  | 
				TransitionFilter_Subtype_CounterClockwiseBottomLeft  | 
				TransitionFilter_Subtype_CounterClockwiseTopRight  | 
				TransitionFilter_Subtype_Crossfade  | 
				TransitionFilter_Subtype_DiagonalBottomLeft  | 
				TransitionFilter_Subtype_DiagonalBottomLeftOpposite  | 
				TransitionFilter_Subtype_DiagonalTopLeft  | 
				TransitionFilter_Subtype_DiagonalTopLeftOpposite  | 
				TransitionFilter_Subtype_Diamond  | 
				TransitionFilter_Subtype_DoubleBarnDoor  | 
				TransitionFilter_Subtype_DoubleDiamond  | 
				TransitionFilter_Subtype_Down  | 
				TransitionFilter_Subtype_FadeFromColor  | 
				TransitionFilter_Subtype_FadeToColor  | 
				TransitionFilter_Subtype_FanInHorizontal  | 
				TransitionFilter_Subtype_FanInVertical  | 
				TransitionFilter_Subtype_FanOutHorizontal  | 
				TransitionFilter_Subtype_FanOutVertical  | 
				TransitionFilter_Subtype_FivePoint  | 
				TransitionFilter_Subtype_FourBlade  | 
				TransitionFilter_Subtype_FourBoxHorizontal  | 
				TransitionFilter_Subtype_FourBoxVertical  | 
				TransitionFilter_Subtype_FourPoint  | 
				TransitionFilter_Subtype_FromBottom  | 
				TransitionFilter_Subtype_FromLeft  | 
				TransitionFilter_Subtype_FromRight  | 
				TransitionFilter_Subtype_FromTop  |  TransitionFilter_Subtype_Heart
				 |  TransitionFilter_Subtype_Horizontal  | 
				TransitionFilter_Subtype_HorizontalLeft  | 
				TransitionFilter_Subtype_HorizontalLeftSame  | 
				TransitionFilter_Subtype_HorizontalRight  | 
				TransitionFilter_Subtype_HorizontalRightSame  | 
				TransitionFilter_Subtype_HorizontalTopLeftOpposite  | 
				TransitionFilter_Subtype_HorizontalTopRightOpposite  | 
				TransitionFilter_Subtype_Keyhole  |  TransitionFilter_Subtype_Left
				 |  TransitionFilter_Subtype_LeftCenter  | 
				TransitionFilter_Subtype_LeftToRight  | 
				TransitionFilter_Subtype_OppositeHorizontal  | 
				TransitionFilter_Subtype_OppositeVertical  | 
				TransitionFilter_Subtype_ParallelDiagonal  | 
				TransitionFilter_Subtype_ParallelDiagonalBottomLeft  | 
				TransitionFilter_Subtype_ParallelDiagonalTopLeft  | 
				TransitionFilter_Subtype_ParallelVertical  | 
				TransitionFilter_Subtype_Rectangle  | 
				TransitionFilter_Subtype_Right  | 
				TransitionFilter_Subtype_RightCenter  | 
				TransitionFilter_Subtype_SixPoint  |  TransitionFilter_Subtype_Top
				 |  TransitionFilter_Subtype_TopCenter  | 
				TransitionFilter_Subtype_TopLeft  | 
				TransitionFilter_Subtype_TopLeftClockwise  | 
				TransitionFilter_Subtype_TopLeftCounterClockwise  | 
				TransitionFilter_Subtype_TopLeftDiagonal  | 
				TransitionFilter_Subtype_TopLeftHorizontal  | 
				TransitionFilter_Subtype_TopLeftVertical  | 
				TransitionFilter_Subtype_TopRight  | 
				TransitionFilter_Subtype_TopRightClockwise  | 
				TransitionFilter_Subtype_TopRightCounterClockwise  | 
				TransitionFilter_Subtype_TopRightDiagonal  | 
				TransitionFilter_Subtype_TopToBottom  | 
				TransitionFilter_Subtype_TwoBladeHorizontal  | 
				TransitionFilter_Subtype_TwoBladeVertical  | 
				TransitionFilter_Subtype_TwoBoxBottom  | 
				TransitionFilter_Subtype_TwoBoxLeft  | 
				TransitionFilter_Subtype_TwoBoxRight  | 
				TransitionFilter_Subtype_TwoBoxTop  |  TransitionFilter_Subtype_Up
				 |  TransitionFilter_Subtype_Vertical  | 
				TransitionFilter_Subtype_VerticalBottomLeftOpposite  | 
				TransitionFilter_Subtype_VerticalBottomSame  | 
				TransitionFilter_Subtype_VerticalLeft  | 
				TransitionFilter_Subtype_VerticalRight  | 
				TransitionFilter_Subtype_VerticalTopLeftOpposite  | 
				TransitionFilter_Subtype_VerticalTopSame
			      deriving (Eq,Show)
data TransitionFilter_Coordinated = TransitionFilter_Coordinated_True
				     |  TransitionFilter_Coordinated_False
				  deriving (Eq,Show)
data TransitionFilter_ClibBoundary = TransitionFilter_ClibBoundary_Parent
				      |  TransitionFilter_ClibBoundary_Children
				   deriving (Eq,Show)
data TransitionFilter_CalcMode = TransitionFilter_CalcMode_Discrete
				  |  TransitionFilter_CalcMode_Linear  | 
				 TransitionFilter_CalcMode_Paced
			       deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Smil where
    fromElem (CElem (Elem "smil" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "smil" (toAttrs as) [])]
instance XmlAttributes Smil where
    fromAttrs as =
	Smil
	  { smilId = possibleA fromAttrToStr "id" as
	  , smilClass = possibleA fromAttrToStr "class" as
	  , smilTitle = possibleA fromAttrToStr "title" as
	  , smilXml'lang = possibleA fromAttrToStr "xml:lang" as
	  , smilXmlns = defaultA fromAttrToStr "http://www.w3.org/TR/REC-smil/SMIL20" "xmlns" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (smilId v)
	, maybeToAttr toAttrFrStr "class" (smilClass v)
	, maybeToAttr toAttrFrStr "title" (smilTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (smilXml'lang v)
	, defaultToAttr toAttrFrStr "xmlns" (smilXmlns v)
	]
instance XmlContent Head where
    fromElem (CElem (Elem "head" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "head" (toAttrs as) [])]
instance XmlAttributes Head where
    fromAttrs as =
	Head
	  { headId = possibleA fromAttrToStr "id" as
	  , headClass = possibleA fromAttrToStr "class" as
	  , headTitle = possibleA fromAttrToStr "title" as
	  , headXml'lang = possibleA fromAttrToStr "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (headId v)
	, maybeToAttr toAttrFrStr "class" (headClass v)
	, maybeToAttr toAttrFrStr "title" (headTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (headXml'lang v)
	]
instance XmlContent Body where
    fromElem (CElem (Elem "body" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "body" (toAttrs as) [])]
instance XmlAttributes Body where
    fromAttrs as =
	Body
	  { bodyId = possibleA fromAttrToStr "id" as
	  , bodyClass = possibleA fromAttrToStr "class" as
	  , bodyTitle = possibleA fromAttrToStr "title" as
	  , bodyXml'lang = possibleA fromAttrToStr "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (bodyId v)
	, maybeToAttr toAttrFrStr "class" (bodyClass v)
	, maybeToAttr toAttrFrStr "title" (bodyTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (bodyXml'lang v)
	]
instance XmlContent Animate where
    fromElem (CElem (Elem "animate" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "animate" (toAttrs as) [])]
instance XmlAttributes Animate where
    fromAttrs as =
	Animate
	  { animateId = possibleA fromAttrToStr "id" as
	  , animateClass = possibleA fromAttrToStr "class" as
	  , animateTitle = possibleA fromAttrToStr "title" as
	  , animateXml'lang = possibleA fromAttrToStr "xml:lang" as
	  , animateCustomTest = possibleA fromAttrToStr "customTest" as
	  , animateSystemBitrate = possibleA fromAttrToStr "systemBitrate" as
	  , animateSystemCaptions = possibleA fromAttrToTyp "systemCaptions" as
	  , animateSystemLanguage = possibleA fromAttrToStr "systemLanguage" as
	  , animateSystemOverdubOrSubtitle = possibleA fromAttrToTyp "systemOverdubOrSubtitle" as
	  , animateSystemRequired = possibleA fromAttrToStr "systemRequired" as
	  , animateSystemScreenSize = possibleA fromAttrToStr "systemScreenSize" as
	  , animateSystemScreenDepth = possibleA fromAttrToStr "systemScreenDepth" as
	  , animateSystemAudioDesc = possibleA fromAttrToTyp "systemAudioDesc" as
	  , animateSystemOperatingSystem = possibleA fromAttrToStr "systemOperatingSystem" as
	  , animateSystemCPU = possibleA fromAttrToStr "systemCPU" as
	  , animateSystemComponent = possibleA fromAttrToStr "systemComponent" as
	  , animateSystem_bitrate = possibleA fromAttrToStr "system-bitrate" as
	  , animateSystem_captions = possibleA fromAttrToTyp "system-captions" as
	  , animateSystem_language = possibleA fromAttrToStr "system-language" as
	  , animateSystem_overdub_or_caption = possibleA fromAttrToTyp "system-overdub-or-caption" as
	  , animateSystem_required = possibleA fromAttrToStr "system-required" as
	  , animateSystem_screen_size = possibleA fromAttrToStr "system-screen-size" as
	  , animateSystem_screen_depth = possibleA fromAttrToStr "system-screen-depth" as
	  , animateDur = possibleA fromAttrToStr "dur" as
	  , animateRepeatCount = possibleA fromAttrToStr "repeatCount" as
	  , animateRepeatDur = possibleA fromAttrToStr "repeatDur" as
	  , animateBegin = possibleA fromAttrToStr "begin" as
	  , animateEnd = possibleA fromAttrToStr "end" as
	  , animateAttributeName = definiteA fromAttrToStr "animate" "attributeName" as
	  , animateAttributeType = possibleA fromAttrToStr "attributeType" as
	  , animateValues = possibleA fromAttrToStr "values" as
	  , animateFrom = possibleA fromAttrToStr "from" as
	  , animateTo = possibleA fromAttrToStr "to" as
	  , animateBy = possibleA fromAttrToStr "by" as
	  , animateCalcMode = defaultA fromAttrToTyp Animate_CalcMode_Linear "calcMode" as
	  , animateAdditive = defaultA fromAttrToTyp Animate_Additive_Replace "additive" as
	  , animateAccumulate = defaultA fromAttrToTyp Animate_Accumulate_None "accumulate" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (animateId v)
	, maybeToAttr toAttrFrStr "class" (animateClass v)
	, maybeToAttr toAttrFrStr "title" (animateTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (animateXml'lang v)
	, maybeToAttr toAttrFrStr "customTest" (animateCustomTest v)
	, maybeToAttr toAttrFrStr "systemBitrate" (animateSystemBitrate v)
	, maybeToAttr toAttrFrTyp "systemCaptions" (animateSystemCaptions v)
	, maybeToAttr toAttrFrStr "systemLanguage" (animateSystemLanguage v)
	, maybeToAttr toAttrFrTyp "systemOverdubOrSubtitle" (animateSystemOverdubOrSubtitle v)
	, maybeToAttr toAttrFrStr "systemRequired" (animateSystemRequired v)
	, maybeToAttr toAttrFrStr "systemScreenSize" (animateSystemScreenSize v)
	, maybeToAttr toAttrFrStr "systemScreenDepth" (animateSystemScreenDepth v)
	, maybeToAttr toAttrFrTyp "systemAudioDesc" (animateSystemAudioDesc v)
	, maybeToAttr toAttrFrStr "systemOperatingSystem" (animateSystemOperatingSystem v)
	, maybeToAttr toAttrFrStr "systemCPU" (animateSystemCPU v)
	, maybeToAttr toAttrFrStr "systemComponent" (animateSystemComponent v)
	, maybeToAttr toAttrFrStr "system-bitrate" (animateSystem_bitrate v)
	, maybeToAttr toAttrFrTyp "system-captions" (animateSystem_captions v)
	, maybeToAttr toAttrFrStr "system-language" (animateSystem_language v)
	, maybeToAttr toAttrFrTyp "system-overdub-or-caption" (animateSystem_overdub_or_caption v)
	, maybeToAttr toAttrFrStr "system-required" (animateSystem_required v)
	, maybeToAttr toAttrFrStr "system-screen-size" (animateSystem_screen_size v)
	, maybeToAttr toAttrFrStr "system-screen-depth" (animateSystem_screen_depth v)
	, maybeToAttr toAttrFrStr "dur" (animateDur v)
	, maybeToAttr toAttrFrStr "repeatCount" (animateRepeatCount v)
	, maybeToAttr toAttrFrStr "repeatDur" (animateRepeatDur v)
	, maybeToAttr toAttrFrStr "begin" (animateBegin v)
	, maybeToAttr toAttrFrStr "end" (animateEnd v)
	, toAttrFrStr "attributeName" (animateAttributeName v)
	, maybeToAttr toAttrFrStr "attributeType" (animateAttributeType v)
	, maybeToAttr toAttrFrStr "values" (animateValues v)
	, maybeToAttr toAttrFrStr "from" (animateFrom v)
	, maybeToAttr toAttrFrStr "to" (animateTo v)
	, maybeToAttr toAttrFrStr "by" (animateBy v)
	, defaultToAttr toAttrFrTyp "calcMode" (animateCalcMode v)
	, defaultToAttr toAttrFrTyp "additive" (animateAdditive v)
	, defaultToAttr toAttrFrTyp "accumulate" (animateAccumulate v)
	]
instance XmlAttrType Animate_SystemCaptions where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "on" = Just Animate_SystemCaptions_On
	    translate "off" = Just Animate_SystemCaptions_Off
	    translate _ = Nothing
    toAttrFrTyp n Animate_SystemCaptions_On = Just (n, str2attr "on")
    toAttrFrTyp n Animate_SystemCaptions_Off = Just (n, str2attr "off")
instance XmlAttrType Animate_SystemOverdubOrSubtitle where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "overdub" = Just Animate_SystemOverdubOrSubtitle_Overdub
	    translate "subtitle" = Just Animate_SystemOverdubOrSubtitle_Subtitle
	    translate _ = Nothing
    toAttrFrTyp n Animate_SystemOverdubOrSubtitle_Overdub = Just (n, str2attr "overdub")
    toAttrFrTyp n Animate_SystemOverdubOrSubtitle_Subtitle = Just (n, str2attr "subtitle")
instance XmlAttrType Animate_SystemAudioDesc where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "on" = Just Animate_SystemAudioDesc_On
	    translate "off" = Just Animate_SystemAudioDesc_Off
	    translate _ = Nothing
    toAttrFrTyp n Animate_SystemAudioDesc_On = Just (n, str2attr "on")
    toAttrFrTyp n Animate_SystemAudioDesc_Off = Just (n, str2attr "off")
instance XmlAttrType Animate_System_captions where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "on" = Just Animate_System_captions_On
	    translate "off" = Just Animate_System_captions_Off
	    translate _ = Nothing
    toAttrFrTyp n Animate_System_captions_On = Just (n, str2attr "on")
    toAttrFrTyp n Animate_System_captions_Off = Just (n, str2attr "off")
instance XmlAttrType Animate_System_overdub_or_caption where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "overdub" = Just Animate_System_overdub_or_caption_Overdub
	    translate "caption" = Just Animate_System_overdub_or_caption_Caption
	    translate _ = Nothing
    toAttrFrTyp n Animate_System_overdub_or_caption_Overdub = Just (n, str2attr "overdub")
    toAttrFrTyp n Animate_System_overdub_or_caption_Caption = Just (n, str2attr "caption")
instance XmlAttrType Animate_CalcMode where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "discrete" = Just Animate_CalcMode_Discrete
	    translate "linear" = Just Animate_CalcMode_Linear
	    translate "paced" = Just Animate_CalcMode_Paced
	    translate _ = Nothing
    toAttrFrTyp n Animate_CalcMode_Discrete = Just (n, str2attr "discrete")
    toAttrFrTyp n Animate_CalcMode_Linear = Just (n, str2attr "linear")
    toAttrFrTyp n Animate_CalcMode_Paced = Just (n, str2attr "paced")
instance XmlAttrType Animate_Additive where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "replace" = Just Animate_Additive_Replace
	    translate "sum" = Just Animate_Additive_Sum
	    translate _ = Nothing
    toAttrFrTyp n Animate_Additive_Replace = Just (n, str2attr "replace")
    toAttrFrTyp n Animate_Additive_Sum = Just (n, str2attr "sum")
instance XmlAttrType Animate_Accumulate where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "none" = Just Animate_Accumulate_None
	    translate "sum" = Just Animate_Accumulate_Sum
	    translate _ = Nothing
    toAttrFrTyp n Animate_Accumulate_None = Just (n, str2attr "none")
    toAttrFrTyp n Animate_Accumulate_Sum = Just (n, str2attr "sum")
instance XmlContent Set where
    fromElem (CElem (Elem "set" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "set" (toAttrs as) [])]
instance XmlAttributes Set where
    fromAttrs as =
	Set
	  { setId = possibleA fromAttrToStr "id" as
	  , setClass = possibleA fromAttrToStr "class" as
	  , setTitle = possibleA fromAttrToStr "title" as
	  , setXml'lang = possibleA fromAttrToStr "xml:lang" as
	  , setCustomTest = possibleA fromAttrToStr "customTest" as
	  , setSystemBitrate = possibleA fromAttrToStr "systemBitrate" as
	  , setSystemCaptions = possibleA fromAttrToTyp "systemCaptions" as
	  , setSystemLanguage = possibleA fromAttrToStr "systemLanguage" as
	  , setSystemOverdubOrSubtitle = possibleA fromAttrToTyp "systemOverdubOrSubtitle" as
	  , setSystemRequired = possibleA fromAttrToStr "systemRequired" as
	  , setSystemScreenSize = possibleA fromAttrToStr "systemScreenSize" as
	  , setSystemScreenDepth = possibleA fromAttrToStr "systemScreenDepth" as
	  , setSystemAudioDesc = possibleA fromAttrToTyp "systemAudioDesc" as
	  , setSystemOperatingSystem = possibleA fromAttrToStr "systemOperatingSystem" as
	  , setSystemCPU = possibleA fromAttrToStr "systemCPU" as
	  , setSystemComponent = possibleA fromAttrToStr "systemComponent" as
	  , setSystem_bitrate = possibleA fromAttrToStr "system-bitrate" as
	  , setSystem_captions = possibleA fromAttrToTyp "system-captions" as
	  , setSystem_language = possibleA fromAttrToStr "system-language" as
	  , setSystem_overdub_or_caption = possibleA fromAttrToTyp "system-overdub-or-caption" as
	  , setSystem_required = possibleA fromAttrToStr "system-required" as
	  , setSystem_screen_size = possibleA fromAttrToStr "system-screen-size" as
	  , setSystem_screen_depth = possibleA fromAttrToStr "system-screen-depth" as
	  , setDur = possibleA fromAttrToStr "dur" as
	  , setRepeatCount = possibleA fromAttrToStr "repeatCount" as
	  , setRepeatDur = possibleA fromAttrToStr "repeatDur" as
	  , setBegin = possibleA fromAttrToStr "begin" as
	  , setEnd = possibleA fromAttrToStr "end" as
	  , setAttributeName = definiteA fromAttrToStr "set" "attributeName" as
	  , setAttributeType = possibleA fromAttrToStr "attributeType" as
	  , setTo = possibleA fromAttrToStr "to" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (setId v)
	, maybeToAttr toAttrFrStr "class" (setClass v)
	, maybeToAttr toAttrFrStr "title" (setTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (setXml'lang v)
	, maybeToAttr toAttrFrStr "customTest" (setCustomTest v)
	, maybeToAttr toAttrFrStr "systemBitrate" (setSystemBitrate v)
	, maybeToAttr toAttrFrTyp "systemCaptions" (setSystemCaptions v)
	, maybeToAttr toAttrFrStr "systemLanguage" (setSystemLanguage v)
	, maybeToAttr toAttrFrTyp "systemOverdubOrSubtitle" (setSystemOverdubOrSubtitle v)
	, maybeToAttr toAttrFrStr "systemRequired" (setSystemRequired v)
	, maybeToAttr toAttrFrStr "systemScreenSize" (setSystemScreenSize v)
	, maybeToAttr toAttrFrStr "systemScreenDepth" (setSystemScreenDepth v)
	, maybeToAttr toAttrFrTyp "systemAudioDesc" (setSystemAudioDesc v)
	, maybeToAttr toAttrFrStr "systemOperatingSystem" (setSystemOperatingSystem v)
	, maybeToAttr toAttrFrStr "systemCPU" (setSystemCPU v)
	, maybeToAttr toAttrFrStr "systemComponent" (setSystemComponent v)
	, maybeToAttr toAttrFrStr "system-bitrate" (setSystem_bitrate v)
	, maybeToAttr toAttrFrTyp "system-captions" (setSystem_captions v)
	, maybeToAttr toAttrFrStr "system-language" (setSystem_language v)
	, maybeToAttr toAttrFrTyp "system-overdub-or-caption" (setSystem_overdub_or_caption v)
	, maybeToAttr toAttrFrStr "system-required" (setSystem_required v)
	, maybeToAttr toAttrFrStr "system-screen-size" (setSystem_screen_size v)
	, maybeToAttr toAttrFrStr "system-screen-depth" (setSystem_screen_depth v)
	, maybeToAttr toAttrFrStr "dur" (setDur v)
	, maybeToAttr toAttrFrStr "repeatCount" (setRepeatCount v)
	, maybeToAttr toAttrFrStr "repeatDur" (setRepeatDur v)
	, maybeToAttr toAttrFrStr "begin" (setBegin v)
	, maybeToAttr toAttrFrStr "end" (setEnd v)
	, toAttrFrStr "attributeName" (setAttributeName v)
	, maybeToAttr toAttrFrStr "attributeType" (setAttributeType v)
	, maybeToAttr toAttrFrStr "to" (setTo v)
	]
instance XmlAttrType Set_SystemCaptions where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "on" = Just Set_SystemCaptions_On
	    translate "off" = Just Set_SystemCaptions_Off
	    translate _ = Nothing
    toAttrFrTyp n Set_SystemCaptions_On = Just (n, str2attr "on")
    toAttrFrTyp n Set_SystemCaptions_Off = Just (n, str2attr "off")
instance XmlAttrType Set_SystemOverdubOrSubtitle where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "overdub" = Just Set_SystemOverdubOrSubtitle_Overdub
	    translate "subtitle" = Just Set_SystemOverdubOrSubtitle_Subtitle
	    translate _ = Nothing
    toAttrFrTyp n Set_SystemOverdubOrSubtitle_Overdub = Just (n, str2attr "overdub")
    toAttrFrTyp n Set_SystemOverdubOrSubtitle_Subtitle = Just (n, str2attr "subtitle")
instance XmlAttrType Set_SystemAudioDesc where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "on" = Just Set_SystemAudioDesc_On
	    translate "off" = Just Set_SystemAudioDesc_Off
	    translate _ = Nothing
    toAttrFrTyp n Set_SystemAudioDesc_On = Just (n, str2attr "on")
    toAttrFrTyp n Set_SystemAudioDesc_Off = Just (n, str2attr "off")
instance XmlAttrType Set_System_captions where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "on" = Just Set_System_captions_On
	    translate "off" = Just Set_System_captions_Off
	    translate _ = Nothing
    toAttrFrTyp n Set_System_captions_On = Just (n, str2attr "on")
    toAttrFrTyp n Set_System_captions_Off = Just (n, str2attr "off")
instance XmlAttrType Set_System_overdub_or_caption where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "overdub" = Just Set_System_overdub_or_caption_Overdub
	    translate "caption" = Just Set_System_overdub_or_caption_Caption
	    translate _ = Nothing
    toAttrFrTyp n Set_System_overdub_or_caption_Overdub = Just (n, str2attr "overdub")
    toAttrFrTyp n Set_System_overdub_or_caption_Caption = Just (n, str2attr "caption")
instance XmlContent AnimateMotion where
    fromElem (CElem (Elem "animateMotion" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "animateMotion" (toAttrs as) [])]
instance XmlAttributes AnimateMotion where
    fromAttrs as =
	AnimateMotion
	  { animateMotionId = possibleA fromAttrToStr "id" as
	  , animateMotionClass = possibleA fromAttrToStr "class" as
	  , animateMotionTitle = possibleA fromAttrToStr "title" as
	  , animateMotionXml'lang = possibleA fromAttrToStr "xml:lang" as
	  , animateMotionCustomTest = possibleA fromAttrToStr "customTest" as
	  , animateMotionSystemBitrate = possibleA fromAttrToStr "systemBitrate" as
	  , animateMotionSystemCaptions = possibleA fromAttrToTyp "systemCaptions" as
	  , animateMotionSystemLanguage = possibleA fromAttrToStr "systemLanguage" as
	  , animateMotionSystemOverdubOrSubtitle = possibleA fromAttrToTyp "systemOverdubOrSubtitle" as
	  , animateMotionSystemRequired = possibleA fromAttrToStr "systemRequired" as
	  , animateMotionSystemScreenSize = possibleA fromAttrToStr "systemScreenSize" as
	  , animateMotionSystemScreenDepth = possibleA fromAttrToStr "systemScreenDepth" as
	  , animateMotionSystemAudioDesc = possibleA fromAttrToTyp "systemAudioDesc" as
	  , animateMotionSystemOperatingSystem = possibleA fromAttrToStr "systemOperatingSystem" as
	  , animateMotionSystemCPU = possibleA fromAttrToStr "systemCPU" as
	  , animateMotionSystemComponent = possibleA fromAttrToStr "systemComponent" as
	  , animateMotionSystem_bitrate = possibleA fromAttrToStr "system-bitrate" as
	  , animateMotionSystem_captions = possibleA fromAttrToTyp "system-captions" as
	  , animateMotionSystem_language = possibleA fromAttrToStr "system-language" as
	  , animateMotionSystem_overdub_or_caption = possibleA fromAttrToTyp "system-overdub-or-caption" as
	  , animateMotionSystem_required = possibleA fromAttrToStr "system-required" as
	  , animateMotionSystem_screen_size = possibleA fromAttrToStr "system-screen-size" as
	  , animateMotionSystem_screen_depth = possibleA fromAttrToStr "system-screen-depth" as
	  , animateMotionDur = possibleA fromAttrToStr "dur" as
	  , animateMotionRepeatCount = possibleA fromAttrToStr "repeatCount" as
	  , animateMotionRepeatDur = possibleA fromAttrToStr "repeatDur" as
	  , animateMotionBegin = possibleA fromAttrToStr "begin" as
	  , animateMotionEnd = possibleA fromAttrToStr "end" as
	  , animateMotionValues = possibleA fromAttrToStr "values" as
	  , animateMotionFrom = possibleA fromAttrToStr "from" as
	  , animateMotionTo = possibleA fromAttrToStr "to" as
	  , animateMotionBy = possibleA fromAttrToStr "by" as
	  , animateMotionCalcMode = defaultA fromAttrToTyp AnimateMotion_CalcMode_Linear "calcMode" as
	  , animateMotionAdditive = defaultA fromAttrToTyp AnimateMotion_Additive_Replace "additive" as
	  , animateMotionAccumulate = defaultA fromAttrToTyp AnimateMotion_Accumulate_None "accumulate" as
	  , animateMotionOrigin = defaultA fromAttrToTyp AnimateMotion_Origin_Default "origin" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (animateMotionId v)
	, maybeToAttr toAttrFrStr "class" (animateMotionClass v)
	, maybeToAttr toAttrFrStr "title" (animateMotionTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (animateMotionXml'lang v)
	, maybeToAttr toAttrFrStr "customTest" (animateMotionCustomTest v)
	, maybeToAttr toAttrFrStr "systemBitrate" (animateMotionSystemBitrate v)
	, maybeToAttr toAttrFrTyp "systemCaptions" (animateMotionSystemCaptions v)
	, maybeToAttr toAttrFrStr "systemLanguage" (animateMotionSystemLanguage v)
	, maybeToAttr toAttrFrTyp "systemOverdubOrSubtitle" (animateMotionSystemOverdubOrSubtitle v)
	, maybeToAttr toAttrFrStr "systemRequired" (animateMotionSystemRequired v)
	, maybeToAttr toAttrFrStr "systemScreenSize" (animateMotionSystemScreenSize v)
	, maybeToAttr toAttrFrStr "systemScreenDepth" (animateMotionSystemScreenDepth v)
	, maybeToAttr toAttrFrTyp "systemAudioDesc" (animateMotionSystemAudioDesc v)
	, maybeToAttr toAttrFrStr "systemOperatingSystem" (animateMotionSystemOperatingSystem v)
	, maybeToAttr toAttrFrStr "systemCPU" (animateMotionSystemCPU v)
	, maybeToAttr toAttrFrStr "systemComponent" (animateMotionSystemComponent v)
	, maybeToAttr toAttrFrStr "system-bitrate" (animateMotionSystem_bitrate v)
	, maybeToAttr toAttrFrTyp "system-captions" (animateMotionSystem_captions v)
	, maybeToAttr toAttrFrStr "system-language" (animateMotionSystem_language v)
	, maybeToAttr toAttrFrTyp "system-overdub-or-caption" (animateMotionSystem_overdub_or_caption v)
	, maybeToAttr toAttrFrStr "system-required" (animateMotionSystem_required v)
	, maybeToAttr toAttrFrStr "system-screen-size" (animateMotionSystem_screen_size v)
	, maybeToAttr toAttrFrStr "system-screen-depth" (animateMotionSystem_screen_depth v)
	, maybeToAttr toAttrFrStr "dur" (animateMotionDur v)
	, maybeToAttr toAttrFrStr "repeatCount" (animateMotionRepeatCount v)
	, maybeToAttr toAttrFrStr "repeatDur" (animateMotionRepeatDur v)
	, maybeToAttr toAttrFrStr "begin" (animateMotionBegin v)
	, maybeToAttr toAttrFrStr "end" (animateMotionEnd v)
	, maybeToAttr toAttrFrStr "values" (animateMotionValues v)
	, maybeToAttr toAttrFrStr "from" (animateMotionFrom v)
	, maybeToAttr toAttrFrStr "to" (animateMotionTo v)
	, maybeToAttr toAttrFrStr "by" (animateMotionBy v)
	, defaultToAttr toAttrFrTyp "calcMode" (animateMotionCalcMode v)
	, defaultToAttr toAttrFrTyp "additive" (animateMotionAdditive v)
	, defaultToAttr toAttrFrTyp "accumulate" (animateMotionAccumulate v)
	, defaultToAttr toAttrFrTyp "origin" (animateMotionOrigin v)
	]
instance XmlAttrType AnimateMotion_SystemCaptions where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "on" = Just AnimateMotion_SystemCaptions_On
	    translate "off" = Just AnimateMotion_SystemCaptions_Off
	    translate _ = Nothing
    toAttrFrTyp n AnimateMotion_SystemCaptions_On = Just (n, str2attr "on")
    toAttrFrTyp n AnimateMotion_SystemCaptions_Off = Just (n, str2attr "off")
instance XmlAttrType AnimateMotion_SystemOverdubOrSubtitle where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "overdub" = Just AnimateMotion_SystemOverdubOrSubtitle_Overdub
	    translate "subtitle" = Just AnimateMotion_SystemOverdubOrSubtitle_Subtitle
	    translate _ = Nothing
    toAttrFrTyp n AnimateMotion_SystemOverdubOrSubtitle_Overdub = Just (n, str2attr "overdub")
    toAttrFrTyp n AnimateMotion_SystemOverdubOrSubtitle_Subtitle = Just (n, str2attr "subtitle")
instance XmlAttrType AnimateMotion_SystemAudioDesc where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "on" = Just AnimateMotion_SystemAudioDesc_On
	    translate "off" = Just AnimateMotion_SystemAudioDesc_Off
	    translate _ = Nothing
    toAttrFrTyp n AnimateMotion_SystemAudioDesc_On = Just (n, str2attr "on")
    toAttrFrTyp n AnimateMotion_SystemAudioDesc_Off = Just (n, str2attr "off")
instance XmlAttrType AnimateMotion_System_captions where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "on" = Just AnimateMotion_System_captions_On
	    translate "off" = Just AnimateMotion_System_captions_Off
	    translate _ = Nothing
    toAttrFrTyp n AnimateMotion_System_captions_On = Just (n, str2attr "on")
    toAttrFrTyp n AnimateMotion_System_captions_Off = Just (n, str2attr "off")
instance XmlAttrType AnimateMotion_System_overdub_or_caption where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "overdub" = Just AnimateMotion_System_overdub_or_caption_Overdub
	    translate "caption" = Just AnimateMotion_System_overdub_or_caption_Caption
	    translate _ = Nothing
    toAttrFrTyp n AnimateMotion_System_overdub_or_caption_Overdub = Just (n, str2attr "overdub")
    toAttrFrTyp n AnimateMotion_System_overdub_or_caption_Caption = Just (n, str2attr "caption")
instance XmlAttrType AnimateMotion_CalcMode where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "discrete" = Just AnimateMotion_CalcMode_Discrete
	    translate "linear" = Just AnimateMotion_CalcMode_Linear
	    translate "paced" = Just AnimateMotion_CalcMode_Paced
	    translate _ = Nothing
    toAttrFrTyp n AnimateMotion_CalcMode_Discrete = Just (n, str2attr "discrete")
    toAttrFrTyp n AnimateMotion_CalcMode_Linear = Just (n, str2attr "linear")
    toAttrFrTyp n AnimateMotion_CalcMode_Paced = Just (n, str2attr "paced")
instance XmlAttrType AnimateMotion_Additive where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "replace" = Just AnimateMotion_Additive_Replace
	    translate "sum" = Just AnimateMotion_Additive_Sum
	    translate _ = Nothing
    toAttrFrTyp n AnimateMotion_Additive_Replace = Just (n, str2attr "replace")
    toAttrFrTyp n AnimateMotion_Additive_Sum = Just (n, str2attr "sum")
instance XmlAttrType AnimateMotion_Accumulate where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "none" = Just AnimateMotion_Accumulate_None
	    translate "sum" = Just AnimateMotion_Accumulate_Sum
	    translate _ = Nothing
    toAttrFrTyp n AnimateMotion_Accumulate_None = Just (n, str2attr "none")
    toAttrFrTyp n AnimateMotion_Accumulate_Sum = Just (n, str2attr "sum")
instance XmlAttrType AnimateMotion_Origin where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "default" = Just AnimateMotion_Origin_Default
	    translate _ = Nothing
    toAttrFrTyp n AnimateMotion_Origin_Default = Just (n, str2attr "default")
instance XmlContent AnimateColor where
    fromElem (CElem (Elem "animateColor" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "animateColor" (toAttrs as) [])]
instance XmlAttributes AnimateColor where
    fromAttrs as =
	AnimateColor
	  { animateColorId = possibleA fromAttrToStr "id" as
	  , animateColorClass = possibleA fromAttrToStr "class" as
	  , animateColorTitle = possibleA fromAttrToStr "title" as
	  , animateColorXml'lang = possibleA fromAttrToStr "xml:lang" as
	  , animateColorCustomTest = possibleA fromAttrToStr "customTest" as
	  , animateColorSystemBitrate = possibleA fromAttrToStr "systemBitrate" as
	  , animateColorSystemCaptions = possibleA fromAttrToTyp "systemCaptions" as
	  , animateColorSystemLanguage = possibleA fromAttrToStr "systemLanguage" as
	  , animateColorSystemOverdubOrSubtitle = possibleA fromAttrToTyp "systemOverdubOrSubtitle" as
	  , animateColorSystemRequired = possibleA fromAttrToStr "systemRequired" as
	  , animateColorSystemScreenSize = possibleA fromAttrToStr "systemScreenSize" as
	  , animateColorSystemScreenDepth = possibleA fromAttrToStr "systemScreenDepth" as
	  , animateColorSystemAudioDesc = possibleA fromAttrToTyp "systemAudioDesc" as
	  , animateColorSystemOperatingSystem = possibleA fromAttrToStr "systemOperatingSystem" as
	  , animateColorSystemCPU = possibleA fromAttrToStr "systemCPU" as
	  , animateColorSystemComponent = possibleA fromAttrToStr "systemComponent" as
	  , animateColorSystem_bitrate = possibleA fromAttrToStr "system-bitrate" as
	  , animateColorSystem_captions = possibleA fromAttrToTyp "system-captions" as
	  , animateColorSystem_language = possibleA fromAttrToStr "system-language" as
	  , animateColorSystem_overdub_or_caption = possibleA fromAttrToTyp "system-overdub-or-caption" as
	  , animateColorSystem_required = possibleA fromAttrToStr "system-required" as
	  , animateColorSystem_screen_size = possibleA fromAttrToStr "system-screen-size" as
	  , animateColorSystem_screen_depth = possibleA fromAttrToStr "system-screen-depth" as
	  , animateColorDur = possibleA fromAttrToStr "dur" as
	  , animateColorRepeatCount = possibleA fromAttrToStr "repeatCount" as
	  , animateColorRepeatDur = possibleA fromAttrToStr "repeatDur" as
	  , animateColorBegin = possibleA fromAttrToStr "begin" as
	  , animateColorEnd = possibleA fromAttrToStr "end" as
	  , animateColorAttributeName = definiteA fromAttrToStr "animateColor" "attributeName" as
	  , animateColorAttributeType = possibleA fromAttrToStr "attributeType" as
	  , animateColorValues = possibleA fromAttrToStr "values" as
	  , animateColorFrom = possibleA fromAttrToStr "from" as
	  , animateColorTo = possibleA fromAttrToStr "to" as
	  , animateColorBy = possibleA fromAttrToStr "by" as
	  , animateColorCalcMode = defaultA fromAttrToTyp AnimateColor_CalcMode_Linear "calcMode" as
	  , animateColorAdditive = defaultA fromAttrToTyp AnimateColor_Additive_Replace "additive" as
	  , animateColorAccumulate = defaultA fromAttrToTyp AnimateColor_Accumulate_None "accumulate" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (animateColorId v)
	, maybeToAttr toAttrFrStr "class" (animateColorClass v)
	, maybeToAttr toAttrFrStr "title" (animateColorTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (animateColorXml'lang v)
	, maybeToAttr toAttrFrStr "customTest" (animateColorCustomTest v)
	, maybeToAttr toAttrFrStr "systemBitrate" (animateColorSystemBitrate v)
	, maybeToAttr toAttrFrTyp "systemCaptions" (animateColorSystemCaptions v)
	, maybeToAttr toAttrFrStr "systemLanguage" (animateColorSystemLanguage v)
	, maybeToAttr toAttrFrTyp "systemOverdubOrSubtitle" (animateColorSystemOverdubOrSubtitle v)
	, maybeToAttr toAttrFrStr "systemRequired" (animateColorSystemRequired v)
	, maybeToAttr toAttrFrStr "systemScreenSize" (animateColorSystemScreenSize v)
	, maybeToAttr toAttrFrStr "systemScreenDepth" (animateColorSystemScreenDepth v)
	, maybeToAttr toAttrFrTyp "systemAudioDesc" (animateColorSystemAudioDesc v)
	, maybeToAttr toAttrFrStr "systemOperatingSystem" (animateColorSystemOperatingSystem v)
	, maybeToAttr toAttrFrStr "systemCPU" (animateColorSystemCPU v)
	, maybeToAttr toAttrFrStr "systemComponent" (animateColorSystemComponent v)
	, maybeToAttr toAttrFrStr "system-bitrate" (animateColorSystem_bitrate v)
	, maybeToAttr toAttrFrTyp "system-captions" (animateColorSystem_captions v)
	, maybeToAttr toAttrFrStr "system-language" (animateColorSystem_language v)
	, maybeToAttr toAttrFrTyp "system-overdub-or-caption" (animateColorSystem_overdub_or_caption v)
	, maybeToAttr toAttrFrStr "system-required" (animateColorSystem_required v)
	, maybeToAttr toAttrFrStr "system-screen-size" (animateColorSystem_screen_size v)
	, maybeToAttr toAttrFrStr "system-screen-depth" (animateColorSystem_screen_depth v)
	, maybeToAttr toAttrFrStr "dur" (animateColorDur v)
	, maybeToAttr toAttrFrStr "repeatCount" (animateColorRepeatCount v)
	, maybeToAttr toAttrFrStr "repeatDur" (animateColorRepeatDur v)
	, maybeToAttr toAttrFrStr "begin" (animateColorBegin v)
	, maybeToAttr toAttrFrStr "end" (animateColorEnd v)
	, toAttrFrStr "attributeName" (animateColorAttributeName v)
	, maybeToAttr toAttrFrStr "attributeType" (animateColorAttributeType v)
	, maybeToAttr toAttrFrStr "values" (animateColorValues v)
	, maybeToAttr toAttrFrStr "from" (animateColorFrom v)
	, maybeToAttr toAttrFrStr "to" (animateColorTo v)
	, maybeToAttr toAttrFrStr "by" (animateColorBy v)
	, defaultToAttr toAttrFrTyp "calcMode" (animateColorCalcMode v)
	, defaultToAttr toAttrFrTyp "additive" (animateColorAdditive v)
	, defaultToAttr toAttrFrTyp "accumulate" (animateColorAccumulate v)
	]
instance XmlAttrType AnimateColor_SystemCaptions where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "on" = Just AnimateColor_SystemCaptions_On
	    translate "off" = Just AnimateColor_SystemCaptions_Off
	    translate _ = Nothing
    toAttrFrTyp n AnimateColor_SystemCaptions_On = Just (n, str2attr "on")
    toAttrFrTyp n AnimateColor_SystemCaptions_Off = Just (n, str2attr "off")
instance XmlAttrType AnimateColor_SystemOverdubOrSubtitle where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "overdub" = Just AnimateColor_SystemOverdubOrSubtitle_Overdub
	    translate "subtitle" = Just AnimateColor_SystemOverdubOrSubtitle_Subtitle
	    translate _ = Nothing
    toAttrFrTyp n AnimateColor_SystemOverdubOrSubtitle_Overdub = Just (n, str2attr "overdub")
    toAttrFrTyp n AnimateColor_SystemOverdubOrSubtitle_Subtitle = Just (n, str2attr "subtitle")
instance XmlAttrType AnimateColor_SystemAudioDesc where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "on" = Just AnimateColor_SystemAudioDesc_On
	    translate "off" = Just AnimateColor_SystemAudioDesc_Off
	    translate _ = Nothing
    toAttrFrTyp n AnimateColor_SystemAudioDesc_On = Just (n, str2attr "on")
    toAttrFrTyp n AnimateColor_SystemAudioDesc_Off = Just (n, str2attr "off")
instance XmlAttrType AnimateColor_System_captions where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "on" = Just AnimateColor_System_captions_On
	    translate "off" = Just AnimateColor_System_captions_Off
	    translate _ = Nothing
    toAttrFrTyp n AnimateColor_System_captions_On = Just (n, str2attr "on")
    toAttrFrTyp n AnimateColor_System_captions_Off = Just (n, str2attr "off")
instance XmlAttrType AnimateColor_System_overdub_or_caption where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "overdub" = Just AnimateColor_System_overdub_or_caption_Overdub
	    translate "caption" = Just AnimateColor_System_overdub_or_caption_Caption
	    translate _ = Nothing
    toAttrFrTyp n AnimateColor_System_overdub_or_caption_Overdub = Just (n, str2attr "overdub")
    toAttrFrTyp n AnimateColor_System_overdub_or_caption_Caption = Just (n, str2attr "caption")
instance XmlAttrType AnimateColor_CalcMode where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "discrete" = Just AnimateColor_CalcMode_Discrete
	    translate "linear" = Just AnimateColor_CalcMode_Linear
	    translate "paced" = Just AnimateColor_CalcMode_Paced
	    translate _ = Nothing
    toAttrFrTyp n AnimateColor_CalcMode_Discrete = Just (n, str2attr "discrete")
    toAttrFrTyp n AnimateColor_CalcMode_Linear = Just (n, str2attr "linear")
    toAttrFrTyp n AnimateColor_CalcMode_Paced = Just (n, str2attr "paced")
instance XmlAttrType AnimateColor_Additive where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "replace" = Just AnimateColor_Additive_Replace
	    translate "sum" = Just AnimateColor_Additive_Sum
	    translate _ = Nothing
    toAttrFrTyp n AnimateColor_Additive_Replace = Just (n, str2attr "replace")
    toAttrFrTyp n AnimateColor_Additive_Sum = Just (n, str2attr "sum")
instance XmlAttrType AnimateColor_Accumulate where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "none" = Just AnimateColor_Accumulate_None
	    translate "sum" = Just AnimateColor_Accumulate_Sum
	    translate _ = Nothing
    toAttrFrTyp n AnimateColor_Accumulate_None = Just (n, str2attr "none")
    toAttrFrTyp n AnimateColor_Accumulate_Sum = Just (n, str2attr "sum")
instance XmlContent Switch where
    fromElem (CElem (Elem "switch" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "switch" (toAttrs as) [])]
instance XmlAttributes Switch where
    fromAttrs as =
	Switch
	  { switchId = possibleA fromAttrToStr "id" as
	  , switchClass = possibleA fromAttrToStr "class" as
	  , switchTitle = possibleA fromAttrToStr "title" as
	  , switchXml'lang = possibleA fromAttrToStr "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (switchId v)
	, maybeToAttr toAttrFrStr "class" (switchClass v)
	, maybeToAttr toAttrFrStr "title" (switchTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (switchXml'lang v)
	]
instance XmlContent Meta where
    fromElem (CElem (Elem "meta" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "meta" (toAttrs as) [])]
instance XmlAttributes Meta where
    fromAttrs as =
	Meta
	  { metaContent = possibleA fromAttrToStr "content" as
	  , metaName = definiteA fromAttrToStr "meta" "name" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "content" (metaContent v)
	, toAttrFrStr "name" (metaName v)
	]
instance XmlContent Metadata where
    fromElem (CElem (Elem "metadata" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "metadata" (toAttrs as) [])]
instance XmlAttributes Metadata where
    fromAttrs as =
	Metadata
	  { metadataId = possibleA fromAttrToStr "id" as
	  , metadataClass = possibleA fromAttrToStr "class" as
	  , metadataTitle = possibleA fromAttrToStr "title" as
	  , metadataXml'lang = possibleA fromAttrToStr "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (metadataId v)
	, maybeToAttr toAttrFrStr "class" (metadataClass v)
	, maybeToAttr toAttrFrStr "title" (metadataTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (metadataXml'lang v)
	]
instance XmlContent Layout where
    fromElem (CElem (Elem "layout" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "layout" (toAttrs as) [])]
instance XmlAttributes Layout where
    fromAttrs as =
	Layout
	  { layoutId = possibleA fromAttrToStr "id" as
	  , layoutClass = possibleA fromAttrToStr "class" as
	  , layoutTitle = possibleA fromAttrToStr "title" as
	  , layoutXml'lang = possibleA fromAttrToStr "xml:lang" as
	  , layoutType = defaultA fromAttrToStr "text/smil-basic-layout" "type" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (layoutId v)
	, maybeToAttr toAttrFrStr "class" (layoutClass v)
	, maybeToAttr toAttrFrStr "title" (layoutTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (layoutXml'lang v)
	, defaultToAttr toAttrFrStr "type" (layoutType v)
	]
instance XmlContent Region where
    fromElem (CElem (Elem "region" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "region" (toAttrs as) [])]
instance XmlAttributes Region where
    fromAttrs as =
	Region
	  { regionId = possibleA fromAttrToStr "id" as
	  , regionClass = possibleA fromAttrToStr "class" as
	  , regionTitle = possibleA fromAttrToStr "title" as
	  , regionXml'lang = possibleA fromAttrToStr "xml:lang" as
	  , regionHeight = defaultA fromAttrToStr "auto" "height" as
	  , regionWidth = defaultA fromAttrToStr "auto" "width" as
	  , regionClose = defaultA fromAttrToTyp Region_Close_Never "close" as
	  , regionOpen = defaultA fromAttrToTyp Region_Open_Always "open" as
	  , regionBackgroundColor = possibleA fromAttrToStr "backgroundColor" as
	  , regionBackground_color = possibleA fromAttrToStr "background-color" as
	  , regionBottom = defaultA fromAttrToStr "auto" "bottom" as
	  , regionLeft = defaultA fromAttrToStr "auto" "left" as
	  , regionRight = defaultA fromAttrToStr "auto" "right" as
	  , regionTop = defaultA fromAttrToStr "auto" "top" as
	  , regionZ_index = possibleA fromAttrToStr "z-index" as
	  , regionShowBackground = defaultA fromAttrToTyp Region_ShowBackground_Always "showBackground" as
	  , regionFit = defaultA fromAttrToTyp Region_Fit_Hidden "fit" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (regionId v)
	, maybeToAttr toAttrFrStr "class" (regionClass v)
	, maybeToAttr toAttrFrStr "title" (regionTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (regionXml'lang v)
	, defaultToAttr toAttrFrStr "height" (regionHeight v)
	, defaultToAttr toAttrFrStr "width" (regionWidth v)
	, defaultToAttr toAttrFrTyp "close" (regionClose v)
	, defaultToAttr toAttrFrTyp "open" (regionOpen v)
	, maybeToAttr toAttrFrStr "backgroundColor" (regionBackgroundColor v)
	, maybeToAttr toAttrFrStr "background-color" (regionBackground_color v)
	, defaultToAttr toAttrFrStr "bottom" (regionBottom v)
	, defaultToAttr toAttrFrStr "left" (regionLeft v)
	, defaultToAttr toAttrFrStr "right" (regionRight v)
	, defaultToAttr toAttrFrStr "top" (regionTop v)
	, maybeToAttr toAttrFrStr "z-index" (regionZ_index v)
	, defaultToAttr toAttrFrTyp "showBackground" (regionShowBackground v)
	, defaultToAttr toAttrFrTyp "fit" (regionFit v)
	]
instance XmlAttrType Region_Close where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "never" = Just Region_Close_Never
	    translate "whenNotActive" = Just Region_Close_WhenNotActive
	    translate _ = Nothing
    toAttrFrTyp n Region_Close_Never = Just (n, str2attr "never")
    toAttrFrTyp n Region_Close_WhenNotActive = Just (n, str2attr "whenNotActive")
instance XmlAttrType Region_Open where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "always" = Just Region_Open_Always
	    translate "whenActive" = Just Region_Open_WhenActive
	    translate _ = Nothing
    toAttrFrTyp n Region_Open_Always = Just (n, str2attr "always")
    toAttrFrTyp n Region_Open_WhenActive = Just (n, str2attr "whenActive")
instance XmlAttrType Region_ShowBackground where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "always" = Just Region_ShowBackground_Always
	    translate "whenActive" = Just Region_ShowBackground_WhenActive
	    translate _ = Nothing
    toAttrFrTyp n Region_ShowBackground_Always = Just (n, str2attr "always")
    toAttrFrTyp n Region_ShowBackground_WhenActive = Just (n, str2attr "whenActive")
instance XmlAttrType Region_Fit where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "hidden" = Just Region_Fit_Hidden
	    translate "fill" = Just Region_Fit_Fill
	    translate "meet" = Just Region_Fit_Meet
	    translate "scroll" = Just Region_Fit_Scroll
	    translate "slice" = Just Region_Fit_Slice
	    translate _ = Nothing
    toAttrFrTyp n Region_Fit_Hidden = Just (n, str2attr "hidden")
    toAttrFrTyp n Region_Fit_Fill = Just (n, str2attr "fill")
    toAttrFrTyp n Region_Fit_Meet = Just (n, str2attr "meet")
    toAttrFrTyp n Region_Fit_Scroll = Just (n, str2attr "scroll")
    toAttrFrTyp n Region_Fit_Slice = Just (n, str2attr "slice")
instance XmlContent Root_layout where
    fromElem (CElem (Elem "root-layout" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "root-layout" (toAttrs as) [])]
instance XmlAttributes Root_layout where
    fromAttrs as =
	Root_layout
	  { root_layoutId = possibleA fromAttrToStr "id" as
	  , root_layoutClass = possibleA fromAttrToStr "class" as
	  , root_layoutTitle = possibleA fromAttrToStr "title" as
	  , root_layoutXml'lang = possibleA fromAttrToStr "xml:lang" as
	  , root_layoutHeight = defaultA fromAttrToStr "auto" "height" as
	  , root_layoutWidth = defaultA fromAttrToStr "auto" "width" as
	  , root_layoutClose = defaultA fromAttrToTyp Root_layout_Close_Never "close" as
	  , root_layoutOpen = defaultA fromAttrToTyp Root_layout_Open_Always "open" as
	  , root_layoutBackgroundColor = possibleA fromAttrToStr "backgroundColor" as
	  , root_layoutBackground_color = possibleA fromAttrToStr "background-color" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (root_layoutId v)
	, maybeToAttr toAttrFrStr "class" (root_layoutClass v)
	, maybeToAttr toAttrFrStr "title" (root_layoutTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (root_layoutXml'lang v)
	, defaultToAttr toAttrFrStr "height" (root_layoutHeight v)
	, defaultToAttr toAttrFrStr "width" (root_layoutWidth v)
	, defaultToAttr toAttrFrTyp "close" (root_layoutClose v)
	, defaultToAttr toAttrFrTyp "open" (root_layoutOpen v)
	, maybeToAttr toAttrFrStr "backgroundColor" (root_layoutBackgroundColor v)
	, maybeToAttr toAttrFrStr "background-color" (root_layoutBackground_color v)
	]
instance XmlAttrType Root_layout_Close where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "never" = Just Root_layout_Close_Never
	    translate "whenNotActive" = Just Root_layout_Close_WhenNotActive
	    translate _ = Nothing
    toAttrFrTyp n Root_layout_Close_Never = Just (n, str2attr "never")
    toAttrFrTyp n Root_layout_Close_WhenNotActive = Just (n, str2attr "whenNotActive")
instance XmlAttrType Root_layout_Open where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "always" = Just Root_layout_Open_Always
	    translate "whenActive" = Just Root_layout_Open_WhenActive
	    translate _ = Nothing
    toAttrFrTyp n Root_layout_Open_Always = Just (n, str2attr "always")
    toAttrFrTyp n Root_layout_Open_WhenActive = Just (n, str2attr "whenActive")
instance XmlContent Ref where
    fromElem (CElem (Elem "ref" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "ref" (toAttrs as) [])]
instance XmlAttributes Ref where
    fromAttrs as =
	Ref
	  { refId = possibleA fromAttrToStr "id" as
	  , refClass = possibleA fromAttrToStr "class" as
	  , refTitle = possibleA fromAttrToStr "title" as
	  , refXml'lang = possibleA fromAttrToStr "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (refId v)
	, maybeToAttr toAttrFrStr "class" (refClass v)
	, maybeToAttr toAttrFrStr "title" (refTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (refXml'lang v)
	]
instance XmlContent Audio where
    fromElem (CElem (Elem "audio" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "audio" (toAttrs as) [])]
instance XmlAttributes Audio where
    fromAttrs as =
	Audio
	  { audioId = possibleA fromAttrToStr "id" as
	  , audioClass = possibleA fromAttrToStr "class" as
	  , audioTitle = possibleA fromAttrToStr "title" as
	  , audioXml'lang = possibleA fromAttrToStr "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (audioId v)
	, maybeToAttr toAttrFrStr "class" (audioClass v)
	, maybeToAttr toAttrFrStr "title" (audioTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (audioXml'lang v)
	]
instance XmlContent Img where
    fromElem (CElem (Elem "img" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "img" (toAttrs as) [])]
instance XmlAttributes Img where
    fromAttrs as =
	Img
	  { imgId = possibleA fromAttrToStr "id" as
	  , imgClass = possibleA fromAttrToStr "class" as
	  , imgTitle = possibleA fromAttrToStr "title" as
	  , imgXml'lang = possibleA fromAttrToStr "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (imgId v)
	, maybeToAttr toAttrFrStr "class" (imgClass v)
	, maybeToAttr toAttrFrStr "title" (imgTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (imgXml'lang v)
	]
instance XmlContent Video where
    fromElem (CElem (Elem "video" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "video" (toAttrs as) [])]
instance XmlAttributes Video where
    fromAttrs as =
	Video
	  { videoId = possibleA fromAttrToStr "id" as
	  , videoClass = possibleA fromAttrToStr "class" as
	  , videoTitle = possibleA fromAttrToStr "title" as
	  , videoXml'lang = possibleA fromAttrToStr "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (videoId v)
	, maybeToAttr toAttrFrStr "class" (videoClass v)
	, maybeToAttr toAttrFrStr "title" (videoTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (videoXml'lang v)
	]
instance XmlContent Text where
    fromElem (CElem (Elem "text" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "text" (toAttrs as) [])]
instance XmlAttributes Text where
    fromAttrs as =
	Text
	  { textId = possibleA fromAttrToStr "id" as
	  , textClass = possibleA fromAttrToStr "class" as
	  , textTitle = possibleA fromAttrToStr "title" as
	  , textXml'lang = possibleA fromAttrToStr "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (textId v)
	, maybeToAttr toAttrFrStr "class" (textClass v)
	, maybeToAttr toAttrFrStr "title" (textTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (textXml'lang v)
	]
instance XmlContent Textstream where
    fromElem (CElem (Elem "textstream" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "textstream" (toAttrs as) [])]
instance XmlAttributes Textstream where
    fromAttrs as =
	Textstream
	  { textstreamId = possibleA fromAttrToStr "id" as
	  , textstreamClass = possibleA fromAttrToStr "class" as
	  , textstreamTitle = possibleA fromAttrToStr "title" as
	  , textstreamXml'lang = possibleA fromAttrToStr "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (textstreamId v)
	, maybeToAttr toAttrFrStr "class" (textstreamClass v)
	, maybeToAttr toAttrFrStr "title" (textstreamTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (textstreamXml'lang v)
	]
instance XmlContent Animation where
    fromElem (CElem (Elem "animation" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "animation" (toAttrs as) [])]
instance XmlAttributes Animation where
    fromAttrs as =
	Animation
	  { animationId = possibleA fromAttrToStr "id" as
	  , animationClass = possibleA fromAttrToStr "class" as
	  , animationTitle = possibleA fromAttrToStr "title" as
	  , animationXml'lang = possibleA fromAttrToStr "xml:lang" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (animationId v)
	, maybeToAttr toAttrFrStr "class" (animationClass v)
	, maybeToAttr toAttrFrStr "title" (animationTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (animationXml'lang v)
	]
instance XmlContent Transition where
    fromElem (CElem (Elem "transition" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "transition" (toAttrs as) [])]
instance XmlAttributes Transition where
    fromAttrs as =
	Transition
	  { transitionId = possibleA fromAttrToStr "id" as
	  , transitionClass = possibleA fromAttrToStr "class" as
	  , transitionTitle = possibleA fromAttrToStr "title" as
	  , transitionXml'lang = possibleA fromAttrToStr "xml:lang" as
	  , transitionType = possibleA fromAttrToTyp "type" as
	  , transitionSubtype = possibleA fromAttrToTyp "subtype" as
	  , transitionHorzRepeat = defaultA fromAttrToStr "0" "horzRepeat" as
	  , transitionVertRepeat = defaultA fromAttrToStr "0" "vertRepeat" as
	  , transitionBorderWidth = defaultA fromAttrToStr "0" "borderWidth" as
	  , transitionBorderColor = defaultA fromAttrToStr "black" "borderColor" as
	  , transitionFadeColor = defaultA fromAttrToStr "black" "fadeColor" as
	  , transitionCoordinated = defaultA fromAttrToTyp Transition_Coordinated_False "coordinated" as
	  , transitionClibBoundary = defaultA fromAttrToTyp Transition_ClibBoundary_Children "clibBoundary" as
	  , transitionDur = possibleA fromAttrToStr "dur" as
	  , transitionStartProgress = defaultA fromAttrToStr "0.0" "startProgress" as
	  , transitionEndProgress = defaultA fromAttrToStr "1.0" "endProgress" as
	  , transitionDirection = defaultA fromAttrToTyp Transition_Direction_Forward "direction" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (transitionId v)
	, maybeToAttr toAttrFrStr "class" (transitionClass v)
	, maybeToAttr toAttrFrStr "title" (transitionTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (transitionXml'lang v)
	, maybeToAttr toAttrFrTyp "type" (transitionType v)
	, maybeToAttr toAttrFrTyp "subtype" (transitionSubtype v)
	, defaultToAttr toAttrFrStr "horzRepeat" (transitionHorzRepeat v)
	, defaultToAttr toAttrFrStr "vertRepeat" (transitionVertRepeat v)
	, defaultToAttr toAttrFrStr "borderWidth" (transitionBorderWidth v)
	, defaultToAttr toAttrFrStr "borderColor" (transitionBorderColor v)
	, defaultToAttr toAttrFrStr "fadeColor" (transitionFadeColor v)
	, defaultToAttr toAttrFrTyp "coordinated" (transitionCoordinated v)
	, defaultToAttr toAttrFrTyp "clibBoundary" (transitionClibBoundary v)
	, maybeToAttr toAttrFrStr "dur" (transitionDur v)
	, defaultToAttr toAttrFrStr "startProgress" (transitionStartProgress v)
	, defaultToAttr toAttrFrStr "endProgress" (transitionEndProgress v)
	, defaultToAttr toAttrFrTyp "direction" (transitionDirection v)
	]
instance XmlAttrType Transition_Type where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "barWipe" = Just Transition_Type_BarWipe
	    translate "boxWipe" = Just Transition_Type_BoxWipe
	    translate "fourBoxWipe" = Just Transition_Type_FourBoxWipe
	    translate "barnDoorWipe" = Just Transition_Type_BarnDoorWipe
	    translate "diagonalWipe" = Just Transition_Type_DiagonalWipe
	    translate "bowTieWipe" = Just Transition_Type_BowTieWipe
	    translate "miscDiagonalWipe" = Just Transition_Type_MiscDiagonalWipe
	    translate "veeWipe" = Just Transition_Type_VeeWipe
	    translate "barnVeeWipe" = Just Transition_Type_BarnVeeWipe
	    translate "zigZagWipe" = Just Transition_Type_ZigZagWipe
	    translate "barnZigZagWipe" = Just Transition_Type_BarnZigZagWipe
	    translate "miscShapeWipe" = Just Transition_Type_MiscShapeWipe
	    translate "triangleWipe" = Just Transition_Type_TriangleWipe
	    translate "arrowHeadWipe" = Just Transition_Type_ArrowHeadWipe
	    translate "pentagonWipe" = Just Transition_Type_PentagonWipe
	    translate "hexagonWipe" = Just Transition_Type_HexagonWipe
	    translate "ellipseWipe" = Just Transition_Type_EllipseWipe
	    translate "eyeWipe" = Just Transition_Type_EyeWipe
	    translate "roundRectWipe" = Just Transition_Type_RoundRectWipe
	    translate "starWipe" = Just Transition_Type_StarWipe
	    translate "clockWipe" = Just Transition_Type_ClockWipe
	    translate "pinWheelWipe" = Just Transition_Type_PinWheelWipe
	    translate "singleSweepWipe" = Just Transition_Type_SingleSweepWipe
	    translate "fanWipe" = Just Transition_Type_FanWipe
	    translate "doubleFanWipe" = Just Transition_Type_DoubleFanWipe
	    translate "doubleSweepWipe" = Just Transition_Type_DoubleSweepWipe
	    translate "saloonDoorWipe" = Just Transition_Type_SaloonDoorWipe
	    translate "windshieldWipe" = Just Transition_Type_WindshieldWipe
	    translate "snakeWipe" = Just Transition_Type_SnakeWipe
	    translate "spiralWipe" = Just Transition_Type_SpiralWipe
	    translate "parallelSnakesWipe" = Just Transition_Type_ParallelSnakesWipe
	    translate "boxSnakesWipe" = Just Transition_Type_BoxSnakesWipe
	    translate "waterfallWipe" = Just Transition_Type_WaterfallWipe
	    translate "pushWipe" = Just Transition_Type_PushWipe
	    translate "slideWipe" = Just Transition_Type_SlideWipe
	    translate "fade" = Just Transition_Type_Fade
	    translate _ = Nothing
    toAttrFrTyp n Transition_Type_BarWipe = Just (n, str2attr "barWipe")
    toAttrFrTyp n Transition_Type_BoxWipe = Just (n, str2attr "boxWipe")
    toAttrFrTyp n Transition_Type_FourBoxWipe = Just (n, str2attr "fourBoxWipe")
    toAttrFrTyp n Transition_Type_BarnDoorWipe = Just (n, str2attr "barnDoorWipe")
    toAttrFrTyp n Transition_Type_DiagonalWipe = Just (n, str2attr "diagonalWipe")
    toAttrFrTyp n Transition_Type_BowTieWipe = Just (n, str2attr "bowTieWipe")
    toAttrFrTyp n Transition_Type_MiscDiagonalWipe = Just (n, str2attr "miscDiagonalWipe")
    toAttrFrTyp n Transition_Type_VeeWipe = Just (n, str2attr "veeWipe")
    toAttrFrTyp n Transition_Type_BarnVeeWipe = Just (n, str2attr "barnVeeWipe")
    toAttrFrTyp n Transition_Type_ZigZagWipe = Just (n, str2attr "zigZagWipe")
    toAttrFrTyp n Transition_Type_BarnZigZagWipe = Just (n, str2attr "barnZigZagWipe")
    toAttrFrTyp n Transition_Type_MiscShapeWipe = Just (n, str2attr "miscShapeWipe")
    toAttrFrTyp n Transition_Type_TriangleWipe = Just (n, str2attr "triangleWipe")
    toAttrFrTyp n Transition_Type_ArrowHeadWipe = Just (n, str2attr "arrowHeadWipe")
    toAttrFrTyp n Transition_Type_PentagonWipe = Just (n, str2attr "pentagonWipe")
    toAttrFrTyp n Transition_Type_HexagonWipe = Just (n, str2attr "hexagonWipe")
    toAttrFrTyp n Transition_Type_EllipseWipe = Just (n, str2attr "ellipseWipe")
    toAttrFrTyp n Transition_Type_EyeWipe = Just (n, str2attr "eyeWipe")
    toAttrFrTyp n Transition_Type_RoundRectWipe = Just (n, str2attr "roundRectWipe")
    toAttrFrTyp n Transition_Type_StarWipe = Just (n, str2attr "starWipe")
    toAttrFrTyp n Transition_Type_ClockWipe = Just (n, str2attr "clockWipe")
    toAttrFrTyp n Transition_Type_PinWheelWipe = Just (n, str2attr "pinWheelWipe")
    toAttrFrTyp n Transition_Type_SingleSweepWipe = Just (n, str2attr "singleSweepWipe")
    toAttrFrTyp n Transition_Type_FanWipe = Just (n, str2attr "fanWipe")
    toAttrFrTyp n Transition_Type_DoubleFanWipe = Just (n, str2attr "doubleFanWipe")
    toAttrFrTyp n Transition_Type_DoubleSweepWipe = Just (n, str2attr "doubleSweepWipe")
    toAttrFrTyp n Transition_Type_SaloonDoorWipe = Just (n, str2attr "saloonDoorWipe")
    toAttrFrTyp n Transition_Type_WindshieldWipe = Just (n, str2attr "windshieldWipe")
    toAttrFrTyp n Transition_Type_SnakeWipe = Just (n, str2attr "snakeWipe")
    toAttrFrTyp n Transition_Type_SpiralWipe = Just (n, str2attr "spiralWipe")
    toAttrFrTyp n Transition_Type_ParallelSnakesWipe = Just (n, str2attr "parallelSnakesWipe")
    toAttrFrTyp n Transition_Type_BoxSnakesWipe = Just (n, str2attr "boxSnakesWipe")
    toAttrFrTyp n Transition_Type_WaterfallWipe = Just (n, str2attr "waterfallWipe")
    toAttrFrTyp n Transition_Type_PushWipe = Just (n, str2attr "pushWipe")
    toAttrFrTyp n Transition_Type_SlideWipe = Just (n, str2attr "slideWipe")
    toAttrFrTyp n Transition_Type_Fade = Just (n, str2attr "fade")
instance XmlAttrType Transition_Subtype where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "bottom" = Just Transition_Subtype_Bottom
	    translate "bottomCenter" = Just Transition_Subtype_BottomCenter
	    translate "bottomLeft" = Just Transition_Subtype_BottomLeft
	    translate "bottomLeftClockwise" = Just Transition_Subtype_BottomLeftClockwise
	    translate "bottomLeftCounterClockwise" = Just Transition_Subtype_BottomLeftCounterClockwise
	    translate "bottomLeftDiagonal" = Just Transition_Subtype_BottomLeftDiagonal
	    translate "bottomRight" = Just Transition_Subtype_BottomRight
	    translate "bottomRightClockwise" = Just Transition_Subtype_BottomRightClockwise
	    translate "bottomRightCounterClockwise" = Just Transition_Subtype_BottomRightCounterClockwise
	    translate "bottomRightDiagonal" = Just Transition_Subtype_BottomRightDiagonal
	    translate "centerRight" = Just Transition_Subtype_CenterRight
	    translate "centerTop" = Just Transition_Subtype_CenterTop
	    translate "circle" = Just Transition_Subtype_Circle
	    translate "clockwiseBottom" = Just Transition_Subtype_ClockwiseBottom
	    translate "clockwiseBottomRight" = Just Transition_Subtype_ClockwiseBottomRight
	    translate "clockwiseLeft" = Just Transition_Subtype_ClockwiseLeft
	    translate "clockwiseNine" = Just Transition_Subtype_ClockwiseNine
	    translate "clockwiseRight" = Just Transition_Subtype_ClockwiseRight
	    translate "clockwiseSix" = Just Transition_Subtype_ClockwiseSix
	    translate "clockwiseThree" = Just Transition_Subtype_ClockwiseThree
	    translate "clockwiseTop" = Just Transition_Subtype_ClockwiseTop
	    translate "clockwiseTopLeft" = Just Transition_Subtype_ClockwiseTopLeft
	    translate "clockwiseTwelve" = Just Transition_Subtype_ClockwiseTwelve
	    translate "cornersIn" = Just Transition_Subtype_CornersIn
	    translate "cornersOut" = Just Transition_Subtype_CornersOut
	    translate "counterClockwiseBottomLeft" = Just Transition_Subtype_CounterClockwiseBottomLeft
	    translate "counterClockwiseTopRight" = Just Transition_Subtype_CounterClockwiseTopRight
	    translate "crossfade" = Just Transition_Subtype_Crossfade
	    translate "diagonalBottomLeft" = Just Transition_Subtype_DiagonalBottomLeft
	    translate "diagonalBottomLeftOpposite" = Just Transition_Subtype_DiagonalBottomLeftOpposite
	    translate "diagonalTopLeft" = Just Transition_Subtype_DiagonalTopLeft
	    translate "diagonalTopLeftOpposite" = Just Transition_Subtype_DiagonalTopLeftOpposite
	    translate "diamond" = Just Transition_Subtype_Diamond
	    translate "doubleBarnDoor" = Just Transition_Subtype_DoubleBarnDoor
	    translate "doubleDiamond" = Just Transition_Subtype_DoubleDiamond
	    translate "down" = Just Transition_Subtype_Down
	    translate "fadeFromColor" = Just Transition_Subtype_FadeFromColor
	    translate "fadeToColor" = Just Transition_Subtype_FadeToColor
	    translate "fanInHorizontal" = Just Transition_Subtype_FanInHorizontal
	    translate "fanInVertical" = Just Transition_Subtype_FanInVertical
	    translate "fanOutHorizontal" = Just Transition_Subtype_FanOutHorizontal
	    translate "fanOutVertical" = Just Transition_Subtype_FanOutVertical
	    translate "fivePoint" = Just Transition_Subtype_FivePoint
	    translate "fourBlade" = Just Transition_Subtype_FourBlade
	    translate "fourBoxHorizontal" = Just Transition_Subtype_FourBoxHorizontal
	    translate "fourBoxVertical" = Just Transition_Subtype_FourBoxVertical
	    translate "fourPoint" = Just Transition_Subtype_FourPoint
	    translate "fromBottom" = Just Transition_Subtype_FromBottom
	    translate "fromLeft" = Just Transition_Subtype_FromLeft
	    translate "fromRight" = Just Transition_Subtype_FromRight
	    translate "fromTop" = Just Transition_Subtype_FromTop
	    translate "heart" = Just Transition_Subtype_Heart
	    translate "horizontal" = Just Transition_Subtype_Horizontal
	    translate "horizontalLeft" = Just Transition_Subtype_HorizontalLeft
	    translate "horizontalLeftSame" = Just Transition_Subtype_HorizontalLeftSame
	    translate "horizontalRight" = Just Transition_Subtype_HorizontalRight
	    translate "horizontalRightSame" = Just Transition_Subtype_HorizontalRightSame
	    translate "horizontalTopLeftOpposite" = Just Transition_Subtype_HorizontalTopLeftOpposite
	    translate "horizontalTopRightOpposite" = Just Transition_Subtype_HorizontalTopRightOpposite
	    translate "keyhole" = Just Transition_Subtype_Keyhole
	    translate "left" = Just Transition_Subtype_Left
	    translate "leftCenter" = Just Transition_Subtype_LeftCenter
	    translate "leftToRight" = Just Transition_Subtype_LeftToRight
	    translate "oppositeHorizontal" = Just Transition_Subtype_OppositeHorizontal
	    translate "oppositeVertical" = Just Transition_Subtype_OppositeVertical
	    translate "parallelDiagonal" = Just Transition_Subtype_ParallelDiagonal
	    translate "parallelDiagonalBottomLeft" = Just Transition_Subtype_ParallelDiagonalBottomLeft
	    translate "parallelDiagonalTopLeft" = Just Transition_Subtype_ParallelDiagonalTopLeft
	    translate "parallelVertical" = Just Transition_Subtype_ParallelVertical
	    translate "rectangle" = Just Transition_Subtype_Rectangle
	    translate "right" = Just Transition_Subtype_Right
	    translate "rightCenter" = Just Transition_Subtype_RightCenter
	    translate "sixPoint" = Just Transition_Subtype_SixPoint
	    translate "top" = Just Transition_Subtype_Top
	    translate "topCenter" = Just Transition_Subtype_TopCenter
	    translate "topLeft" = Just Transition_Subtype_TopLeft
	    translate "topLeftClockwise" = Just Transition_Subtype_TopLeftClockwise
	    translate "topLeftCounterClockwise" = Just Transition_Subtype_TopLeftCounterClockwise
	    translate "topLeftDiagonal" = Just Transition_Subtype_TopLeftDiagonal
	    translate "topLeftHorizontal" = Just Transition_Subtype_TopLeftHorizontal
	    translate "topLeftVertical" = Just Transition_Subtype_TopLeftVertical
	    translate "topRight" = Just Transition_Subtype_TopRight
	    translate "topRightClockwise" = Just Transition_Subtype_TopRightClockwise
	    translate "topRightCounterClockwise" = Just Transition_Subtype_TopRightCounterClockwise
	    translate "topRightDiagonal" = Just Transition_Subtype_TopRightDiagonal
	    translate "topToBottom" = Just Transition_Subtype_TopToBottom
	    translate "twoBladeHorizontal" = Just Transition_Subtype_TwoBladeHorizontal
	    translate "twoBladeVertical" = Just Transition_Subtype_TwoBladeVertical
	    translate "twoBoxBottom" = Just Transition_Subtype_TwoBoxBottom
	    translate "twoBoxLeft" = Just Transition_Subtype_TwoBoxLeft
	    translate "twoBoxRight" = Just Transition_Subtype_TwoBoxRight
	    translate "twoBoxTop" = Just Transition_Subtype_TwoBoxTop
	    translate "up" = Just Transition_Subtype_Up
	    translate "vertical" = Just Transition_Subtype_Vertical
	    translate "verticalBottomLeftOpposite" = Just Transition_Subtype_VerticalBottomLeftOpposite
	    translate "verticalBottomSame" = Just Transition_Subtype_VerticalBottomSame
	    translate "verticalLeft" = Just Transition_Subtype_VerticalLeft
	    translate "verticalRight" = Just Transition_Subtype_VerticalRight
	    translate "verticalTopLeftOpposite" = Just Transition_Subtype_VerticalTopLeftOpposite
	    translate "verticalTopSame" = Just Transition_Subtype_VerticalTopSame
	    translate _ = Nothing
    toAttrFrTyp n Transition_Subtype_Bottom = Just (n, str2attr "bottom")
    toAttrFrTyp n Transition_Subtype_BottomCenter = Just (n, str2attr "bottomCenter")
    toAttrFrTyp n Transition_Subtype_BottomLeft = Just (n, str2attr "bottomLeft")
    toAttrFrTyp n Transition_Subtype_BottomLeftClockwise = Just (n, str2attr "bottomLeftClockwise")
    toAttrFrTyp n Transition_Subtype_BottomLeftCounterClockwise = Just (n, str2attr "bottomLeftCounterClockwise")
    toAttrFrTyp n Transition_Subtype_BottomLeftDiagonal = Just (n, str2attr "bottomLeftDiagonal")
    toAttrFrTyp n Transition_Subtype_BottomRight = Just (n, str2attr "bottomRight")
    toAttrFrTyp n Transition_Subtype_BottomRightClockwise = Just (n, str2attr "bottomRightClockwise")
    toAttrFrTyp n Transition_Subtype_BottomRightCounterClockwise = Just (n, str2attr "bottomRightCounterClockwise")
    toAttrFrTyp n Transition_Subtype_BottomRightDiagonal = Just (n, str2attr "bottomRightDiagonal")
    toAttrFrTyp n Transition_Subtype_CenterRight = Just (n, str2attr "centerRight")
    toAttrFrTyp n Transition_Subtype_CenterTop = Just (n, str2attr "centerTop")
    toAttrFrTyp n Transition_Subtype_Circle = Just (n, str2attr "circle")
    toAttrFrTyp n Transition_Subtype_ClockwiseBottom = Just (n, str2attr "clockwiseBottom")
    toAttrFrTyp n Transition_Subtype_ClockwiseBottomRight = Just (n, str2attr "clockwiseBottomRight")
    toAttrFrTyp n Transition_Subtype_ClockwiseLeft = Just (n, str2attr "clockwiseLeft")
    toAttrFrTyp n Transition_Subtype_ClockwiseNine = Just (n, str2attr "clockwiseNine")
    toAttrFrTyp n Transition_Subtype_ClockwiseRight = Just (n, str2attr "clockwiseRight")
    toAttrFrTyp n Transition_Subtype_ClockwiseSix = Just (n, str2attr "clockwiseSix")
    toAttrFrTyp n Transition_Subtype_ClockwiseThree = Just (n, str2attr "clockwiseThree")
    toAttrFrTyp n Transition_Subtype_ClockwiseTop = Just (n, str2attr "clockwiseTop")
    toAttrFrTyp n Transition_Subtype_ClockwiseTopLeft = Just (n, str2attr "clockwiseTopLeft")
    toAttrFrTyp n Transition_Subtype_ClockwiseTwelve = Just (n, str2attr "clockwiseTwelve")
    toAttrFrTyp n Transition_Subtype_CornersIn = Just (n, str2attr "cornersIn")
    toAttrFrTyp n Transition_Subtype_CornersOut = Just (n, str2attr "cornersOut")
    toAttrFrTyp n Transition_Subtype_CounterClockwiseBottomLeft = Just (n, str2attr "counterClockwiseBottomLeft")
    toAttrFrTyp n Transition_Subtype_CounterClockwiseTopRight = Just (n, str2attr "counterClockwiseTopRight")
    toAttrFrTyp n Transition_Subtype_Crossfade = Just (n, str2attr "crossfade")
    toAttrFrTyp n Transition_Subtype_DiagonalBottomLeft = Just (n, str2attr "diagonalBottomLeft")
    toAttrFrTyp n Transition_Subtype_DiagonalBottomLeftOpposite = Just (n, str2attr "diagonalBottomLeftOpposite")
    toAttrFrTyp n Transition_Subtype_DiagonalTopLeft = Just (n, str2attr "diagonalTopLeft")
    toAttrFrTyp n Transition_Subtype_DiagonalTopLeftOpposite = Just (n, str2attr "diagonalTopLeftOpposite")
    toAttrFrTyp n Transition_Subtype_Diamond = Just (n, str2attr "diamond")
    toAttrFrTyp n Transition_Subtype_DoubleBarnDoor = Just (n, str2attr "doubleBarnDoor")
    toAttrFrTyp n Transition_Subtype_DoubleDiamond = Just (n, str2attr "doubleDiamond")
    toAttrFrTyp n Transition_Subtype_Down = Just (n, str2attr "down")
    toAttrFrTyp n Transition_Subtype_FadeFromColor = Just (n, str2attr "fadeFromColor")
    toAttrFrTyp n Transition_Subtype_FadeToColor = Just (n, str2attr "fadeToColor")
    toAttrFrTyp n Transition_Subtype_FanInHorizontal = Just (n, str2attr "fanInHorizontal")
    toAttrFrTyp n Transition_Subtype_FanInVertical = Just (n, str2attr "fanInVertical")
    toAttrFrTyp n Transition_Subtype_FanOutHorizontal = Just (n, str2attr "fanOutHorizontal")
    toAttrFrTyp n Transition_Subtype_FanOutVertical = Just (n, str2attr "fanOutVertical")
    toAttrFrTyp n Transition_Subtype_FivePoint = Just (n, str2attr "fivePoint")
    toAttrFrTyp n Transition_Subtype_FourBlade = Just (n, str2attr "fourBlade")
    toAttrFrTyp n Transition_Subtype_FourBoxHorizontal = Just (n, str2attr "fourBoxHorizontal")
    toAttrFrTyp n Transition_Subtype_FourBoxVertical = Just (n, str2attr "fourBoxVertical")
    toAttrFrTyp n Transition_Subtype_FourPoint = Just (n, str2attr "fourPoint")
    toAttrFrTyp n Transition_Subtype_FromBottom = Just (n, str2attr "fromBottom")
    toAttrFrTyp n Transition_Subtype_FromLeft = Just (n, str2attr "fromLeft")
    toAttrFrTyp n Transition_Subtype_FromRight = Just (n, str2attr "fromRight")
    toAttrFrTyp n Transition_Subtype_FromTop = Just (n, str2attr "fromTop")
    toAttrFrTyp n Transition_Subtype_Heart = Just (n, str2attr "heart")
    toAttrFrTyp n Transition_Subtype_Horizontal = Just (n, str2attr "horizontal")
    toAttrFrTyp n Transition_Subtype_HorizontalLeft = Just (n, str2attr "horizontalLeft")
    toAttrFrTyp n Transition_Subtype_HorizontalLeftSame = Just (n, str2attr "horizontalLeftSame")
    toAttrFrTyp n Transition_Subtype_HorizontalRight = Just (n, str2attr "horizontalRight")
    toAttrFrTyp n Transition_Subtype_HorizontalRightSame = Just (n, str2attr "horizontalRightSame")
    toAttrFrTyp n Transition_Subtype_HorizontalTopLeftOpposite = Just (n, str2attr "horizontalTopLeftOpposite")
    toAttrFrTyp n Transition_Subtype_HorizontalTopRightOpposite = Just (n, str2attr "horizontalTopRightOpposite")
    toAttrFrTyp n Transition_Subtype_Keyhole = Just (n, str2attr "keyhole")
    toAttrFrTyp n Transition_Subtype_Left = Just (n, str2attr "left")
    toAttrFrTyp n Transition_Subtype_LeftCenter = Just (n, str2attr "leftCenter")
    toAttrFrTyp n Transition_Subtype_LeftToRight = Just (n, str2attr "leftToRight")
    toAttrFrTyp n Transition_Subtype_OppositeHorizontal = Just (n, str2attr "oppositeHorizontal")
    toAttrFrTyp n Transition_Subtype_OppositeVertical = Just (n, str2attr "oppositeVertical")
    toAttrFrTyp n Transition_Subtype_ParallelDiagonal = Just (n, str2attr "parallelDiagonal")
    toAttrFrTyp n Transition_Subtype_ParallelDiagonalBottomLeft = Just (n, str2attr "parallelDiagonalBottomLeft")
    toAttrFrTyp n Transition_Subtype_ParallelDiagonalTopLeft = Just (n, str2attr "parallelDiagonalTopLeft")
    toAttrFrTyp n Transition_Subtype_ParallelVertical = Just (n, str2attr "parallelVertical")
    toAttrFrTyp n Transition_Subtype_Rectangle = Just (n, str2attr "rectangle")
    toAttrFrTyp n Transition_Subtype_Right = Just (n, str2attr "right")
    toAttrFrTyp n Transition_Subtype_RightCenter = Just (n, str2attr "rightCenter")
    toAttrFrTyp n Transition_Subtype_SixPoint = Just (n, str2attr "sixPoint")
    toAttrFrTyp n Transition_Subtype_Top = Just (n, str2attr "top")
    toAttrFrTyp n Transition_Subtype_TopCenter = Just (n, str2attr "topCenter")
    toAttrFrTyp n Transition_Subtype_TopLeft = Just (n, str2attr "topLeft")
    toAttrFrTyp n Transition_Subtype_TopLeftClockwise = Just (n, str2attr "topLeftClockwise")
    toAttrFrTyp n Transition_Subtype_TopLeftCounterClockwise = Just (n, str2attr "topLeftCounterClockwise")
    toAttrFrTyp n Transition_Subtype_TopLeftDiagonal = Just (n, str2attr "topLeftDiagonal")
    toAttrFrTyp n Transition_Subtype_TopLeftHorizontal = Just (n, str2attr "topLeftHorizontal")
    toAttrFrTyp n Transition_Subtype_TopLeftVertical = Just (n, str2attr "topLeftVertical")
    toAttrFrTyp n Transition_Subtype_TopRight = Just (n, str2attr "topRight")
    toAttrFrTyp n Transition_Subtype_TopRightClockwise = Just (n, str2attr "topRightClockwise")
    toAttrFrTyp n Transition_Subtype_TopRightCounterClockwise = Just (n, str2attr "topRightCounterClockwise")
    toAttrFrTyp n Transition_Subtype_TopRightDiagonal = Just (n, str2attr "topRightDiagonal")
    toAttrFrTyp n Transition_Subtype_TopToBottom = Just (n, str2attr "topToBottom")
    toAttrFrTyp n Transition_Subtype_TwoBladeHorizontal = Just (n, str2attr "twoBladeHorizontal")
    toAttrFrTyp n Transition_Subtype_TwoBladeVertical = Just (n, str2attr "twoBladeVertical")
    toAttrFrTyp n Transition_Subtype_TwoBoxBottom = Just (n, str2attr "twoBoxBottom")
    toAttrFrTyp n Transition_Subtype_TwoBoxLeft = Just (n, str2attr "twoBoxLeft")
    toAttrFrTyp n Transition_Subtype_TwoBoxRight = Just (n, str2attr "twoBoxRight")
    toAttrFrTyp n Transition_Subtype_TwoBoxTop = Just (n, str2attr "twoBoxTop")
    toAttrFrTyp n Transition_Subtype_Up = Just (n, str2attr "up")
    toAttrFrTyp n Transition_Subtype_Vertical = Just (n, str2attr "vertical")
    toAttrFrTyp n Transition_Subtype_VerticalBottomLeftOpposite = Just (n, str2attr "verticalBottomLeftOpposite")
    toAttrFrTyp n Transition_Subtype_VerticalBottomSame = Just (n, str2attr "verticalBottomSame")
    toAttrFrTyp n Transition_Subtype_VerticalLeft = Just (n, str2attr "verticalLeft")
    toAttrFrTyp n Transition_Subtype_VerticalRight = Just (n, str2attr "verticalRight")
    toAttrFrTyp n Transition_Subtype_VerticalTopLeftOpposite = Just (n, str2attr "verticalTopLeftOpposite")
    toAttrFrTyp n Transition_Subtype_VerticalTopSame = Just (n, str2attr "verticalTopSame")
instance XmlAttrType Transition_Coordinated where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "true" = Just Transition_Coordinated_True
	    translate "false" = Just Transition_Coordinated_False
	    translate _ = Nothing
    toAttrFrTyp n Transition_Coordinated_True = Just (n, str2attr "true")
    toAttrFrTyp n Transition_Coordinated_False = Just (n, str2attr "false")
instance XmlAttrType Transition_ClibBoundary where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "parent" = Just Transition_ClibBoundary_Parent
	    translate "children" = Just Transition_ClibBoundary_Children
	    translate _ = Nothing
    toAttrFrTyp n Transition_ClibBoundary_Parent = Just (n, str2attr "parent")
    toAttrFrTyp n Transition_ClibBoundary_Children = Just (n, str2attr "children")
instance XmlAttrType Transition_Direction where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "forward" = Just Transition_Direction_Forward
	    translate "reverse" = Just Transition_Direction_Reverse
	    translate _ = Nothing
    toAttrFrTyp n Transition_Direction_Forward = Just (n, str2attr "forward")
    toAttrFrTyp n Transition_Direction_Reverse = Just (n, str2attr "reverse")
instance XmlContent TransitionFilter where
    fromElem (CElem (Elem "transitionFilter" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "transitionFilter" (toAttrs as) [])]
instance XmlAttributes TransitionFilter where
    fromAttrs as =
	TransitionFilter
	  { transitionFilterId = possibleA fromAttrToStr "id" as
	  , transitionFilterClass = possibleA fromAttrToStr "class" as
	  , transitionFilterTitle = possibleA fromAttrToStr "title" as
	  , transitionFilterXml'lang = possibleA fromAttrToStr "xml:lang" as
	  , transitionFilterType = possibleA fromAttrToTyp "type" as
	  , transitionFilterSubtype = possibleA fromAttrToTyp "subtype" as
	  , transitionFilterHorzRepeat = defaultA fromAttrToStr "0" "horzRepeat" as
	  , transitionFilterVertRepeat = defaultA fromAttrToStr "0" "vertRepeat" as
	  , transitionFilterBorderWidth = defaultA fromAttrToStr "0" "borderWidth" as
	  , transitionFilterBorderColor = defaultA fromAttrToStr "black" "borderColor" as
	  , transitionFilterFadeColor = defaultA fromAttrToStr "black" "fadeColor" as
	  , transitionFilterCoordinated = defaultA fromAttrToTyp TransitionFilter_Coordinated_False "coordinated" as
	  , transitionFilterClibBoundary = defaultA fromAttrToTyp TransitionFilter_ClibBoundary_Children "clibBoundary" as
	  , transitionFilterDur = possibleA fromAttrToStr "dur" as
	  , transitionFilterRepeatCount = possibleA fromAttrToStr "repeatCount" as
	  , transitionFilterRepeatDur = possibleA fromAttrToStr "repeatDur" as
	  , transitionFilterBegin = possibleA fromAttrToStr "begin" as
	  , transitionFilterEnd = possibleA fromAttrToStr "end" as
	  , transitionFilterValues = possibleA fromAttrToStr "values" as
	  , transitionFilterFrom = possibleA fromAttrToStr "from" as
	  , transitionFilterTo = possibleA fromAttrToStr "to" as
	  , transitionFilterBy = possibleA fromAttrToStr "by" as
	  , transitionFilterCalcMode = defaultA fromAttrToTyp TransitionFilter_CalcMode_Linear "calcMode" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "id" (transitionFilterId v)
	, maybeToAttr toAttrFrStr "class" (transitionFilterClass v)
	, maybeToAttr toAttrFrStr "title" (transitionFilterTitle v)
	, maybeToAttr toAttrFrStr "xml:lang" (transitionFilterXml'lang v)
	, maybeToAttr toAttrFrTyp "type" (transitionFilterType v)
	, maybeToAttr toAttrFrTyp "subtype" (transitionFilterSubtype v)
	, defaultToAttr toAttrFrStr "horzRepeat" (transitionFilterHorzRepeat v)
	, defaultToAttr toAttrFrStr "vertRepeat" (transitionFilterVertRepeat v)
	, defaultToAttr toAttrFrStr "borderWidth" (transitionFilterBorderWidth v)
	, defaultToAttr toAttrFrStr "borderColor" (transitionFilterBorderColor v)
	, defaultToAttr toAttrFrStr "fadeColor" (transitionFilterFadeColor v)
	, defaultToAttr toAttrFrTyp "coordinated" (transitionFilterCoordinated v)
	, defaultToAttr toAttrFrTyp "clibBoundary" (transitionFilterClibBoundary v)
	, maybeToAttr toAttrFrStr "dur" (transitionFilterDur v)
	, maybeToAttr toAttrFrStr "repeatCount" (transitionFilterRepeatCount v)
	, maybeToAttr toAttrFrStr "repeatDur" (transitionFilterRepeatDur v)
	, maybeToAttr toAttrFrStr "begin" (transitionFilterBegin v)
	, maybeToAttr toAttrFrStr "end" (transitionFilterEnd v)
	, maybeToAttr toAttrFrStr "values" (transitionFilterValues v)
	, maybeToAttr toAttrFrStr "from" (transitionFilterFrom v)
	, maybeToAttr toAttrFrStr "to" (transitionFilterTo v)
	, maybeToAttr toAttrFrStr "by" (transitionFilterBy v)
	, defaultToAttr toAttrFrTyp "calcMode" (transitionFilterCalcMode v)
	]
instance XmlAttrType TransitionFilter_Type where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "barWipe" = Just TransitionFilter_Type_BarWipe
	    translate "boxWipe" = Just TransitionFilter_Type_BoxWipe
	    translate "fourBoxWipe" = Just TransitionFilter_Type_FourBoxWipe
	    translate "barnDoorWipe" = Just TransitionFilter_Type_BarnDoorWipe
	    translate "diagonalWipe" = Just TransitionFilter_Type_DiagonalWipe
	    translate "bowTieWipe" = Just TransitionFilter_Type_BowTieWipe
	    translate "miscDiagonalWipe" = Just TransitionFilter_Type_MiscDiagonalWipe
	    translate "veeWipe" = Just TransitionFilter_Type_VeeWipe
	    translate "barnVeeWipe" = Just TransitionFilter_Type_BarnVeeWipe
	    translate "zigZagWipe" = Just TransitionFilter_Type_ZigZagWipe
	    translate "barnZigZagWipe" = Just TransitionFilter_Type_BarnZigZagWipe
	    translate "miscShapeWipe" = Just TransitionFilter_Type_MiscShapeWipe
	    translate "triangleWipe" = Just TransitionFilter_Type_TriangleWipe
	    translate "arrowHeadWipe" = Just TransitionFilter_Type_ArrowHeadWipe
	    translate "pentagonWipe" = Just TransitionFilter_Type_PentagonWipe
	    translate "hexagonWipe" = Just TransitionFilter_Type_HexagonWipe
	    translate "ellipseWipe" = Just TransitionFilter_Type_EllipseWipe
	    translate "eyeWipe" = Just TransitionFilter_Type_EyeWipe
	    translate "roundRectWipe" = Just TransitionFilter_Type_RoundRectWipe
	    translate "starWipe" = Just TransitionFilter_Type_StarWipe
	    translate "clockWipe" = Just TransitionFilter_Type_ClockWipe
	    translate "pinWheelWipe" = Just TransitionFilter_Type_PinWheelWipe
	    translate "singleSweepWipe" = Just TransitionFilter_Type_SingleSweepWipe
	    translate "fanWipe" = Just TransitionFilter_Type_FanWipe
	    translate "doubleFanWipe" = Just TransitionFilter_Type_DoubleFanWipe
	    translate "doubleSweepWipe" = Just TransitionFilter_Type_DoubleSweepWipe
	    translate "saloonDoorWipe" = Just TransitionFilter_Type_SaloonDoorWipe
	    translate "windshieldWipe" = Just TransitionFilter_Type_WindshieldWipe
	    translate "snakeWipe" = Just TransitionFilter_Type_SnakeWipe
	    translate "spiralWipe" = Just TransitionFilter_Type_SpiralWipe
	    translate "parallelSnakesWipe" = Just TransitionFilter_Type_ParallelSnakesWipe
	    translate "boxSnakesWipe" = Just TransitionFilter_Type_BoxSnakesWipe
	    translate "waterfallWipe" = Just TransitionFilter_Type_WaterfallWipe
	    translate "pushWipe" = Just TransitionFilter_Type_PushWipe
	    translate "slideWipe" = Just TransitionFilter_Type_SlideWipe
	    translate "fade" = Just TransitionFilter_Type_Fade
	    translate _ = Nothing
    toAttrFrTyp n TransitionFilter_Type_BarWipe = Just (n, str2attr "barWipe")
    toAttrFrTyp n TransitionFilter_Type_BoxWipe = Just (n, str2attr "boxWipe")
    toAttrFrTyp n TransitionFilter_Type_FourBoxWipe = Just (n, str2attr "fourBoxWipe")
    toAttrFrTyp n TransitionFilter_Type_BarnDoorWipe = Just (n, str2attr "barnDoorWipe")
    toAttrFrTyp n TransitionFilter_Type_DiagonalWipe = Just (n, str2attr "diagonalWipe")
    toAttrFrTyp n TransitionFilter_Type_BowTieWipe = Just (n, str2attr "bowTieWipe")
    toAttrFrTyp n TransitionFilter_Type_MiscDiagonalWipe = Just (n, str2attr "miscDiagonalWipe")
    toAttrFrTyp n TransitionFilter_Type_VeeWipe = Just (n, str2attr "veeWipe")
    toAttrFrTyp n TransitionFilter_Type_BarnVeeWipe = Just (n, str2attr "barnVeeWipe")
    toAttrFrTyp n TransitionFilter_Type_ZigZagWipe = Just (n, str2attr "zigZagWipe")
    toAttrFrTyp n TransitionFilter_Type_BarnZigZagWipe = Just (n, str2attr "barnZigZagWipe")
    toAttrFrTyp n TransitionFilter_Type_MiscShapeWipe = Just (n, str2attr "miscShapeWipe")
    toAttrFrTyp n TransitionFilter_Type_TriangleWipe = Just (n, str2attr "triangleWipe")
    toAttrFrTyp n TransitionFilter_Type_ArrowHeadWipe = Just (n, str2attr "arrowHeadWipe")
    toAttrFrTyp n TransitionFilter_Type_PentagonWipe = Just (n, str2attr "pentagonWipe")
    toAttrFrTyp n TransitionFilter_Type_HexagonWipe = Just (n, str2attr "hexagonWipe")
    toAttrFrTyp n TransitionFilter_Type_EllipseWipe = Just (n, str2attr "ellipseWipe")
    toAttrFrTyp n TransitionFilter_Type_EyeWipe = Just (n, str2attr "eyeWipe")
    toAttrFrTyp n TransitionFilter_Type_RoundRectWipe = Just (n, str2attr "roundRectWipe")
    toAttrFrTyp n TransitionFilter_Type_StarWipe = Just (n, str2attr "starWipe")
    toAttrFrTyp n TransitionFilter_Type_ClockWipe = Just (n, str2attr "clockWipe")
    toAttrFrTyp n TransitionFilter_Type_PinWheelWipe = Just (n, str2attr "pinWheelWipe")
    toAttrFrTyp n TransitionFilter_Type_SingleSweepWipe = Just (n, str2attr "singleSweepWipe")
    toAttrFrTyp n TransitionFilter_Type_FanWipe = Just (n, str2attr "fanWipe")
    toAttrFrTyp n TransitionFilter_Type_DoubleFanWipe = Just (n, str2attr "doubleFanWipe")
    toAttrFrTyp n TransitionFilter_Type_DoubleSweepWipe = Just (n, str2attr "doubleSweepWipe")
    toAttrFrTyp n TransitionFilter_Type_SaloonDoorWipe = Just (n, str2attr "saloonDoorWipe")
    toAttrFrTyp n TransitionFilter_Type_WindshieldWipe = Just (n, str2attr "windshieldWipe")
    toAttrFrTyp n TransitionFilter_Type_SnakeWipe = Just (n, str2attr "snakeWipe")
    toAttrFrTyp n TransitionFilter_Type_SpiralWipe = Just (n, str2attr "spiralWipe")
    toAttrFrTyp n TransitionFilter_Type_ParallelSnakesWipe = Just (n, str2attr "parallelSnakesWipe")
    toAttrFrTyp n TransitionFilter_Type_BoxSnakesWipe = Just (n, str2attr "boxSnakesWipe")
    toAttrFrTyp n TransitionFilter_Type_WaterfallWipe = Just (n, str2attr "waterfallWipe")
    toAttrFrTyp n TransitionFilter_Type_PushWipe = Just (n, str2attr "pushWipe")
    toAttrFrTyp n TransitionFilter_Type_SlideWipe = Just (n, str2attr "slideWipe")
    toAttrFrTyp n TransitionFilter_Type_Fade = Just (n, str2attr "fade")
instance XmlAttrType TransitionFilter_Subtype where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "bottom" = Just TransitionFilter_Subtype_Bottom
	    translate "bottomCenter" = Just TransitionFilter_Subtype_BottomCenter
	    translate "bottomLeft" = Just TransitionFilter_Subtype_BottomLeft
	    translate "bottomLeftClockwise" = Just TransitionFilter_Subtype_BottomLeftClockwise
	    translate "bottomLeftCounterClockwise" = Just TransitionFilter_Subtype_BottomLeftCounterClockwise
	    translate "bottomLeftDiagonal" = Just TransitionFilter_Subtype_BottomLeftDiagonal
	    translate "bottomRight" = Just TransitionFilter_Subtype_BottomRight
	    translate "bottomRightClockwise" = Just TransitionFilter_Subtype_BottomRightClockwise
	    translate "bottomRightCounterClockwise" = Just TransitionFilter_Subtype_BottomRightCounterClockwise
	    translate "bottomRightDiagonal" = Just TransitionFilter_Subtype_BottomRightDiagonal
	    translate "centerRight" = Just TransitionFilter_Subtype_CenterRight
	    translate "centerTop" = Just TransitionFilter_Subtype_CenterTop
	    translate "circle" = Just TransitionFilter_Subtype_Circle
	    translate "clockwiseBottom" = Just TransitionFilter_Subtype_ClockwiseBottom
	    translate "clockwiseBottomRight" = Just TransitionFilter_Subtype_ClockwiseBottomRight
	    translate "clockwiseLeft" = Just TransitionFilter_Subtype_ClockwiseLeft
	    translate "clockwiseNine" = Just TransitionFilter_Subtype_ClockwiseNine
	    translate "clockwiseRight" = Just TransitionFilter_Subtype_ClockwiseRight
	    translate "clockwiseSix" = Just TransitionFilter_Subtype_ClockwiseSix
	    translate "clockwiseThree" = Just TransitionFilter_Subtype_ClockwiseThree
	    translate "clockwiseTop" = Just TransitionFilter_Subtype_ClockwiseTop
	    translate "clockwiseTopLeft" = Just TransitionFilter_Subtype_ClockwiseTopLeft
	    translate "clockwiseTwelve" = Just TransitionFilter_Subtype_ClockwiseTwelve
	    translate "cornersIn" = Just TransitionFilter_Subtype_CornersIn
	    translate "cornersOut" = Just TransitionFilter_Subtype_CornersOut
	    translate "counterClockwiseBottomLeft" = Just TransitionFilter_Subtype_CounterClockwiseBottomLeft
	    translate "counterClockwiseTopRight" = Just TransitionFilter_Subtype_CounterClockwiseTopRight
	    translate "crossfade" = Just TransitionFilter_Subtype_Crossfade
	    translate "diagonalBottomLeft" = Just TransitionFilter_Subtype_DiagonalBottomLeft
	    translate "diagonalBottomLeftOpposite" = Just TransitionFilter_Subtype_DiagonalBottomLeftOpposite
	    translate "diagonalTopLeft" = Just TransitionFilter_Subtype_DiagonalTopLeft
	    translate "diagonalTopLeftOpposite" = Just TransitionFilter_Subtype_DiagonalTopLeftOpposite
	    translate "diamond" = Just TransitionFilter_Subtype_Diamond
	    translate "doubleBarnDoor" = Just TransitionFilter_Subtype_DoubleBarnDoor
	    translate "doubleDiamond" = Just TransitionFilter_Subtype_DoubleDiamond
	    translate "down" = Just TransitionFilter_Subtype_Down
	    translate "fadeFromColor" = Just TransitionFilter_Subtype_FadeFromColor
	    translate "fadeToColor" = Just TransitionFilter_Subtype_FadeToColor
	    translate "fanInHorizontal" = Just TransitionFilter_Subtype_FanInHorizontal
	    translate "fanInVertical" = Just TransitionFilter_Subtype_FanInVertical
	    translate "fanOutHorizontal" = Just TransitionFilter_Subtype_FanOutHorizontal
	    translate "fanOutVertical" = Just TransitionFilter_Subtype_FanOutVertical
	    translate "fivePoint" = Just TransitionFilter_Subtype_FivePoint
	    translate "fourBlade" = Just TransitionFilter_Subtype_FourBlade
	    translate "fourBoxHorizontal" = Just TransitionFilter_Subtype_FourBoxHorizontal
	    translate "fourBoxVertical" = Just TransitionFilter_Subtype_FourBoxVertical
	    translate "fourPoint" = Just TransitionFilter_Subtype_FourPoint
	    translate "fromBottom" = Just TransitionFilter_Subtype_FromBottom
	    translate "fromLeft" = Just TransitionFilter_Subtype_FromLeft
	    translate "fromRight" = Just TransitionFilter_Subtype_FromRight
	    translate "fromTop" = Just TransitionFilter_Subtype_FromTop
	    translate "heart" = Just TransitionFilter_Subtype_Heart
	    translate "horizontal" = Just TransitionFilter_Subtype_Horizontal
	    translate "horizontalLeft" = Just TransitionFilter_Subtype_HorizontalLeft
	    translate "horizontalLeftSame" = Just TransitionFilter_Subtype_HorizontalLeftSame
	    translate "horizontalRight" = Just TransitionFilter_Subtype_HorizontalRight
	    translate "horizontalRightSame" = Just TransitionFilter_Subtype_HorizontalRightSame
	    translate "horizontalTopLeftOpposite" = Just TransitionFilter_Subtype_HorizontalTopLeftOpposite
	    translate "horizontalTopRightOpposite" = Just TransitionFilter_Subtype_HorizontalTopRightOpposite
	    translate "keyhole" = Just TransitionFilter_Subtype_Keyhole
	    translate "left" = Just TransitionFilter_Subtype_Left
	    translate "leftCenter" = Just TransitionFilter_Subtype_LeftCenter
	    translate "leftToRight" = Just TransitionFilter_Subtype_LeftToRight
	    translate "oppositeHorizontal" = Just TransitionFilter_Subtype_OppositeHorizontal
	    translate "oppositeVertical" = Just TransitionFilter_Subtype_OppositeVertical
	    translate "parallelDiagonal" = Just TransitionFilter_Subtype_ParallelDiagonal
	    translate "parallelDiagonalBottomLeft" = Just TransitionFilter_Subtype_ParallelDiagonalBottomLeft
	    translate "parallelDiagonalTopLeft" = Just TransitionFilter_Subtype_ParallelDiagonalTopLeft
	    translate "parallelVertical" = Just TransitionFilter_Subtype_ParallelVertical
	    translate "rectangle" = Just TransitionFilter_Subtype_Rectangle
	    translate "right" = Just TransitionFilter_Subtype_Right
	    translate "rightCenter" = Just TransitionFilter_Subtype_RightCenter
	    translate "sixPoint" = Just TransitionFilter_Subtype_SixPoint
	    translate "top" = Just TransitionFilter_Subtype_Top
	    translate "topCenter" = Just TransitionFilter_Subtype_TopCenter
	    translate "topLeft" = Just TransitionFilter_Subtype_TopLeft
	    translate "topLeftClockwise" = Just TransitionFilter_Subtype_TopLeftClockwise
	    translate "topLeftCounterClockwise" = Just TransitionFilter_Subtype_TopLeftCounterClockwise
	    translate "topLeftDiagonal" = Just TransitionFilter_Subtype_TopLeftDiagonal
	    translate "topLeftHorizontal" = Just TransitionFilter_Subtype_TopLeftHorizontal
	    translate "topLeftVertical" = Just TransitionFilter_Subtype_TopLeftVertical
	    translate "topRight" = Just TransitionFilter_Subtype_TopRight
	    translate "topRightClockwise" = Just TransitionFilter_Subtype_TopRightClockwise
	    translate "topRightCounterClockwise" = Just TransitionFilter_Subtype_TopRightCounterClockwise
	    translate "topRightDiagonal" = Just TransitionFilter_Subtype_TopRightDiagonal
	    translate "topToBottom" = Just TransitionFilter_Subtype_TopToBottom
	    translate "twoBladeHorizontal" = Just TransitionFilter_Subtype_TwoBladeHorizontal
	    translate "twoBladeVertical" = Just TransitionFilter_Subtype_TwoBladeVertical
	    translate "twoBoxBottom" = Just TransitionFilter_Subtype_TwoBoxBottom
	    translate "twoBoxLeft" = Just TransitionFilter_Subtype_TwoBoxLeft
	    translate "twoBoxRight" = Just TransitionFilter_Subtype_TwoBoxRight
	    translate "twoBoxTop" = Just TransitionFilter_Subtype_TwoBoxTop
	    translate "up" = Just TransitionFilter_Subtype_Up
	    translate "vertical" = Just TransitionFilter_Subtype_Vertical
	    translate "verticalBottomLeftOpposite" = Just TransitionFilter_Subtype_VerticalBottomLeftOpposite
	    translate "verticalBottomSame" = Just TransitionFilter_Subtype_VerticalBottomSame
	    translate "verticalLeft" = Just TransitionFilter_Subtype_VerticalLeft
	    translate "verticalRight" = Just TransitionFilter_Subtype_VerticalRight
	    translate "verticalTopLeftOpposite" = Just TransitionFilter_Subtype_VerticalTopLeftOpposite
	    translate "verticalTopSame" = Just TransitionFilter_Subtype_VerticalTopSame
	    translate _ = Nothing
    toAttrFrTyp n TransitionFilter_Subtype_Bottom = Just (n, str2attr "bottom")
    toAttrFrTyp n TransitionFilter_Subtype_BottomCenter = Just (n, str2attr "bottomCenter")
    toAttrFrTyp n TransitionFilter_Subtype_BottomLeft = Just (n, str2attr "bottomLeft")
    toAttrFrTyp n TransitionFilter_Subtype_BottomLeftClockwise = Just (n, str2attr "bottomLeftClockwise")
    toAttrFrTyp n TransitionFilter_Subtype_BottomLeftCounterClockwise = Just (n, str2attr "bottomLeftCounterClockwise")
    toAttrFrTyp n TransitionFilter_Subtype_BottomLeftDiagonal = Just (n, str2attr "bottomLeftDiagonal")
    toAttrFrTyp n TransitionFilter_Subtype_BottomRight = Just (n, str2attr "bottomRight")
    toAttrFrTyp n TransitionFilter_Subtype_BottomRightClockwise = Just (n, str2attr "bottomRightClockwise")
    toAttrFrTyp n TransitionFilter_Subtype_BottomRightCounterClockwise = Just (n, str2attr "bottomRightCounterClockwise")
    toAttrFrTyp n TransitionFilter_Subtype_BottomRightDiagonal = Just (n, str2attr "bottomRightDiagonal")
    toAttrFrTyp n TransitionFilter_Subtype_CenterRight = Just (n, str2attr "centerRight")
    toAttrFrTyp n TransitionFilter_Subtype_CenterTop = Just (n, str2attr "centerTop")
    toAttrFrTyp n TransitionFilter_Subtype_Circle = Just (n, str2attr "circle")
    toAttrFrTyp n TransitionFilter_Subtype_ClockwiseBottom = Just (n, str2attr "clockwiseBottom")
    toAttrFrTyp n TransitionFilter_Subtype_ClockwiseBottomRight = Just (n, str2attr "clockwiseBottomRight")
    toAttrFrTyp n TransitionFilter_Subtype_ClockwiseLeft = Just (n, str2attr "clockwiseLeft")
    toAttrFrTyp n TransitionFilter_Subtype_ClockwiseNine = Just (n, str2attr "clockwiseNine")
    toAttrFrTyp n TransitionFilter_Subtype_ClockwiseRight = Just (n, str2attr "clockwiseRight")
    toAttrFrTyp n TransitionFilter_Subtype_ClockwiseSix = Just (n, str2attr "clockwiseSix")
    toAttrFrTyp n TransitionFilter_Subtype_ClockwiseThree = Just (n, str2attr "clockwiseThree")
    toAttrFrTyp n TransitionFilter_Subtype_ClockwiseTop = Just (n, str2attr "clockwiseTop")
    toAttrFrTyp n TransitionFilter_Subtype_ClockwiseTopLeft = Just (n, str2attr "clockwiseTopLeft")
    toAttrFrTyp n TransitionFilter_Subtype_ClockwiseTwelve = Just (n, str2attr "clockwiseTwelve")
    toAttrFrTyp n TransitionFilter_Subtype_CornersIn = Just (n, str2attr "cornersIn")
    toAttrFrTyp n TransitionFilter_Subtype_CornersOut = Just (n, str2attr "cornersOut")
    toAttrFrTyp n TransitionFilter_Subtype_CounterClockwiseBottomLeft = Just (n, str2attr "counterClockwiseBottomLeft")
    toAttrFrTyp n TransitionFilter_Subtype_CounterClockwiseTopRight = Just (n, str2attr "counterClockwiseTopRight")
    toAttrFrTyp n TransitionFilter_Subtype_Crossfade = Just (n, str2attr "crossfade")
    toAttrFrTyp n TransitionFilter_Subtype_DiagonalBottomLeft = Just (n, str2attr "diagonalBottomLeft")
    toAttrFrTyp n TransitionFilter_Subtype_DiagonalBottomLeftOpposite = Just (n, str2attr "diagonalBottomLeftOpposite")
    toAttrFrTyp n TransitionFilter_Subtype_DiagonalTopLeft = Just (n, str2attr "diagonalTopLeft")
    toAttrFrTyp n TransitionFilter_Subtype_DiagonalTopLeftOpposite = Just (n, str2attr "diagonalTopLeftOpposite")
    toAttrFrTyp n TransitionFilter_Subtype_Diamond = Just (n, str2attr "diamond")
    toAttrFrTyp n TransitionFilter_Subtype_DoubleBarnDoor = Just (n, str2attr "doubleBarnDoor")
    toAttrFrTyp n TransitionFilter_Subtype_DoubleDiamond = Just (n, str2attr "doubleDiamond")
    toAttrFrTyp n TransitionFilter_Subtype_Down = Just (n, str2attr "down")
    toAttrFrTyp n TransitionFilter_Subtype_FadeFromColor = Just (n, str2attr "fadeFromColor")
    toAttrFrTyp n TransitionFilter_Subtype_FadeToColor = Just (n, str2attr "fadeToColor")
    toAttrFrTyp n TransitionFilter_Subtype_FanInHorizontal = Just (n, str2attr "fanInHorizontal")
    toAttrFrTyp n TransitionFilter_Subtype_FanInVertical = Just (n, str2attr "fanInVertical")
    toAttrFrTyp n TransitionFilter_Subtype_FanOutHorizontal = Just (n, str2attr "fanOutHorizontal")
    toAttrFrTyp n TransitionFilter_Subtype_FanOutVertical = Just (n, str2attr "fanOutVertical")
    toAttrFrTyp n TransitionFilter_Subtype_FivePoint = Just (n, str2attr "fivePoint")
    toAttrFrTyp n TransitionFilter_Subtype_FourBlade = Just (n, str2attr "fourBlade")
    toAttrFrTyp n TransitionFilter_Subtype_FourBoxHorizontal = Just (n, str2attr "fourBoxHorizontal")
    toAttrFrTyp n TransitionFilter_Subtype_FourBoxVertical = Just (n, str2attr "fourBoxVertical")
    toAttrFrTyp n TransitionFilter_Subtype_FourPoint = Just (n, str2attr "fourPoint")
    toAttrFrTyp n TransitionFilter_Subtype_FromBottom = Just (n, str2attr "fromBottom")
    toAttrFrTyp n TransitionFilter_Subtype_FromLeft = Just (n, str2attr "fromLeft")
    toAttrFrTyp n TransitionFilter_Subtype_FromRight = Just (n, str2attr "fromRight")
    toAttrFrTyp n TransitionFilter_Subtype_FromTop = Just (n, str2attr "fromTop")
    toAttrFrTyp n TransitionFilter_Subtype_Heart = Just (n, str2attr "heart")
    toAttrFrTyp n TransitionFilter_Subtype_Horizontal = Just (n, str2attr "horizontal")
    toAttrFrTyp n TransitionFilter_Subtype_HorizontalLeft = Just (n, str2attr "horizontalLeft")
    toAttrFrTyp n TransitionFilter_Subtype_HorizontalLeftSame = Just (n, str2attr "horizontalLeftSame")
    toAttrFrTyp n TransitionFilter_Subtype_HorizontalRight = Just (n, str2attr "horizontalRight")
    toAttrFrTyp n TransitionFilter_Subtype_HorizontalRightSame = Just (n, str2attr "horizontalRightSame")
    toAttrFrTyp n TransitionFilter_Subtype_HorizontalTopLeftOpposite = Just (n, str2attr "horizontalTopLeftOpposite")
    toAttrFrTyp n TransitionFilter_Subtype_HorizontalTopRightOpposite = Just (n, str2attr "horizontalTopRightOpposite")
    toAttrFrTyp n TransitionFilter_Subtype_Keyhole = Just (n, str2attr "keyhole")
    toAttrFrTyp n TransitionFilter_Subtype_Left = Just (n, str2attr "left")
    toAttrFrTyp n TransitionFilter_Subtype_LeftCenter = Just (n, str2attr "leftCenter")
    toAttrFrTyp n TransitionFilter_Subtype_LeftToRight = Just (n, str2attr "leftToRight")
    toAttrFrTyp n TransitionFilter_Subtype_OppositeHorizontal = Just (n, str2attr "oppositeHorizontal")
    toAttrFrTyp n TransitionFilter_Subtype_OppositeVertical = Just (n, str2attr "oppositeVertical")
    toAttrFrTyp n TransitionFilter_Subtype_ParallelDiagonal = Just (n, str2attr "parallelDiagonal")
    toAttrFrTyp n TransitionFilter_Subtype_ParallelDiagonalBottomLeft = Just (n, str2attr "parallelDiagonalBottomLeft")
    toAttrFrTyp n TransitionFilter_Subtype_ParallelDiagonalTopLeft = Just (n, str2attr "parallelDiagonalTopLeft")
    toAttrFrTyp n TransitionFilter_Subtype_ParallelVertical = Just (n, str2attr "parallelVertical")
    toAttrFrTyp n TransitionFilter_Subtype_Rectangle = Just (n, str2attr "rectangle")
    toAttrFrTyp n TransitionFilter_Subtype_Right = Just (n, str2attr "right")
    toAttrFrTyp n TransitionFilter_Subtype_RightCenter = Just (n, str2attr "rightCenter")
    toAttrFrTyp n TransitionFilter_Subtype_SixPoint = Just (n, str2attr "sixPoint")
    toAttrFrTyp n TransitionFilter_Subtype_Top = Just (n, str2attr "top")
    toAttrFrTyp n TransitionFilter_Subtype_TopCenter = Just (n, str2attr "topCenter")
    toAttrFrTyp n TransitionFilter_Subtype_TopLeft = Just (n, str2attr "topLeft")
    toAttrFrTyp n TransitionFilter_Subtype_TopLeftClockwise = Just (n, str2attr "topLeftClockwise")
    toAttrFrTyp n TransitionFilter_Subtype_TopLeftCounterClockwise = Just (n, str2attr "topLeftCounterClockwise")
    toAttrFrTyp n TransitionFilter_Subtype_TopLeftDiagonal = Just (n, str2attr "topLeftDiagonal")
    toAttrFrTyp n TransitionFilter_Subtype_TopLeftHorizontal = Just (n, str2attr "topLeftHorizontal")
    toAttrFrTyp n TransitionFilter_Subtype_TopLeftVertical = Just (n, str2attr "topLeftVertical")
    toAttrFrTyp n TransitionFilter_Subtype_TopRight = Just (n, str2attr "topRight")
    toAttrFrTyp n TransitionFilter_Subtype_TopRightClockwise = Just (n, str2attr "topRightClockwise")
    toAttrFrTyp n TransitionFilter_Subtype_TopRightCounterClockwise = Just (n, str2attr "topRightCounterClockwise")
    toAttrFrTyp n TransitionFilter_Subtype_TopRightDiagonal = Just (n, str2attr "topRightDiagonal")
    toAttrFrTyp n TransitionFilter_Subtype_TopToBottom = Just (n, str2attr "topToBottom")
    toAttrFrTyp n TransitionFilter_Subtype_TwoBladeHorizontal = Just (n, str2attr "twoBladeHorizontal")
    toAttrFrTyp n TransitionFilter_Subtype_TwoBladeVertical = Just (n, str2attr "twoBladeVertical")
    toAttrFrTyp n TransitionFilter_Subtype_TwoBoxBottom = Just (n, str2attr "twoBoxBottom")
    toAttrFrTyp n TransitionFilter_Subtype_TwoBoxLeft = Just (n, str2attr "twoBoxLeft")
    toAttrFrTyp n TransitionFilter_Subtype_TwoBoxRight = Just (n, str2attr "twoBoxRight")
    toAttrFrTyp n TransitionFilter_Subtype_TwoBoxTop = Just (n, str2attr "twoBoxTop")
    toAttrFrTyp n TransitionFilter_Subtype_Up = Just (n, str2attr "up")
    toAttrFrTyp n TransitionFilter_Subtype_Vertical = Just (n, str2attr "vertical")
    toAttrFrTyp n TransitionFilter_Subtype_VerticalBottomLeftOpposite = Just (n, str2attr "verticalBottomLeftOpposite")
    toAttrFrTyp n TransitionFilter_Subtype_VerticalBottomSame = Just (n, str2attr "verticalBottomSame")
    toAttrFrTyp n TransitionFilter_Subtype_VerticalLeft = Just (n, str2attr "verticalLeft")
    toAttrFrTyp n TransitionFilter_Subtype_VerticalRight = Just (n, str2attr "verticalRight")
    toAttrFrTyp n TransitionFilter_Subtype_VerticalTopLeftOpposite = Just (n, str2attr "verticalTopLeftOpposite")
    toAttrFrTyp n TransitionFilter_Subtype_VerticalTopSame = Just (n, str2attr "verticalTopSame")
instance XmlAttrType TransitionFilter_Coordinated where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "true" = Just TransitionFilter_Coordinated_True
	    translate "false" = Just TransitionFilter_Coordinated_False
	    translate _ = Nothing
    toAttrFrTyp n TransitionFilter_Coordinated_True = Just (n, str2attr "true")
    toAttrFrTyp n TransitionFilter_Coordinated_False = Just (n, str2attr "false")
instance XmlAttrType TransitionFilter_ClibBoundary where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "parent" = Just TransitionFilter_ClibBoundary_Parent
	    translate "children" = Just TransitionFilter_ClibBoundary_Children
	    translate _ = Nothing
    toAttrFrTyp n TransitionFilter_ClibBoundary_Parent = Just (n, str2attr "parent")
    toAttrFrTyp n TransitionFilter_ClibBoundary_Children = Just (n, str2attr "children")
instance XmlAttrType TransitionFilter_CalcMode where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "discrete" = Just TransitionFilter_CalcMode_Discrete
	    translate "linear" = Just TransitionFilter_CalcMode_Linear
	    translate "paced" = Just TransitionFilter_CalcMode_Paced
	    translate _ = Nothing
    toAttrFrTyp n TransitionFilter_CalcMode_Discrete = Just (n, str2attr "discrete")
    toAttrFrTyp n TransitionFilter_CalcMode_Linear = Just (n, str2attr "linear")
    toAttrFrTyp n TransitionFilter_CalcMode_Paced = Just (n, str2attr "paced")


{-Done-}
