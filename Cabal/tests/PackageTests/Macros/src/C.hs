{-# LANGUAGE CPP #-}
module C where
#ifdef VERSION_deepseq
#error "Should not see macro from executable macros-a"
#endif
#ifdef VERSION_containers
#error "Should not see macro from executable macros-b"
#endif
c :: String
c = CURRENT_COMPONENT_ID
