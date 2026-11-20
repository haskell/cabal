-- | A compiler plugin used to check Haddock argument forwarding and reuse.
module HaddockProbePlugin (plugin) where

import GHC.Plugins (Plugin (..), defaultPlugin, purePlugin)

-- | A plugin whose presence must agree between compilation and documentation.
plugin :: Plugin
plugin = defaultPlugin{pluginRecompile = purePlugin}
