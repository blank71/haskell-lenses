module Lens.Helpers.Format where

import Data.Text.Buildable (Buildable)
import Data.Text.Format (Only (..), build)
import Data.Text.Lazy.Builder (Builder)

buildSep :: (Buildable sep, Buildable a) => sep -> [a] -> Builder
buildSep _ [] = build "" ()
buildSep _ [x] = build "{}" (Only x)
buildSep sep (x : xs) = build "{}{}{}" (x, sep, buildSep sep xs)

buildSepStr :: Buildable a => String -> [a] -> Builder
buildSepStr = buildSep

buildSepComma :: Buildable a => [a] -> Builder
buildSepComma = buildSepStr ", "

buildSepSpace :: Buildable a => [a] -> Builder
buildSepSpace = buildSepStr ", "
