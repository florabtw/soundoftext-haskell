module Languages
( lookupLanguage
, languagePairs
) where

import Data.Maybe (fromJust, isJust)
import qualified Data.Map as Map

languagePairs :: [(String,String)]
languagePairs = Map.assocs languageMap

lookupLanguage :: String -> String
lookupLanguage l
    | isJust mLang = fromJust mLang
    | otherwise    = "Unknown"
    where
        mLang = Map.lookup l languageMap

languageMap :: Map.Map String String
languageMap =
    Map.fromList [ ("af"   , "Afrikaans")
                 , ("sq"   , "Albanian")
                 , ("ar"   , "Arabic")
                 , ("hy"   , "Armenian")
                 , ("bs"   , "Bosnian")
                 , ("ca"   , "Catalan")
                 , ("zh-CN", "Chinese Simplified")
                 , ("zh-TW", "Chinese Traditional")
                 , ("hr"   , "Croatian")
                 , ("cs"   , "Czech")
                 , ("da"   , "Danish")
                 , ("nl"   , "Dutch")
                 , ("en"   , "English")
                 , ("eo"   , "Esperanto")
                 , ("fi"   , "Finnish")
                 , ("fr"   , "French")
                 , ("de"   , "German")
                 , ("el"   , "Greek")
                 , ("ht"   , "Haitian Creole")
                 , ("hi"   , "Hindi")
                 , ("hu"   , "Hungarian")
                 , ("is"   , "Icelandic")
                 , ("id"   , "Indonesian")
                 , ("it"   , "Italian")
                 , ("ja"   , "Japanese")
                 , ("ko"   , "Korean")
                 , ("la"   , "Latin")
                 , ("lv"   , "Latvian")
                 , ("mk"   , "Macedonian")
                 , ("no"   , "Norwegian")
                 , ("pl"   , "Polish")
                 , ("pt"   , "Portuguese")
                 , ("ro"   , "Romanian")
                 , ("ru"   , "Russian")
                 , ("sr"   , "Serbian")
                 , ("sk"   , "Slovak")
                 , ("es"   , "Spanish")
                 , ("sw"   , "Swahili")
                 , ("sv"   , "Swedish")
                 , ("ta"   , "Tamil")
                 , ("th"   , "Thai")
                 , ("tr"   , "Turkish")
                 , ("vi"   , "Vietnamese")
                 , ("cy"   , "Welsh")
                 ]
