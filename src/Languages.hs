module Languages
( lookupLanguage
) where

import Data.Maybe (fromJust, isJust)
import qualified Data.Map as Map

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
                 , ("az"   , "Azerbaijani")
                 , ("eu"   , "Basque")
                 , ("bn"   , "Bengali")
                 , ("be"   , "Belarusian")
                 , ("bg"   , "Bulgarian")
                 , ("ca"   , "Catalan")
                 , ("zh-CN", "Chinese Simplified")
                 , ("zh-TW", "Chinese Traditional")
                 , ("hr"   , "Croatian")
                 , ("cs"   , "Czech")
                 , ("da"   , "Danish")
                 , ("nl"   , "Dutch")
                 , ("en"   , "English")
                 , ("eo"   , "Esperanto")
                 , ("et"   , "Estonian")
                 , ("tl"   , "Filipino")
                 , ("fi"   , "Finnish")
                 , ("fr"   , "French")
                 , ("gl"   , "Galician")
                 , ("ka"   , "Georgian")
                 , ("de"   , "German")
                 , ("el"   , "Greek")
                 , ("gu"   , "Gujarati")
                 , ("ht"   , "Haitian Creole")
                 , ("iw"   , "Hebrew")
                 , ("hi"   , "Hindi")
                 , ("hu"   , "Hungarian")
                 , ("is"   , "Icelandic")
                 , ("id"   , "Indonesian")
                 , ("ga"   , "Irish")
                 , ("it"   , "Italian")
                 , ("ja"   , "Japanese")
                 , ("kn"   , "Kannada")
                 , ("ko"   , "Korean")
                 , ("la"   , "Latin")
                 , ("lv"   , "Latvian")
                 , ("lt"   , "Lithuanian")
                 , ("mk"   , "Macedonian")
                 , ("ms"   , "Malay")
                 , ("mt"   , "Maltese")
                 , ("no"   , "Norwegian")
                 , ("fa"   , "Persian")
                 , ("pl"   , "Polish")
                 , ("pt"   , "Portuguese")
                 , ("ro"   , "Romanian")
                 , ("ru"   , "Russian")
                 , ("sr"   , "Serbian")
                 , ("sk"   , "Slovak")
                 , ("sl"   , "Slovenian")
                 , ("es"   , "Spanish")
                 , ("sw"   , "Swahili")
                 , ("sv"   , "Swedish")
                 , ("ta"   , "Tamil")
                 , ("te"   , "Telugu")
                 , ("th"   , "Thai")
                 , ("tr"   , "Turkish")
                 , ("uk"   , "Ukrainian")
                 , ("ur"   , "Urdu")
                 , ("vi"   , "Vietnamese")
                 , ("cy"   , "Welsh")
                 , ("yi"   , "Yiddish")
                 ]
