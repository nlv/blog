{-
    Модуль, отвечающий за формирование главной страницы.
    https://github.com/nlv/blog
    Все права принадлежат Никитину Льву
-}

{-# LANGUAGE OverloadedStrings #-}

module IndexPage (
    createIndexPage
) where

import Context              (postContext)
import Tags                 (categorisedTags)
import Misc                 (TagsReader, getRussianNameOfCategory)
import Data.List            (intercalate)
import Control.Monad.Reader
import Hakyll

import           Text.Blaze.Html                 (toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

createIndexPage :: TagsReader
createIndexPage = do
    tagsAndAuthors@[tags, categories, authors] <- ask
    lift $ create ["index.html"] $ do
        route idRoute
        compile $ do
            -- На главной странице будет отражено 7 последних публикаций.
            last7Posts <- fmap (take 7) . recentFirst =<< loadAll "posts/**"
            let indexContext = mconcat [ listField "posts" (postContext tagsAndAuthors) (return last7Posts)
                                       , constField "title" "Личный блог Льва Никитина"
                                       , defaultContext
--                                       , constField "categories" (showCategorised $ categorisedTags categories tags)
                                       , constField "categories" (showCategorised (tagsMakeId categories) (tagsMakeId tags) $ categorisedTags categories tags)
                                       ]

            makeItem "" >>= loadAndApplyTemplate "templates/index.html" indexContext
                        >>= loadAndApplyTemplate "templates/default.html" indexContext
                        >>= relativizeUrls
    return ()
    where showCategorised :: (String -> Identifier) -> (String -> Identifier) -> [(String, [String])] -> String
          showCategorised cat2Id tag2Id cats = concat (map showCategorised' cats)
            where showCategorised' :: (String, [String]) -> String
                  showCategorised' (cat, tags) = renderHtml $ do
                      H.li $ do
                        H.p $ H.a ! A.href (toValue $ toFilePath $ cat2Id cat) $ H.preEscapedToHtml $ getRussianNameOfCategory cat
                        H.ul $ mapM_ (\t -> H.li $ H.a ! A.href (toValue $toFilePath $ tag2Id t) $ H.preEscapedToHtml t) tags
