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
import Tags                 (showCategorised, showAuthors)
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
                                       , constField "categories" (showCategorised categories tags)
                                       , constField "authors" (showAuthors authors)
                                       , defaultContext
                                       ]

            makeItem "" >>= loadAndApplyTemplate "templates/index.html" indexContext
                        >>= loadAndApplyTemplate "templates/default.html" indexContext
                        >>= relativizeUrls
    return ()
