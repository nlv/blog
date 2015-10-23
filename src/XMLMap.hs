{-
    Модуль, отвечающий за построение XML-карты сайта.
    https://github.com/nlv/blog
    Все права принадлежат Никитину Льву
-}

{-# LANGUAGE OverloadedStrings #-}

module XMLMap (
    createXMLMap
) where

import Context              (postContext)
import Misc                 (aHost, TagsReader)
import Control.Monad.Reader
import Hakyll

createXMLMap :: TagsReader
createXMLMap = do
    tagsAndAuthors <- ask
    lift $ create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            allPosts <- recentFirst =<< loadAll "posts/**"
            let sitemapContext = mconcat [ listField "entries" (postContext tagsAndAuthors) (return allPosts)
                                         , constField "host" aHost
                                         , defaultContext
                                         ]
            -- Формируем страницу sitemap.xml, применяя к ней шаблон и контекст.
            makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapContext
    return ()
