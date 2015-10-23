{-
    Модуль, отвечающий за работу с RSS.
    https://github.com/nlv/blog
    Все права принадлежат Никитину Льву
-}

{-# LANGUAGE OverloadedStrings #-}

module RSSFeed (
    setupRSSFeed,
) where

import Misc                 (aHost, TagsReader)
import Context              (postContext)
import Control.Monad.Reader
import Hakyll

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration { feedTitle       = "nlv"
                                      , feedDescription = "Личный блог Никитина Льва (Омск)"
                                      , feedAuthorName  = "Никитин Лев"
                                      , feedAuthorEmail = "Leon.V.Nikitin@pravmail.ru"
                                      , feedRoot        = aHost
                                      }

-- Формируем стандартную RSS-ленту, на основе последних 10 публикаций.
setupRSSFeed :: TagsReader
setupRSSFeed = do
    tagsAndAuthors <- ask
    lift $ create ["feed.xml"] $ do
        route idRoute
        compile $ do
            let feedContext = mconcat [ postContext tagsAndAuthors
                                      , constField "description" ""
                                      ]
            -- Учитываем 10 последних статей.
            last10Posts <- fmap (take 10) . recentFirst =<< loadAll "posts/**"
            renderRss feedConfiguration
                      feedContext
                      last10Posts
    return ()

