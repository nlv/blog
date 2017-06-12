{-
    Модуль, отвечающий за формирование архива (списка всех статей).
    https://github.com/nlv/blog
    Все права принадлежат Никитину Льву
-}

{-# LANGUAGE OverloadedStrings #-}

module Archive (
    createPageWithAllPosts
) where

import Context              (postContext)
import Misc                 (TagsReader)
import Tags                 (showCategorised, showAuthors)
import Control.Monad.Reader
import Hakyll

createPageWithAllPosts :: TagsReader
createPageWithAllPosts = do
    tagsAndAuthors@[tags, categories, authors] <- ask
    lift $ create ["archive.html"] $ do
        route idRoute
        compile $ do
            allPosts <- recentFirst =<< loadAll "posts/**"
            let archiveContext = mconcat [ listField "posts" (postContext tagsAndAuthors) (return allPosts)
                                         , constField "title" "Архив статей"
                                         , constField "categories" (showCategorised categories tags)
                                         , constField "authors" (showAuthors authors)
                                         , defaultContext
                                         ]

            makeItem "" >>= loadAndApplyTemplate "templates/archive.html" archiveContext
                        >>= loadAndApplyTemplate "templates/default.html" archiveContext
                        >>= relativizeUrls
    return ()
