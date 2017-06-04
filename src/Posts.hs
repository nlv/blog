{-
    Модуль, отвечающий за преобразование статей и в формирование корректных путей к ним.
    https://github.com/nlv/blog
    Все права принадлежат Никитину Льву
-}

{-# LANGUAGE OverloadedStrings #-}

module Posts (
    createPosts,
    copyPrinted,
    createPostsDocx
) where

import Data.List.Split      (splitOn)
import Data.List            (intersperse)
import Context              (postContext, directorizedDatePath)
import Misc                 (TagsReader)
import Control.Monad.Reader
import Hakyll
-- import qualified Text.BlogLiterately.Highlight as Highlight


-- Дата публикации будет отражена в URL в виде подкаталогов.
directorizeDate :: Routes
directorizeDate = customRoute (\i -> directorizedDatePath $ toFilePath i)




-- like pandocCompiler, but uses hscolour for highlighting haskell code
-- pandocWithHighlighter :: Compiler (Item String)
-- pandocWithHighlighter =
--     pandocCompilerWithTransform defaultHakyllReaderOptions
--                                 defaultHakyllWriterOptions
--                                 (Highlight.colourisePandoc Highlight.HsColourCSS True)

createPosts :: TagsReader
createPosts = do
    tagsAndAuthors <- ask
    -- Берём все файлы из каталога posts.
    lift $ match "posts/**" $ do
        route $ directorizeDate `composeRoutes`
                setExtension "html"
        -- Для превращения Markdown в HTML используем pandocCompiler
        compile $ pandocCompiler
              >>= loadAndApplyTemplate "templates/post.html" (postContext tagsAndAuthors)
              >>= loadAndApplyTemplate "templates/default.html" (postContext tagsAndAuthors)
              >>= relativizeUrls
    return ()

copyPrinted :: TagsReader
copyPrinted = do
    lift $ match "printed/**" $ do
        route $ directorizeDate
        compile copyFileCompiler
    return ()

createPostsDocx :: TagsReader
createPostsDocx = return ()
{-
createPostsDocx = do
    lift $ match "posts/**" $ version "docx" $ do
        route $ directorizeDate `composeRoutes`
                setExtension "docx"
        compile $ getResourceString >>=
          withItemBody (unixFilter "2docx.sh" [])
        --compile $ pandocCompileraWidth def def {Stand}
    return ()
-}
