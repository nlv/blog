{-
    Модуль, отвечающий за преобразование статей и в формирование корректных путей к ним.
    https://github.com/nlv/blog
    Все права принадлежат Никитину Льву
-}

{-# LANGUAGE OverloadedStrings #-}

module Posts (
    createPosts
) where

import Data.List.Split      (splitOn)
import Data.List            (intersperse)
import System.FilePath      (splitExtension)
import Context              (postContext)
import Misc                 (TagsReader)
import Control.Monad.Reader
import Hakyll
-- import qualified Text.BlogLiterately.Highlight as Highlight


-- Дата публикации будет отражена в URL в виде подкаталогов.
directorizeDate :: Routes
directorizeDate = customRoute (\i -> directorize $ toFilePath i)
    where
        directorize path = dirs
            where
                (dirs, _) = splitExtension $
                            concat $
                            (intersperse "/" date) ++ ["/"] ++ (intersperse "-" rest)
                minusBetweenDateAndTitle = 3
                (date, rest) = splitAt minusBetweenDateAndTitle $ splitOn "-" path


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

