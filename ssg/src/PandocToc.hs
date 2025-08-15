{-# LANGUAGE OverloadedStrings #-}

module PandocToc (insertToc) where

import Data.List (span)
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Walk (query, walk)
import Prelude

-- | Extracts header information if a block is a header.
extractHeaders :: Block -> [(Int, [Inline], Attr)]
extractHeaders (Header level attr@(ident, _, _) inlines) =
  -- Only include headers with an ID (for linking) and that are not h1
  if level > 1 && not (T.null ident) then [(level, inlines, attr)] else []
extractHeaders _ = []

-- | Converts a single header to a list item for the TOC.
-- The Attr includes the identifier needed for the link.
headerToTocItem :: (Int, [Inline], Attr) -> [Block]
headerToTocItem (_, inlines, (ident, _, _)) =
  [Para [Link ("", [], []) inlines (("#" <> ident), "")]]

-- | Recursively builds a nested list of TOC items.
buildNestedToc :: [(Int, [Inline], Attr)] -> [[Block]]
buildNestedToc [] = []
buildNestedToc (h@(level, _, _) : rest) =
  let (children, siblings) = span (\(l, _, _) -> l > level) rest
      listItem = headerToTocItem h
      nestedList = if Prelude.null children then [] else [BulletList (buildNestedToc children)]
   in (listItem ++ nestedList) : buildNestedToc siblings

-- | Builds the TOC block from the collected headers.
buildToc :: [(Int, [Inline], Attr)] -> Block
buildToc headers = BulletList (buildNestedToc headers)

-- | Replaces a placeholder paragraph "[TOC]" with the generated table of contents.
replacePlaceholder :: Block -> Block -> Block
replacePlaceholder toc (Para [Str "[TOC]"]) = Div ("toc", [], []) [toc]
replacePlaceholder _ block = block

-- | The main function to walk through the Pandoc document, generate the TOC,
-- and insert it at the placeholder location.
insertToc :: Pandoc -> Pandoc
insertToc pandoc@(Pandoc meta blocks) =
  let headers = query extractHeaders blocks
      toc = buildToc headers
   in if Prelude.null headers
        then pandoc -- Don't modify the document if there are no headers to list
        else walk (replacePlaceholder toc) pandoc
