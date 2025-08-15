{-# LANGUAGE OverloadedStrings #-}

module PandocCollapsible (makeCollapsible) where


import Data.Text (Text)
import Text.Pandoc.Definition

-- | Wraps sections following a header in <details> and <summary> tags to make them collapsible.
makeCollapsible :: Int -> Pandoc -> Pandoc
makeCollapsible expandedLevel (Pandoc meta blocks) = Pandoc meta (processBlocks blocks)
  where
    processBlocks :: [Block] -> [Block]
    processBlocks [] = []
    -- Don't make H1 tags collapsible.
    processBlocks (h@(Header level _ _) : xs) | level > 1 =
      let (sectionBlocks, rest) = break (isHeaderOfSameOrHigherLevel level) xs
          detailsTag = if level == expandedLevel then "<details open>" else "<details>"
       in wrapInDetails detailsTag h (processBlocks sectionBlocks) ++ processBlocks rest
    processBlocks (x : xs) = x : processBlocks xs

isHeaderOfSameOrHigherLevel :: Int -> Block -> Bool
isHeaderOfSameOrHigherLevel level (Header h _ _) = h <= level
isHeaderOfSameOrHigherLevel _ _ = False

wrapInDetails :: Text -> Block -> [Block] -> [Block]
wrapInDetails detailsTagText header section =
  [ RawBlock (Format "html") detailsTagText,
    RawBlock (Format "html") "<summary>",
    header,
    RawBlock (Format "html") "</summary>"
  ]
    ++ section
    ++ [RawBlock (Format "html") "</details>"]