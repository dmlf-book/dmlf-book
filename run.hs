#!/usr/bin/env stack
-- stack --resolver lts-12.11 script --package pandoc --package text --package directory --package filepath --package bytestring --package containers --package pandoc-types
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-} 

-- base
import Control.Monad
import System.IO as IO
import Data.List

-- containers
import qualified Data.Map as Map

-- pandoc
import           Text.Pandoc
import           Text.Pandoc.PDF

-- pandoc-types
import           Text.Pandoc.Builder

-- text
import qualified  Data.Text as Text
import qualified  Data.Text.IO as Text
import qualified  Data.Text.Encoding as Text

-- bytestring
import qualified  Data.ByteString.Lazy as BL

-- directory
import System.Directory

-- filepath
import System.FilePath

main :: IO ()
main = do

  chapters <- loadChapters "chapters" readerOptions

  let doc = mkDocument "java" chapters

  createDirectoryIfMissing True "output" 

  htmlOpts <- htmlWriterOptions
  render <- runIOorExplode $ writeHtml5String htmlOpts doc

  Text.writeFile "output/index.html" render
  
  latexOpts <- latexWriterOptions
  latex <- runIOorExplode $ 
    writeLaTeX latexOpts doc

  Text.writeFile "output/main.tex" render

  epdf <- runIOorExplode $ 
    makePDF "pdflatex" [] 
      writeLaTeX latexOpts doc
  
  case epdf of
    Right pdf -> 
      BL.writeFile "output/main.pdf" pdf
    Left err -> do
      BL.hPutStr stderr err
      fail "failed"


mkDocument :: String -> [(String, Pandoc)] -> Pandoc
mkDocument codeType chapters = 
  let 
    blocks = 
      concatMap (\(fn, Pandoc meta blocks) -> 
        let title = docTitle meta
            authors = docAuthors meta 
        in Header 1 (fn, [], []) title 
        : (filter onlyCodeType . map incrHeader $ blocks)) chapters
     in Pandoc (Meta (Map.fromList [("title", MetaInlines (toList "mdfl"))])) blocks
  where
    incrHeader = \case
      Header i attr inline -> Header (i+1) attr inline
      a -> a

    onlyCodeType = \case
      CodeBlock (_, attr, _) _ -> elem codeType attr
      _ -> True

loadChapters :: FilePath -> ReaderOptions -> IO [(String, Pandoc)]
loadChapters folder opt = do
  chapterFiles <- sort <$> listDirectory folder
  runIOorExplode . mapM 
    (\fp -> do
      content <- Text.decodeUtf8 <$> readFileStrict (folder </> fp)
      (dropExtension fp ,) <$> readMarkdown opt content
    ) $ chapterFiles
    
readerOptions :: ReaderOptions
readerOptions = def 
  { readerExtensions = extensionsFromList 
    [ Ext_yaml_metadata_block 
    , Ext_auto_identifiers
    , Ext_citations
    , Ext_footnotes
    , Ext_tex_math_dollars
    , Ext_backtick_code_blocks
    ]
  , readerStandalone = True
  }


htmlWriterOptions :: IO WriterOptions
htmlWriterOptions = do
  template <- IO.readFile "templates/template.html"
  return $ def { writerTemplate = Just template }

latexWriterOptions :: IO WriterOptions
latexWriterOptions =  do
  template <- IO.readFile "templates/template.latex"
  return $ def { writerTemplate = Just template }
