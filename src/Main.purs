module Main
  ( main
  ) where

import Options.Applicative
import Prelude

import Control.Alt ((<|>))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Foreign (unsafeFromForeign, unsafeToForeign)
import Node.Buffer (Buffer, fromString)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path
import Partial.Unsafe (unsafeCrashWith)
import Text.Smolder.HTML as S
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (text, (!), safe)
import Text.Smolder.Renderer.String (render)
import Toppokki as T

type Options
  = { input :: Input
    , output :: Output
    , cdnVer :: String
    , debug :: Boolean
    }

data Input
  = InputStdin
  | InputFilePath FilePath

data Output
  = OutputStdout OutputExt
  | OutputFilePath FilePath

data OutputExt
  = SVG
  | PNG

optsParser :: Parser Options
optsParser = ado
  input <-
    parseInput
      <$> strOption
          ( long "input"
              <> short 'i'
              <> metavar "PATH"
              <> help "Input excalidraw file. Specify '-' to input from terminal. Required."
          )
  output <-
    ( OutputFilePath
        <$> strOption
            ( long "output"
                <> short 'o'
                <> metavar "PATH"
                <> help "Output file. It should have either .svg or .png extension."
            )
    )
      <|> ( (OutputStdout <<< parseFormat)
            <$> strOption
                ( long "stdout"
                    <> short 's'
                    <> metavar "svg|png"
                    <> help "Output format. It should be either svg or png."
                )
        )
  cdnVer <-
    strOption
      ( long "cdn-version"
          <> help "You can specify which version of @excalidraw/excalidraw to use. Will use official CDN."
          <> showDefault
          <> value defaultCdnVer
          <> metavar "VER"
      )
  debug <-
    switch
      ( long "debug"
          <> help "Show the browser and don't close it atomatically even after successfully created."
          <> showDefault
      )
  in { input, output, cdnVer, debug }

parseInput :: String -> Input
parseInput = case _ of
  "-" -> InputStdin
  path -> InputFilePath path
parseFormat :: String -> OutputExt

parseFormat = case _ of
  "svg" -> SVG
  "png" -> PNG
  _ -> unsafeCrashWith "??svg|png"

optsInfo :: ParserInfo Options
optsInfo =
  info
    (optsParser <**> helper)
    ( fullDesc
        <> header "excalidraw-cli - cli command for excalidraw"
    )

defaultCdnVer :: String
defaultCdnVer = "0.11.0"

cdnUrl :: String -> String
cdnUrl ver = "https://unpkg.com/@excalidraw/excalidraw@" <> ver <> "/dist/excalidraw.production.min.js"

cdnReactUrl :: String
cdnReactUrl = "https://unpkg.com/react/umd/react.production.min.js"

cdnReactDomUrl :: String
cdnReactDomUrl = "https://unpkg.com/react-dom/umd/react.production.min.js"

-- headless が常駐って可能なのか？
main :: Effect Unit
main = do
  opts <- execParser optsInfo
  input <-
    pure case opts.input of
      InputStdin -> "/dev/stdin"
      InputFilePath path -> path
  output <-
    pure case opts.output of
      OutputStdout _ -> "/dev/stdout"
      OutputFilePath path -> path
  outputExt <- case opts.output of
    OutputStdout ext -> pure ext
    OutputFilePath path -> case Path.extname path of
      ".svg" -> pure SVG
      ".png" -> pure PNG
      _ -> throwError (error "Unsupported output extension.")
  headless <- pure $ not opts.debug
  doClose <- pure $ not opts.debug
  launchAff_ do
    source <- FS.readTextFile UTF8 input
    browser <- T.launch { headless }
    svg <- excalidrawRender browser opts.cdnVer source
    buf <- case outputExt of
      SVG -> liftEffect $ fromString svg UTF8
      PNG -> convertSvgToPng browser svg
    FS.writeFile output buf
    when doClose $ T.close browser

-- | MermaidAPI を利用してSVGを得る。
-- | 一時的に画面には(hiddenな)DOMが追加されるが、最終的には何もない状態。
-- | ビューポートのサイズはレンダリングに影響を与えない。
--
-- Excalidraw.exportToSvg({..}) : Promise
excalidrawRender :: T.Browser -> String -> String -> Aff String
excalidrawRender browser cdnVer source = do
  page <- T.newPage browser
  T.setContent indexHtml page
  svg' <- T.unsafeEvaluateWithArgs renderFn [ unsafeToForeign source ] page
  pure $ unsafeFromForeign svg'
  where
  renderFn :: String
  renderFn =
    """
      (source) => {
        return Excalidraw.exportToSvg(JSON.parse(source))
          .then(function(svg) { return (new XMLSerializer()).serializeToString(svg)});
      }
    """

  indexHtml :: String
  indexHtml = render template
    where
    template =
      S.html do
        S.head do
          S.script ! safe (A.src cdnReactUrl) ! A.type' "text/javascript" $ text ""
          S.script ! safe (A.src cdnReactDomUrl) ! A.type' "text/javascript" $ text ""
          S.script ! safe (A.src (cdnUrl cdnVer)) ! A.type' "text/javascript" $ text ""
        S.body do
          pure unit

-- | SVG を png 画像に変換する。
-- | ビューポートは svg 要素以上の大きさが必要。生成される png 画像のサイズは変わらないが、
-- | ビューポートからはみ出た部分は白抜きになる。
-- | ?? viewport サイズ無指定の場合はどうなる？
--
-- inkscapeやimagemagickを使う場合に比べて(依存を増やさない以外に)メリットはあるのか？
--
convertSvgToPng :: T.Browser -> String -> Aff Buffer
convertSvgToPng browser svg = do
  page <- T.newPage browser
  _ <- T.setContent indexHtml page
  clip' <- T.unsafePageEval (T.Selector "svg") svgRect page
  T.screenshot { clip: unsafeFromForeign clip' } page
  where
  indexHtml = "<!DOCTYPE html>\n<html><body>" <> svg <> "</body></html>"

  svgRect =
    """
      (svg) => {
        const react = svg.getBoundingClientRect();
        return { x: react.left, y: react.top, width: react.width, height: react.height }
      }
    """
