module Main
  ( main
  , mainHeadlessBrowser
  ) where

import Options.Applicative
import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, error, launchAff_, throwError, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Foreign (unsafeFromForeign, unsafeToForeign)
import Node.Buffer (Buffer, fromString)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (exists, mkdir, unlink)
import Node.FS.Aff as FS
import Node.Path (FilePath, dirname)
import Node.Path as Path
import Node.Process (execArgv, getEnv, lookupEnv)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Text.Smolder.HTML as S
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (text, (!), safe)
import Text.Smolder.Renderer.String (render)
import Toppokki (Browser)
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

type Config
  = { wsEndpointPath :: FilePath
    }

mkConfig :: Effect Config
mkConfig = do
  homeDir <-
    lookupEnv "HOME"
      >>= case _ of
          Just dir -> pure dir
          Nothing -> throw "No home directory?"
  let
    rootDir = Path.concat [ homeDir, ".excalidraw-cli" ]
  pure
    { wsEndpointPath: Path.concat [ rootDir, "wsendpoint" ]
    }

-- headless が常駐って可能なのか？
main :: Effect Unit
main = do
  config <- mkConfig
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
  launchAff_ do
    source <- FS.readTextFile UTF8 input
    withBrowser config \browser -> do
      svg <- excalidrawRender browser opts.cdnVer source
      buf <- case outputExt of
        SVG -> liftEffect $ fromString svg UTF8
        PNG -> convertSvgToPng browser svg
      FS.writeFile output buf

-- puppeteerブラウザ起動して放置すれば勝手に走り続けるはず
-- forkして呼出すことを想定
mainHeadlessBrowser :: Effect Unit
mainHeadlessBrowser = do
  config <- mkConfig
  launchAff_ do
    -- デバッグ時は headless: falseに
    browser <- T.launch { headless: true }
    -- browser <- T.launch { headless: false }
    wsEndpoint <- liftEffect $ T.wsEndpoint browser
    let dir = dirname config.wsEndpointPath
    unlessM (exists dir) $ mkdir dir
    FS.writeTextFile UTF8 config.wsEndpointPath (unwrap wsEndpoint)
    -- いらないかも...
    -- _ <- waitForever
    pure unit
  where
    waitForever = do
      delay (Milliseconds 10000000.0)
      waitForever


-- ブラウザをlaunchした場合、disconnectしても
-- TODO: デバッグ起動対応
-- https://nodejs.org/api/child_process.html#optionsdetached
withBrowser :: forall a. Config -> (Browser -> Aff a) -> Aff a
withBrowser config action = do
  browser <- tryConnect >>= case _ of
    Left _ -> do
      log "restart chrome"
      whenM (exists config.wsEndpointPath) $ FS.unlink config.wsEndpointPath
      -- ???これ可能か???
      cp <- liftEffect $ fork "?" []
              { detached: true
              , stdio: "ignore"
              , execArgv: ["-e", "require(\"Main\").mainHeadlessBrowser()"]
              }
      liftEffect do
        disconnect cp
        unref cp
      waitForChild 0
      tryConnect >>= case _ of
        Left _ ->
          throwError (error "run failed")
        Right b -> do
          log "restarted"
          pure b
    Right b -> do
      log "chrome running"
      pure b
  a <- action browser
  liftEffect $ T.disconnect browser
  pure a
  where
    tryConnect =
      try do
        wsEndpoint <- wrap <$> FS.readTextFile UTF8 config.wsEndpointPath
        T.connect { browserWSEndpoint: wsEndpoint }

    waitForChild i = do
      delay (Milliseconds 500.0)
      when (i > 4) $ throwError (error "too long")
      ifM (exists config.wsEndpointPath) (pure unit) (waitForChild (i + 1))

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
  -- T.close page
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

-- purescript-node-process だと forkメソッドに引数が渡せなかった
-- https://nodejs.org/api/child_process.html#child_processforkmodulepath-args-options
-- https://nodejs.org/api/child_process.html#optionsdetached

type ForkOptions =
  ( cwd :: String
  , detached :: Boolean
  , stdio :: String
  , execArgv :: Array String
  )

fork
  :: forall options trash
   . Row.Union options trash ForkOptions
  => String
  -> Array String
  -> { | options }
  -> Effect ChildProcess
fork = _fork

unref :: ChildProcess -> Effect Unit
unref = _unref

disconnect :: ChildProcess -> Effect Unit
disconnect = _disconnect

foreign import data ChildProcess :: Type
foreign import _fork :: forall options. String -> Array String -> options -> Effect ChildProcess
foreign import _unref :: ChildProcess -> Effect Unit
foreign import _disconnect :: ChildProcess -> Effect Unit
