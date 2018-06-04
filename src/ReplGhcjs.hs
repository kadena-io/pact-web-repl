{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

-- |
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Main where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.String.QQ
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom hiding (Element, fromJSString)
import           Reflex.Dom.ACE
------------------------------------------------------------------------------
import           Pact.Repl
import           Pact.Repl.Types
import           Pact.Types.Lang
------------------------------------------------------------------------------

main :: IO ()
main = mainWidget app

data ControlOut t = ControlOut
    { controlDropdown  :: Dynamic t Text
    , controlLoadEvent :: Event t ()
    }

codeFromResponse :: XhrResponse -> Text
codeFromResponse =
    fromMaybe "error: could not connect to server" . _xhrResponse_responseText

app :: MonadWidget t m => m ()
app = do
    pb <- getPostBuild
    ControlOut d le <- controlBar
    elAttr "div" ("id" =: "editor_view") $ do
      let getDemo = snd . (demos M.!)
      ex <- performRequestAsync ((\u -> xhrRequest "GET" u def) <$> updated d)
      --initialCode <- performRequestAsync (xhrRequest "GET" (snd $ head exampleData) def <$ pb)
      code <- elAttr "div" ("id" =: "column1") $ do
        codeWidget startingExpression $ codeFromResponse <$> ex
      elAttr "div" ("id" =: "separator" <> "class" =: "separator") blank
      (e,newExpr) <- elAttr' "div" ("id" =: "column2") $ do
        elAttr "div" ("id" =: "code") $ do
          mapM_ snippetWidget staticReplHeader
          widgetHold (replWidget startingExpression) (replWidget <$> tag (current code) le)
      timeToScroll <- delay 0.1 $ switch $ current newExpr
      _ <- performEvent (scrollToBottom (_element_raw e) <$ timeToScroll)
      return ()

scrollToBottom :: (PToJSVal t, MonadIO m, MonadJSM m) => t -> m ()
scrollToBottom e = liftJSM $ do
    let pElem = pToJSVal e
    (pElem <# ("scrollTop" :: String)) (pElem ^. js ("scrollHeight" :: String))

codeWidget :: MonadWidget t m => Text -> Event t Text -> m (Dynamic t Text)
codeWidget iv sv = do
    let ac = def { _aceConfigMode = Just "ace/mode/pact"
                 , _aceConfigElemAttrs = "id" =: "ace-widget"
                 }
    ace <- aceWidgetStatic ac (AceDynConfig $ Just AceTheme_SolarizedLight) iv
    _ <- withAceInstance ace (setValueACE <$> sv)
    return $ aceValue ace


data DisplayedSnippet
  = InputSnippet Text
  | OutputSnippet Text
  deriving (Eq,Ord,Show,Read)

staticReplHeader :: Seq DisplayedSnippet
staticReplHeader = S.fromList
      [ OutputSnippet ";; Welcome to the Pact interactive repl"
      , OutputSnippet ";; Use LOAD button to execute editor text"
      , OutputSnippet ";; then just type at the \"pact>\" prompt to interact!"
      ]

snippetWidget :: MonadWidget t m => DisplayedSnippet -> m ()
snippetWidget (InputSnippet t) = el "pre" $ text t
snippetWidget (OutputSnippet t) = el "pre" $ text t

------------------------------------------------------------------------------
replWidget
    :: MonadWidget t m
    => Text
    -> m (Event t Text)
replWidget initialCode = mdo
    initState <- liftIO $ initReplState StringEval
    stateAndOut0 <- runReplStep0 (initState, mempty) initialCode
    stateAndOut <- holdDyn stateAndOut0 evalResult

    _ <- dyn (mapM_ snippetWidget . snd <$> stateAndOut)
    newInput <- replInput
    evalResult <- performEvent $
      attachWith runReplStep (current stateAndOut) newInput
    return newInput

replInput :: MonadWidget t m => m (Event t Text)
replInput = do
    divClass "repl-input-controls" $ mdo
      el "span" $ text "pact>"
      ta <- textInput
            (def & setValue .~ (mempty <$ buttonClicked))
      let buttonClicked = keypress Enter ta
      -- (b,_) <- el' "button" $ text "Evaluate"
      -- let buttonClicked = domEvent Click b
      -- _ <- performEvent (liftJSM (pToJSVal (_textArea_element ta) ^. js0 ("focus" :: String)) <$ buttonClicked)
      return $ tag (current $ value ta) buttonClicked

runReplStep0
    :: MonadIO m
    => (ReplState, Seq DisplayedSnippet)
    -> Text
    -> m (ReplState, Seq DisplayedSnippet)
runReplStep0 (s1,snippets1) e = do
    (_,s2) <- liftIO $ runStateT (evalRepl' $ T.unpack e) s1
    return (s2, snippets1 <> S.singleton (OutputSnippet $ T.pack $ _rOut s2))

runReplStep
    :: MonadIO m
    => (ReplState, Seq DisplayedSnippet)
    -> Text
    -> m (ReplState, Seq DisplayedSnippet)
runReplStep (s1,snippets1) e = do
    (eterm,s2) <- liftIO $ runStateT (evalRepl' $ T.unpack e) s1
    return (s2, snippets1 <> S.fromList [InputSnippet ("pact> " <> e), OutputSnippet $ showResult eterm])

showResult :: Show a => Either String a -> Text
showResult (Right v) = T.pack $ show v
showResult (Left e) = "Error: " <> T.pack e

controlBar :: MonadWidget t m => m (ControlOut t)
controlBar = do
    elAttr "div" ("id" =: "options") $ do
      elAttr "div" ("style" =: "display: block;") $ do
        o <- elAttr "ul" ("class" =: "view_mode") $ do
          elAttr "li" ("id" =: "pactVersion") $
            elAttr "a" ("target" =: "_blank" <> "href" =: "https://github.com/kadena-io/pact") $ do
              is <- liftIO $ initReplState StringEval
              Right (TLiteral (LString ver) _) <- liftIO $ evalStateT (evalRepl' "(pact-version)") is
              text $ "Pact Version " <> ver
          d <- elAttr "li" ("class" =: "code_examples_fieldset") $ do
            elAttr "fieldset" ("class" =: "code_examples_fieldset") $ do
                dropdown 0 (constDyn $ fmap fst demos) def
          load <- elAttr "li" ("id" =: "loadButton" <> "class" =: "tooltip") $ do
            (e,_) <- elAttr' "label" ("id" =: "compile_now") $ text "Load"
            return $ domEvent Click e

          let intToCode n = snd $ fromJust $ M.lookup n demos
          return (ControlOut (intToCode <$> value d) load)
        elAttr "ul" ("class" =: "view_mode" <> "style" =: "float: right") $ do
          el "li" $
            el "label" $
              elAttr "a" ("target" =: "_blank" <>
                          "style" =: "color:black;text-decoration:none;" <>
                          "href" =: "http://pact-language.readthedocs.io"
                          ) $ do
                elAttr "i" ("class" =: "fa fa-book" <> "aria-hidden" =: "true") blank
                elAttr "span" ("id" =: "hideIfTiny" <> "class" =: "menu-link") $ text "Docs"
          el "li" $
            elAttr "a" ("target" =: "_blank" <> "href" =: "http://kadena.io") $
              elAttr "img" ("src" =: "img/kadena-logo84x20.png" <> "class" =: "logo-image") blank
        return o

exampleData :: [(Text, Text)]
exampleData =
  [ ("Hello World", "examples/helloWorld-1.0.repl")
  , ("Simple Payment", "examples/simplePayments-1.0.repl")
  , ("International Payment", "examples/internationalPayments-1.0.repl")
  , ("Commercial Paper", "examples/commercialPaper-1.0.repl")
  ]

demos :: Map Int (Text, Text)
demos = M.fromList $ zip [0..] exampleData

------------------------------------------------------------------------------
-- | We still have this hard coded initial value because Reflex has to put
-- some value in before the first event fires, so we use this one.  It should
-- match the first element of the exampleData list.
startingExpression :: Text
startingExpression = [s|
;;
;; "Hello, world!" smart contract/module
;;

;; Simulate message data specifying an administrator keyset.
;; In production use 'mockAdminKey' would be an ED25519 hex-encoded public key.
(env-data { "admin-keyset": ["mockAdminKey"] })

;; Simulate that we've signed this transaction with the keyset.
;; In pact, signatures are pre-validated and represented in the
;; environment as a list of public keys.
(env-keys ["mockAdminKey"])

;; Keysets cannot be created in code, thus we read them in
;; from the load message data.
(define-keyset 'admin-keyset (read-keyset "admin-keyset"))

;; Define the module.
(module helloWorld 'admin-keyset
  "A smart contract to greet the world."
  (defun hello (name)
    "Do the hello-world dance"
    (format "Hello {}!" [name]))
)

;; and say hello!
(hello "world")
|]
