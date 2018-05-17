{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

-- |
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Main where

------------------------------------------------------------------------------
import           Control.Monad.State.Strict
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex
import           Reflex.Dom hiding (Element, fromJSString)
import           Reflex.Dom.ACE
------------------------------------------------------------------------------
import           Examples
import           Pact.Repl
import           Pact.Repl.Types
import           Pact.Types.Lang
------------------------------------------------------------------------------

main :: IO ()
main = mainWidget app

data ControlOut t = ControlOut
    { controlDropdown  :: Dropdown t Int
    , controlLoadEvent :: Event t ()
    }

app :: MonadWidget t m => m ()
app = do
    ControlOut d le <- controlBar
    elAttr "div" ("id" =: "editor_view") $ do
      let getDemo = snd . (demos M.!)
      let changeExample = getDemo <$> updated (value d)
      code <- elAttr "div" ("id" =: "column1") $ codeWidget hello changeExample
      elAttr "div" ("id" =: "separator" <> "class" =: "separator") blank
      elAttr "div" ("id" =: "column2") $ do
        elAttr "div" ("id" =: "code") $ do
          mapM_ snippetWidget staticReplHeader
          widgetHold blank (replWidget <$> tag (current code) le)
          return ()

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
      , OutputSnippet ";; Enter pact commands here"
      ]

snippetWidget :: MonadWidget t m => DisplayedSnippet -> m ()
snippetWidget (InputSnippet t) = el "pre" $ text t
snippetWidget (OutputSnippet t) = el "pre" $ text t

------------------------------------------------------------------------------
replWidget
    :: MonadWidget t m
    => Text
    -> m ()
replWidget initialCode = mdo
    initState <- liftIO $ initReplState StringEval
    stateAndOut0 <- runReplStep0 (initState, mempty) initialCode
    stateAndOut <- holdDyn stateAndOut0 evalResult

    _ <- dyn (mapM_ snippetWidget . snd <$> stateAndOut)
    newInput <- replInput
    evalResult <- performEvent $
      attachWith runReplStep (current stateAndOut) newInput
    return ()

replInput :: MonadWidget t m => m (Event t Text)
replInput = do
    divClass "repl-input-controls" $ mdo
      ti <- textArea $ def
        & attributes .~ constDyn mempty -- ("class" =: "repl-input")
        & setValue .~ (mempty <$ buttonClicked)
      (b,_) <- el' "button" $ text "Evaluate"
      let buttonClicked = domEvent Click b
      return $ tag (current $ value ti) buttonClicked

runReplStep0
    :: MonadIO m
    => (ReplState, Seq DisplayedSnippet)
    -> Text
    -> m (ReplState, Seq DisplayedSnippet)
runReplStep0 (s1,snippets1) e = do
    (eterm,s2) <- liftIO $ runStateT (evalRepl' $ T.unpack e) s1
    return (s2, snippets1 <> S.singleton (OutputSnippet $ T.pack $ _rOut s2))

runReplStep
    :: MonadIO m
    => (ReplState, Seq DisplayedSnippet)
    -> Text
    -> m (ReplState, Seq DisplayedSnippet)
runReplStep (s1,snippets1) e = do
    (eterm,s2) <- liftIO $ runStateT (evalRepl' $ T.unpack e) s1
    return (s2, snippets1 <> S.fromList [InputSnippet e, OutputSnippet $ showResult eterm])

showResult :: Show a => Either String a -> Text
showResult (Right v) = T.pack $ show v
showResult (Left e) = "Error: " <> T.pack e

demos :: Map Int (Text, Text)
demos = M.fromList $ zip [0..] exampleData

controlBar :: MonadWidget t m => m (ControlOut t)
controlBar = do
    elAttr "div" ("id" =: "options") $ do
      elAttr "div" ("style" =: "display: block;") $ do
        o <- elAttr "ul" ("class" =: "view_mode") $ do
          d <- elAttr "li" ("class" =: "code-example-dropdown") $ do
            elAttr "fieldset" ("class" =: "code_examples_fieldset") $ do
              elAttr "li" ("id" =: "code_examples_fieldset") $ do
                dropdown 0 (constDyn $ fmap fst demos) def
          load <- elAttr "li" ("style" =: "padding-left: 10px;" <> "class" =: "tooltip") $ do
            (e,_) <- elAttr' "label" ("id" =: "compile_now") $ text "Load"
            return $ domEvent Click e
            --elClass "span" "tooltiptext" $ text "Click or Shift-Enter"
          return (ControlOut d load)
        elAttr "ul" ("class" =: "view_mode" <> "style" =: "float: right") $ do
          elAttr "img" ("src" =: "img/kadena-logo84x20.png" <> "class" =: "logo-image") blank
        return o
