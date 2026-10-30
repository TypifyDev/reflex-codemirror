{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
module Reflex.CodeMirror ( module Reflex.CodeMirror.Types
                         , module Reflex.CodeMirror.FFI
                         , codemirror
                         ) where
import "base"             Control.Monad.IO.Class (liftIO)
import "base"             Data.IORef (IORef, newIORef, writeIORef, readIORef)
import "text"             Data.Text (Text)
import "jsaddle"          Language.Javascript.JSaddle hiding (askJSM) -- (JSVal) --  GHCJS.Types (JSVal)
import                    GHCJS.DOM.Element hiding (scrollIntoView) -- (IsElement)
import                    Reflex.CodeMirror.FFI
import                    Reflex.CodeMirror.Types hiding (configuration)

import           Effectful                              (Eff, (:>), IOE)
import qualified Reflex.Class                                 as R
import qualified Reflex.Effectful.DomHost                   as ED
import           Reflex.Effectful.DomHost (_element_raw)
import qualified Reflex.Host.HeadlessEffectful      as EPB
import qualified Reflex.Host.HeadlessEffectful   as EPE
import qualified Reflex.Host.HeadlessEffectful   as ETE
import           Reflex.Effectful.DomHost (JSM', askJSM)

codemirror
  :: forall es t.
     (
       ED.DomBuilder t (ED.DomBuilderSpace es) :> es
     , EPB.PostBuild t :> es
     , EPE.PerformEventH t :> es
     , ETE.TriggerEvent t :> es
     , JSM' :> es
     , IOE :> es
     , R.Reflex t
     , t ~ ED.DomBuilderTimeline es
     , ED.DomSpace (ED.DomBuilderSpace es)
     , R.Reflex (ED.DomBuilderTimeline es)
     , IsElement (ED.RawElement (ED.DomBuilderSpace es))
     )
  => Configuration
  -> R.Event t Text
  -> R.Event t (Maybe LineChar)
  -> Eff es (R.Event t Text)
codemirror configuration textE scrollToE = do
    -- HTML element
    (element_ :: ED.Element ED.EventResult (ED.DomBuilderSpace es) t, _)
      <- ED.el' "textarea" $ ED.text $ maybe "" id (_configuration_value configuration)

    -- local state
    (ref :: IORef (Maybe CodeMirrorRef)) <- liftIO $ newIORef Nothing

    -- input event
    postBuildTaggedE <- EPB.getPostBuildE
    let inputE = R.leftmost
               [ (Nothing, Nothing)        <$  postBuildTaggedE
               , (\x -> (Nothing,x))       <$> scrollToE
               , (\x -> (Just x, Nothing)) <$> textE
               ]

    -- output event + trigger
    (outE :: R.Event t Text, triggerOut) <- ETE.newTriggerEventE

    -- Capture the ambient JS context ONCE at widget-build time so we can
    -- run 'JSM' actions inside 'IO' callbacks (performEvent_ demands
    -- 'EventME Global', which wraps IO — no ambient MonadJSM available in
    -- the callback body).
    ctx <- askJSM

    -- handle input event
    EPE.performEventH_ $ R.ffor inputE $ \(mText, mScrollTo) ->
      (runJSM
        (handle (_element_raw element_)
                ref
                triggerOut
                configuration
                mText
                mScrollTo)
        ctx)
    return outE
  where
        handle :: (IsElement el)
               => el
               -- ^ Element
               -> IORef (Maybe CodeMirrorRef)
               -- ^ Local state
               -> (Text -> IO ())
               -- ^ Trigger for output event
               -> Configuration
               -- ^ Chart data
               -> (Maybe Text)
               -- ^ Text
               -> (Maybe LineChar)
               -- ^ Scroll to
               -> JSM ()
        handle element_ ref trigger configuration_ mText mScrollTo = do
            currentRef_ <- liftIO $ readIORef ref
            case currentRef_ of
                Nothing   -> onFirstTime element_ ref trigger configuration_ mText mScrollTo
                Just ref_ -> onNextTime  ref_                 configuration_ mText mScrollTo

        onFirstTime :: (IsElement el)
                    => el
                    -> IORef (Maybe CodeMirrorRef)
                    -> (Text -> IO ())
                    -> Configuration
                    -> (Maybe Text)
                    -> (Maybe LineChar)
                    -> JSM ()
        onFirstTime element_ ref trigger configuration_ mText mScrollTo = do
            ref_ <- fromTextArea element_ configuration_
            liftIO $ writeIORef ref (Just ref_)
            registerOnChange ref_
                             (onChangeCallback trigger)
            case mText of
                Nothing -> return ()
                Just text_ -> setValue ref_ text_
            case mScrollTo of
                Nothing       -> return ()
                Just scrollTo_ -> scrollIntoView ref_ scrollTo_ 200

            return ()

        onNextTime :: CodeMirrorRef
                   -> Configuration
                   -> (Maybe Text)
                   -> (Maybe LineChar)
                   -> JSM ()
        onNextTime ref_ _ mText mScrollTo = do
            case mText of
                Nothing -> return ()
                Just text_ -> setValue ref_ text_
            case mScrollTo of
                Nothing        -> return ()
                Just scrollTo_ -> scrollIntoView ref_ scrollTo_ 200

        onChangeCallback :: (Text -> IO ())
                         -> Text
                         -> JSM ()
        onChangeCallback trigger t = do
            liftIO $ trigger t
            return ()
