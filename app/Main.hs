{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import                     Prelude hiding (head)
import "lens"              Control.Lens
import "data-default"      Data.Default (def)
import "aeson"             Data.Aeson (Value(..))
import "text"              Data.Text (pack)
import "reflex-utils"      Reflex.Utils
import "reflex-codemirror" Reflex.CodeMirror

import           Effectful                              (Eff, (:>), IOE)
import qualified Reflex.Class                                 as R
import qualified Reflex.Dom.Core                   as ED
import qualified Reflex.Host.HeadlessEffectful           as EHold
import qualified Reflex.PostBuild.Class      as EPB
import qualified Reflex.PerformEvent.Class   as EPE
import qualified Reflex.TriggerEvent.Class   as ETE
import           Reflex.Dom.Core (JSM')
import           Reflex.Dom.Core              (mainWidget)

--
main :: IO ()
main = mainWidget main_
    where
        main_ :: forall es t.
                 (
                   ED.DomBuilder t (ED.DomBuilderSpace es) :> es, ED.DomSpace (ED.DomBuilderSpace es), t ~ ED.DomBuilderTimeline es
                 , EPB.PostBuild t :> es
                 , EPE.PerformEvent t :> es
                 , ETE.TriggerEvent t :> es
                 , EHold.Hold t :> es
                 , JSM' :> es
                 , IOE :> es
                 , R.Reflex t
                 )
              => Eff es ()
        main_ = do
            headD <- head
            whenLoaded [headD] ED.blank body
            return ()

--
head :: forall es t.
        (
          ED.DomBuilder t (ED.DomBuilderSpace es) :> es, ED.DomSpace (ED.DomBuilderSpace es), t ~ ED.DomBuilderTimeline es
        , EPB.PostBuild t :> es
        , EPE.PerformEvent t :> es
        , ETE.TriggerEvent t :> es
        , JSM' :> es
        , IOE :> es
        , R.Reflex t
        )
     => Eff es (R.Dynamic t Bool)
head = do
    s1Ds <- sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/codemirror.min.js"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/codemirror.min.css"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/theme/zenburn.css"
                     ]
    whenLoaded s1Ds ED.blank $ do
        _ <- sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/mode/haskell/haskell.min.js"
                      ]
        return ()

--
body :: forall es t.
        (
          ED.DomBuilder t (ED.DomBuilderSpace es) :> es, ED.DomSpace (ED.DomBuilderSpace es), t ~ ED.DomBuilderTimeline es
        , EPB.PostBuild t :> es
        , EPE.PerformEvent t :> es
        , ETE.TriggerEvent t :> es
        , EHold.Hold t :> es
        , JSM' :> es
        , IOE :> es
        , R.Reflex t
        )
     => Eff es ()
body = do
    clickE <- ED.button "goto line 3"
    let lineCharE = (Just $ LineChar 3 1) <$ clickE
    click2E <- ED.button "change text"
    let textE = ("from event" <$ click2E)
    textE' <- codemirror config textE lineCharE
    textD <- EHold.holdDyn "" textE'
    ED.display textD
    where
        config :: Configuration
        config
            = def
            & configuration_value ?~ pack "hello\nworld"
            & configuration_theme ?~ pack "zenburn"
            & configuration_mode  ?~ (String $ pack "haskell")
