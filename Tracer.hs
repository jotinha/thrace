module Tracer (traceRay) where

import qualified Tracers.Basic as TB
import qualified Tracers.SimpleRecursive as TSR
import qualified Tracers.IndirectLighting as TIL
import Config

traceRay = case tracerType of 
  Basic -> TB.traceRay
  SimpleRecursive -> TSR.traceRay
  IndirectLighting -> TIL.traceRay

