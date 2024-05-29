{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module OptEnvConf.ArgMap
  ( ArgMap (..),
    empty,
    hasUnconsumed,
    Dashed (..),
    renderDashed,
    parse,
    consumeArg,
    consumeOpt,
    parseSingleArg,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity
import Data.Validity.Containers ()
import GHC.Generics (Generic)

data ArgMap = ArgMap
  { argMapArgs :: ![String],
    argMapSwitches :: ![Dashed],
    argMapOptions :: !(Map Dashed (NonEmpty String)),
    argMapLeftovers :: ![String]
  }
  deriving (Show, Eq, Generic)

instance Validity ArgMap

empty :: ArgMap
empty =
  ArgMap
    { argMapArgs = [],
      argMapSwitches = [],
      argMapOptions = M.empty,
      argMapLeftovers = []
    }

hasUnconsumed :: ArgMap -> Bool
hasUnconsumed am =
  not (null (argMapArgs am))
    || not (null (argMapSwitches am))
    || not (null (argMapOptions am))

data Dashed
  = DashedShort !Char
  | DashedLong !(NonEmpty Char)
  deriving (Show, Eq, Ord, Generic)

instance Validity Dashed

renderDashed :: Dashed -> String
renderDashed = \case
  DashedShort c -> ['-', c]
  DashedLong cs -> '-' : '-' : NE.toList cs

parse :: [String] -> ArgMap
parse args =
  let (os, leftovers) = parseOpts args
      am = go os
   in am {argMapLeftovers = leftovers}
  where
    go :: [Opt] -> ArgMap
    go = \case
      [] -> empty
      (a : rest) ->
        let am = go rest
         in case a of
              OptArg s -> am {argMapArgs = s : argMapArgs am}
              OptSwitch d -> am {argMapSwitches = d : argMapSwitches am}
              OptOption d v -> am {argMapOptions = M.insertWith (<>) d (v :| []) (argMapOptions am)}

-- The type is a bit strange, but it makes dealing with the state monad easier
consumeArg :: ArgMap -> (Maybe String, ArgMap)
consumeArg am = case argMapArgs am of
  [] -> (Nothing, am)
  (a : rest) -> (Just a, am {argMapArgs = rest})

consumeOpt :: Dashed -> ArgMap -> (Maybe String, ArgMap)
consumeOpt dashed am =
  let m = argMapOptions am
   in case M.lookup dashed m of
        Nothing -> (Nothing, am)
        Just (v :| vs) ->
          ( Just v,
            am
              { argMapOptions = case NE.nonEmpty vs of
                  Nothing -> M.delete dashed m
                  Just ne -> M.insert dashed ne m
              }
          )

data Opt
  = OptArg String
  | OptSwitch !Dashed
  | OptOption !Dashed !String

parseOpts :: [String] -> ([Opt], [String])
parseOpts = go
  where
    go = \case
      [] -> ([], [])
      (s : rest) ->
        let combs ls (ls', leftovers) = (ls ++ ls', leftovers)
            comb l = combs [l]
         in case parseSingleArg s of
              ArgBareDoubleDash -> ([], rest)
              ArgBareDash -> OptArg "-" `comb` go rest
              ArgPlain a -> OptArg a `comb` go rest
              ArgDashed isLong key ->
                let ds = parseDasheds isLong key
                    asSwitches = map OptSwitch (NE.toList ds) `combs` go rest
                 in case NE.nonEmpty rest of
                      Nothing -> asSwitches
                      Just (a :| others) ->
                        let asOption v =
                              let ss = NE.init ds
                                  o = NE.last ds
                               in (map OptSwitch ss ++ [OptOption o v]) `combs` go others
                         in case parseSingleArg a of
                              ArgBareDoubleDash -> asSwitches
                              ArgDashed _ _ -> asSwitches
                              ArgPlain val -> asOption val
                              ArgBareDash -> asOption "-"

    parseDasheds :: Bool -> NonEmpty Char -> NonEmpty Dashed
    parseDasheds b s =
      if b
        then DashedLong s :| []
        else NE.map DashedShort s

data Arg
  = ArgBareDoubleDash
  | ArgBareDash
  | ArgDashed !Bool !(NonEmpty Char) -- True means long
  | ArgPlain !String
  deriving (Show, Eq, Generic)

instance Validity Arg where
  validate arg =
    mconcat
      [ genericValidate arg,
        case arg of
          ArgDashed False (c :| _) -> declare "The first character of a short dashed is not a dash" $ c /= '-'
          ArgPlain s -> declare "does not start with a dash" $ case s of
            ('-' : _) -> False
            _ -> True
          _ -> valid
      ]

parseSingleArg :: String -> Arg
parseSingleArg = \case
  '-' : '-' : rest -> case NE.nonEmpty rest of
    Nothing -> ArgBareDoubleDash
    Just ne -> ArgDashed True ne
  '-' : rest -> case NE.nonEmpty rest of
    Nothing -> ArgBareDash
    Just ne -> ArgDashed False ne
  s -> ArgPlain s
