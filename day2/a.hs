{-# LANGUAGE LambdaCase #-}

import Control.Monad.State

data Mvmnt = Forward Int | Down Int | Up Int deriving (Show, Eq)

data Submarine = Submarine {y :: Int, x :: Int, aim :: Int} deriving (Show)

type SubState a = StateT Submarine IO a

parse :: [String] -> [Mvmnt]
parse [] = []
parse (x : y : xs) = case x of
  "forward" -> Forward (read y) : parse xs
  "up" -> Up (read y) : parse xs
  "down" -> Down (read y) : parse xs
  _ -> error $ "bad parse: " ++ x
parse _ = error "bad parse, missing arguments"

step :: Mvmnt -> SubState ()
step = \case
  Up a -> modify (\s -> s {aim = aim s - a}) -- >> printState
  Down a -> modify (\s -> s {aim = aim s + a}) -- >> printState
  Forward a -> modify (\s -> s {y = aim s * a + y s, x = a + x s}) -- >> printState

printState :: SubState ()
printState = do
  sub <- get
  (liftIO . print) $
    concat
      [ "depth:",
        show (y sub),
        "",
        " - horizontal: ",
        show (x sub),
        "",
        " - aim:",
        show (aim sub)
      ]

go :: [Mvmnt] -> SubState Submarine
go xs = mapM_ step xs >> printState >> get

main = flip evalStateT (Submarine 0 0 0) . go . parse . words =<< readFile "inp.txt"