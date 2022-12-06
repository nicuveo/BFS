module Misc where

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f ... g) x y = f $ g x y

onNothing :: Monad m => Maybe a -> m a -> m a
onNothing m a = maybe a pure m

onNothingM :: Monad m => m (Maybe a) -> m a -> m a
onNothingM m a = maybe a pure =<< m

onLeft :: Monad m => Either e a -> (e -> m a) -> m a
onLeft m a = either a pure m

onLeftM :: Monad m => m (Either e a) -> (e -> m a) -> m a
onLeftM m a = either a pure =<< m

brainfuckChars :: String
brainfuckChars = "+-,.<>[]#"
