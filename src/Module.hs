module Module where



-- imports

import           Text.Printf



-- location info

data Location = SourceFile { locSource :: String
                           , locLine   :: Int
                           , locColumn :: Int
                           }
              | CommandLineArgument
              | BuiltIn

data WithLocation a = WL { getLocation :: Location
                         , getEntry    :: a
                         }

instance Show Location where
  show CommandLineArgument = "command-line argument"
  show BuiltIn             = "<built-in function>"
  show (SourceFile s l c)  = printf "%s:%d:%d" s l c

instance Show a => Show (WithLocation a) where
  show (WL loc x) = printf "%s: %s" (show loc) $ show x

instance Functor WithLocation where
  fmap f (WL l x) = WL l $ f x
