
> {-# LANGUAGE Rank2Types, TypeFamilies, FlexibleInstances #-}
> module Pi where

> import Control.Applicative
> import Control.Concurrent

A finally tagless encoding of the Pi calculus. Symantics is a portmanteau of Syntax and Semantics.

> class Symantics p where
>     type Name p :: *
>     new :: (Name p -> p) -> p
>     out :: Name p -> Name p -> p -> p
>     (|||) :: p -> p -> p
>     inn :: Name p -> (Name p -> p) -> p
>     rep :: p -> p
>     nil :: p
>     embed :: IO () -> p

Type level fixed points

> newtype Nu f = Nu { nu :: f (Nu f) }

> fork :: IO () -> IO ()
> fork a = forkIO a >> return ()

> forever :: IO a -> IO a
> forever p = p >> forever p

Executable semantics

> instance Symantics (IO ()) where
>     type Name (IO ()) = Nu Chan
>     new f = Nu <$> newChan >>= f
>     a ||| b = forkIO a >> fork b
>     inn (Nu x) f = readChan x >>= fork . f
>     out (Nu x) y b = writeChan x y >> b
>     rep = forever
>     nil = return ()
>     embed = id

A closed pi calculus term

> newtype Pi = Pi { runPi :: forall a. Symantics a => a }

> run :: Pi -> IO ()
> run (Pi a) = a

> example = Pi (new $ \z -> (new $ \x -> out x z nil
>                                    ||| (inn x $ \y -> out y x $ inn x $ \ y -> nil))
>                   ||| inn z (\v -> out v v nil))

A pretty printer for the pi calculus

> newtype Pretty = Pretty { runPretty :: [String] -> Int -> ShowS }
>
> instance Symantics Pretty where
>     type Name Pretty = String
>     new f = Pretty $ \(v:vs) n ->
>         showParen (n > 10) $
>             showString "nu " . showString v . showString ". " .
>             runPretty (f v) vs 10
>     out x y b = Pretty $ \vs n ->
>         showParen (n > 10) $
>             showString x . showChar '<' . showString y . showString ">. " .
>             runPretty b vs 10
>     inn x f = Pretty $ \(v:vs) n ->
>         showParen (n > 10) $
>             showString x . showChar '(' . showString v . showString "). " .
>             runPretty (f v) vs 10
>     p ||| q = Pretty $ \vs n ->
>         showParen (n > 4) $
>             runPretty p vs 5 .
>             showString " | " .
>             runPretty q vs 4
>     rep p = Pretty $ \vs n ->
>         showParen (n > 10) $
>             showString "!" .
>             runPretty p vs 10
>     nil = Pretty $ \_ _ -> showChar '0'
>     embed io = Pretty $ \_ _ -> showString "{IO}"

> instance Show Pi where
>     showsPrec n (Pi p) = runPretty p vars n
>         where
>             vars = fmap return vs ++
>                    [i : show j | j <- [1..], i <- vs] where
>             vs = ['a'..'z']
