module ShowQ where

import Language.Haskell.TH

-- | Show string representation of template haskell expression, such as AST:
-- @$(showQ [| \x -> x |])@
--
-- This is useful to see how the code is represented
showQ :: Show a => Q a -> Q Exp
showQ x = LitE . StringL . show <$> x

-- | Show string representation of template haskell expression without 'Q' Monad, such as names:
-- @$(showQ' 'id)@
--
-- This is useful to see how the code is represented
showQ' :: Show a => a -> Q Exp
showQ' = pure . LitE . StringL . show
