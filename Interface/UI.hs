module Interface.UI where

import Interface.Board


class Monad ui => UI ui where
    initialize :: BoardState b => b -> ui (Status state)
    loop :: state -> ui (Status state)


data Status state = Continue state
                  | Exit Int String  -- status code and user message
