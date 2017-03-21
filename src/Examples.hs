{-# LANGUAGE RecursiveDo #-}
module Examples where

import Ant

monad_test :: AntM L ()
monad_test = mdo
    senseFood <- label
    sense ahead food
      (mdo
        -- there is food.
        -- move, go back if fails
        move_   (goto senseFood)
        -- pick up the food
        pickup_ (goto senseFood)

        -- go home
        goto senseHome;
      )
      (mdo
        -- there is no food, so either go left,
        noFood <- label

        flip' 3 (turn left)
            -- or go right,
            (flip' 2 (turn right)
               -- or go forward
               (move_ (goto noFood)))
        goto senseFood
     )

    senseHome <- label
    sense ahead home (
        -- we are home!
        move  (drop' >> goto senseFood) (goto senseHome)

     ) (mdo {
        -- we are not home, so either go left,
        notHome <- label;
        flip' 3 (turn left) (mdo
           -- or go right,
           flip' 2 (turn right) (mdo
              -- or go forward
              move_ (goto notHome)
            )
        );
        goto senseHome;
     })

