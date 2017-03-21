{-# LANGUAGE RecursiveDo #-}
module Examples where

import Ant

monad_test :: AntM Int ()
monad_test = mdo
    senseFood <- label
    sense Ahead Food
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

        flip' 3 (turn DLeft)
            -- or go right,
            (flip' 2 (turn DRight)
               -- or go forward
               (move_ (goto noFood)))
        goto senseFood
     )

    senseHome <- label
    sense Ahead Home (
        -- we are home!
        move  (drop' >> goto senseFood) (goto senseHome)

     ) (mdo {
        -- we are not home, so either go left,
        notHome <- label;
        flip' 3 (turn DLeft) (mdo
           -- or go right,
           flip' 2 (turn DRight) (mdo
              -- or go forward
              move_ (goto notHome)
            )
        );
        goto senseHome;
     })

