-- ex6.2.hs

import TimeMachine

updatePrice :: Float -> [TimeMachine] -> [TimeMachine]
updatePrice percent ts = ts & traversed.price %~ (\p -> p*(1+percent/100))