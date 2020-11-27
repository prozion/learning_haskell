-- 1.2.2
sign x = if x >= 0
            then (if x == 0 then 0 else 1)
            else (-1)
