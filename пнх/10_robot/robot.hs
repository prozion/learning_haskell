
-- Robot = (([Char], Int, Int) -> (([Char], Int, Int) -> a) -> a) -> a
robot (name, attack, hp) = \message -> message (name, attack, hp)

name (n, _, _) = n
attack (_, a, _) = a
hp (_, _, hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

setName aRobot newName = aRobot (\(_, a, h) -> robot(newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, _, h) -> robot(n, newAttack, h))
setHP aRobot newHP = aRobot (\(n, a, _) -> robot(n, a, newHP))

printRobot aRobot = aRobot (\(n, a, h) -> n ++ " attack:" ++ show(a) ++ " hp:"++ show(h))

-- damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))
damage aRobot attackDamage = robot (n, a, h - attackDamage)
                              where n = getName aRobot
                                    a = getAttack aRobot
                                    h = getHP aRobot

fight attacker defender = damage defender attack
  where attack = if getHP attacker > 10
                  then getAttack attacker
                  else 0

all_robots = [
  robot ("T800", 15, 40),
  robot ("T101", 35, 100),
  robot ("T1000", 30, 300),
  robot ("TX", 60, 250)]


-- task 1
hps = map getHP all_robots

-- task 2
threeRoundFight r1 r2 = threeRoundFightIter r1 r2 3
  where threeRoundFightIter r1 r2 0 = if getHP r1 > getHP r2
                                      then r1
                                      else r2
        threeRoundFightIter r1 r2 n = threeRoundFightIter (fight r2 r1) (fight r1 r2) (n-1)


-- t = threeRoundFight (all_robots !! 1) (all_robots !! 2)

-- task 3
robot1 = all_robots !! 0
robot2 = all_robots !! 1
robot3 = all_robots !! 2

robot4 = all_robots !! 3

t = map (fight robot4) [robot1 robot2 robot3]
