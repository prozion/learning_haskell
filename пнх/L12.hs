data Foo = Foo | Bar | Baz

foo :: Foo -> Bool
foo Bar = True
foo _ = False

data Patient = Patient {
                name :: Name,
                sex :: Sex,
                age :: Int,
                height :: Int,
                weight :: Int,
                bloodType :: BloodType
              }

data Name = Name FirstName LastName |
            NameWithMiddle FirstName MiddleName LastName

type FirstName = String
type MiddleName = String
type LastName = String

data Sex = Male | Female

data BloodType = BloodType AB0Type RhType

data AB0Type = O | A | B | AB
data RhType = Pos | Neg

canDonateTo :: Patient -> Patient -> Bool
canDonateTo patient1 patient2 = canDonateTo' (bloodType patient1) (bloodType patient2)
  where canDonateTo' (BloodType O _) _ = True
        canDonateTo' _ (BloodType AB _) = True
        canDonateTo' (BloodType A _) (BloodType A _) = True
        canDonateTo' (BloodType B _) (BloodType B _) = True
        canDonateTo' _ _ = False

-- data Reverse = Reverse

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
-- showName (Name f l) Reverse = l ++ ", " ++ f
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l
-- showName (NameWithMiddle f m l) Reverse = l ++ ", " ++ f ++ " " ++ m

showSex :: Sex -> String
showSex Male = "мужской"
showSex Female = "женский"

showAB0 :: AB0Type -> String
showAB0 O = "O"
showAB0 A = "A"
showAB0 B = "B"
showAB0 AB = "AB"

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showBloodType :: BloodType -> String
showBloodType (BloodType ab0 rh) = showAB0 ab0 ++ showRh rh

patientSummary :: Patient -> String
patientSummary patient =
  (take fw $ repeat '*')
  ++ "Name: " ++ showName (name patient) ++ br
  ++ "Пол: " ++ showSex (sex patient) ++ br
  ++ "Возраст: " ++ show (age patient) ++ br
  ++ "Рост: " ++ show (height patient) ++ "см" ++ br
  ++ "Вес: " ++ show (weight patient) ++ "кг" ++ br
  ++ "Тип крови: " ++ showBloodType (bloodType patient) ++ br
  ++ (take fw (repeat '*'))
  where fw = 14
        br = "\n"

p1 = Patient {
      name = Name "John" "Ratty",
      sex = Male,
      age = 30,
      height = 169,
      weight = 70,
      bloodType = BloodType AB Neg
    }
