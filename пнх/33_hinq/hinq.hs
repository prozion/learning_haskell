import Control.Monad

data Name = Name { firstName :: String, lastName :: String }
data GradeLevel = Freshman | Sophomore | Junior | Senior deriving (Eq, Ord, Enum, Show)
data Student = Student { studentId :: Int, gradeLevel :: GradeLevel, studentName :: Name } deriving Show
data Teacher = Teacher { teacherId :: Int, teacherName :: Name } deriving Show
data Course = Course { courseId :: Int, courseTitle :: String, teacher :: Int } deriving Show
data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a) | HINQ_ (m a -> m b) (m a)
data Enrollment = Enrollment { student :: Int, course :: Int } deriving Show

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

students :: [Student]
students = [(Student 1 Senior (Name "Audrey" "Lord")),
            (Student 2 Junior (Name "Lesley" "Silco")),
            (Student 3 Freshman (Name "Judith" "Buttler")),
            (Student 4 Senior (Name "Gee" "Debore")),
            (Student 5 Sophomore (Name "Jean" "Baudrillard")),
            (Student 6 Junior (Name "Julia" "Kristeva"))]

teachers :: [Teacher]
teachers = [Teacher 100 (Name "Simone" "de Beauvoir"), Teacher 200 (Name "Susan" "Sontag")]

courses :: [Course]
courses = [Course 101 "French language" 100, Course 201 "English language"]

enrollments :: [Enrollment]
enrollments = [(Enrollment 1 101), (Enrollment 2 101), (Enrollment 2 201), (Enrollment 3 101),
               (Enrollment 4 201), (Enrollment 4 101), (Enrollment 5 101), (Enrollment 6 201)]

-- _select :: (a -> b) -> [a] -> [b]
_select :: Monad m => (a -> b) -> m a -> m b
_select prop vals = do
  val <- vals
  return (prop val)

_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where test vals = do
  val <- vals
  guard (test val)
  return val

_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)
_join data1 data2 prop1 prop2 = do
  d1 <- data1
  d2 <- data2
  let dpairs = (d1, d2)
  guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs))
  return dpairs

-- selectQuery (whereQuery joinQuery)
_hinq selectQuery joinQuery whereQuery =
  (\ joinData ->
    (\ whereResult ->
      selectQuery whereResult)
    (whereQuery joinData)
  ) joinQuery

-- finalResult :: [Name]
-- finalResult = _hinq (_select (teacherName . fst))
--                     (_join teachers courses teacherId teacher)
--                     (_where ((== "English language") . courseTitle . snd))

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ sClause jClause) = _hinq sClause jClause (_where (\ _ -> True))

query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ (_select (teacherName . fst))
              (_join teachers courses teacherId teacher)
              (_where ((== "English language") . courseTitle . snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName) teachers

----- 33.6.1
possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 = HINQ
                (_select (teacherName . fst))
                (_join possibleTeacher possibleCourse teacherId teacher)
                (_where ((== "French language") . courseTitle . snd))

missingCourse Maybe Course
missingCourse = Nothing

maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 = HINQ
                (_select (teacherName . fst))
                (_join possibleTeacher missingCourse teacherId teacher)
                (_where ((== "French language") . courseTitle . snd))

studentEnrollmentsQ = HINQ_ (_select (\(st, en) -> (studentName st, course en))
                            (_join students enrollments studentId student)

studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQ                            
