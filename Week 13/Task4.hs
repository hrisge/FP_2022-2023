import Data.List

main :: IO()
main = do
    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 2), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 2)] == "English"
    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 5), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 5)] == "Maths"

hardestSubject :: [Record] -> Subject
hardestSubject [] = error "Empty List!"
hardestSubject rs = fst $ foldl1 (\ s1@(subject1, avgNote1) s2@(subject2, avgNote2) -> if avgNote1 < avgNote2 then s1 else s2) notes
 where
    subjects = nub [y | (_, y, _) <- rs]
    notes = [ (s, average [note | (_, cs, note) <- rs, s == cs]) | s <- subjects]

average :: (Num a, Fractional a) => [a] -> a
average xs = sum xs / (fromIntegral $ length xs) 

type Student = String
type Subject = String
type Note = Double
type Record = (Student, Subject, Note)