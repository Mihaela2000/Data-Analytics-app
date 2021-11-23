module Tasks where

import Dataset
import Text.Printf
import Data.List

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

{-
    TASK SET 1
-}

-- Task 1
null_to_zero :: [Value] -> [Value]
null_to_zero [] = []
null_to_zero (x:xs)
    | x == "" = ("0"):(null_to_zero xs)
    | otherwise = x:(null_to_zero xs)

oral_notes :: [Value] -> [Value]
oral_notes l = null_to_zero (init (tail l))

float_oral_notes :: [Value] -> [Float]
float_oral_notes (x:xs) = map (read :: String -> Float) (oral_notes (x:xs))

add_oral_grades l = foldr (\x y-> x+y) 0 l
compute_exam_grades_line l = (add_oral_grades (float_oral_notes l))/4 + (read (last l) :: Float)

compute_exam_grades :: Table -> Table
compute_exam_grades [] =[]
compute_exam_grades (x:xs) = if (head x) == "Nume" then (first_line x):(compute_exam_grades xs)
    else (other_line x):(compute_exam_grades xs)

first_line x = ["Nume","Punctaj Exam"]
other_line x = (head x):[printf "%.2f" (compute_exam_grades_line x)]


-- Task 2
-- Number of students who have passed the exam:
get_passed_students_num_line l = if (read (last l) :: Float ) >= 2.5 then 1 else 0

get_passed_students_num :: Table -> Int
get_passed_students_num l = foldr aux 0 (tail (compute_exam_grades l))
    where
        aux el acc = if (get_passed_students_num_line el) == 1 then acc + 1 else acc

-- Percentage of students who have passed the exam:
get_students_num :: Table -> Int
get_students_num l = foldr aux 0 (tail (compute_exam_grades l))
    where
        aux el acc = acc + 1

get_passed_students_percentage :: Table -> Float
get_passed_students_percentage l = (fromIntegral (get_passed_students_num l)) / (fromIntegral (get_students_num l))

-- Average exam grade

get_students_note_line l = read (last l) :: Float

get_students_note l = foldr aux 0 (tail (compute_exam_grades l))
    where
        aux el acc = acc + (get_students_note_line el)

get_exam_avg :: Table -> Float
get_exam_avg l = (get_students_note l)/(fromIntegral (get_students_num l))

-- Number of students who gained at least 1.5p from homework:
get_sum_hw_num_line l = foldr aux 0 (get_read_hw_num_line l)
    where
        aux el acc = acc + el
get_passed_hw_num_line l = if (get_sum_hw_num_line l) >= 1.5 then 1 else 0

get_passed_hw_num :: Table -> Int
get_passed_hw_num l = foldr aux 0 (tail l)
    where
        aux el acc = if (get_passed_hw_num_line el) == 1 then acc + 1 else acc

get_read_hw_num_line l = map (read :: String -> Float ) (null_to_zero (take 3 (drop 2 l)))

-- Task 3
get_read_responses_per_qs_line l = map (read :: String -> Float ) (null_to_zero (tail l))

get_sum_responses_per_qs_line l = foldr aux 0 l
    where
        aux el acc = acc + el

get_avg_responses_per_qs_x i l = (get_sum_responses_per_qs_line (get_read_responses_per_qs_line
    (head (drop i (transpose l)))))/(fromIntegral (get_students_num l))

get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs l =[["Q1","Q2","Q3","Q4","Q5","Q6"]]++[[printf "%.2f" (get_avg_responses_per_qs_x 1 l)]++
    [printf "%.2f" (get_avg_responses_per_qs_x 2 l)]++[printf "%.2f" (get_avg_responses_per_qs_x 3 l)]++
    [printf "%.2f" (get_avg_responses_per_qs_x 4 l)]++[printf "%.2f" (get_avg_responses_per_qs_x 5 l)]++
    [printf "%.2f" (get_avg_responses_per_qs_x 6 l)]]

-- Task 4
get_read_summary_line l = map (read :: String -> Int ) (null_to_zero (tail l))

get_exam_summary_sum_Qij i j l = length (filter (\x -> x == j) (get_read_summary_line (head (drop i (transpose l)))))

get_exam_summary :: Table -> Table
get_exam_summary l = [["Q","0","1","2"]]++
    [["Q1"]++[show (get_exam_summary_sum_Qij 1 0 l)]++
    [show (get_exam_summary_sum_Qij 1 1 l)]++
    [show (get_exam_summary_sum_Qij 1 2 l)]]++
    [["Q2"]++[show (get_exam_summary_sum_Qij 2 0 l)]++
    [show (get_exam_summary_sum_Qij 2 1 l)]++
    [show (get_exam_summary_sum_Qij 2 2 l)]]++
    [["Q3"]++[show (get_exam_summary_sum_Qij 3 0 l)]++
    [show (get_exam_summary_sum_Qij 3 1 l)]++
    [show (get_exam_summary_sum_Qij 3 2 l)]]++
    [["Q4"]++[show (get_exam_summary_sum_Qij 4 0 l)]++
    [show (get_exam_summary_sum_Qij 4 1 l)]++
    [show (get_exam_summary_sum_Qij 4 2 l)]]++
    [["Q5"]++[show (get_exam_summary_sum_Qij 5 0 l)]++
    [show (get_exam_summary_sum_Qij 5 1 l)]++
    [show (get_exam_summary_sum_Qij 5 2 l)]]++
    [["Q6"]++[show (get_exam_summary_sum_Qij 6 0 l)]++
    [show (get_exam_summary_sum_Qij 6 1 l)]++
    [show (get_exam_summary_sum_Qij 6 2 l)]]

-- Task 5
get_ranking :: Table -> Table
get_table_name_exam_notfinal [] = []
get_table_name_exam_notfinal (x:xs) = ([(last x)]++[(head x)]):(get_table_name_exam_notfinal xs)

get_table_name_exam l = sort (tail (get_table_name_exam_notfinal (compute_exam_grades l)))

get_table_name_exam_not [] = []
get_table_name_exam_not l = (get_table_name_exam (tail (compute_exam_grades l)))

get_ranking_line l = [(last l)]++[(head l)]
get_ranking_notfinal [] = []
get_ranking_notfinal (x:xs) = (get_ranking_line x):(get_ranking_notfinal xs)

get_ranking l = [["Nume","Punctaj Exam"]]++(get_ranking_notfinal (get_table_name_exam l))

-- Task 6

compute_exam_oral_grades_line' l = (add_oral_grades (float_oral_notes l))/4

compute_exam_oral_grades' :: Table -> Table
compute_exam_oral_grades' [] =[]
compute_exam_oral_grades' (x:xs) = if (head x) == "Nume" then (compute_exam_oral_grades' xs)
    else (other_line_oral' x):(compute_exam_oral_grades' xs)
other_line_oral' x = [printf "%.2f" (compute_exam_oral_grades_line' x)]

exam_written_grades' :: Table -> Table
exam_written_grades' [] =[]
exam_written_grades' (x:xs) = if (head x) == "Nume" then (exam_written_grades' xs)
    else (other_line_written' x):(exam_written_grades' xs)
other_line_written' x = [last x]

get_exam_table' l= zipWith (++) (compute_exam_oral_grades' l) (exam_written_grades' l)


minus :: [String] -> String

minus l
    | (read (head l) :: Float) > (read (last l) :: Float) = printf "%.2f" ((read (head l) :: Float) - (read (last l) :: Float))
    | otherwise = printf "%.2f" ((read (last l) :: Float) - (read (head l) :: Float))

get_read_exam_table' [] = []
get_read_exam_table' (x:xs) = (minus x):(get_read_exam_table' xs)

get_exam_diff_table_notfinal l = map words (["Diferenta"]++(get_read_exam_table' (get_exam_table' l)))


compute_exam_oral_grades_line l = (add_oral_grades (float_oral_notes l))/4

compute_exam_oral_grades :: Table -> Table
compute_exam_oral_grades [] =[]
compute_exam_oral_grades (x:xs) = if (head x) == "Nume" then (first_line_oral x):(compute_exam_oral_grades xs)
    else (other_line_oral x):(compute_exam_oral_grades xs)

first_line_oral x = ["Nume","Punctaj Interviu"]
other_line_oral x = (head x):[printf "%.2f" (compute_exam_oral_grades_line x)]

exam_written_grades :: Table -> Table
exam_written_grades [] =[]
exam_written_grades (x:xs) = if (head x) == "Nume" then (first_line_written x):(exam_written_grades xs)
    else (other_line_written x):(exam_written_grades xs)
first_line_written x = ["Punctaj scris"]
other_line_written :: [String] -> [String]
other_line_written x
    | length (last x) == 4 = [last x]
    | otherwise = [(last x)++"0"]

get_exam_table l= zipWith (++) (compute_exam_oral_grades l) (exam_written_grades l)

get_exam_table_with_diff l = zipWith (++) (get_exam_table l) (get_exam_diff_table_notfinal l)

get_table_name_exam_notfinal_6 [] = []
get_table_name_exam_notfinal_6 (x:xs) = ([(head ( reverse x))]++[(head x)]++[head (tail x)]++[last (init x)]):(get_table_name_exam_notfinal_6 xs)


get_table_name_exam_6 l = sort (tail (get_table_name_exam_notfinal_6 (get_exam_table_with_diff l)))


get_table_name_exam_not_6 [] = []
get_table_name_exam_not_6 l = (get_table_name_exam_6 (tail (get_exam_table_with_diff l)))

get_ranking_line_6 l = [(head ( tail l))]++[head (drop 2 l)]++[last (drop 2 l)]++[(head l)]
get_ranking_notfinal_6 [] = []
get_ranking_notfinal_6 (x:xs) = (get_ranking_line_6 x):(get_ranking_notfinal_6 xs)

get_exam_diff_table :: Table -> Table
get_exam_diff_table l = [["Nume","Punctaj interviu","Punctaj scris","Diferenta"]]++(get_ranking_notfinal_6 (get_table_name_exam_6 l))

{-
    TASK SET 2
-}

-- CSV to Table
split_csv_row :: CSV -> Row
split_csv_row = foldr op []
    where
        op c [] = [[c]]
        op '\n' acc = []:acc
        op c (x:xs) = (c:x):xs

split_csv_table :: String -> [String]
split_csv_table l = map (reverse) (reverse
    (foldl (\(x:xs) el -> if el == ',' then []:(x:xs) else (el:x):xs) [[]] l))

read_csv :: CSV -> Table
read_csv l = map (split_csv_table) (split_csv_row l)

-- Table to CSV
write_csv_row :: Row -> CSV
write_csv_row (x:xs) = foldl (\a b -> a ++ "," ++ b) x xs

write_csv_table :: Table -> CSV
write_csv_table [] = ""
write_csv_table (x:xs) = (write_csv_row x) ++ "\n" ++ (write_csv_table xs)

write_csv :: Table -> CSV
write_csv (x:xs) = init $ (write_csv_table (x:xs))

-- Task 1
as_list :: String -> Table -> [String]
as_list col_name (x:xs)
    | (head x) == col_name = tail $ head $ transpose (x:xs)
    | otherwise = as_list col_name $ transpose $ tail $ transpose (x:xs)

-- Task 2
mysort colnr r1 r2 
    |(r1 !! colnr) > (r2 !! colnr) || (r1 !! colnr) == (r2 !! colnr) && (head r1) > (head r2) = GT
    |otherwise = LT

tsort :: String -> Table -> Table
tsort colname (x:xs) = x : (sortBy (mysort (col_nr x colname)) xs)
-- Task 3
vmap :: (Value -> Value) -> Table -> Table
vmap f table = map (map f) table

-- Task 4
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f col_name table = (col_name):(map (f) (tail table))

get_hw_grade_total :: Row -> Row
get_hw_grade_total row = [head row]++[nr_len (printf "%.2f" (foldl (+) 0 (float_hw_notes row)))]

nr_len :: Value -> Value
nr_len x
    | length x == 3 = x++"0"
    | otherwise = x

float_hw_notes :: [Value] -> [Float]
float_hw_notes (x:xs) = map (read :: String -> Float) (hw_notes (x:xs))

hw_notes :: [Value] -> [Value]
hw_notes l = null_to_zero (take 7 (drop 2 l))

-- Task 5
vunion :: Table -> Table -> Table
vunion t1 t2
    | head t1 == head t2 = t1++(tail t2)
    | otherwise = t1

-- Task 6
hunion :: Table -> Table -> Table
hunion t1 t2 
    | (length t1) > (length t2) = zipWith (++) t1 (t2++(full_table ((length t1)-(length t2)) (length (head t2))))
    | otherwise = zipWith (++) t1 t2

full_table nr_row nr_col = [[""| x<-[1.. nr_col]] | y<-[1..nr_row]]

-- Task 7
tjoin :: String -> Table -> Table -> Table
tjoin colname [] [] = []
tjoin colname t1 t2
    | ((head (head t1))==(head (head t2))) = [((head t1)++(tail (head t2)))]++(tjoin colname (tail t1) (tail t2))
    | otherwise = [((head t1)++(full_row ((length (head t2))-1)))]++(tjoin colname (tail t1) (t2))

full_row nr_col = [""| x<-[1..nr_col]]

-- Task 8
cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian f _ [] t2 = []
cartesian f _ t1 [] = []
cartesian f [] t1 t2 = (map (f (head t1)) t2)++(cartesian f [] (tail t1) t2)
cartesian f col t1 t2 = [col]++(cartesian f [] (tail t1) (tail t2))

-- Task 9
search name [] = 0
search name names
    | name == head names = 1
    | otherwise = search name (tail names)

projection_tr :: [String] -> Table -> Table
projection_tr names [] = []
projection_tr names table = (if ((search (head (head (transpose table))) names) == 1) then [(head (transpose table))] else [])++projection_tr names (transpose (tail (transpose table)))

projection :: [String] -> Table -> Table
projection names table = transpose (projection_tr names table)

{-
    TASK SET 3
-}

data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query
 
-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

data QResult = CSV CSV | Table Table | List [String]
-- don't confuse first 'CSV' and second 'CSV': first refers to constructor name,
-- second refers to type CSV (defined in taskset 1); same with 'Table'.

-- Task 3.1
instance Show QResult where
    show (CSV str) = show str
    show (Table table) = write_csv table
    show (List list) = show list

-- Task 3.2
class Eval a where
    eval :: a -> QResult
 
instance Eval Query where
    eval (FromCSV str) = Table (read_csv str)
    eval (ToCSV query) = CSV (show (eval query))
    eval (AsList colname query) = List (as_list colname (read_csv (show (eval query))))
    eval (Sort colname query) = Table (tsort colname (read_csv (show (eval query))))
    eval (ValueMap op query) = Table (vmap op (read_csv (show (eval query))))
    eval (RowMap op colnames query) = Table (rmap op colnames (read_csv (show (eval query))))
    eval (VUnion query1 query2) = Table (vunion (read_csv (show (eval query1))) (read_csv (show (eval query2))))
    eval (HUnion query1 query2) = Table (hunion (read_csv (show (eval query1))) (read_csv (show (eval query2))))
    eval (TableJoin colname query1 query2) = Table (tjoin colname (read_csv (show (eval query1))) (read_csv (show (eval query2))))
    eval (Cartesian op colnames query1 query2) = Table (cartesian op colnames (read_csv (show (eval query1))) (read_csv (show (eval query2))) )
    eval (Projection colnames query) = Table (projection colnames (read_csv (show (eval query))))
    eval (Filter condition query) = Table ([head (read_csv (show (eval query)))] ++ (filter (feval (head (read_csv (show (eval query)))) condition) (tail (read_csv (show (eval query))))))

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

type FilterOp = Row -> Bool

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

-- Task 3.3
instance FEval Float where
    feval l (Eq colname ref) = \row -> (read (head (drop ((col_nr l colname)) row))  :: Float) == ref
    feval l (Lt colname ref) = \row -> (read (head (drop ((col_nr l colname)) row))  :: Float) < ref
    feval l (Gt colname ref) = \row -> (read (head (drop ((col_nr l colname)) row))  :: Float) > ref
    feval l (In colname list) = \row -> (search (read (head (drop ((col_nr l colname)) (null_to_zero row)))  :: Float) list) == 1
    feval l (FNot cond) = \row -> not ((feval l cond row))
    feval l (FieldEq colname1 colname2) = \row -> (read (head (drop ((col_nr l colname1)) row))  :: Float) == (read (head (drop ((col_nr l colname2)) row))  :: Float)
    
col_nr [] colname = 0
col_nr l colname 
    | colname == head l = 0
    | otherwise = 1+(col_nr (tail l) colname)

instance FEval String where
    feval l (Eq colname ref) = \row -> head (drop ((col_nr l colname)) row) == ref
    feval l (Lt colname ref) = \row -> head (drop ((col_nr l colname)) row) < ref
    feval l (Gt colname ref) = \row -> head (drop ((col_nr l colname)) row) > ref
    feval l (In colname list) = \row -> (search (head (drop ((col_nr l colname)) row)) list) == 1
    feval l (FNot cond) = \row -> not ((feval l cond row))
    feval l (FieldEq colname1 colname2) = \row -> head (drop ((col_nr l colname1)) row) == head (drop ((col_nr l colname2)) row)


similarities_query :: Query
similarities_query = undefined
