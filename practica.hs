import Data.Char (digitToInt)

-- Get the digits of a number as a list
digits :: Int -> [Int]
digits = map digitToInt . show

-- Take a sublist of elements
sublist :: Int -> Int -> [Int] -> [Int]
sublist x y list = take (y - x + 1) (drop x list)

-- Transform a list of numbers into a number
backToNumber :: [Int] -> Int
backToNumber list = read(concat(map show list))

-- Make a number out of some digits 
getDigits :: Int -> Int -> Int -> Int
getDigits x y num = backToNumber(sublist x y (digits num))

-- Function to get the academic period
firstDigit = getDigits 0 

year = firstDigit 1

academicPeriod :: Int -> String
academicPeriod n = "20" ++ show(year n) ++ "-" ++ show(digits n !! 2)

-- Function to calculate the aliquot sum
aliquotSum :: Int -> Int
aliquotSum n = sum [x | x <- [1..(n `div` 2)], n `mod` x == 0]

-- Function to get the academic program category
program = getDigits 3 4

classifyProgram :: Int -> String
classifyProgram n
    | s == program n  = "Engineering"
    | s > program n   = "Administrative"
    | s < program n  = "Humanities"
  where s = aliquotSum (program n)

-- Function to get the consecutive admission number from the last three digits
admissionNumber :: Int -> String
admissionNumber code = "num" ++ show (code `mod` 1000)

-- Function to determine if the number is odd or even
oddOrEven :: Int -> String
oddOrEven code = if code `mod` 2 == 0 then "even" else "odd"

-- Function to process the student ID code
processCode :: Int -> String
processCode code = academicPeriod code ++ " " ++ classifyProgram code ++ " " ++ admissionNumber code ++ " " ++ oddOrEven code

-- Main function for testing
main :: IO()
main = do
    a <- readLn :: IO Int
    putStrLn $ processCode a
