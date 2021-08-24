doubleUs x y = x*2 + y*2
doubleMe x = x * 2

doubleSmallNumber x = if x > 100
                        then x
                        else x*2;

doubleSmallNumber' x = (if x > 100 then x else x*2)+1

testName = "Test Name"

identifyCamel humps = if humps == 1
                        then "Bactrian"
                        else "Duoback"

hypotenuse x y = sqrt (x ^ 2 + y ^ 2)

estCapital z = z+2

identifyUno uno = if uno == 1
                then "One"
                else "Not One" 
judge min max value = value <= max && value >= min

f :: [Int] -> Int
z :: [Int] -> Int
   
f ls = head ls + length ls
z h = length h + last h 

dividesEvenly :: Int -> Int -> Bool
dividesEvenly x y = (y `div` x) * x == y

increasing :: (Ord a)=>[a]->Bool
increasing xs = if xs == []
                then True
                else if tail xs == []
                    then True
                    else if head xs <= head (tail xs)
                        then increasing (tail xs)
                    else False

-- | Pattern matching syntax

increasing' :: (Ord a)=>[a]->Bool
increasing' [] = True
increasing' [x] = True
increasing' (x:y:z) = x <= y && increasing (y:z)

increasing'' :: (Ord a)=>[a]->Bool
increasing'' (x:y:z) = x <= y && increasing (y:z)
increasing'' _ = True

noVowels :: [Char] -> [Char]
noVowels word = if word == ""
                then ""
                else if head word `elem` "AEIOUaeiou"
                    then noVowels (tail word)
                    else head word : noVowels (tail word)

-- | Pattern matched
noVowels' :: [Char]->[Char]
noVowels' "" = ""
noVowels' (x:xs) = if x `elem` "AEIOUaeiou"
                  then noVowels' xs
                  else x : noVowels' xs  

-- | Guards -> | Bool  = then expression

noVowels'' :: [Char] -> [Char]
noVowels'' "" = ""
noVowels'' (x:xs)
                |x `elem` "AEIOUaeiou" = noVowels'' xs
                | otherwise            = x : noVowels'' xs

watch :: Int -> [Char]
watch n = if n == 7
          then show n ++ " o'clock and.... TORNADO"
          else show n ++ " o'clock and all's well"

-- | Pattern matching

watch' :: Int -> [Char]
watch' n = show n ++ " o clock and" ++ message n -- | result where
           where message 7 = "...Sharknado" -- | pattern =  result
                 message _ = " all is well" -- | pattern = result

-- | Case/Switch statement
watch'' :: Int -> [Char]
watch'' n = show n ++ " o clock and " ++ case n of 7 -> "...Sharknado"  -- | case expression of pattern -> result
                                                   _ -> " all is welldoS"

gravity :: (Fractional a) => a -> a
gravity r = 6.674e-11 * 5.972e24 / (r ^ 2)

lucky :: (Integral a)=> a -> String
lucky 7 = "Lucky Number Seven"
lucky x = "Sorry pal"

sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!" 
sayMe x = "Not between 1 and 5"

charName :: Char -> String
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"
charName _ = "No existence"

first :: (a,b,c) -> a
first (x,_,_) = x

second:: (a,b,c) -> b
second (_,y,_) = y

third :: (a,b,c) -> c
third (_,_,z) = z

-- | Pattern matching with lis
head' :: [a] -> a
head' [] = error "No empty list idiot"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "This is an empty list"
tell (x:[]) = "This list contain only one element which is: " ++ show x -- | Deal with singleton list
tell (x:y:[]) = "This list contains only 2 elements which are: " ++ show x ++ " and " ++ show y -- | Deals with 2 elements list
tell (x:y:_) = "This list contains too many elements, the first 2 are: " ++ show x ++ " and " ++ show y -- | Deals with more than 2 elements list

-- | Custom length function using combination of pattern matching and recursion
length' :: Num b => [a] -> b -- | the variable is important, the function ultimately wants length of list and hence why Num b type is used
length' [] = 0
length' (_:xs) = 1 + length' xs


-- | Custom sum using combination of pattern matching and recursion
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs 

-- | Utilization of patterns
lines' :: String -> String
lines' "" = error "Empty String"
lines' o@(x:xs) = "The 1st letter of " ++ o ++ " is " ++ [x]

-- |Guards
tellBmi :: (RealFloat a) =>a -> String
tellBmi bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal but I bet you're ugly"
    | bmi <= 30.0 = "You're fat! Lose some weight fatty"
    | otherwise = "Congratulations, you're a whale"

test' :: Int -> String
test' a = if a == 0
        then "Hello World"
        else if a == 1 
            then "Hello No"
            else if a == 2
                then "Wassup"
                else "Chew"

tellBmi' :: (RealFloat a) => a -> a -> String
tellBmi' weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
    | weight / height ^ 2<= 25.0 = "You're supposedly normal but I bet you're ugly"
    | weight / height ^ 2<= 30.0 = "You're fat! Lose some weight fatty"
    | otherwise = "Congratulations, you're a whale"

max' :: (Ord a) => a -> a -> a
max' a b
    | a < b = b
    | otherwise = a

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
    | a > b = GT
    | a < b = LT 
    | otherwise = EQ

improvBmi :: (RealFloat a) => a -> a -> String
improvBmi weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal but I bet you're ugly"
    | bmi <= fat = "You're fat! Lose some weight fatty"
    | otherwise = "Congratulations, you're a whale"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

initials :: String -> String -> String
initials first last = [f] ++ ". " ++ [l] ++ ". "
    where (f:_) = first
          (l:_) = last

initials'' :: String -> String -> String
initials'' (f:_) (l:_) = [f] ++ ". " ++ [l]

calcBmis::(RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w,h) <- xs]
    where bmi x z = x / z ^ 2

-- | versus
calcBmis' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- | list, predicate to return specific results
calcBmis'' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]


-- | Case expression (similar to pattern matching)
head'' :: [a] -> a
head'' xs =  case xs of [] -> error "No head in this list"
                        (x:_) -> x

-- | case expression and its flexible positioning
describeList :: [a] -> String
describeList xs = "The list is" ++ case xs of [] -> " No list"
                                              [x] -> " Singleton"
                                              xs -> " longer list"

-- | Pattern matching in definations is syntactic sugar to case expression so:
describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what [] = "no list"
          what [x] = " Singleton list"
          what xs = " longer list"