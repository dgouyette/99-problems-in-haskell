import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

    it "finds the last element of a list of numbers" $ do
      myLast [1, 2, 3, 4] `shouldBe` 4
    
    it "should raise an error if array is empty" $ do 
      myLast [] `shouldThrow `anyException 
   
    it "should find antepenultimate element" $ do 
      myAntepenultimate [ 1,2,3,4,5] `shouldBe` 4 

    it "should find an element by index" $ do
     myThElement [1,2,3,4, 5] 2 `shouldBe` 3 

    it "should return the length of a list" $ do
      myLength [123, 456, 789] `shouldBe` 3

    it "should reverse a string" $ do 
      myReverse "abcdedf" `shouldBe` "fdedcba"

    it "should say that madamimadam is a palindrom" $ do
       isPalindrome "madamimadam" `shouldBe` True 

isPalindrome :: (Eq a) => [a] -> Bool 
isPalindrome  a = (myReverse a) == a 
myReverse :: [a] -> [a]
myReverse l = internalReverse l []
internalReverse (x : xs) r = internalReverse xs (x : r )
internalReverse [] r = r

myLength :: [a] -> Integer
myLength (_ : xs) = 1 + myLength xs 
myLength [] = 0 
myThElement :: [a] -> Integer -> a
myThElement (x:_) 0 = x
myThElement (x:xs) n = myThElement xs (n - 1)

myAntepenultimate :: [a]-> a
myAntepenultimate (a : (_ : [])) = a
myAntepenultimate (a : xs) = myAntepenultimate xs
myAntepenultimate [] = error "no antepnultimate on empty list" 

myLast :: [a] -> a 
myLast [] = error "no end on an empty list"
myLast [a] = a
myLast (_ : xs) = myLast xs

