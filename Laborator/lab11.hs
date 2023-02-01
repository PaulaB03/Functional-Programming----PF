{-
class Functor f where
    fmap :: (a -> b) -> f a -> f b
-}

{-
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

Just length <*> Just "world"                -- Just 5
Just (++ " world") <*> Just "hello,"        -- Just "hello, world"
pure (+) <*> Just 3 <*> Just 5              -- Just 8
pure (+) <*> Just 3 <*> Nothing             -- Nothing
(++) <$> ["ha", "heh"] <*> ["?", "!0"]      -- ["ha?","ha!0","heh?","heh!0"]
-}

-- Exercitiul 1
data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b)
instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil

    (Cons f fs) <*> a = fmap f a `concat` (fs <*> a)
        where
            concat Nil b = b
            concat (Cons a xa) b = Cons a (xa `concat` b)

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))


-- Exercitiul 2
data Cow = Cow {
    name :: String,
    age :: Int,
    weight :: Int
    } deriving (Eq, Show)

-- a
noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty x = Just x

noNegative :: Int -> Maybe Int
noNegative x = if x < 0 then Nothing else Just x

test21 = noEmpty "abc"      -- Just "abc"
test22 = noNegative (-5)    -- Nothing
test23 = noNegative 5       -- Just 5]

-- b
cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString s x y = if ((Nothing == noEmpty s) || (Nothing `elem` [noNegative x, noNegative y])) 
                      then Nothing 
                      else Just (Cow s x y)

test24 = cowFromString "Milka" 5 100    -- Just (Cow {name = "Milka", age = 5, weight = 100})

-- c 
cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' s x y = Cow <$> noEmpty s <*> noNegative x <*> noNegative y


-- Exercitiul 3
newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)
data Person = Person Name Address
    deriving (Eq, Show)

-- a
validateLength :: Int -> String -> Maybe String
validateLength a str = if a > length str then Just str else Nothing

-- b
mkName :: String -> Maybe Name
-- mkName str = if validateLength 25 str == Nothing then Nothing else Just (Name str)
mkName s = case l of
    Nothing -> Nothing
    Just x -> Just (Name x)
    where l = validateLength 25 s

mkAddress :: String -> Maybe Address
-- mkAddress str = if validateLength 100 str == Nothing then Nothing else Just (Address str)
mkAddress s = case l of
    Nothing -> Nothing
    Just x -> Just (Address x)
    where l = validateLength 25 s

-- c
mkPerson :: String -> String -> Maybe Person
-- mkPerson s1 s2 = if elem Nothing [mkName s1, mkName s2] then Nothing else Just (Person (Name s1) (Address s2))
mkPerson s1 s2 = if (Nothing == mkName s1 || Nothing == mkAddress s2) then Nothing else Just (Person (Name s1) (Address s2))

-- d
mkName' :: String -> Maybe Name
mkName' s = Name <$> validateLength 25 s

mkAddress' :: String -> Maybe Address
mkAddress' s = Address <$> validateLength 100 s

mkPerson' :: String -> String -> Maybe Person
mkPerson' s1 s2 = Person <$> mkName' s1 <*> mkAddress' s2