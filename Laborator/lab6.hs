-- Exercitiul 1
data Fruct 
    = Mar String Bool
    | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [ Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]


-- a)
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala x _) = elem x ["Tarocco", "Moro", "Sanguinello"]
ePortocalaDeSicilia _ = False

test_ePortocalaDeSicilia1 = ePortocalaDeSicilia (Portocala "Moro" 12)
test_ePortocalaDeSicilia2 = ePortocalaDeSicilia (Mar "Ionatan" True)


-- b)
nrFelii :: Fruct -> Int
nrFelii (Portocala x y) = y

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (h:t)
    | ePortocalaDeSicilia h = nrFelii h + nrFeliiSicilia t
    | otherwise = nrFeliiSicilia t

test_nrFeliiSicilia = nrFeliiSicilia listaFructe


-- c)
areViermi :: Fruct -> Int
areViermi (Mar _ True) = 1
areViermi (Mar _ False) = 0
areViermi (Portocala _ _) = 0

nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi (h:t) = areViermi h + nrMereViermi t

test_nrMereViermi = nrMereViermi listaFructe


-- Exercitiul 2
type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show 


-- a) 
vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"


-- b) 
rasa :: Animal -> Maybe String
rasa (Pisica _) = Nothing
rasa (Caine _ r) = Just r


-- Exercitiul 3
data Linie = L [Int]
    deriving Show
data Matrice = M [Linie]
    deriving Show


-- a)
sumLinie :: Linie -> Int
sumLinie (L l) = sum l

verifica :: Matrice -> Int -> Bool
verifica (M ls) n = foldr (&&) True [sumLinie l == n | l <- ls]

test_veri1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10
test_veri2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25


-- b)
doarPoz :: Linie -> Bool
doarPoz (L l) = foldr (&&) True [x > 0 | x <- l]

lengthLinie :: Linie -> Int
lengthLinie (L l) = length l

doarPozN :: Matrice -> Int -> Bool
doarPozN (M ls) n = foldr (&&) True [doarPoz l | l <- ls, n == lengthLinie l]
 
testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3
testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3


-- c)
corect :: Matrice -> Bool
corect (M ls) = foldr (&&) True [lengthLinie (head ls) == lengthLinie l' | l' <- tail ls]

testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]])
testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]])