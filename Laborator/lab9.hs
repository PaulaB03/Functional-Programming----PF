import Main (Prop)

type Nume = String
data Prelude
    = Var Nume
    | F
    | T 
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop
    deriving Eq
infixr 2 :|:
infixr 3 :&:

-- Exercitiul 1
p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: ((Not (Var "P")) :&: (Not F))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: (Not (Var "P") :&: Not (Var "Q")) :&: (Not (Var "P") :&: Not (Var "R"))

-- Exercitiul 2
instance Show Prop where
    show (Var x) = x
    show F = "False"
    show T = "True"
    show (Not p) = "~" ++ show p
    show (p :|: q) = "(" ++ show p ++ "|" ++ show q ++ ")"
    show (p :&: q) = "(" ++ show p ++ "&" ++ show q ++ ")"
    show (p :->: q) = "(" ++ show p ++ "->" ++ show q ++ ")"
    show (p :<->: q) = "(" ++ show p ++ "<->" ++ show q ++ ")"

-- Exercitiul 3
type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval (Var x) env = impureLookup x env
eval T env = True
eval F env = False
eval (Not p) env = not (eval p env)
eval (p :|: q) env = (eval p env) || (eval q env)
eval (p :&: q) env = (eval p env) && (eval q env)
eval (p :->: q) env = not (eval p env) || (eval q env)
eval (p :<->: q) env = (eval p env) == (eval q env)

test_eval = eval  (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

-- Exercitiul 4
variabile :: Prop -> [Nume]
variabile (Var p) = [p]
variabile (Not p) = variabile p
variabile (p :&: q) = nub $ variabile p ++ variabile q
variabile (p :|: q) = nub $ variabile p ++ variabile q
variabile (p :->: q) = nub $ variabile p ++ variabile q
variabile (p :<->: q) = nub $ variabile p ++ variabile q
variabile _ = [] -- T si F  

test_variabile = variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

-- Exercitiul 5
envs :: [Nume] -> [Env]
envs [] = []
envs [x] = [[(x, False)], [(x, True)]]
envs (l:ls) = foldr (\x t -> [((l, False) : x), ((l, True) : x)] ++ t) [] (envs ls)

test_envs = 
    envs ["P", "Q"]
    ==
    [ [ ("P",False)
      , ("Q",False)
      ]
    , [ ("P",False)
      , ("Q",True)
      ]
    , [ ("P",True)
      , ("Q",False)
      ]
    , [ ("P",True)
      , ("Q",True)
      ]
    ]

-- Exercitiul 6
satisfiabila :: Prop -> Bool
satisfiabila p = or $ map (eval p) $ envs $ variabile p

test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

-- Exercitiul 7
valida :: Prop -> Bool
valida p = not $ satisfiabila (Not p)

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True

-- Exercitiul 10
echivalenta :: Prop -> Prop -> Bool
echivalenta p1 p2 = valida (p1 :<->: p2)

test_echivalenta1 =
  True
  ==
  (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 =
  False
  ==
  (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 =
  True
  ==
  (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))