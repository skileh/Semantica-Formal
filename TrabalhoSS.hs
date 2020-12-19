import Estado


data AExp = Num Int
     |Var String
     |Som AExp AExp
     |Sub AExp AExp
     |Mul AExp AExp
  deriving(Show)

data BExp = TRUE
     | FALSE
     | Not BExp
     | And BExp BExp
     | Or  BExp BExp
     | Ig  AExp AExp
     | Men  AExp AExp
   deriving(Show)

data CExp =    While BExp CExp
     | DoWhile BExp CExp
     | If BExp CExp CExp
     | Seq CExp CExp
     | Atrib AExp AExp
     | Skip
   deriving(Show)                



aSmallStep :: (AExp,Estado) -> (AExp,Estado)
-- variavel
aSmallStep (Var x,s) = (Num (procuraVar s x),s)
-- soma
aSmallStep (Som (Num x) (Num y), s) = (Num (x+y),s)
aSmallStep (Som (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
                                 in (Som (Num x) ef,s)
aSmallStep (Som e1 e2,s)  = let (ef,_) = aSmallStep (e1, s)
                            in (Som ef e2,s)
--subtração
aSmallStep (Sub (Num x) (Num y), s) = (Num (x-y),s)
aSmallStep (Sub (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
                                 in (Sub (Num x) ef,s)
aSmallStep (Sub e1 e2,s)  = let (ef,_) = aSmallStep (e1, s)
                            in (Sub ef e2,s)
--multiplicação
aSmallStep (Mul (Num x) (Num y), s) = (Num (x*y),s)
aSmallStep (Mul (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
                                 in (Mul (Num x) ef,s)
aSmallStep (Mul e1 e2,s)  = let (ef,_) = aSmallStep (e1, s)
                            in (Mul ef e2,s)


interpretA :: (AExp,Estado) -> (AExp,Estado)
interpretA (a,s) = if isFinalA a then (a,s) else interpretA (aSmallStep (a,s))

isFinalA :: AExp -> Bool
isFinalA (Num a) = True
isFinalA x = False

bSmallStep :: (BExp,Estado) -> (BExp,Estado)
bSmallStep (Not FALSE,s)      = (TRUE,s)

-- NOT
bSmallStep (Not TRUE,s)       = (FALSE, s)
bSmallStep (Not b, s) = let (bn,sn) = bSmallStep (b,s)
                        in (Not bn ,sn)

--AND
bSmallStep (And TRUE b2,s)  = (b2,s)
bSmallStep (And FALSE b2,s) = (FALSE,s)
bSmallStep (And b1 b2,s)    = let (bn,sn) = bSmallStep (b1,s)
                              in (And bn b2,sn)

--OR
bSmallStep (Or TRUE b2,s)  = (TRUE,s)
bSmallStep (Or FALSE b2,s) = (b2,s)
bSmallStep (Or b1 b2,s)    = let (bn,sn) = bSmallStep (b1,s)
                              in (Or bn b2,sn)

--IGUAL
bSmallStep (Ig (Num x) (Num y), s) = if (x==y) then (TRUE,s) else (FALSE, s)
bSmallStep (Ig (Num x) e2, s) = let (el,_) = aSmallStep (e2 ,s)
                                 in (Ig (Num x) el,s)
bSmallStep (Ig e1 e2,s)  = let (el,_) = aSmallStep (e1, s)
                            in (Ig el e2,s)

--MENOR
bSmallStep (Men (Num x) (Num y), s) = if (x<y) then (TRUE,s) else (FALSE, s)
bSmallStep (Men (Num x) e2, s) = let (el,_) = aSmallStep (e2 ,s)
                                 in (Men (Num x) el,s)
bSmallStep (Men e1 e2,s)  = let (el,_) = aSmallStep (e1, s)
                            in (Men el e2,s)

interpretB :: (BExp,Estado) -> (BExp,Estado)
interpretB (b,s) = if isFinalB b then (b,s) else interpretB (bSmallStep (b,s))

isFinalB :: BExp -> Bool
isFinalB TRUE = True
isFinalB FALSE = True
isFinalB x = False


--If
cSmallStep (If TRUE c1 c2,s) =  (c1,s) 
cSmallStep (If FALSE c1 c2,s) = (c2,s) 
cSmallStep (If b c1 c2,s) = let(bn,sn) = bSmallStep(b,s) 
                            in (If bn c1 c2, sn)   

--Atrib
cSmallStep (Atrib (Var x) (Num e),s) = (Skip, mudaVar s x e)
cSmallStep (Atrib (Var x) e,s) = let(el,sn) = aSmallStep(e,s) 
                                 in (Atrib (Var x) el, sn)

--Seq
cSmallStep (Seq Skip c,s) = (c,s) 
cSmallStep (Seq c1 c2,s) = let(cl,sn) = cSmallStep(c1,s) 
                           in (Seq cl c2, sn)
 
--While
cSmallStep (While b c, s) = ((If b (Seq c (While b c)) Skip), s)

--doWhile
cSmallStep (DoWhile b c, s) = (Seq c (While b c), s)

interpretC :: (CExp,Estado) -> (CExp,Estado)
interpretC (c,s) = if isFinalC c then (c,s) else interpretC (cSmallStep (c,s))

isFinalC :: CExp -> Bool
isFinalC Skip = True
isFinalC _ = False


meuEstado :: Estado
meuEstado = [("x",0), ("y",1), ("z",0)]

-- outroEstado :: Estado
-- outroEstado = [("x", TRUE), ("y", FALSE), ("z", TRUE)]

exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))
exemplo3 :: AExp
exemplo3 = Sub (Num 3) (Sub (Var "x") (Var "y"))
exemplo4 :: AExp
exemplo4 = Mul (Num 3) (Mul (Var "x") (Var "y"))

exemploAtrib :: CExp
exemploAtrib = Atrib (Var "x") (Var "y")

-- RODANDO O EXEMPLO:
-- Hugs> interpretA (exemplo, meuEstado)

testeNovo :: CExp
testeNovo = (While (Not (Ig (Var "x") (Num 3))) (Seq (Atrib (Var "x") 
                              (Som (Var "x") (Num 1))) (Atrib (Var "y") (Som (Var "y") (Num 3)))) )

exemplo2 :: BExp
exemplo2 = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE)

exemploOR :: BExp
exemploOR = Or FALSE (And TRUE (Not FALSE)) 
-- *Main> interpretB (exemplo2,meuEstado)
-- (TRUE,[("x",3),("y",0),("z",0)])

meuSigma :: Estado
meuSigma = [("x",10), ("y",1)]
--- TESTES

testec1 :: CExp
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y")))(Atrib (Var "y") (Var "z")))

fatorial :: CExp
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Ig (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mul (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))


menor1 :: BExp 
menor1 = (Men (Var "x") (Num 10))

exercicio :: CExp
exercicio = (Seq (Atrib (Var "x") (Num 0))
                  (While (Men (Var "x") (Num 10)) ( Atrib (Var "x") (Som (Var "x") (Num 1)))))