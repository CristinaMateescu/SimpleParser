module Parser
where

import Util
import Data.Maybe
import Data.List
import ClassState
import InferenceDataType
import Data.Map (Map)
import qualified Data.Map as Map

--OBS : Sursa are 3 parti : 
   -- rezolvarea subpunctului b si definitia functiilor auxiliare
   -- rezolvarea subpunctului c si definitia functiilor auxiliare 
   -- definitia functiilor auxiliare pentru bonus


-- definire Program : Map ( cheie: nume_clasa , valoare : pereche ( Clasa, nume_parinte  )
data Program = P (Map String (ClassState, String) )



                                       --- SUBPUNCTUL b ---

-- Initializarea unui program cu Clasa Global (prin conventie, avand parintele Global)
initEmptyProgram :: Program
initEmptyProgram = P (Map.insert "Global" ((initEmptyClass) , "Global" ) Map.empty) 


-- Variabilele (aflate in Global) se returneaza folosind un apel al functiei getValues (a)
getVars :: Program -> [[String]]
getVars (P program) = getValues clasa Var 
                      where  clasa = fst (Map.findWithDefault (Class Map.empty,[]) "Global" program) 


-- Simbolurile claselor reprezinta chei in program, returnam o lista formata din toate cheile
getClasses :: Program -> [String]
getClasses (P program) = Map.keys program


-- Se cauta clasa in program (map) folosind cheia : numele functiei
-- Valorile map-ului sunt de forma ( ClassState , simbol_parinte ) 
-- -> extragem parintele si il returnam
getParentClass :: String -> Program -> String
getParentClass clasa (P program) = snd (Map.findWithDefault (Class Map.empty,[]) clasa program) 


-- Se cauta clasa in Map 
-- Folosim un apel getValues (a) cu cheia Func pentru a returna lista functiilor 
getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass clasa (P prog) = getValues (fst (Map.findWithDefault (initEmptyClass,"") clasa prog )) Func


-- Tipul Instruction are 4 constructori
--  Clasa nume_clasa ( ClassState, nume_parinte )
--  Variabila [nume_variabila, tip_variabila]
--  Functie [ clasa, nume, tip_returnat, lista_parametri ]
--  Expresie nume_variabila expresie

data Instruction = Clasa String (ClassState, String) | Variabila [String] | Functie [String] | Expresie String Expr


-- Parsarea se face spargand inputul in linii
-- Fiecare linie se parseaza folosind functia auxiliara parseLine
-- Liniile goale se ignora
parse :: String -> [Instruction]
parse input = map (\line -> (parseLine line)) (filter (/="") (lines input))


-- Interpretarea se face in functie de tipul instructiunii
interpret :: Instruction -> Program -> Program


-- Interpret  Clasa - inseram in map : ( cheie : nume, valoare : (clasa, parinte) )
-- Daca parintele clasei nu exista, el este setat ca "Global"
interpret (Clasa name (clasa, parent)) (P prog) = if (Map.member parent prog ) 
                                                  then P (Map.insert name (clasa, parent) prog) 
                                                  else P ( Map.insert name ( clasa, "Global") prog )


-- Interpret Variabila -- inseram variabila in clasa Global
-- Se insereaza doar daca tipul variabilei este o clasa existenta in program
interpret (Variabila elements) (P prog) = if ( Map.member (head ( drop 1 elements) ) prog ) 
                                          then P (Map.insert "Global" (clasa, "Global") prog) 
                                          else (P prog) 
                                          where clasa = insertIntoClass (getClass "Global" (P prog)) Var elements



-- Interpret Functie -- tinem cont de conventie : Functie [ clasa, nume, tip_returnat, lista_parametri ]
-- Se insereaza daca instructiunea e valida, adica : 
--  Clasa din care face parte exista
--  Tipul de return este o clasa existenta in program
--  Instructiunea respecta formatul
--  Argumentele au tipuri valide - clase existente in program : folosim isParam definita mai jos
interpret (Functie elements) (P prog) = if ( Map.member clasa prog ) && (Map.member ret prog) 
                                        && (isParam (split ',' params) (P prog)) 
                                        && ( length (split ' ' name) == 1 )
                                        then P (Map.insert clasa (classState, parent) prog) else (P prog)
                                         where clasa = head elements
                                               name = head ( drop 1 elements)
                                               ret = head ( drop 2 elements )
                                               params = head ( drop 3 elements ) 
                                               parent = getParentClass clasa (P prog)
                                               classState =  insertIntoClass (getClass clasa (P prog)) 
                                                             Func ((filter (/=' ') name):ret:(split ',' params))


-- Interpret -- BONUS
-- Interpret Expresie - se adauga o noua variabila in program, avand acelasi tip cu tipul expresiei
-- Expresia se evalueaza folosind apelul functiei "infer"
-- Variabila este adaugata in clasa "Global" doar daca inferenta de tip reuseste
interpret (Expresie var expr) (P prog) = case (infer expr (P prog)) of
                                          Nothing -> (P prog)
                                          _ -> P (Map.insert "Global" (clasa, "Global") prog) 
                                            where expr_tip = (infer expr (P prog))
                                                  clasa = insertIntoClass (getClass "Global" (P prog)) Var [var,(fromJust expr_tip)]



                                      --- Functii auxiliare b) ---



-- returneaza ClassState-ul asociat unui nume 
getClass :: String -> Program -> ClassState
getClass name (P program) = fst (Map.findWithDefault (initEmptyClass,"") name program)


-- split dupa un caracter
split::Char-> String-> [String]
split c s = case dropWhile (==c) s of
            "" -> []
            s' -> w : split c s''
                  where (w, s'') = break (==c) s'


-- isParam : verifica daca tipurile parametrilor exista
isParam :: [String] -> Program -> Bool
isParam [] (P prog ) = True
isParam (p:params) (P prog ) = (Map.member p prog ) && (isParam params (P prog) )


-- parseLine : primeste o linie si returneaza o Instructiune rezultata in urma parsarii
-- parsarea liniei se face dupa tipul instructiunii
-- pentru fiecare tip de instructiune se apeleaza o functie specializata 
parseLine :: String -> Instruction
parseLine line = if (head elements) == "class" then (parseClass elements)
                 else if (head elements) == "newvar" then (parseVar line)
                 else if (head elements) == "infer" then (parseExpr line) -- BONUS
                 else (parseFunc line)
                 where elements = words line



-- parseClass : parseaza o linie ( o lista de campuri ) cu o instructiune ce declara o noua Clasa
-- parseLine sparge linia in campuri : numele clasei, numele parintelui ( daca exista, altfel : "Global" )
parseClass :: [String] -> Instruction
parseClass elements = if length elements == 4 then  Clasa (head (drop 1 elements)) (initEmptyClass, (head (drop 3 elements)) ) 
                      else Clasa (head (drop 1 elements)) (initEmptyClass, "Global")



-- parseVar : parseaza o linie ce contine declararea unei Variabile noi
-- se extrag campurile : numele variabilei, tipul variabilei si se intoarce instructiunea
parseVar :: String -> Instruction
parseVar line = Variabila (  (filter (/=' ') (head (tail (split ' ' (head elements))) )):(filter (/=' ') 
                          (head (tail elements ))):[] )
                where elements = split '=' line


-- parseFunc : parseaza o linie ce contine declararea unei Functii noi
-- se extrag toate campurile prin parsare dupa ':' ')' '(' si spatii
parseFunc :: String -> Instruction
parseFunc line = Functie (clasa:name:ret:params:[])
                  where signature = split ':' line
                        ret = head ( words ( head signature ) )
                        clasa = head ( tail ( words ( head signature ) ) )
                        name = head ( split '(' (head (tail signature) ) ) 
                        params = filter (/=')') ( filter (/=' ') (head (tail ( split '(' (head (tail signature) )) ) ) )



                                       --- SUBPUNCTUL c) ---


-- infer
-- infer Expresie de tip Variabila
-- Daca variabila exista, atunci se returneaza tipul ei
infer :: Expr -> Program -> Maybe String
infer (Va var )  (P prog ) = case variable of 
                              Nothing -> Nothing
                              _ -> Just (head (tail (fromJust variable)))  
                              where variable = find (\list -> if head list == var then True else False )  
                                                    (getValues (getClass "Global" (P prog)) Var )


-- infer Apel de functie
-- inferenta de tip reuseste daca : exista variabila instantiata, tipurile functiei sunt corecte
-- Daca se reuseste se returneaza tipul de return al functiei 
-- se folosesc functii auxiliare
infer (FCall var func expr) ( P prog ) = case variable of
                                            Nothing -> Nothing
                                            _ -> case functions of  
                                                 [] -> Nothing
                                                 _ -> ( validFuncs (map tail functions) expr (P prog) )
                                            where variable = find (\list -> if head list == var then True else False )  
                                                                  (getValues (getClass "Global" (P prog)) Var )
                                                  clasa = (head (tail (fromJust variable)))
                                                  functions = (searchF func clasa (P prog) [] )



                                         -- Functii AUX c) --



-- Verifica daca un apel de functie se potriveste cu semnatura unei functii date ca argument
-- validCall :: functia -> [expresii] -> un program -> Nothing/Just return type
-- functia = [ret, params]
validCall :: [String] -> [Expr] -> Program -> Maybe String
validCall func expr (P prog) = if ((length func) - 1) /= length expr then Nothing
                               else if ( elem Nothing params ) then Nothing
                               else if ( (tail func) /= params_type ) then Nothing 
                               else Just (head func)
                                where params =  map (\expresie -> infer expresie (P prog) ) expr
                                      params_type = map fromJust params



--validFuncs : Cauta o functie valida intr-o lista de functii
validFuncs :: [[String]] -> [Expr] -> Program -> Maybe String
validFuncs [] expr (P prog) = Nothing
validFuncs (f:funcs) expr (P prog) = case f_call of 
                                      Nothing -> (validFuncs funcs expr (P prog))
                                      _ -> f_call
                                      where f_call = (validCall f expr (P prog))
                                

-- getFunc : Returneaza toate functiile cu numele f_name din lista de functii 
getFunc :: [[String]]-> String ->[[String]]-> [[String]]
getFunc [] f_name acc = acc
getFunc (f:funcs) f_name acc = if (head f) == f_name then getFunc funcs f_name (f:acc) else getFunc funcs f_name acc


-- searchF : toate functiile cu numele func in clasa sau pe lantul de mostenire
searchF :: String -> String -> Program->[[String]]-> [[String]]
searchF func clasa (P prog) acc = if  ( clasa/="Global" )
                                  then searchF func (getParentClass clasa (P prog)) (P prog) new_acc
                                  else new_acc
                                  where functions = getValues (getClass clasa (P prog) ) Func
                                        new_acc = acc++(getFunc functions func [])




                                       --- BONUS ---


-- parseExpr : parseaza o linie ce contine o expresie atribuita unei variabile
-- Se sparge expresia in campuri si se returneaza o instructiune : Expresie variabila apel
parseExpr :: String -> Instruction
parseExpr line = Expresie newvar (FCall var func expr )
                 where newvar = filter (/=' ') (takeWhile (/='=') ( dropWhile (/=' ') line ))
                       apel =  filter (/=' ') (filter (/='=') ( dropWhile (/='=') line ))
                       var = takeWhile (/='.') apel
                       func = filter (/='.') (takeWhile (/='(') (dropWhile (/='.') apel))
                       expr = parseParamsE apel 



-- parseParamsE : Primeste un apel de functie var.func ( expr, expr ) 
-- Intoarce o lista de expresii
parseParamsE :: String->[Expr]
parseParamsE apel  =  map (\expr -> if ( elem '.' expr ) then (FCall (takeWhile (/='.') expr) (filter (/='.') (takeWhile (/='(') (dropWhile (/='.') expr))) (parseParamsE expr)) else (Va expr)) expr_list 
                      where params = dropWhile (/='(') apel
                            expr_list = splitParams (split ',' (tail (init params)) ) []


-- Primeste o lista de string-uri
-- Intoarce lista de parametri = expresii, dar in forma string
splitParams :: [String]->[String]->[String]
splitParams (x:[]) acc = (acc++[x])
splitParams (x:y:xs) acc = if ( length ( filter ( == '(' ) x) /= length ( filter ( == ')' ) x) )
                       then splitParams ((x++","++y):xs) acc
                       else splitParams (y:xs) (acc++[x])

