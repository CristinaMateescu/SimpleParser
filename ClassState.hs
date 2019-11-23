module ClassState
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq, Ord)

-- Tip de date pentru clase : Map ( cheie = Var/Func, valoare = lista de variabile/functii )
data ClassState = Class (Map InstrType [[String]]) deriving Show

-- Folosim functiile tipului Map
initEmptyClass :: ClassState
initEmptyClass = Class Map.empty

-- O noua var/functie se insereaza prin concatenarea la lista de var/functii existente
insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass (Class map) instrType format = Class (Map.insertWith (++) instrType [format] map)

-- Returneaza valorea = lista de variabile/functii asociata cheii Var/Func sau [] daca nu exista cheia
getValues :: ClassState -> InstrType -> [[String]]
getValues  (Class map) instrType = Map.findWithDefault [] instrType map

