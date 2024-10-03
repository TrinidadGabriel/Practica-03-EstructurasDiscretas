--Tipo--
data List a = Void | Node a (List a) deriving Show

--Longitud de una lista.--
longitud :: List a -> Int
longitud Void = 0
longitud (Node a lista) = 1 + longitud lista

---Contencion de un elemento en una lista:---
estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void y = False
estaContenido (Node x xs) elemento= if elemento == x
                                        then True
                                        else estaContenido xs elemento
