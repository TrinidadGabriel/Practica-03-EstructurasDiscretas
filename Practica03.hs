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

--Convertir una lista de haskell a nuestra nueva estructura de lista:--


---Convertir nuestra nueva estructura de lista a una lista ya definida en haskell---
convertirALista :: List a -> [a]
convertirALista Void = []
convertirALista (Node a lista) = a : convertirALista lista

--Convertir en un conjunto nuestra nueva estructura de lista:--

--Eliminar un elemento en un ındice espec ́ıfico.:--
eliminarIndice :: List a -> Int -> List a
eliminarIndice Void indice = error "No existe indice en la lista vacia"
eliminarIndice (Node a lista) indice = if indice == 0
                                            then lista
                                            else Node a (eliminarIndice lista  ( 
                                                indice + 1 ))
