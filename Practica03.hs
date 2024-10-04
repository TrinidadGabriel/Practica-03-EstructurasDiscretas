--Definicióon de una lista--
data List a = Void | Node a (List a) deriving Show

--Longitud de una lista.--
longitud :: List a -> Int
longitud Void = 0
longitud (Node a lista) = 1 + longitud lista

--Contención de un elemento en una lista.--
estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void y = False
estaContenido (Node x xs) elemento = if elemento == x 
                                    then True
                                    else estaContenido xs elemento 

--Convertir una lista de haskell a nuestra nueva estructura de lista.--
convertirAEstructura :: [a] -> List a
convertirAEstructura [] = Void
convertirAEstructura (x:xs) = Node x (convertirAEstructura xs)  

--Convertir nuestra nueva estructura de lista a una lista ya definida en haskell--
convertirALista :: List a -> [a]
convertirALista Void = []
convertirALista (Node a lista) = a : convertirALista lista   

--Convertir en un conjunto nuestra nueva estructura de lista.--
conjunto :: Eq a => List a -> List a
conjunto Void = Void
conjunto (Node x lista) = if estaContenido lista x 
                            then conjunto lista
                            else Node x (conjunto lista)

--Eliminar un elemento en un  ́ındice espec ́ıfico.--
eliminarIndice :: List a -> Int -> List a
eliminarIndice Void indice = error "No existe indice en la lista vacia"
eliminarIndice (Node a lista) indice = if indice < 0 || indice > longitud (Node a lista) - 1
                                        then error "El indice esta fuera de la lista"
                                        else if indice == 0 
                                            then lista
                                            else Node a (eliminarIndice lista (indice - 1))

--Agregar un elemento en un índice específico.--
insertarIndice :: List a -> Int -> a -> List a
insertarIndice Void indice x = if indice /= 0
                                then error "El indice esta fuera de la lista"
                                else if indice == 0 
                                then (Node x Void)
                                else error "El indice esta fuera de la lista"
insertarIndice  (Node a lista) indice x = if indice < 0 || indice > longitud (Node a lista) 
                                            then error "El indice excede la longitud de la lista"
                                            else if indice == 0
                                            then Node x (Node a lista)
                                            else Node a (insertarIndice lista (indice-1) x) 

--Recorrer n veces a la derecha los elementos de nuestra nueva estructura lista.--
recorrerLista :: List a -> Int -> List a
recorrerLista Void recorrido = Void
recorrerLista lista 0 = lista
recorrerLista (Node x xs) recorrido = recorrerLista (insertarIndice xs (longitud xs) x ) (recorrido-1)
