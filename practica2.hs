--1. Ejercicio: firstToEnd
firstToEnd :: (Read a)=>[a]->[a] --recibe una lista y regresa una lista
firstToEnd []=[]
firstToEnd[x]=[x]
firstToEnd (x:xs) =  xs ++ [x] --se descompone la lista, regresa la cola y al final la cabeza

--2. Ejercicio : minAndMax
minAndMax :: (Ord a)=>[a]->[a] --recibe una lista y regresa una lista
minAndMax []= error "Lista vacia"
minAndMax [x]= error "Falta 1 elemento"
minAndMax xs= [minimum xs, maximum xs] --regresa una lista con el minimo y maximo de los elementos

--3. Ejercicio: minorsFirstElement
minorsFirstElement :: (Integral a)=>[a]->[a]
minorsFirstElement [] = error "Lista vacia"
minorsFirstElement [a] = error "Solo hay un elemento"
minorsFirstElement list = [x | x <- tail list , x<head list]--muestra una lista con los elemento 
--de la cola sean menores que la cabeza

--4. Ejercicio: greaterOrEqualFirstElement
greaterOrEqualFirstElement :: (Integral a)=>[a]->[a] --recibe una lista y regresa una lista
greaterOrEqualFirstElement [] = error "Lista vacia"
greaterOrEqualFirstElement [a] = error "Solo hay un elemento"
greaterOrEqualFirstElement list = [x | x <- tail list , x >=head list]
--regrasa una lista de los elementos de la cola que sean menores que la cabeza

--5. Ejercio: minorsToSumFirstAndSecondElement
minorsToSumFirstAndSecondElement :: (Integral a)=>[a]->[a] --recibe una lista y regresa una lista
minorsToSumFirstAndSecondElement [] = error "Lista vacia"
minorsToSumFirstAndSecondElement [a] = error "Solo hay un elemento"
minorsToSumFirstAndSecondElement list= [x | x <- tail (tail list) , x <head list + head(tail list)]

--6. Ejercicio: listSumDuplaToList
listSumDuplaToList :: (Integral a) => [(a,a)] -> [a]--recibe una lista de duplas y regresa una lista
listSumDuplaToList [] = []
listSumDuplaToList list = fst(head list) + snd (head list) : listSumDuplaToList (tail list)
--regresa una lista de la suma del primer y segundo elemnto de cada dupla

--7. Ejercicio: listMultTripletaToList
listMultTripletaToList ::(Integral a)=>[(a,a,a)]->[a]--recibe una lista de tripletas y regresa una lista
listMultTripletaToList []=[]
listMultTripletaToList xs= [x*y*z|(x,y,z) <- xs]
--muestra una lista donde cada elemento es la multiplicación de cada tripleta

--8. Ejercicio: changeFstToSnd
changeFstToSnd :: (Integral a)=>[(a,a)]->[(a,a)]
changeFstToSnd []=[]
changeFstToSnd xs = [(y,x) | (x,y)<-xs]

--9. Ejercicio: sumVectors
sumVectors :: (Integral a)=>[(a,a)]->(a,a)--recibe una lista de duplas y egresa una dupla
sumVectors []=(0,0)
sumVectors  [x] = (fst x,snd x)
sumVectors list = (fst (head list) + fst (sumVectors (tail list)),snd (head list) + snd (sumVectors (tail list)))
--regresa un dupla con la suma de vectores

--10. Ejercicio: dividers
dividers :: Int -> [Int]
dividers x = [y | y <- [1..a], x `mod` y == 0] ++[x]
    where a = x `div` 2

--11. Ejercicio: primeNumbers
esPrimo :: (Integral a)=>a -> Bool
esPrimo n = factor n == [1,n]
 where factor n = [x | x <- [1..n], n `mod` x == 0] --encuentra si su factor es 1o el mismo
primeNumbers :: (Integral a)=>a->[a]
primeNumbers 0=[]
primeNumbers n = [x | x <- [1..n], esPrimo x]--hace la lista con los elementos del 1 a n que 
--pertenescan a la funcion esPrimo 

--12. Ejercicio: infinitePrimeNumbers
infinitePrimeNumbers :: [Integer]
infinitePrimeNumbers = filterPrime [2..]
  where filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]