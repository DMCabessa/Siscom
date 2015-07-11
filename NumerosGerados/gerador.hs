{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import GHC.Base (IO(..), realWorld#)
import System.Random
import Control.Arrow



--removeDuplicatesUsing::Ord b=>(a->b)->[a]->[a]
--removeDuplicatesUsing f theList = [x|(Just x, _, _) <- iterate getNext (Nothing, theList, empty)]
--  where 
--    getNext (_, x:xs, used) 
--      | f x `member` used = (Nothing, xs, used)
--      | otherwise = (Just x, xs, insert (f x) used)



--http://codereview.stackexchange.com/questions/36671/function-to-produce-unique-random-numbers
--https://www.fpcomplete.com/school/starting-with-haskell/libraries-and-frameworks/randoms
--https://en.wikibooks.org/wiki/Haskell/Libraries/Random
--http://swizec.com/blog/haskell-and-randomness/swizec/4763
--https://hackage.haskell.org/package/random-1.0.1.1/docs/System-Random.html
--https://www.google.com.br/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=opera%C3%A7oes%20com%20lista%20haskell


--comum a todos
binaDeci:: (Integer,Integer) ->Integer
binaDeci(x,c)= if(x< 10) then (x*(potencia(2,c))) else ((x `mod` 10)*(potencia(2,c)))+(binaDeci(x`div`10,c+1))

potencia:: (Integer,Integer) -> Integer
potencia (x,0)=1
potencia (x,n)=x*(potencia(x,n-1))

toBinary :: Integer -> [ Integer ]
toBinary 0 = [ 0 ]
toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]

transEmString :: [Integer] -> String
transEmString [] = []
transEmString (x:xs) = (show x) ++ transEmString xs 

unIO (IO m) = case m realWorld# of (# _, r #) -> r

tiraIo :: IO Integer -> Integer
tiraIo a = (unIO a) 

impedirRepeticao :: [String] -> Integer -> [String] ->  [String] 
impedirRepeticao [] fixoTamanho salvandoLista = [(show fixoTamanho)] ++ salvandoLista  
impedirRepeticao (x:xs) fixoTamanho salvandoLista
	| compara x xs = impedirRepeticao  xs fixoTamanho salvandoLista  
	| otherwise = impedirRepeticao  xs fixoTamanho (x : salvandoLista)

--compara
compara :: String -> [String] -> Bool
compara y [] = False
compara y (x:xs)
	| y == x = True
	| otherwise = compara y xs


countt ::  [String]  -> Integer
countt [] = 0
countt (x:xs) = 1 + countt xs
 


--nas 2 linahs abaixo temos respectivamente, o maior numero de bits e logo em seguida seu valor decimal;
--111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
--79228162514264337593543950335

--96 bits random
-----------------------------------------------------------------------------------------------------------------------------------------
setaSeTemRep ::  [String]  -> [String]
setaSeTemRep listaSalva 
	| ((length listaSalva) - 1) <  read (head listaSalva) = impedirRepeticao (toListBin ( (read (head listaSalva)::Integer) - (countt listaSalva) -1) (read (head listaSalva)::Integer) (tail listaSalva)) (read (head listaSalva)) []
	| otherwise = (tail listaSalva)


toListBin :: Integer -> Integer -> [String] ->  [String]
toListBin 0 fixoTamanho a = a 
--toListBin tamanho fixoTamanho a = impedirRepeticao (transEmString (tail(toBinary(tiraIo ((randomRIO (0,10) :: IO Integer))))) : toListBin (tamanho-1) fixoTamanho  a) fixoTamanho []
toListBin tamanho fixoTamanho a =  transEmString (tail(toBinary(tiraIo (randomRIO (0,79228162514264337593543950335) :: IO Integer)))) : toListBin (tamanho-1) fixoTamanho  a 
--toListBin x a =  transEmString (tail(toBinary(tiraIo (uniqueRandomInts (0, 79228162514264337593543950335) 5 $ mkStdGen 42)))) : toListBin (x-1)  a





checa96 :: [String] -> [String]
checa96 [] = []
checa96 (x:xs) 
	| length x == 96 = x : checa96 xs
	| otherwise = ((por96 (length x))++x) : checa96 xs 

por96 ::  Int -> String
por96 96 = []
por96 y = '0' : por96 (y+1)

--------------------------------------------------------------------------------------------------------------------------------------------
--36 variavel 60 fixo fim
--111111111111111111111111111111111111
--68719476735
--000000000000000000000000000000000000000000000000000000000000

toListBin36Variavel :: Integer -> Integer -> [String] ->  [String]
toListBin36Variavel  0 fixoTamanho a = a 
toListBin36Variavel tamanho fixoTamanho a  =  transEmString (tail(toBinary(tiraIo ((randomRIO (0,68719476735) :: IO Integer))))) : toListBin36Variavel (tamanho-1)  fixoTamanho a

--toListBin36Variavel :: Integer -> [String]
--toListBin36Variavel 0  = []
--toListBin36Variavel x  =  transEmString (tail(toBinary(tiraIo ((randomRIO (0,68719476735) :: IO Integer))))) : toListBin36Variavel (x-1)  
setaSeTemRep2 ::  [String]  -> [String]
setaSeTemRep2 listaSalva 
	| ((length listaSalva) - 1) <  read (head listaSalva) = toListBin36Variavel ( (read (head listaSalva)::Integer) - (countt listaSalva) -1) (read (head listaSalva)::Integer) (tail listaSalva)
	| otherwise = (tail listaSalva)


checa36 :: [String] -> [String]
checa36 [] = []
checa36 (x:xs) 
	| length x == 36 = x : checa36 xs
	| otherwise = ((por36 (length x))++x) : checa36 xs 

por36 ::  Int -> String
por36 36 = []
por36 y = '0' : por36 (y+1) 


inserir60Fim :: [String] -> [String]
inserir60Fim [] = []
inserir60Fim (x:xs) = (x++"000000000000000000000000000000000000000000000000000000000000") : inserir60Fim xs

-----------------------------------------------------------------------------------------------------------------------------------------------
--36 variavel 60 fixo inicio

inserir60Ini :: [String] -> [String]
inserir60Ini [] = []
inserir60Ini (x:xs) = ("000000000000000000000000000000000000000000000000000000000000"++x) : inserir60Ini xs


--passa o numero de vezes, random decimal, bin 

--(mapM_ print [i | i <- (toInter (read line::Integer)) ])







getInfi :: IO ()
getInfi =
	do {
		
		--putStrLn  (toInter (read line::Integer))


		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/1.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/2.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/3.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/4.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/5.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/6.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/7.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/8.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/9.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/10.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/11.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/12.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/13.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/14.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/15.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/16.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/17.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/18.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/19.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/100/20.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 100 100 []) 100 [])));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/1.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/2.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/3.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/4.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/5.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/6.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/7.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/8.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/9.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/10.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/11.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/12.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/13.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/14.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/15.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/16.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/17.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/18.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/19.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/100/20.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/1.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/2.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/3.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/4.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/5.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/6.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/7.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/8.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/9.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/10.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/11.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/12.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/13.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/14.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/15.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/16.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/17.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/18.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/19.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/100/20.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 100 100 []) 100 []))));


		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/1.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/2.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/3.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/4.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/5.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/6.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/7.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/8.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/9.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/10.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/11.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/12.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/13.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/14.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/15.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/16.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/17.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/18.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/19.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/200/20.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 200 200 []) 200 [])));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/1.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/2.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/3.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/4.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/5.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/6.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/7.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/8.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/9.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/10.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/11.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/12.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/13.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/14.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/15.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/16.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/17.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/18.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/19.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/200/20.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/1.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/2.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/3.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/4.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/5.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/6.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/7.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/8.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/9.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/10.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/11.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/12.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/13.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/14.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/15.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/16.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/17.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/18.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/19.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/200/20.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 200 200 []) 200 []))));


		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/1.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/2.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/3.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/4.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/5.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/6.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/7.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/8.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/9.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/10.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/11.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/12.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/13.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/14.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/15.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/16.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/17.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/18.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/19.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/300/20.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 300 300 []) 300 [])));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/1.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/2.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/3.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/4.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/5.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/6.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/7.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/8.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/9.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/10.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/11.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/12.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/13.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/14.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/15.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/16.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/17.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/18.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/19.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/300/20.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/1.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/2.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/3.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/4.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/5.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/6.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/7.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/8.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/9.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/10.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/11.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/12.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/13.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/14.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/15.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/16.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/17.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/18.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/19.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/300/20.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 300 300 []) 300 []))));





		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/1.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/2.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/3.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/4.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/5.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/6.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/7.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/8.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/9.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/10.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/11.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/12.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/13.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/14.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/15.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/16.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/17.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/18.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/19.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/400/20.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 400 400 []) 400 [])));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/1.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/2.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/3.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/4.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/5.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/6.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/7.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/8.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/9.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/10.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/11.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/12.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/13.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/14.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/15.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/16.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/17.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/18.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/19.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/400/20.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/1.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/2.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/3.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/4.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/5.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/6.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/7.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/8.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/9.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/10.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/11.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/12.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/13.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/14.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/15.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/16.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/17.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/18.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/19.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/400/20.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 400 400 []) 400 []))));



		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/1.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/2.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/3.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/4.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/5.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/6.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/7.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/8.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/9.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/10.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/11.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/12.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/13.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/14.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/15.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/16.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/17.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/18.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/19.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/500/20.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 500 500 []) 500 [])));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/1.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/2.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/3.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/4.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/5.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/6.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/7.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/8.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/9.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/10.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/11.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/12.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/13.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/14.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/15.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/16.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/17.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/18.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/19.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/500/20.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/1.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/2.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/3.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/4.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/5.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/6.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/7.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/8.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/9.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/10.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/11.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/12.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/13.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/14.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/15.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/16.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/17.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/18.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/19.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/500/20.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 500 500 []) 500 []))));


		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/1.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/2.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/3.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/4.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/5.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/6.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/7.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/8.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/9.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/10.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/11.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/12.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/13.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/14.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/15.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/16.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/17.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/18.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/19.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/600/20.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 600 600 []) 600 [])));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/1.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/2.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/3.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/4.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/5.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/6.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/7.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/8.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/9.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/10.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/11.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/12.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/13.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/14.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/15.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/16.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/17.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/18.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/19.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/600/20.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/1.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/2.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/3.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/4.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/5.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/6.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/7.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/8.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/9.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/10.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/11.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/12.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/13.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/14.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/15.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/16.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/17.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/18.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/19.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/600/20.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 600 600 []) 600 []))));


		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/1.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/2.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/3.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/4.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/5.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/6.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/7.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/8.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/9.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/10.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/11.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/12.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/13.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/14.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/15.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/16.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/17.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/18.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/19.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/700/20.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 700 700 []) 700 [])));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/1.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/2.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/3.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/4.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/5.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/6.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/7.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/8.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/9.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/10.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/11.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/12.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/13.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/14.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/15.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/16.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/17.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/18.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/19.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/700/20.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/1.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/2.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/3.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/4.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/5.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/6.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/7.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/8.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/9.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/10.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/11.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/12.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/13.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/14.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/15.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/16.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/17.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/18.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/19.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/700/20.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 700 700 []) 700 []))));


		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/1.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/2.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/3.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/4.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/5.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/6.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/7.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/8.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/9.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/10.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/11.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/12.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/13.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/14.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/15.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/16.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/17.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/18.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/19.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/800/20.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 800 800 []) 800 [])));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/1.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/2.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/3.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/4.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/5.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/6.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/7.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/8.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/9.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/10.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/11.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/12.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/13.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/14.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/15.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/16.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/17.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/18.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/19.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/800/20.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/1.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/2.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/3.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/4.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/5.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/6.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/7.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/8.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/9.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/10.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/11.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/12.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/13.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/14.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/15.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/16.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/17.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/18.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/19.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/800/20.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 800 800 []) 800 []))));


		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/1.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/2.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/3.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/4.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/5.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/6.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/7.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/8.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/9.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/10.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/11.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/12.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/13.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/14.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/15.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/16.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/17.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/18.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/19.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/900/20.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 900 900 []) 900 [])));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/1.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/2.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/3.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/4.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/5.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/6.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/7.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/8.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/9.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/10.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/11.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/12.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/13.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/14.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/15.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/16.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/17.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/18.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/19.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/900/20.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/1.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/2.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/3.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/4.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/5.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/6.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/7.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/8.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/9.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/10.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/11.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/12.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/13.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/14.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/15.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/16.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/17.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/18.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/19.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/900/20.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 900 900 []) 900 []))));


		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/1.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/2.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/3.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/4.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/5.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/6.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/7.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/8.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/9.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/10.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/11.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/12.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/13.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/14.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/15.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/16.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/17.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/18.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/19.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));
		writeFile "C:/Users/ezequiel/Desktop/plc/testeAllRando/1000/20.txt" $ unlines (checa96 (setaSeTemRep (impedirRepeticao (toListBin 1000 1000 []) 1000 [])));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/1.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/2.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/3.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/4.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/5.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/6.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/7.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/8.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/9.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/10.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/11.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/12.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/13.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/14.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/15.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/16.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/17.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/18.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/19.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoFim/1000/20.txt" $ unlines (inserir60Fim (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));

		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/1.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/2.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/3.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/4.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/5.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/6.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/7.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/8.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/9.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/10.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/11.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/12.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/13.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/14.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/15.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/16.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/17.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/18.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/19.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));
		writeFile "C:/Users/ezequiel/Desktop/plc/teste60FixoInicio/1000/20.txt" $ unlines (inserir60Ini (checa36 (setaSeTemRep2 (impedirRepeticao (toListBin36Variavel 1000 1000 []) 1000 []))));

	}
