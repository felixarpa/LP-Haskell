# Pràctica

##### [github](https://github.com/felixarpa/LP-Haskell/tree/master/practica#pràctica)

## K Nearest Neighbors

Per executar la practia només cal compilar l'arxiu `main.hs` i executar-lo:

```
➜ ghc main.hs
➜ ./main
```

## Entrada

L'entrada per defecte són els fitxers `iris.train.txt` i `iris.test.txt`. Si voleu canviar l'entrada cal modificar l'arxiu [files.hs](files.hs):

```haskell
module Files where
trainFile = TRAINING_PATH
testFile  = TESTING_PATH
```

## Exemple d'execució

```
  Benvingut al Iris k Nearest Neighbors de Fèlix Arribas

  Variable k (entre 1 i 100): 
➜ 70

  Funció de distancia. Euclediana (E) o Manhattan (M): 
➜ E

  Mecanisme de votació: Simple (S) o Ponderat (P): 
➜ S

  Avaluació accuracy o exactitud: 0.72
  Avaluació lost o error:         0.28

  Gràcies. Pots veure el codi a https://github.com/felixarpa/LP-Haskell/tree/master/practica
```

## Contingut

- [main](main.hs): Senzill programa que llegeix les variables d'entrada i executa la funció K Nearest Neighbors.
- [reader](reader.hs): Llegeix els fitxers i els transforma a Data Types de haskell. També ofereix els missatges de sortida
- [knn](knn.hs): Tot l'algoritme KNN i les seves funcions auxiliars.
- [reader.test](reader.test.hs): Tests per [reader.hs](reader.hs)
- [knn.test](knn.test.hs): Tests per [reader.hs](knn.hs)

Per executar els tests cal fer:

```
➜ ghci
...
ghci➜ :l reader.test.hs
ghci➜ runTestTT tests
...
ghci➜ :l knn.test.hs
ghci➜ runTestTT tests
...
```

