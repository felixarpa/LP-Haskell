# Pràctica

**Atenció!** Aquest codi no te llicencia, és a dir, no es pot copiar. ([explicació](https://choosealicense.com/no-license/), [Wikipedia](https://en.wikipedia.org/wiki/License-free_software)).

## K Nearest Neighbors

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
70

Funció de distancia. Euclediana (E) o Manhattan (M): 
E

Mecanisme de votació: Simple (S) o Ponderat (P): 
S

Avaluació accuracy o exactitud: 0.72
Avaluació lost o error:         0.28

Gràcies. Pots veure el codi a https://github.com/felixarpa/LP-Haskell/tree/master/practica
```