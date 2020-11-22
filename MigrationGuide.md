# Curry 2 -> 3 Migration Guide

Between version 2 and 3 the standard library has undergone some notable changes, like aligning module and function names more closely with their Haskell equivalents and moving more specialized functionality into separate packages.

This migration guide aims to provide a comprehensive outline of the changes to make the transition of existing packages more seamless.

## Standard Library

### Renamed Modules

| Old         | New                     | Notes                       |
| ----------- | ----------------------- | --------------------------- |
| `List`      | `Data.List`             |                             |
| `Maybe`     | `Data.Maybe`            | Now using `Monad` instance. |
| `Char`      | `Data.Char`             |                             |
| `Either`    | `Data.Either`           |                             |
| `Function`  | `Data.Function`         |                             |
| `IO`        | `System.IO`             |                             |
| `GetOpt`    | `System.Console.GetOpt` |                             |

### Moved and Renamed Modules

| Old            | New                           | New Package     |
| -------------- | ----------------------------- | --------------- |
| `FiniteMap`    | `Data.Map`                    | `finite-map`    |
| `Distribution` | `Language.Curry.Distribution` | `distribution`  |
| `Directory`    | `System.Directory`            | `directory`     |
| `FilePath`     | `System.FilePath`             | `filepath`      |
| `Random`       | `System.Random`               | `random`        |
| `State`        | `Control.Monad.Trans.State`   | `transformers`  |
| `ErrorState`   | `Control.Monad.Trans.Error`   | `transformers`  |
| `AnsiCodes`    | `System.Console.ANSI.Codes`   | `ansi-terminal` |
| `CPNS`         | `Network.CPNS`                | `cpns`          |
| `NamedSocket`  | `Network.NamedSocket`         | `cpns`          |
| `Socket`       | `Network.Socket`              | `socket`        |
| `Dequeue`      | `Data.Queue`                  | `queue`         |
| `RedBlackTree` | `Data.RedBlackTree`           | `redblacktree`  |

### Moved Modules

| Old                     | New Package    |
| ----------------------- | -------------- |
| `Control.Monad.Extra`   | `extra`        |
| `Control.Monad.Trans.*` | `transformers` |
| `Data.Tuple.Extra`      | `extra`        |

### Deleted Modules

| Old                       |
| ------------------------- |
| `Data.Function.Inversion` |
| `Data.Global`             |
| `Sort`                    |

### Renamed Functions

#### Prelude

| Old          | New            |
| ------------ | -------------- |
| `showError`  | `show`         |

### Added Functions

#### Prelude

| New            |
| -------------- |
| `pi`           |
| `(^)`          |
| `(<$>)`        |
| `(<$)`         |

## Finite Map Package

Note that the explicit ordering function is not necessary anymore. Instead, an `Ord` context is needed.

### Renamed Types and Functions

#### Map

| Old                   | New                     | Notes                   |
| --------------------- | ----------------------- | ----------------------- |
| `FM`                  | `Map`                   |                         |
| `emptyFM`             | `empty`                 |                         |
| `unitFM`              | `singleton`             |                         |
| `listToFM`            | `fromList`              |                         |
| `addToFM`             | `insert`                | argument order changed! |
| `addToFM_C`           | `insertWith`            | argument order changed! |
| `addListToFM`         | `insertList`            | argument order changed! |
| `addListToFM_C`       | `insertListWith`        | argument order changed! |
| `delFromFM`           | `delete`                | argument order changed! |
| `delListFromFM`       | `deleteAll`             | argument order changed! |
| `updFM`               | `adjust`                | argument order changed! |
| `splitFM`             |                         | removed, can be user-defined (split lookup is different) |
| `plusFM`              | `union`                 | first two arguments are swapped! |
| `plusFM_C`            | `unionWith`             |                         |
| `minusFM`             | `difference`            |                         |
| `intersectFM`         | `intersection`          |                         |
| `intersectFM_C`       | `intersectionWith`      |                         |
| `foldFM`              | `foldrWithKey`          |                         |
| `mapFM`               | `mapWithKey`            |                         |
| `filterFM`            | `filterWithKey`         |                         |
| `sizeFM`              | `size`                  |                         |
| `eqFM`                |                         | Map has "Eq" context instead |
| `isEmptyFM`           | `null`                  |                         |
| `elemFM`              | `member`                | argument order changed! |
| `lookupFM`            | `lookup`                | argument order changed! |
| `lookupWithDefaultFM` | `findWithDefault`       | argument order changed! |
| `keyOrder`            |                         | removed, because "Ord" context is used now |
| `fmToList`            | `toList`                |                         |
| `keysFM`              | `keys`                  |                         |
| `eltsFM`              | `elems`                 |                         |
| `fmSortBy`            | `sortWithMap`           | Will sort given by "<" of "Ord" context |
| `minFM`               | `lookupMin`             |                         |
| `maxFM`               | `lookupMax`             |                         |
| `fmToListPreOrder`    | `toPreOrderList`        |                         |

#### Set

| Old          | New            |
| ------------ | -------------- |
| `FiniteSet`  | `Set`          |
| `emptySet`   | `empty`        |
| `mkSet`      | `fromList`     |
| `isEmptySet` | `null`         |
| `elementOf`  | `member`       |
| `minusSet`   | `difference`   |
| `setToList`  | `toList`       |
| `union`      | `union`        |

## Directory Package

### Renamed Functions

| Old          | New            |
| ------------ | -------------- |
| `fileSize`   | `getFileSize`  |

## Transformers Package

### Renamed Functions

#### State

| Old            | New                        |
| -------------- | -------------------------- |
| `bindS`        | Monad instance, `(>>=)`    |
| `bindS_`       | Monad instance, `(>>)`     |
| `returnS`      | Monad instance, `return`   |
| `getS`         | `get`                      |
| `putS`         | `put`                      |
| `modifyS`      | `modify`                   |
| `sequenceS`    | `sequence`                 |
| `sequenceS_`   | `sequence_`                |
| `mapS`         | `mapM`                     |
| `mapS_`        | `mapM_`                    |
| `liftS`        | `fmap`                     |
| `liftM`        | `fmap`                     |
| `liftS2`       | `liftM2`                   |

#### ErrorState

| Old            | New                              |
| -------------- | -------------------------------- |
| `returnES`     | Monad instance, `return`         |
| `>+=`          | Monad instance, `(>>=)`          |
| `>+`           | `(>>)`                           |
| `getS`         | `lift . get`                     |
| `putS`         | `lift . put`                     |
| `modifyS`      | `lift . modify`                  |
| `mapS`         | `mapM`                           |
| `<*>,<*,`      | `Applicative instance`           |

### Moved and Renamed Functions

#### ErrorState

| Old            | New                              | New Package |
| -------------- | -------------------------------- | ----------- |
| `concatMapES`  | `Control.Monad.Extra.concatMapM` | `extra`     |
| `mapAccumES`   | `Control.Monad.Extra.mapAccumM`  | `extra`     |

## Socket Package

### Renamed Functions

| Old            | New      |
| -------------- | -------- |
| `sClose`       | `close`  |
| `socketAccept` | `accept` |

## RedBlackTree Package

### Renamed Functions

| Old            | New      |
| -------------- | -------- |
| `tree2list`    | `toList` |
