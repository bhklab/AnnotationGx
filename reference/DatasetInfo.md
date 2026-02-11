# BioMart Dataset Information Class

Class that stores information about a dataset within a BioMart database.

## Details

This class encapsulates metadata about a specific dataset in a BioMart
database, including its name, description, display name, and a reference
to the parent mart.

## Public fields

- `name`:

  The internal name of the dataset

- `description`:

  A textual description of the dataset

- `displayName`:

  The human-readable name of the dataset

- `mart`:

  The MartInfo object this dataset belongs to

## Methods

### Public methods

- [`DatasetInfo$new()`](#method-DatasetInfo-new)

- [`DatasetInfo$print()`](#method-DatasetInfo-print)

- [`DatasetInfo$clone()`](#method-DatasetInfo-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new DatasetInfo object

#### Usage

    DatasetInfo$new(name, description, displayName, mart)

#### Arguments

- `name`:

  Character, the internal name of the dataset

- `description`:

  Character, the dataset description

- `displayName`:

  Character, the human-readable name

- `mart`:

  MartInfo object, the parent mart

#### Returns

A new DatasetInfo object

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for DatasetInfo objects

#### Usage

    DatasetInfo$print(...)

#### Arguments

- `...`:

  Additional arguments (not used)

#### Returns

Invisibly returns self

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DatasetInfo$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
