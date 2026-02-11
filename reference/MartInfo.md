# BioMart Information Class

Class that stores information about a BioMart database.

## Details

This class encapsulates metadata about a BioMart database, including its
name, display name, description, configuration, and other properties.

## Public fields

- `name`:

  The internal name of the mart

- `displayName`:

  The human-readable name of the mart

- `description`:

  A textual description of the mart

- `config`:

  Configuration identifier for the mart

- `isHidden`:

  Flag indicating if the mart is hidden

- `operation`:

  Operation mode of the mart

- `meta`:

  Additional metadata for the mart

- `group`:

  Group the mart belongs to

## Methods

### Public methods

- [`MartInfo$new()`](#method-MartInfo-new)

- [`MartInfo$print()`](#method-MartInfo-print)

- [`MartInfo$clone()`](#method-MartInfo-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new MartInfo object

#### Usage

    MartInfo$new(
      name,
      displayName,
      description,
      config,
      isHidden,
      operation,
      meta,
      group
    )

#### Arguments

- `name`:

  Character, the internal name of the mart

- `displayName`:

  Character, the human-readable name

- `description`:

  Character, the mart description

- `config`:

  Character, the configuration identifier

- `isHidden`:

  Logical, whether the mart is hidden

- `operation`:

  Character, the operation mode

- `meta`:

  List, additional metadata

- `group`:

  Character, the group the mart belongs to

#### Returns

A new MartInfo object

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for MartInfo objects

#### Usage

    MartInfo$print(...)

#### Arguments

- `...`:

  Additional arguments (not used)

#### Returns

Invisibly returns self

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MartInfo$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
