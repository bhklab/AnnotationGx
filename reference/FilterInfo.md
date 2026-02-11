# BioMart Filter Information Class

Represents a filter from a BioMart dataset.

## Details

This class encapsulates information about a filter available in a
BioMart dataset. Filters are used to specify conditions for querying and
subsetting data from BioMart.

## Public fields

- `name`:

  The internal name of the filter used in BioMart queries

- `displayName`:

  The human-readable name of the filter shown in user interfaces

- `description`:

  A textual description explaining the filter's purpose

- `type`:

  The data type of the filter (e.g., "string", "boolean", "list")

- `isHidden`:

  Logical flag indicating if the filter should be hidden in user
  interfaces

- `values`:

  Possible values for the filter if it has a fixed/enumerated set of
  options

## Methods

### Public methods

- [`FilterInfo$new()`](#method-FilterInfo-new)

- [`FilterInfo$print()`](#method-FilterInfo-print)

- [`FilterInfo$clone()`](#method-FilterInfo-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new FilterInfo object

#### Usage

    FilterInfo$new(
      name,
      displayName = NULL,
      description = NULL,
      type = NULL,
      isHidden = NULL,
      values = NULL
    )

#### Arguments

- `name`:

  Character, the internal name of the filter used in API calls

- `displayName`:

  Character, the human-readable name shown to users

- `description`:

  Character, the filter description explaining its purpose

- `type`:

  Character, the data type of the filter (e.g., "string", "boolean")

- `isHidden`:

  Logical, whether the filter should be hidden in UIs

- `values`:

  List or vector, possible values for the filter if applicable

#### Returns

A new FilterInfo object

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for FilterInfo objects

#### Usage

    FilterInfo$print(...)

#### Arguments

- `...`:

  Additional arguments passed to print methods (not used)

#### Returns

Invisibly returns self for method chaining

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FilterInfo$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
