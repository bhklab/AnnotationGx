# BioMart Attribute Information Class

Represents an attribute from a BioMart dataset.

## Details

This class encapsulates information about an attribute available in a
BioMart dataset. Attributes are data fields that can be selected for
retrieval in BioMart query results.

## Public fields

- `name`:

  The internal name of the attribute used in BioMart queries

- `displayName`:

  The human-readable name of the attribute shown in user interfaces

- `description`:

  A textual description explaining what the attribute represents

- `linkURL`:

  URL that provides additional information about the attribute

- `isHidden`:

  Logical flag indicating if the attribute should be hidden in user
  interfaces

## Methods

### Public methods

- [`AttributeInfo$new()`](#method-AttributeInfo-new)

- [`AttributeInfo$print()`](#method-AttributeInfo-print)

- [`AttributeInfo$clone()`](#method-AttributeInfo-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new AttributeInfo object

#### Usage

    AttributeInfo$new(
      name,
      displayName = NULL,
      description = NULL,
      linkURL = NULL,
      isHidden = NULL
    )

#### Arguments

- `name`:

  Character, the internal name of the attribute used in API calls

- `displayName`:

  Character, the human-readable name shown to users

- `description`:

  Character, the attribute description explaining what it represents

- `linkURL`:

  Character, URL for additional information about the attribute

- `isHidden`:

  Logical, whether the attribute should be hidden in UIs

#### Returns

A new AttributeInfo object

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for AttributeInfo objects

#### Usage

    AttributeInfo$print(...)

#### Arguments

- `...`:

  Additional arguments passed to print methods (not used)

#### Returns

Invisibly returns self for method chaining

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AttributeInfo$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
