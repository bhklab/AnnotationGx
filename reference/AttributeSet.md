# BioMart Attribute Set Class

Represents a set of attributes from a BioMart dataset.

## Public fields

- `attributes`:

  List of AttributeInfo objects

## Methods

### Public methods

- [`AttributeSet$new()`](#method-AttributeSet-new)

- [`AttributeSet$get_by_display_name()`](#method-AttributeSet-get_by_display_name)

- [`AttributeSet$print()`](#method-AttributeSet-print)

- [`AttributeSet$as.list()`](#method-AttributeSet-as.list)

- [`AttributeSet$filter()`](#method-AttributeSet-filter)

- [`AttributeSet$clone()`](#method-AttributeSet-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new AttributeSet object

#### Usage

    AttributeSet$new(attributes)

#### Arguments

- `attributes`:

  List of AttributeInfo objects

------------------------------------------------------------------------

### Method `get_by_display_name()`

#### Usage

    AttributeSet$get_by_display_name(display_names)

#### Arguments

- `display_names`:

  Character vector of display names

#### Returns

List of matching AttributeInfo objects

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for AttributeSet objects

#### Usage

    AttributeSet$print(...)

#### Arguments

- `...`:

  Additional arguments passed to print methods (not used)

#### Returns

Invisibly returns self

------------------------------------------------------------------------

### Method [`as.list()`](https://rdrr.io/r/base/list.html)

Convert AttributeSet to list

#### Usage

    AttributeSet$as.list()

#### Returns

List of attributes DisplayName as strings

------------------------------------------------------------------------

### Method [`filter()`](https://rdrr.io/r/stats/filter.html)

Filter attributes based on regex pattern

#### Usage

    AttributeSet$filter(pattern, exclude = FALSE)

#### Arguments

- `pattern`:

  Regular expression pattern to match against display names

- `exclude`:

  Logical, if TRUE excludes matching patterns, if FALSE includes them

#### Returns

A new AttributeSet with filtered attributes

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AttributeSet$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
