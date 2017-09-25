# numbr

Convenience functions for working with numbers.

## Installation

`devtools::install_github("tomhopper/numbr")`

## Description

Provides functions missing in base R, including working with scientific notation and checking for integers.

### is.int()

Determines if each element of a numeric vector fits the IEEE-754 definition of an integer that can be exactly represented with a double-precision number. This is different than the base R function `is.integer()`, which only checks if the numbers are stored as type *integer*.

```{r}
x <- c(1, 412, 2.2)
is.int(x)
[1]  TRUE  TRUE FALSE

is.integer(x)
[1] FALSE
```

### mantissa()

The `mantissa` function extracts the base-10 mantissa of a number, allowing further manipulation such as conversion to human-readable format.

### exponent()

The `exponent` function extracts the base-10 exponent of a number, allowing further manipulation such as conversion to human-readable format. 

### num_order_to_word()

The `num_order_to_word` function converts a number to its large number name representation using the so-called "short scale," in which the name changes with each increase by a power of 3. For instance,

```{r}
num_order_to_word(123456789)

$number
[1] 1e+08

$name
[1] "100 million"

```
