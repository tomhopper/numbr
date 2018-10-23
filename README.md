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

The `num_order_to_word` function converts a number to its large number name representation using the so-called "short scale," in which the name changes with each increase by a power of 3. For example:

```{r}
x <- c(323, 32423525, 86756456354)
num_order_to_word(x)
```
```
   number       name
1 3.2e+02        320
2 3.0e+07 30 million
3 9.0e+10 90 billion

```

### mode_stat()

`mode_stat` returns the modal values of a given vector. It works with any type of vector.

### mean_geom()

`mean_geom` calculates the geometric mean of a numeric or integer vector, returning a single value. The geometric mean,
like the mean and the median, is an indication of the central tendancy of a set of numbers.
The geometric mean is  the *n*th root of the product of *n* numbers. For instance,
the geometric mean of 2 and 8 is `sqrt(2 * 8) = 4`.

The geometric mean is useful when computing the central tendency of measures that have different
ranges. For instance, when computing a single "figure of merit" from differentrating scales that
have ranges 0 to 5 and 0 to 100.
