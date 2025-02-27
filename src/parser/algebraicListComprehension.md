# λits List Comprehension Syntax Guide

This guide focuses on the list comprehension syntax in λits, which draws inspiration from Clojure's `for` macro but uses algebraic notation.

## Table of Contents
- [Introduction](#introduction)
- [Basic Syntax](#basic-syntax)
- [Examples](#examples)
- [Advanced Features](#advanced-features)
- [Real World Example](#real-world-example)
- [Syntax Reference](#syntax-reference)

## Introduction

List comprehensions provide a concise way to create sequences by iterating over collections with optional filtering and transformations. In λits, the `for` comprehension allows you to express complex sequence operations in a readable algebraic style.

## Basic Syntax

The basic syntax for a λits list comprehension is:

```
for(variable of collection, result)
```

For multiple variables:

```
for(var1 of collection1, var2 of collection2, result)
```

## Examples

### Simple Iteration

```
// Doubles each number
for(x of [1, 2, 3], x * 2)  // => [2, 4, 6]

// Cartesian product of two collections
for(x of [1, 2], y of ["a", "b"], [x, y])
// => [[1, "a"], [1, "b"], [2, "a"], [2, "b"]]
```

### Empty Collections

```
for(x of [], x)  // => []
for(x of [1, 2, 3], y of [], x)  // => []
```

### Iterating Over Different Collection Types

```
// String iteration (by character)
for(x of "Al", x)  // => ["A", "l"]

// Object iteration (by key-value pairs)
for(x of { a=10, b=20 }, x)  // => [["a", 10], ["b", 20]]
```

## Advanced Features

### Computed Bindings with `let`

The `let` keyword allows you to create computed values that can be used in the result or subsequent bindings:

```
for(x of [1, 2, 3] let { square = x * x }, [x, square])
// => [[1, 1], [2, 4], [3, 9]]

// Using a computed binding in another computation
for(x of [1, 2] let { squared = x * x, cubed = squared * x }, [x, squared, cubed])
// => [[1, 1, 1], [2, 4, 8]]
```

### Filtering with `when`

The `when` keyword filters iterations based on a predicate:

```
// Keep only even numbers
for(x of [1, 2, 3, 4, 5] when even(x), x)  // => [2, 4]

// Combine with computed bindings
for(x of [0, 1, 2, 3, 4, 5] let { y = x * 3 } when even(y), y)
// => [0, 6, 12]
```

### Early Termination with `while`

The `while` keyword stops iteration as soon as its condition becomes false:

```
// Stop when we reach 6
for(x of [1, 3, 5, 6, 7, 9] while x < 6, x)  // => [1, 3, 5]

// Combine with computed bindings
for(x of [0, 1, 2, 3, 4, 5] let { y = x * 3 } while even(y), y)
// => [0]
```

## Real-World Example
### Product Bundle Recommender
Here's a practical example that might be used in an e-commerce system to generate personalized product bundle recommendations:

```
// Imagine these are coming from a database
let({
  products = [
    { id="P1", name="Phone", price=500, category="electronics", stockLevel=23 },
    { id="P2", name="Headphones", price=150, category="electronics", stockLevel=42 },
    { id="P3", name="Case", price=30, category="accessories", stockLevel=56 },
  ],
  customerPreferences = {
    priceLimit=700,
    preferredCategories=["electronics", "accessories"],
    recentViews=["P1", "P3", "P5"]
  }
},

  // Generate personalized bundle recommendations
  for(
    // Start with main products
    mainProduct of products
      let {
        isInStock = mainProduct.stockLevel > 0,
        isPreferredCategory = has?(customerPreferences.preferredCategories, mainProduct.category),
        isPriceOk = mainProduct.price <= customerPreferences.priceLimit * 0.8
      }
      when (isInStock && isPreferredCategory && isPriceOk),

    // Add compatible accessories
    accessory of products
      let {
        isCompatible = mainProduct.id != accessory.id && accessory.stockLevel > 0,
        totalPrice = mainProduct.price + accessory.price,
        isRecentlyViewed = has?(customerPreferences.recentViews, accessory.id)
      }
      when (isCompatible && totalPrice <= customerPreferences.priceLimit)
      while totalPrice <= customerPreferences.priceLimit * 0.9,

    // For high-value bundles, consider a third complementary item
    complItem of products
      let {
        isValid = mainProduct.id != complItem.id && accessory.id != complItem.id && complItem.stockLevel > 0,
        finalPrice = mainProduct.price + accessory.price + complItem.price,
        discount = if(finalPrice > 500, 0.1, 0.05),
        discountedPrice = finalPrice * (1 - discount),
        matchesPreferences = has?(customerPreferences.preferredCategories, complItem.category)
      }
      when (isValid && finalPrice <= customerPreferences.priceLimit && matchesPreferences)
      while discountedPrice <= customerPreferences.priceLimit,

    // Return bundle information object
    {
      bundle=[mainProduct, accessory, complItem],
      originalPrice=finalPrice,
      discountedPrice=discountedPrice,
      savingsAmount=discount * finalPrice,
      savingsPercentage=discount * 100
    }
  )
)```

### Understanding the Product Bundle Recommender

This code example demonstrates a sophisticated product bundle recommender system using λits list comprehensions. Let's break down how it works:

#### Data Sources
The system works with two main data sources:
- A collection of products with attributes like price, category, and stock level
- Customer preferences including price limits and category preferences

#### Comprehension Structure
The comprehension consists of three nested iterations:

1. **Main Product Selection**
  - Iterates over all available products
  - Computes key attributes: stock status, category match, and price suitability
  - Filters using a combined condition to ensure the main product meets all criteria

2. **Accessory Selection**
  - For each valid main product, finds compatible accessories
  - Computes compatibility, total price, and relevance score
  - Uses `when` to filter for compatibility and budget
  - Uses `while` to stop early if approaching budget limits

3. **Complementary Item Selection**
  - For each main product + accessory pair, finds a third complementary item
  - Computes validity, final pricing, and discounts
  - Filters items based on multiple validity conditions
  - Terminates early if the discounted price exceeds budget

#### Result Generation
For each valid combination, the comprehension produces a bundle object containing:
- The selected products
- Original price
- Discounted price
- Savings amount and percentage

This example shows how list comprehensions can elegantly express complex business logic that would typically require multiple nested loops and conditional statements in imperative code.
## Syntax Reference

```
for(
  variable of collection
    [let { binding = expression [, ...] }]
    [when condition]
    [while condition],
  [another_variable of another_collection ...],
  result_expression
)
```

Where:
- `variable`: Name to bind each element to
- `collection`: Any iterable collection (array, string, object)
- `let { ... }`: Optional computed bindings
- `when condition`: Optional filter predicate
- `while condition`: Optional early termination predicate
- `result_expression`: Expression to evaluate for each valid iteration
