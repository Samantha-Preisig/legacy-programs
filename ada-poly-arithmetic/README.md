# Polynomial Arithmetic

### Compile and run
Compile: `gnatmake -Wall poly.adb`\
Run: `./poly`

## About
A polynomial is a type of algebraic expression in which the exponents of all variables should be a whole number\
In this assignment, a polynomial is stored in a node which is then added to a linked list
- Each node represents a polynomial. Specifically:\
`id: integer;` -> polynomial id, to allow linked list indexing\
`poly : coeffArr;` -> coefficient array of integers (index representing degree; index 0: x^0, index 1: x^1, etc)\
`exp: integer;` -> degree (highest power) of the polynomial\
`next : list;` -- pointer to next node\

With a populated linked list of polynomials, we can now perform addition, subtraction, and multiplication on 2 polynomials. We can also evaluate a polynomial with a given value\
These math functions are found in the `polymath` package\
The `polylink` package contains all subprograms dealing with the linked list; printing, appending, retrieving size, and retrieving a specific polynomial\
`poly.adb` is a wrapper program which interacts with both `polylink` and `polymath` packages to create a CLI program to build a linked list and perform math on chosen nodes in the list

## Assumptions/Limitations
- Coefficients and exponents of a given polynomial are integers
- Error handling and their respective messages are not extensive

## CLI demo
```
----------- POLYNOMIAL Arithmetic CLI -----------
Enter a number to perform one of the following commands:
1. Input a polynomial
2. Print a polynomial
3. Add two polynomials
4. Subtract two polynomials
5. Multiply two polynomials
6. Evaluate a polynomial
7. Quit

> 1
Enter a polynomial:
   Highest exponent: 2
   Coefficient for x^ 0: 1
   Coefficient for x^ 1: 2
   Coefficient for x^ 2: 3

----------- POLYNOMIAL Arithmetic CLI -----------
Enter a number to perform one of the following commands:
1. Input a polynomial
2. Print a polynomial
3. Add two polynomials
4. Subtract two polynomials
5. Multiply two polynomials
6. Evaluate a polynomial
7. Quit

> 2
Index of polynomial to print out [0.. 0]: 0
 3x^ 2 +  2x^ 1 +  1

----------- POLYNOMIAL Arithmetic CLI -----------
Enter a number to perform one of the following commands:
1. Input a polynomial
2. Print a polynomial
3. Add two polynomials
4. Subtract two polynomials
5. Multiply two polynomials
6. Evaluate a polynomial
7. Quit

> 1
Enter a polynomial:
   Highest exponent: 3
   Coefficient for x^ 0: 1
   Coefficient for x^ 1: 2
   Coefficient for x^ 2: 3
   Coefficient for x^ 3: 4

----------- POLYNOMIAL Arithmetic CLI -----------
Enter a number to perform one of the following commands:
1. Input a polynomial
2. Print a polynomial
3. Add two polynomials
4. Subtract two polynomials
5. Multiply two polynomials
6. Evaluate a polynomial
7. Quit

> 3
You will be adding two polynomials, p1 and p2
   Index for p1 [0.. 1]: 0
   Index for p2 [0.. 1]: 1

   3x^ 2 +  2x^ 1 +  1
+  4x^ 3 +  3x^ 2 +  2x^ 1 +  1
=  4x^ 3 +  6x^ 2 +  4x^ 1 +  2

----------- POLYNOMIAL Arithmetic CLI -----------
Enter a number to perform one of the following commands:
1. Input a polynomial
2. Print a polynomial
3. Add two polynomials
4. Subtract two polynomials
5. Multiply two polynomials
6. Evaluate a polynomial
7. Quit

> 6
   Index for p1 [0.. 1]: 0
   Enter a value to evaluate this polynomial with: 2
f(x) =  3x^ 2 +  2x^ 1 +  1
f( 2) =  17
----------- POLYNOMIAL Arithmetic CLI -----------
Enter a number to perform one of the following commands:
1. Input a polynomial
2. Print a polynomial
3. Add two polynomials
4. Subtract two polynomials
5. Multiply two polynomials
6. Evaluate a polynomial
7. Quit

> 7
```