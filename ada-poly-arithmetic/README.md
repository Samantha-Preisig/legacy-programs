# Polynomial Arithmetic

# TODO
- Fix coding style (capitalization with imports, camelCase)
- Full documentation with headers for each file
- Explain assignment and user interface in README
- Error handling for user input

### Requirements

### Compile and run
Compile: `gnatmake poly.adb`\
Run: `./poly`

## Assumptions/Limitations
- Coefficients and exponents of a given polynomial are integers
```
   3x^ 2 +  4x^ 1 +  5x^ 0
-  4x^ 3 +  5x^ 2 +  6x^ 1 +  7x^ 0
= -1x^ 3 + -1x^ 2 + -1x^ 1 +  7x^ 0
```
- the above subtraction is wrong (should be -7)