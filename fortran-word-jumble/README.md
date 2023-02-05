# Fortran Word Jumble

### Requirements
- Fortran 95+

### Compile and run
Build lexicon module: `gfortran -c lexicon.f95`\
Compile using: `gfortran -Wall solvejumble.f95 lexicon.f95`\
Run: `./a.out`

## Word Jumble
An anagram is a type of word play with the letters of a word or phrase rearranged to produce a new word or phrase.\
For example, the anagrams of **tea** are: tea, tae, eat, eta, aet, ate.\
### First puzzle: 'Word Jumble'
The first puzzle (1) takes in a series of jumbled words given by the user, (2) generates all anagrams for each word, and (3) checks if any anagrams exist in the dictionary (provided in 'dict2.txt'). The anagrams appearing in the dictionary are printed as potential solutions to the puzzle.\

### Second puzzle: 'Circled Word Puzzle'
After the list of given jumbled words have been solved, the user choose letters from each solved word to form another word.\

### Example
```
How many jumbled words? 4

Enter the 4 jumbled words:
> mirge
> chenb
> adezma
> pexden

The following jumbles have been solved:
mirge   grime
chenb   bench
adezma  amazed
pexden  expend

Solve word jumble puzzle? Y

Select circled letters from word puzzle:
grime:  gi
bench:  b
amazed: ma
expend: en
Jumbled word: gibmaen

Solved jumble: beaming
```

### Limitations
- To generate anagrams, I chose to build an algorithm loosely based on Heap's algorithm `[1]` which generates all possible permutations of n-lengthed words by using recursion. Since the length of the word dictates the computational power to complete the anagram generation and word detection, this program only allows jumbled words of length 8 or less to be solved.
- If the user is providing multiple circled letters or circled positions, there must be no spaces between the letters/numbers. Otherwise, letters/numbers beyond the first space will be ignored.

### References:
1. https://craftofcoding.wordpress.com/