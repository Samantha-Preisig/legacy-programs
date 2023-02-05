# Fortran Word Jumble

## TODO
- Allow uppercase words (and mix of upper/lower case words)

### Requirements
- Fortran 95+

### Compile and run
Build lexicon module: `gfortran -c lexicon.f95`\
Compile using: `gfortran -Wall solvejumble.f95 lexicon.f95`\
Run: `./a.out`

## Word Jumble
An anagram is a type of word play with the letters of a word or phrase rearranged to produce a new word or phrase.\
For example, the anagrams of **tea** are: tea, tae, eat, eta, aet, ate.
### **First puzzle:** 'Word Jumble'
The first puzzle (1) takes in a series of jumbled words given by the user, (2) generates all anagrams for each word, and (3) checks if any anagrams exist in the dictionary (provided in 'dict2.txt'). The anagrams appearing in the dictionary are printed as potential solutions to the puzzle.

### **Second puzzle:** 'Circled Word Puzzle'
After the list of given jumbled words have been solved, the user choose letters from each solved word to form another word.

## User interface
The program can run in one of two ways:
1. Enter all jumbled words as a group, process them as a group, then (optional) solve circled word puzzle
2. Enter each jumbled word with its circled positions one at a time, then process each word individually with the circled word puzzle solved

### Example
#### User interface #1:
```
How many jumbled words? 4

Enter circled positions with each word [Y/y or N/n]? N

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

Solve word jumble puzzle [Y/y or N/n]? Y

Select circled letters from word puzzle:
grime:  gi
bench:  b
amazed: ma
expend: en
Jumbled word: gibmaen

Solved jumble: beaming
```
#### User interface #2:
```
How many jumbled words? 4

Enter circled positions with each word [Y/y or N/n]? Y

Enter the jumbled word:
> mirge
Enter the circled position: 13
> chenb
Enter the circled position: 1
> adezma
Enter the circled position: 23
> pexden
Enter the circled position: 45

The following jumbles have been solved:
mirge   grime
chenb   bench
adezma  amazed
pexden  expend
Jumbled word: gibmaen

Solved jumble: beaming
```

### Limitations
- To generate anagrams, I chose to build an algorithm loosely based on Heap's algorithm `[1]` which generates all possible permutations of n-lengthed words by using recursion. Since the length of the word dictates the computational power to complete the anagram generation and word detection, this program only allows jumbled words of length 8 or less to be solved.
- If the user is providing multiple circled letters or circled positions, there must be no spaces between the letters/numbers. Otherwise, letters/numbers beyond the first space will be ignored.
- **Words and circled letters must be lowercase**

### References:
- `[1]` https://craftofcoding.wordpress.com/
- Knight, D.G., “Anagrams and recursion”, Teaching Mathematics and its Applications, Vol.5(3),
pp.138-140 (1986).
- Morton, M., “Recursion + data structures = Anagrams”, BYTE, Vol.12(13), pp.325-334 (1987)