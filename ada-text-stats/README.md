# ada-text-stats

## Compile and run
Compile: `gnatmake -Wall textyzer.adb`\
Run: `./textyzer`

## About
`textyzer.adb` performs text analysis of a file of human readable English text. The text could contain anywhere from 100 to 1000 words.\
The analysis involves calculating the following numerics:
- the number of sentences
- the number of words
- the number of characters (all characters)
- the number of numbers (i.e., dates that only contain digits)
- the number of punctuation characters
- the average number of words per sentence
- the average number of characters per word
- a histogram representing the word length distribution

## Algorithm synopsis
- User is prompted for a filename
- Perform error checking to make sure file exists
- Read and process text file to calculate quantative values (listed above)
- Output statistics to stdout

## Assumptions
- Sentences are split by either periods, exclamation points, or question marks
- Words only contain alphabetical characters (a..z, A..Z)
- Numbers can only contain numerical characters (0-9, '.' for decimal, and '-' for negatives)
    - However, '.' and '-' will be counted towards punctuation, no matter the context
- Text such as `a4, sam.12, 12_34-56` are neither words nor numbers. Therefore, they will not be represented within the final stats

## Limitations
- Rounds averages to whole number --> TODO: fix