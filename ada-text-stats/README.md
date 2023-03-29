# ada-text-stats

## Compile and run
Compile: `gnatmake -Wall textyzer.adb`
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
- Perform error checking to amke sure file exists
- Read and process file to calculate quantative values above
- Output statistics to stdout

## Testing