-- Text statistics in Ada
-- Authour: Samantha Preisig
-- Date: Apr 8, 2023

with ada.text_io; use ada.text_io;
with ada.integer_text_io; use ada.integer_text_io;
with ada.strings.unbounded; use ada.strings.unbounded;
with ada.strings.unbounded.text_io; use ada.strings.unbounded.text_io;
with ada.directories; use ada.directories;
with ada.strings; use ada.strings;
with gnat.string_split; use gnat.string_split;
with ada.float_text_io; use ada.float_text_io;

procedure textyzer is

------------------------------ Variables ------------------------------

    CR : constant Character := Character'Val(13); -- Carriage return
    LF : constant Character := Character'Val(10); -- Line Feed
    NL : constant String := CR & LF; -- Newline escape sequence

    -- File information to open/read/close file
    filename : unbounded_string;
    infp : file_type;
    sline : unbounded_string;

    -- Global statistics
    charCount, wordCount, sentenceCount, numCount, punctCount : integer := 0;
    charPerWord, wordsPerSentence : float := 0.0; -- Averages
    wordDist : array(1..20) of integer; -- Histogram - Word Length Distribution

----------------------------- Subprograms -----------------------------

    -- Reference: https://craftofcoding.wordpress.com/2018/04/04/coding-ada-reading-lines-from-files/
    -- Prompts the user for a filename, checks the existence fo the file, and returns the filename to
    -- the `main` routine. If the file does not exist, the user is re-prompted until a valid filename
    -- is provided.
    function getFilename return unbounded_string is

        fname : unbounded_string;
        fileValid : boolean := false;

    begin

        loop
            exit when fileValid;
            put("Enter the filename: ");
            get_line(fname);
            fileValid := exists(to_string(fname));
        end loop;

        return fname;

    end getFilename;

-----------------------------------------------------------------------

    -- Checks if a word is purely numeric (can be converted into a float)
    -- Returns true if the given word can be converted into a float, false otherwise
    function isNumeric(word : in string) return boolean is

        dummy : float;
    
    begin

        dummy := float'value(word); -- Attempt float conversion
        return true;

    exception

        when others =>
            return false;

    end isNumeric;

-----------------------------------------------------------------------

    -- Checks if a word contains numerical characters. A word is numeric if it contains
    -- at least one number
    -- Character and punctuation counts are updated
    -- Returns true if the given word contains numbers, false otherwise
    function isNumber(word : in string) return boolean is
    
        unboundedWord : constant unbounded_string := to_unbounded_string(word);

    begin

        if(isNumeric(word)) then
            for i in 1..length(unboundedWord) loop
                -- Numbers can be accompanied with periods and dashes to represent decimal and negative
                -- values. However, periods and dashes are considered punctuation and are added to the
                -- overall punctuation count (no matter its context)
                if(element(unboundedWord, i) = '.' or
                   element(unboundedWord, i) = '-') then
                   punctCount := punctCount + 1;
                end if;
            end loop;
            return true;
        end if;

        -- Check if the given word contains numbers
        for i in 1..length(unboundedWord) loop
            if not (element(unboundedWord, i) in '0'..'9') then
                return false; -- The word must contain at least one number to be classified as a number
            -- Adding to character and punctuation count for the given word
            elsif (element(unboundedWord, i) in 'a'..'z' or
                   element(unboundedWord, i) in 'A'..'Z') then
                charCount := charCount + 1;
            else
                -- If the current character is not a number or a letter (a..z), it is assumed to be
                -- punctuation
                punctCount := punctCount + 1;
            end if;
        end loop;

        return true;

    end isNumber;

-----------------------------------------------------------------------

    -- Checks if a word contains only alpha characters (a..z, A..Z)
    -- Character and punctuation counts are updated, word distribution array is updated
    -- Returns true if the given word only contains alpha characters, false otherwise
    function isWord(word : in string) return boolean is

        unboundedWord : constant unbounded_string := to_unbounded_string(word);
        -- subCounter counts punctuation present at the start or end of the given word. Since the words
        -- are split by whitespace, punctuation is attached to words but are not considered part of the
        -- word length. subCounter will be subtracted by the given word's length to get its true length
        subCounter, wordLen : integer := 0; -- to subtract from word length

    begin

        if(isNumeric(word)) then
            return false;
        end if;
                    
        -- Check if the given word contains numbers
        for i in 1..length(unboundedWord) loop
            if(element(unboundedWord, i) in '0'..'9') then
                return false; -- A word cannot contain numerical characters, only alpha characters
            -- Adding to character and punctuation count for the given word
            elsif (element(unboundedWord, i) in 'a'..'z' or
                   element(unboundedWord, i) in 'A'..'Z') then
                charCount := charCount + 1;
            else
                subCounter := subCounter + 1; -- Punctuation can be present in the word
                punctCount := punctCount + 1;
            end if;
        end loop;

        wordLen := length(unboundedWord) - subCounter; -- Finding the word's true length
        wordDist(wordLen) := wordDist(wordLen) + 1; -- Adding word's length to word distribution array
        return true;
    
    end isWord;

-----------------------------------------------------------------------

    -- Counts the number of sentences within the text file
    -- Assumption: sentences are identified by periods, exclamation point, and question
    -- marks
    procedure countSentences is

        counter : integer := 0;

    begin

        open(infp, in_file, to_string(filename));

        loop
            exit when end_of_file(infp);
            get_line(infp, sline); -- Read file line by line

            -- Search for specified punctuation to identify sentences within the file line
            for i in 1..length(sline) loop
                if(element(sline, i) = '.' or
                   element(sline, i) = '!' or
                   element(sline, i) = '?') then
                    counter := counter + 1;
                end if;
            end loop;

        end loop;

        close(infp);
        sentenceCount := counter; -- Updating global stat

    end countSentences;

-----------------------------------------------------------------------

    -- Counts the number of words within the text file
    -- Word and number counts are updated
    procedure countWords is

        tokens : slice_set;

    begin
    
        open(infp, in_file, to_string(filename));

        while not end_of_file(infp) loop
            create(tokens, get_line(infp), " ", multiple); -- Split file line by spaces to identify words
            
            -- Loop through words
            for i in 1..slice_count(tokens) loop
                -- Check if word is a word or number and update global word/number counts
                if(isWord(slice(tokens, i))) then
                    wordCount := wordCount + 1;
                elsif(isNumber(slice(tokens, i))) then
                    numCount := numCount + 1;
                end if;
            end loop;
        end loop;

        close(infp);

    end countWords;

-----------------------------------------------------------------------

    -- Calls countWords and countSentences to populate global text statistics
    procedure analyzeText is

    begin

        countWords;
        countSentences;

    end analyzeText;

-----------------------------------------------------------------------

    -- Uses global text statistics to print summary to stdout
    procedure printStats is

    begin

        put_line("Here are some stats:" & NL &
             "Character count (a-z)      : " & integer'image(charCount) & NL &
             "Word count                 : " & integer'image(wordCount) & NL &
             "Sentence count             : " & integer'image(sentenceCount) & NL &
             "Number count               : " & integer'image(numCount) & NL &
             "Punctuation count          : " & integer'image(punctCount));
        put("Characters per word        :  ");
        put(charPerWord, 1, 1, 0);
        put(NL & "Words per sentence         :  ");
        put(wordsPerSentence, 1, 1, 0);

    end printStats;

-----------------------------------------------------------------------

    -- Uses global word length distribution array to print histogram to stdout
    procedure printHist is

    begin

        put_line("Histogram - Word Length Distribution");
        for i in 1..20 loop
            put(item => i, width => 2);
            put("   ");
            for j in 1..wordDist(i) loop
                put("*");
            end loop;
            put(NL);
        end loop;

    end printHist;

-----------------------------------------------------------------------

begin

    filename := getFilename;
    
    -- Initiallizing all elements in word distribution array to 0 before analyzing text
    for i in 1..20 loop
        wordDist(i) := 0;
    end loop;

    -- Populates all statistics
    analyzeText;
    
    -- Calculating averages
    charPerWord := float(charCount/wordCount);
    wordsPerSentence := float(wordCount/sentenceCount);
    
    -- Printing text statistics
    put(NL);
    printStats;

    -- Printing histogram
    put_line(NL);
    printHist;
    
end textyzer;