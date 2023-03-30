with ada.text_io; use ada.text_io;
with ada.integer_text_io; use ada.integer_text_io;
with ada.strings.unbounded; use ada.strings.unbounded;
with ada.strings.unbounded.text_io; use ada.strings.unbounded.text_io;
with ada.directories; use ada.directories;
with ada.strings; use ada.strings;
with ada.strings.fixed; use ada.strings.fixed;
with ada.strings.maps; use ada.strings.maps;
with ada.strings.bounded; use ada.strings.bounded;
with gnat.string_split; use gnat.string_split;
with ada.float_text_io; use ada.float_text_io;

procedure textyzer is

------------------------------ Variables ------------------------------

    CR : constant Character := Character'Val(13); -- Carriage return
    LF : constant Character := Character'Val(10); -- Line Feed
    NL : constant String := CR & LF; -- Newline escape sequence

    filename : unbounded_string;
    infp : file_type;
    sline : unbounded_string;

    -- Stats
    charCount, wordCount, sentenceCount, numCount, punctCount : integer := 0;
    charPerWord, wordsPerSentence : float := 0.0;

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

    function isNumeric(word : in string) return boolean is

        dummy : float;
    
    begin

        dummy := float'value(word);
        --  numCount := numCount + 1;
        return true;

    exception

        when others =>
            return false;

    end isNumeric;

-----------------------------------------------------------------------

    function isNumber(word : in string) return boolean is
    
        unboundedWord : unbounded_string := to_unbounded_string(word);

    begin

        if(isNumeric(word)) then
            for i in 1..length(unboundedWord) loop
                if(element(unboundedWord, i) = '.' or
                   element(unboundedWord, i) = '-') then
                   punctCount := punctCount + 1;
                end if;
            end loop;
            return true;
        end if;
                    
        -- Check if word has any numbers in it
        for i in 1..length(unboundedWord) loop
            if not (element(unboundedWord, i) in '0'..'9') then
                return false;
            elsif (element(unboundedWord, i) in 'a'..'z' or
                   element(unboundedWord, i) in 'A'..'Z') then
                charCount := charCount + 1;
                --  put_line("Char mark: " & element(unboundedWord, i));
            else
                --  put_line("Punct mark: " & element(unboundedWord, i));
                punctCount := punctCount + 1;
            end if;
        end loop;

        return true;

    end isNumber;

-----------------------------------------------------------------------

    function isWord(word : in string) return boolean is

        unboundedWord : unbounded_string := to_unbounded_string(word);

    begin

        if(isNumeric(word)) then
            return false;
        end if;
                    
        -- Check if word has any numbers in it
        for i in 1..length(unboundedWord) loop
            if(element(unboundedWord, i) in '0'..'9') then
                return false;
            elsif (element(unboundedWord, i) in 'a'..'z' or
                   element(unboundedWord, i) in 'A'..'Z') then
                charCount := charCount + 1;
                --  put_line("Char mark: " & element(unboundedWord, i));
            else
                --  put_line("Punct mark: " & element(unboundedWord, i));
                punctCount := punctCount + 1;
            end if;
        end loop;

        return true;
    
    end isWord;

-----------------------------------------------------------------------

    procedure countSentences is

        counter : integer := 0;

    begin

        open(infp, in_file, to_string(filename));

        loop
            exit when end_of_file(infp);
            get_line(infp, sline);

            for i in 1..length(sline) loop
                if(element(sline, i) = '.') then
                    counter := counter + 1;
                end if;
            end loop;

        end loop;

        close(infp);
        sentenceCount := counter;

    end countSentences;

-----------------------------------------------------------------------

    procedure countWords is

        tokens : slice_set;

    begin
    
        open(infp, in_file, to_string(filename));

        while not end_of_file(infp) loop
            create(tokens, get_line(infp), " ", multiple);
            
            -- Loop through words in line
            for i in 1..slice_count(tokens) loop
                if(isWord(slice(tokens, i))) then
                    wordCount := wordCount + 1;
                elsif(isNumber(slice(tokens, i))) then
                    numCount := numCount + 1;
                end if;
                --  put_line(slice(tokens, i));
            end loop;

        end loop;

        close(infp);

    end countWords;

-----------------------------------------------------------------------

    -- Reference: https://learn.adacore.com/courses/intro-to-ada/chapters/standard_library_strings.html#string-operations
    -- Reads the file line by line, breaking each line up into individual words.
    procedure analyzeText(filename : in unbounded_string) is

    begin

        countWords;
        countSentences;

    end analyzeText;

-----------------------------------------------------------------------
    tmp : integer;

begin

    filename := getFilename;
    analyzeText(filename);

    charPerWord := float(charCount/wordCount);
    wordsPerSentence := float(wordCount/sentenceCount);
    
    put_line("Character count (a-z)      : " & integer'image(charCount) & NL &
             "Word count                 : " & integer'image(wordCount) & NL &
             "Sentence count             : " & integer'image(sentenceCount) & NL &
             "Number count               : " & integer'image(numCount) & NL &
             "Punctuation count          : " & integer'image(punctCount));
    put("Characters per word        :  ");
    put(charPerWord, 1, 1, 0);
    put(NL & "Words per sentence         :  ");
    put(wordsPerSentence, 1, 1, 0);

end textyzer;