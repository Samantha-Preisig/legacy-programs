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

procedure textyzer is

------------------------------ Variables ------------------------------

    filename : unbounded_string;

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

    -- Reference: https://learn.adacore.com/courses/intro-to-ada/chapters/standard_library_strings.html#string-operations
    -- Reads the file line by line, breaking each line up into individual words.
    procedure analyzeText(filename : unbounded_string) is
        
        -- Reading/parsing file
        infp : file_type;
        sline : unbounded_string;
        tokens : slice_set;
        line : unbounded_string;

        -- Stats
        charCount, wordCount, sentenceCounter : integer := 0;

    begin

        --  open(infp, in_file, to_string(filename));
        --  loop
        --      exit when end_of_file(infp);
        --      -- Process each line from the file
        --      get_line(infp,sline);
        --      put_line(sline);
        --      -- do something with the line of text
        --  end loop;

        open(infp, in_file, to_string(filename));

        loop
            exit when end_of_file(infp);
            -- Process each line from the file
            get_line(infp,sline);
            --  put_line(sline);

            --  create(tokens, get_line(infp), " ", multiple);
            
            --  -- Loop through words in line
            --  for i in 1..slice_count(tokens) loop
            --      put_line(slice(tokens, i));            
            --  end loop;

            for i in reverse 1..length(sline) loop
                --  put(element(line, i) & " ");
                if(element(sline, i) = '.') then
                    sentenceCounter := sentenceCounter + 1;
                end if;
            end loop;
            put_line("Sentence counter = " & integer'image(sentenceCounter));
        end loop;

        --  while not end_of_file(infp) loop
        --      line := get_line(infp);
        --      create(tokens, get_line(infp), " ", multiple);
            

        --      -- Loop through words in line
        --      for i in 1..slice_count(tokens) loop
        --          put_line(slice(tokens, i));            
        --      end loop;

        --      --  for i in reverse 1..length(line) loop
        --      --      --  put(element(line, i) & " ");
        --      --      if(element(line, i) = '.') then
        --      --          sentenceCounter := sentenceCounter + 1;
        --      --      end if;
        --      --  end loop;

        --      --  put_line("Sentence counter = " & integer'image(sentenceCounter));
        --  end loop;

        close(infp);

    end analyzeText;

-----------------------------------------------------------------------

begin

    filename := getFilename;
    put_line(to_string(filename));

    analyzeText(filename);

end textyzer;