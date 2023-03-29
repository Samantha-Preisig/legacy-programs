with ada.text_io; use ada.text_io;
with ada.integer_text_io; use ada.integer_text_io;
with ada.strings.unbounded; use ada.strings.unbounded;
with ada.strings.unbounded.text_io; use ada.strings.unbounded.text_io;
with ada.directories; use ada.directories;

procedure textyzer is

------------------------------ Variables ------------------------------

----------------------------- Subprograms -----------------------------

-----------------------------------------------------------------------

    -- Reference: https://craftofcoding.wordpress.com/2018/04/04/coding-ada-reading-lines-from-files/
    procedure getFilename is

        --  filename : string(1..256);
        infp : file_type;
        sline : unbounded_string;
        filename : unbounded_string;
        fileValid : boolean := false;

    begin

        loop
            exit when fileValid;
            put("Enter the filename: ");
            get_line(filename);
            fileValid := exists(to_string(filename));
        end loop;
        
        open(infp, in_file, to_string(filename));

        loop
            exit when end_of_file(infp);
            -- Process each line from the file
            get_line(infp, sline);
            put_line("sline .. " & sline);
        end loop;

    end getFilename;

-----------------------------------------------------------------------

begin

    getFilename;

end textyzer;