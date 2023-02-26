with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Gnat.IO; -- used when printing text before user input (on same line)
with Ada.Strings.unbounded; use Ada.Strings.unbounded;
with Ada.Strings.unbounded.text_io; use Ada.Strings.unbounded.text_io;
with polylink; use polylink;

procedure poly is

------------------------------ Variables ------------------------------

    CR : constant Character := Character'Val(13); -- Carriage return
    LF : constant Character := Character'Val(10); -- Line Feed
    NL : constant String := CR & LF; -- Newline escape sequence

    cli_choice : integer;
    head : list;

-----------------------------------------------------------------------

begin

    put_line("POLYNOMIAL Arithmetic CLI");
    put_line("1. Input a polynomial" & NL &
             "2. Print a polynomial" & NL &
             "3. Add two polynomials" & NL &
             "4. Subtract two polynomials" & NL &
             "5. Multiply two polynomials" & NL &
             "6. Evaluate a polynomial" & NL &
             "7. Quit" & NL);

    --  CLI loop --
    loop
        put("> ");
        get(cli_choice);

        if(cli_choice >= 1 or cli_choice <= 7) then

            if cli_choice = 1 then
                readPOLY;
            elsif cli_choice = 2 then
                writePOLY(head);
            end if;
            exit when cli_choice = 7;
        end if;
    end loop;

end poly;