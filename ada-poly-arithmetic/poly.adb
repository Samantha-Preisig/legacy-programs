with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with polylink; use polylink;
with polymath; use polymath;

procedure poly is

------------------------------ Variables ------------------------------

    CR : constant Character := Character'Val(13); -- Carriage return
    LF : constant Character := Character'Val(10); -- Line Feed
    NL : constant String := CR & LF; -- Newline escape sequence

    cli_choice, listSize, p1, p2, addLen, j : integer;
    poly1, poly2 : list;
    retArr : polylink.coeffArr;

----------------------------- Subprograms -----------------------------



-----------------------------------------------------------------------

begin

    --  CLI loop --
    loop
        -- Menu --
        put_line(NL & "----------- POLYNOMIAL Arithmetic CLI -----------");
        put_line("1. Input a polynomial" & NL &
                 "2. Print a polynomial" & NL &
                 "3. Add two polynomials" & NL &
                 "4. Subtract two polynomials" & NL &
                 "5. Multiply two polynomials" & NL &
                 "6. Evaluate a polynomial" & NL &
                 "7. Quit" & NL);

        put("> ");
        get(cli_choice);

        if(cli_choice >= 1 or cli_choice <= 7) then

            -- Add polynomial to linked list --
            if cli_choice = 1 then
                readPOLY;
            
            -- Print an indexed polynomial --
            elsif cli_choice = 2 then
                listSize := getListSize(polylink.head);
                put("Index of polynomial to print out [0.." & integer'image(listSize-1) & "]: ");
                get(p1);
                writePOLY(polylink.head, p1);
            
            -- Add two polynomials --
            elsif cli_choice = 3 then
                listSize := getListSize(polylink.head);

                -- Error handling: must be at least 2 polynomials in linked list to
                -- perform this command
                if(listSize = 0) then
                    put_line("There are " & integer'image(listSize) & " polynomials in the linked list" & NL &
                             "Please enter at least 2 polynomials to use this command");
                else
                    put_line("You will be adding two polynomials, p1 and p2");

                    -- Provide index of linked list (starting from 0)
                    put("   Index for p1 [0.." & integer'image(listSize-1) & "]: ");
                    get(p1);
                    put("   Index for p2 [0.." & integer'image(listSize-1) & "]: ");
                    get(p2);

                    listSize := getListSize(polylink.head);
                    poly1 := getAPoly(polylink.head, p1);
                    poly2 := getAPoly(polylink.head, p2);
                    
                    -- Performing addition by calling addpoly() from polymath package
                    retArr := polymath.addpoly(poly1, poly2);

                    -- Gathering info for addition output
                    if(poly1.exp >= poly2.exp) then
                        addLen := poly1.exp;
                    else
                        addLen := poly2.exp;
                    end if;

                    -- Addition output --
                    put("  ");
                    writePOLY(polylink.head, p1);
                    put("+ ");
                    writePOLY(polylink.head, p2);
                    put("= ");
                    j := 0;
                    for i in reverse 1..addLen loop
                        put(integer'image(retArr(j)) & "x^" & integer'image(i) & " + ");
                        j := j + 1;
                    end loop;
                    put_line(integer'image(retArr(j)) & "x^" & integer'image(0));
                end if;
            
            -- Subtract two polynomials --
            elsif cli_choice = 4 then
                put_line("TODO");

            end if;
            exit when cli_choice = 7;
        end if;
    end loop;

end poly;