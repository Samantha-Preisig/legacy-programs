-- Wrapper program containing CLI which interacts with both polylink
-- and polymath packages to perform linked list and math operations
-- (respectively)
-- Authour: Samantha Preisig
-- Date: Mar 3, 2023

with ada.text_io; use ada.text_io;
with ada.integer_text_io; use ada.integer_text_io;
with ada.io_exceptions;
with polylink; use polylink;
with polymath; use polymath;

procedure poly is

------------------------------ Variables ------------------------------

    CR : constant Character := Character'Val(13); -- Carriage return
    LF : constant Character := Character'Val(10); -- Line Feed
    NL : constant String := CR & LF; -- Newline escape sequence

    cliChoice, listSize, p1, p2, newLen, j, retVal : integer;
    poly1, poly2 : list;
    retArr : polylink.coeffArr;

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
        get(cliChoice);

        if(cliChoice >= 1 or cliChoice <= 7) then

            -- Add polynomial to linked list --
            if(cliChoice = 1) then
                readPOLY;
            
            -- Print an indexed polynomial --
            elsif(cliChoice = 2) then

                listSize := getListSize(polylink.head);

                if(listSize = 0) then
                    put_line("There are no polynomials in the linked list" & NL &
                             "Please enter at least 1 polynomials to use this command");
                else
                    put("Index of polynomial to print out [0.." & integer'image(listSize-1) & "]: ");
                    get(p1);
                    writePOLY(polylink.head, p1);
                end if;
            
            -- Add, subtract, multiply two polynomials --
            elsif(cliChoice = 3 or cliChoice = 4 or cliChoice = 5) then
                
                listSize := getListSize(polylink.head);

                -- Error handling: must be at least 2 polynomials in linked list to
                -- perform this command
                if(listSize < 2) then
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
                    if(cliChoice = 3) then
                        retArr := polymath.addpoly(poly1, poly2);
                    elsif(cliChoice = 4) then
                        retArr := polymath.subpoly(poly1, poly2);
                    elsif(cliChoice = 5) then
                        retArr := polymath.multpoly(poly1, poly2);
                    end if;

                    -- Gathering info for addition output
                    if(cliChoice = 3 or cliChoice = 4) then
                        if(poly1.exp >= poly2.exp) then
                            newLen := poly1.exp;
                        else
                            newLen := poly2.exp;
                        end if;
                    else
                        newLen := poly1.exp + poly2.exp;
                    end if;

                    -- Math result output --
                    new_line;
                    put("  ");
                    writePOLY(polylink.head, p1);
                    if(cliChoice = 3) then
                        put("+ ");
                    elsif(cliChoice = 4) then
                        put("- ");
                    elsif(cliChoice = 5) then
                        put("x ");
                    end if;
                    writePOLY(polylink.head, p2);
                    put("= ");
                    j := 0;
                    for i in reverse 1..newLen loop
                        put(integer'image(retArr(j)) & "x^" & integer'image(i) & " + ");
                        j := j + 1;
                    end loop;
                    put_line(integer'image(retArr(j)));
                end if;

            -- Evaluate a polynomial --
            elsif(cliChoice = 6) then
            
                listSize := getListSize(polylink.head);

                if(listSize = 0) then
                    put_line("There are no polynomials in the linked list" & NL &
                             "Please enter at least 1 polynomials to use this command");
                else
                    -- Provide index of linked list (starting from 0)
                    put("   Index for p1 [0.." & integer'image(listSize-1) & "]: ");
                    get(p1);
                    put("   Enter a value to evaluate this polynomial with: ");
                    get(j);
                    poly1 := getAPoly(polylink.head, p1);
                    retVal := evalpoly(poly1, j);

                    -- Evaluation output --
                    put("f(x) = ");
                    writePOLY(polylink.head, p1);
                    put("f(" & integer'image(j) & ") = " & integer'image(retVal));
                end if;

            end if;

            exit when cliChoice = 7;

        end if;

    end loop;

exception
    when data_error =>
        put_line("Invalid input, must be an integer");

end poly;