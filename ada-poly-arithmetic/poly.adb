with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Gnat.IO; -- used when printing text before user input (on same line)
with Ada.Strings.unbounded; use Ada.Strings.unbounded;
with Ada.Strings.unbounded.text_io; use Ada.Strings.unbounded.text_io;

procedure poly is

------------------------------ Variables ------------------------------

    CR : constant Character := Character'Val(13); -- Carriage return
    LF : constant Character := Character'Val(10); -- Line Feed
    NL : constant String := CR & LF; -- Newline escape sequence
    
    highestExp : integer := 100; -- highestExp defaults at 100 (placeholder)
    type coeffArr is array(0..highestExp) of integer;

    type node;
    type list is access node;

    type node is record
        poly : coeffArr;
        next : list;
    end record;

    cli_choice : integer;
    head : list;

----------------------------- Subprograms -----------------------------

    procedure buildList(head : in out list; polyArr : in coeffArr);
    procedure readPOLY; -- Reads and stores polynomials given by user input
    procedure writePOLY(head : in list);
    --  function getCoefficients return coeffsArr;

-----------------------------------------------------------------------

    procedure buildList(head : in out list; polyArr : in coeffArr) is
        newNode : list;
    begin
        put_line(integer'image(polyArr'length));
        newNode := new node'(poly=>polyArr, next=>null);
        newNode.next := head;
        head := newNode;
    end buildList;

-----------------------------------------------------------------------

    procedure readPOLY is
        coeffInput, coeffIndex : integer;
        coeffStr : unbounded_string;
        coeffs : coeffArr;
    begin
        put_line("Enter a polynomial:");
        put("   Highest exponent: ");
        get(highestExp);
        put_line(integer'image(highestExp));
    
        --  declare
        --      coeffs : coeffArr(0..highestExp);
        --  begin

        coeffIndex := 0;
        for i in reverse 0..highestExp loop
            put("   Coefficient for x^" & integer'image(i) & ": ");
            get(coeffInput);
            --  put_line(integer'image(i));
            --  put_line(integer'image(coeffInput));
            put_line(integer'image(coeffIndex));
            coeffs(coeffIndex) := coeffInput;
            coeffIndex := coeffIndex + 1;
        end loop;

        for i in 0..coeffIndex-1 loop
            put_line(integer'image(coeffs(i)));
        end loop;
        buildList(head, coeffs);
        writePOLY(head);
        --  end;
    end readPOLY;

-----------------------------------------------------------------------

    procedure writePOLY(head : in list) is
        scanPtr : list;
    begin
        scanPtr := head;
        loop
            exit when scanPtr = null;
            for i in 0..highestExp loop
                put(scanPtr.poly(i));
                put("x^" & integer'image(i) & " + ");
            end loop;
            --  put(scanPtr.poly);
            --  new_line;
            scanPtr := scanPtr.next;
            put_line(", ");
        end loop;
    end writePOLY;

begin

    put_line("POLYNOMIAL Arithmetic CLI");
    put_line("1. Input a polynomial" & NL &
             "2. Print out a polynomial" & NL &
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
            end if;
            exit when cli_choice = 7;
        end if;
    end loop;

    --  put_line(integer'image(result'length));
    --  for e of result loop
    --      put(integer'image(e) & " ");
    --  end loop;

end poly;