with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Gnat.IO; -- used when printing text before user input (on same line)
with Ada.Strings.unbounded; use Ada.Strings.unbounded;
with Ada.Strings.unbounded.text_io; use Ada.Strings.unbounded.text_io;

package body polylink is

------------------------------ Variables ------------------------------



----------------------------- Subprograms -----------------------------



-----------------------------------------------------------------------

    procedure buildList(head : in out list; polyArr : in coeffArr; highestE : in integer) is
        newNode : list;
    begin
        put_line(integer'image(polyArr'length));
        newNode := new node'(poly=>polyArr, exp=>highestE, next=>null);
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
        --  put_line(integer'image(highestExp));

        coeffIndex := 0;
        for i in reverse 0..highestExp loop
            put("   Coefficient for x^" & integer'image(i) & ": ");
            get(coeffInput);
            coeffs(coeffIndex) := coeffInput;
            coeffIndex := coeffIndex + 1;
        end loop;

        --  for i in 0..coeffIndex-1 loop
        --      put_line(integer'image(coeffs(i)));
        --  end loop;
        buildList(head, coeffs, highestExp);
        writePOLY(head);
    end readPOLY;

-----------------------------------------------------------------------

    procedure writePOLY(head : in list) is
        scanPtr : list;
    begin
        scanPtr := head;
        loop
            exit when scanPtr = null;
            --  put(scanPtr.exp);
            for i in 0..scanPtr.exp loop
                put(scanPtr.poly(i));
                put("x^" & integer'image(i) & " + "); -- TODO: needs formatting
            end loop;
            scanPtr := scanPtr.next;
            put_line(", ");
        end loop;
    end writePOLY;

-----------------------------------------------------------------------

end polylink;