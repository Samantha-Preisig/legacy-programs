with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body polylink is

------------------------------ Variables ------------------------------

    idCounter : integer := 0;

----------------------------- Subprograms -----------------------------

    function getListSize(head : in list) return integer is
        scanPtr : list;
        counter : integer := 0;
    begin
        scanPtr := head;
        loop
            exit when scanPtr = null;
            scanPtr := scanPtr.next;
            counter := counter + 1;
        end loop;
    
        return counter;
    end getListSize;

-----------------------------------------------------------------------

    procedure buildList(head : in out list; polyArr : in coeffArr; highestE : in integer) is
        newNode : list;
    begin
        newNode := new node'(id=>idCounter, poly=>polyArr, exp=>highestE, next=>null);
        idCounter := idCounter + 1;
        newNode.next := head;
        head := newNode;
    end buildList;

-----------------------------------------------------------------------

    procedure readPOLY is
        coeffInput, coeffIndex : integer;
        coeffs : coeffArr;
    begin
        put_line("Enter a polynomial:");
        put("   Highest exponent: ");
        get(highestExp);

        coeffIndex := 0;
        for i in 0..highestExp loop -- for i in reverse 0..highestExp loop
            put("   Coefficient for x^" & integer'image(i) & ": ");
            get(coeffInput);
            coeffs(coeffIndex) := coeffInput;
            coeffIndex := coeffIndex + 1;
        end loop;
        
        buildList(head, coeffs, highestExp);
        writePOLY(head, idCounter);
    end readPOLY;

-----------------------------------------------------------------------

    procedure writePOLY(head : in list; polyId : in integer) is
        scanPtr : list;
        j : integer := 0;
    begin
        scanPtr := head;
        loop
            exit when scanPtr = null;
            if(scanPtr.id = polyId) then
                --  for i in reverse 1..scanPtr.exp loop
                --      put(integer'image(scanPtr.poly(j)) & "x^" & integer'image(i) & " + ");
                --      j := j + 1;
                --  end loop;
                --  put_line(integer'image(scanPtr.poly(j)));

                for i in reverse 1..scanPtr.exp loop
                    put(integer'image(scanPtr.poly(i)) & "x^" & integer'image(i) & " + ");
                    j := j + 1;
                end loop;
                put_line(integer'image(scanPtr.poly(0)));
            end if;
            scanPtr := scanPtr.next;
        end loop;
    end writePOLY;

-----------------------------------------------------------------------

    function getAPoly(head : in list; polyId : in integer) return list is
        aPoly, scanPtr : list;
        listSize : integer;    
    begin

        listSize := getListSize(head);
        scanPtr := head;
        aPoly := null;
        loop
            exit when scanPtr = null;
            if(scanPtr.id = polyId) then
                aPoly := scanPtr;
                return aPoly;
            end if;
            scanPtr := scanPtr.next;
        end loop;
        return aPoly;

    end getAPoly;

-----------------------------------------------------------------------

end polylink;