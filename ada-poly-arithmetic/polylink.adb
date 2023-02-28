-- Package body for polylink containing subprogram definitions
-- Authour: Samantha Preisig
-- Date: Mar 3, 2023

with ada.text_io; use ada.text_io;
with ada.integer_text_io; use ada.integer_text_io;

package body polylink is

------------------------------ Variables ------------------------------

    idCounter : integer := 0;

----------------------------- Subprograms -----------------------------

    -- Heavily referenced from "The Craft of Coding" blog post
    -- Blog post URL: https://craftofcoding.wordpress.com/2023/02/23/a-basic-linked-list-of-words-in-ada-i/
    -- Creates new node and adds to linked list
    -- Takes in information to populate node:
    --      - polyArr: array (type coeffArr) holding coefficients
    --      - highestE: integer representing degree (highest power) of polynomial
    procedure buildList(head : in out list; polyArr : in coeffArr; highestE : in integer) is

        newNode : list;

    begin

        newNode := new node'(id=>idCounter, poly=>polyArr, exp=>highestE, next=>null);
        idCounter := idCounter + 1;
        newNode.next := head;
        head := newNode;

    end buildList;

-----------------------------------------------------------------------

    -- Reads user input to gather information for a new polynomial to be added to the
    -- linked list
    -- Calls buildList to add the polynomial to the linked list
    -- The coefficient array (coeffs) is populated with the index referencing the term's power
    -- For example, coeffs(0) contains the constant in the polynomial and coeffs(3) contains the
    -- constant attached to x^3
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

    -- Prints a polynomial
    -- polyId is an index for writePOLY to identify the specific polynomial
    -- being requested to output
    procedure writePOLY(head : in list; polyId : in integer) is

        scanPtr : list;

    begin

        scanPtr := head;
        loop
            exit when scanPtr = null;
            if(scanPtr.id = polyId) then
                for i in reverse 1..scanPtr.exp loop
                    put(integer'image(scanPtr.poly(i)) & "x^" & integer'image(i) & " + ");
                end loop;
                put_line(integer'image(scanPtr.poly(0)));
            end if;
            scanPtr := scanPtr.next;
        end loop;

    end writePOLY;

-----------------------------------------------------------------------

    -- Helper function (mainly used for error handling) to get linked list size
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

    -- Helper function to retrieve the entire node of a given polynomial id (polyId)
    function getAPoly(head : in list; polyId : in integer) return list is

        scanPtr : list;
        listSize : integer;

    begin

        listSize := getListSize(head);
        scanPtr := head;
        loop
            exit when scanPtr = null;
            if(scanPtr.id = polyId) then
                return scanPtr;
            end if;
            scanPtr := scanPtr.next;
        end loop;
        return null;

    end getAPoly;

end polylink;