-- Package specification for polylink containing subprogram declarations
-- Authour: Samantha Preisig
-- Date: Mar 3, 2023

package polylink is

------------------------------ Variables ------------------------------
    
    highestExp : integer := 100; -- highestExp defaults at 100 (placeholder)
    type coeffArr is array(0..highestExp) of integer;

    type node;
    type list is access node;

    type node is record
        id: integer; -- in order to index/identify nodes in linked list
        poly : coeffArr; -- coefficient array of integers (index representing degree)
        exp: integer; -- degree of the given node
        next : list; -- pointer to next node
    end record;

    head : list;

----------------------------- Subprograms -----------------------------

    procedure buildList(head : in out list; polyArr : in coeffArr; highestE : in integer);
    procedure readPOLY;
    procedure writePOLY(head : in list; polyId : in integer);
    function getListSize(head : in list) return integer;
    function getAPoly(head : in list; polyId : in integer) return list;

end polylink;