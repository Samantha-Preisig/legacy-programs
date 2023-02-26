package polylink is

------------------------------ Variables ------------------------------
    
    highestExp : integer := 100; -- highestExp defaults at 100 (placeholder)
    type coeffArr is array(0..highestExp) of integer;

    type node;
    type list is access node;

    type node is record
        poly : coeffArr;
        exp: integer; -- holds highest exponent for a given node/polynomial
        next : list;
    end record;

    head : list;

----------------------------- Subprograms -----------------------------

    procedure buildList(head : in out list; polyArr : in coeffArr; highestE : in integer);
    procedure readPOLY; -- Reads and stores polynomials given by user input
    procedure writePOLY(head : in list);

end polylink;