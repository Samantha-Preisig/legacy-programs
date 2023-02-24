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

    type node;
    type list is access node;

    type node is record
        poly : unbounded_string;
        next : list;
    end record;

    aPoly : unbounded_string;
    head : list;

----------------------------- Subprograms -----------------------------

    procedure buildList(head : in out list; aPoly : in unbounded_string);
    procedure readPOLY; -- Reads and stores polynomials given by user input

-----------------------------------------------------------------------

    procedure buildList(head : in out list; aPoly : in unbounded_string) is
        newNode : list;
    begin
        newNode := new node'(poly=>aPoly, next=>null);
        newNode.next := head;
        head := newNode;
    end buildList;

-----------------------------------------------------------------------

    procedure readPOLY is
        input_str : string(1..80);
        length : integer;
    begin
        gnat.io.put("Choice? ");
        get_line(input_str, length);
        put_line(input_str(input_str'first .. length));

        if input_str = "hi" then
            put_line("1 was chosen");
        end if;
    end readPOLY;

-----------------------------------------------------------------------

    procedure writePOLY(head : in list) is
        scanPtr : list;
    begin
        scanPtr := head;
        loop
            exit when scanPtr = null;
            put(scanPtr.poly);
            scanPtr := scanPtr.next;
            put(" ");
        end loop;
    end writePOLY;

begin

    put_line("POLYNOMIAL Arithmetic");
    put_line("1. Input a polynomial" & NL & "2. ...");
    readPOLY;

    loop
        put("> ");
        get_line(aPoly);
        exit when aPoly = "q";
        buildList(head, aPoly);
    end loop;

    put_line("The list as read: ");
    writePOLY(head);
    new_line;

end poly;