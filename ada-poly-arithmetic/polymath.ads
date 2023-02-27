with polylink; use polylink;

package polymath is

------------------------------ Variables ------------------------------

    highestExp : integer := 100; -- highestExp defaults at 100 (placeholder)
    type coeffArr is array(0..highestExp) of integer;

----------------------------- Subprograms -----------------------------

    function addpoly(poly1 : in polylink.list; poly2 : in polylink.list) return polylink.coeffArr;
    function subpoly(poly1 : in polylink.list; poly2 : in polylink.list) return polylink.coeffArr;
    function multpoly(poly1 : in polylink.list; poly2 : in polylink.list) return polylink.coeffArr;

end polymath;