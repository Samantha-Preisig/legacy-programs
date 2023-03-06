-- Package body for polymath containing subprogram definitions
-- Authour: Samantha Preisig
-- Date: Mar 3, 2023

--  with polylink; use polylink;

package body polymath is

----------------------------- Subprograms -----------------------------

    -- Adds two polynomials together (poly1 + poly2)
    function addpoly(poly1 : in polylink.list; poly2 : in polylink.list) return polylink.coeffArr is

        retArr : polylink.coeffArr;
        largestLen, index : integer;

    begin

        -- Finding the polynomial with the highest degree: all terms above the lower degree value will be
        -- stored in retArr (no like-terms beyond the degree of the lower-degree polynomial)
        if(poly1.exp < poly2.exp) then
            largestLen := poly2.exp;
        elsif(poly1.exp > poly1.exp) then
            largestLen := poly1.exp;
        else
            largestLen := poly1.exp;
        end if;

        -- Building retArr from largest index (largest exponent) to 0 to replicate the polynomial
        -- arrays in linked list
        index := largestLen; -- retArr's index
        for i in 0..largestLen loop
            if(poly1.poly(i) = 0) then -- no x^i term in poly1
                retArr(index) := poly2.poly(i);
            elsif(poly2.poly(i) = 0) then -- no x^i term in poly2
                retArr(index) := poly1.poly(i);
            else
                retArr(index) := poly1.poly(i) + poly2.poly(i);
            end if;
            index := index - 1;
        end loop;
        
        return retArr;

    end addpoly;

-----------------------------------------------------------------------

    -- Subtracts one polynomial from another (poly1 - poly2)
    function subpoly(poly1 : in polylink.list; poly2 : in polylink.list) return polylink.coeffArr is

        retArr : polylink.coeffArr;
        largestLen, index : integer;

    begin

        -- Finding the polynomial with the highest degree: all terms above the lower degree value will be
        -- stored in retArr (no like-terms beyond the degree of the lower-degree polynomial)
        if(poly1.exp < poly2.exp) then
            largestLen := poly2.exp;
        elsif(poly1.exp > poly1.exp) then
            largestLen := poly1.exp;
        else
            largestLen := poly1.exp;
        end if;

        -- Building retArr from largest index (largest exponent) to 0 to replicate the polynomial
        -- arrays in linked list
        index := largestLen; -- retArr's index
        for i in 0..largestLen loop
            if(poly1.poly(i) = 0) then -- no x^i term in poly1
                -- poly2 is being subtracted from poly1, therefore it's negative (if there is no like-term)
                retArr(index) := poly2.poly(i)*(-1);
            elsif(poly2.poly(i) = 0) then -- no x^i term in poly2
                retArr(index) := poly1.poly(i);
            else
                retArr(index) := poly1.poly(i) - poly2.poly(i);
            end if;
            index := index - 1;
        end loop;
        
        return retArr;

    end subpoly;

-----------------------------------------------------------------------

    -- Multiplies one polynomial by another (poly1 * poly2)
    function multpoly(poly1 : in polylink.list; poly2 : in polylink.list) return polylink.coeffArr is

        retArr : polylink.coeffArr;
        index : integer := 0;

    begin

        -- Initializing retArr before populating (to ensure predictability on line 135)
        for i in 0..poly1.exp loop
            for j in 0..poly2.exp loop
                index := i + j;
                retArr(index) := 0;
            end loop;
        end loop;

        for i in 0..poly1.exp loop
            for j in 0..poly2.exp loop
                index := i + j; -- Power multiplication rule
                retArr(index) := retArr(index) + (poly1.poly(i) * poly2.poly(j));
            end loop;
        end loop;
        
        return retArr;

    end multpoly;

-----------------------------------------------------------------------

    -- Evaluates a polynomial with a given value x
    function evalpoly(poly1 : in polylink.list; x : in integer) return integer is

        ret : integer := 0;

    begin

        for i in 1..poly1.exp loop
            ret := ret + poly1.poly(i)*(x**i);
        end loop;
        ret := ret + poly1.poly(0); -- add constant
        
        return ret;

    end evalpoly;

end polymath;