with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with polylink; use polylink;

package body polymath is

------------------------------ Variables ------------------------------



----------------------------- Subprograms -----------------------------

    function addpoly(poly1 : in polylink.list; poly2 : in polylink.list) return polylink.coeffArr is

        retArr : polylink.coeffArr;
        diffLens, smallestLen, largestLen, index, j : integer;

    begin

        diffLens := 0; -- Both polynomials are of the same degree
        -- Finding information about each polynomial:
        --  - the polynomial with the lowest degree: all terms less than or equal to this degree will
        -- be added together
        --  - the polynomial with the highest degree: all terms above the lower degree value will be
        -- stored in retArr (no like-terms beyond the degree of the lower-degree polynomial)
        if(poly1.exp < poly2.exp) then
            smallestLen := poly1.exp;
            largestLen := poly2.exp;
            diffLens := 1; -- Both polynomials are of different degrees
        elsif(poly1.exp > poly1.exp) then
            smallestLen := poly2.exp;
            largestLen := poly1.exp;
            diffLens := 1; -- Both polynomials are of different degrees
        else
            smallestLen := poly1.exp;
        end if;

        index := 0; -- retArr's index
        -- Like terms only exist with terms less than or equal to the lower-degree polynomial, therefore
        -- addition is only performed on this subset of values
        for i in 0..smallestLen loop
            retArr(index) := poly1.poly(i) + poly2.poly(i);
            index := index + 1;
        end loop;

        j := index; -- j bookmarks retArr's current index after performing addition
        if(diffLens = 1) then
            -- Storing the terms of higher degree in retArr
            for i in j..largestLen loop
                if(poly1.exp = largestLen) then
                    retArr(index) := poly1.poly(i);
                else
                    retArr(index) := poly2.poly(i);
                end if;
                index := index + 1;
            end loop;
        end if;
        
        return retArr;

    end addpoly;

-----------------------------------------------------------------------

    function subpoly(poly1 : in polylink.list; poly2 : in polylink.list) return polylink.coeffArr is

        retArr : polylink.coeffArr;
        diffLens, smallestLen, largestLen, index, j : integer;

    begin

        diffLens := 0; -- Both polynomials are of the same degree
        -- Finding information about each polynomial:
        --  - the polynomial with the lowest degree: all terms less than or equal to this degree will
        -- be added together
        --  - the polynomial with the highest degree: all terms above the lower degree value will be
        -- stored in retArr (no like-terms beyond the degree of the lower-degree polynomial)
        if(poly1.exp < poly2.exp) then
            smallestLen := poly1.exp;
            largestLen := poly2.exp;
            diffLens := 1; -- Both polynomials are of different degrees
        elsif(poly1.exp > poly1.exp) then
            smallestLen := poly2.exp;
            largestLen := poly1.exp;
            diffLens := 1; -- Both polynomials are of different degrees
        else
            smallestLen := poly1.exp;
        end if;

        index := 0; -- retArr's index
        -- Like terms only exist with terms less than or equal to the lower-degree polynomial, therefore
        -- addition is only performed on this subset of values
        for i in 0..smallestLen loop
            retArr(index) := poly1.poly(i) - poly2.poly(i);
            index := index + 1;
        end loop;

        j := index; -- j bookmarks retArr's current index after performing addition
        if(diffLens = 1) then
            -- Storing the terms of higher degree in retArr
            for i in j..largestLen loop
                if(poly1.exp = largestLen) then
                    retArr(index) := poly1.poly(i);
                else
                    retArr(index) := poly2.poly(i);
                end if;
                index := index + 1;
            end loop;
        end if;
        
        return retArr;

    end subpoly;

-----------------------------------------------------------------------

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

begin

    put("stub");

end polymath;