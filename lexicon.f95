module lexicon
    implicit none

    ! Types in Fortran are equivalent to structs in C
    ! Use % to "dereference"
    type :: raggedStr
        character(len=:), allocatable :: word ! word is a string of deferred length, i.e. allocatable
    end type raggedStr

    type :: strList
        type (raggedStr), dimension(:), allocatable :: list ! list is a deferred length "array" of type raggedStr
    end type strList

    type(strList) :: jumbledWords, anagrams
    integer, parameter :: maxSize=20

end module lexicon