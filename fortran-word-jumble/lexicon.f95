! Lexicon Module for Word Jumble
! Authour: Samantha Preisig
! Date: Feb 5, 2023

module lexicon
    
    implicit none

    ! Types in Fortran are equivalent to structs in C
    ! Use % to "dereference"
    type :: raggedStr
        character(len=:), allocatable :: word ! word is a string of deferred length, i.e. allocatable
    end type raggedStr

    ! Dynamic array of ragged strings
    type :: strList
        type (raggedStr), dimension(:), allocatable :: list ! list is a deferred length "array" of type raggedStr
    end type strList

    type(strList) :: dictionary
    type(strList) :: jumbledWords ! Stores all jumbled words given by user
    type(strList) :: solvedWords ! Store all solved words
    ! Assumptions:
        ! No word is greater than 20 characters (hence, maxSize = 20)
        ! No line in dict2.txt is greater than 256 in length (hence, maxLen = 256)
    integer, parameter :: maxSize=20, maxLen=256, fileUnit = 1000
    character(len=9), parameter :: filename = 'dict2.txt'
    integer :: lines
    logical :: found

    contains

        ! Subroutine buildLexicon() reads dict2.txt, appropriately allocates the dynamic ragged array 'dictionary',
        ! and stores each word (line) from dict2.txt into dictionary
        subroutine buildLexicon()

            integer :: i
            character(len=maxSize) :: lineInput

            ! Open dictonary file (words.txt) and read/store words:
            open(unit=fileUnit, file=filename)
            lines = findNumLines(fileUnit)
            allocate(dictionary%list(lines))
            do i = 1, lines
                read(fileUnit, '(A)') lineInput
                allocate(character(len=len(trim(lineInput))) :: dictionary%list(i)%word)
                dictionary%list(i)%word = trim(lineInput)
            end do
            close(fileUnit)

        end subroutine buildLexicon

        ! Integer function findNumLines() is a helper function to buildLexicon() which returns the number
        ! of lines in a file given its fileUnit
        integer function findNumLines(f_id) result(numLines)

            ! Function arguments
            integer, intent(in) :: f_id

            integer :: eof
            character(len=1) :: temp

            rewind(f_id) ! rewind to the beginning of the file
            
            numLines = 0
            do
                read(f_id, '(A1)', iostat=eof) temp
                if(eof < 0) exit ! end of file
                numLines = numLines + 1
            end do

            rewind(f_id) ! rewind to the beginning of the file

        end function findNumLines

        ! Subroutine findLex() searches the dictionary for a match with the given anagram
        ! Params:
            ! anagram: the word it's trying to match with the dictionary
            ! anagramLen: the length of the anagram
            ! solvedIndex: the global index for solvedWords which keeps track of what jumbled word is currently being
            ! solved
            ! ret: used as a return value; true if a match is found, false otherwise
        subroutine findLex(anagram, anagramLen, solvedIndex, ret)

            ! Function arguments
            character, dimension(1), intent(in) :: anagram
            integer, intent(in) :: anagramLen, solvedIndex
            logical, intent(inout) :: ret

            ! Function variables
            integer :: i, j, matched=0 ! matched is a counter
            character :: dict_letter
            logical :: found

            found = ret
            do i = 1, lines
                matched = 0
                if(len(dictionary%list(i)%word) == anagramLen) then ! Only search words of the same length
                    
                    do j = 1, len(dictionary%list(i)%word)
                        dict_letter = achar(iachar(dictionary%list(i)%word(j:j)))
                        
                        if(dict_letter == anagram(j)) then
                            matched = matched + 1
                        end if

                        ! Solved word found, store in solvedWord ragged array and set ret = T
                        if(matched == anagramLen .and. j == anagramLen) then
                            solvedWords%list(solvedIndex)%word = dictionary%list(i)%word
                            found = .true.
                        end if

                        if(found) then
                            ret = .true.
                            return
                        end if

                    end do
                end if
            end do
            ret = .false.

        end subroutine findLex

end module lexicon