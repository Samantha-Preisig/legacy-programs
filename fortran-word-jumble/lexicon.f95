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

    type(strList) :: jumbledWords
    type(strList) :: dictionary
    type(strList) :: solvedWords
    integer, parameter :: maxSize=20, maxLen=256
    character(len=9), parameter :: filename = 'dict2.txt'
    integer, parameter :: fileUnit = 1000
    integer :: lines
    logical :: found

    contains

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

        integer function findNumLines(f_id) result(numLines)

            integer :: eof
            integer, intent(in) :: f_id
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

        subroutine findLex(anagram, anagramLen, solvedIndex, ret)

            character, dimension(1), intent(in) :: anagram
            integer, intent(in) :: anagramLen, solvedIndex
            logical, intent(inout) :: ret
            integer :: i, j, matched=0
            character :: dict_letter
            logical :: found

            found = ret
            do i = 1, lines
                matched = 0
                if(len(dictionary%list(i)%word) == anagramLen) then
                    do j = 1, len(dictionary%list(i)%word)
                        dict_letter = achar(iachar(dictionary%list(i)%word(j:j)))
                        if(dict_letter == anagram(j)) then
                            matched = matched + 1
                        end if
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