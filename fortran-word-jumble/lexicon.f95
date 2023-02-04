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

            integer :: i, eof
            ! integer, intent(out) :: numLines
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

        subroutine findLex(anagram, anagramLen, ret)

            character, dimension(1), intent(in) :: anagram
            integer, intent(in) :: anagramLen
            ! character(len=anagramLen), intent(in) :: anagram
            logical, intent(out) :: ret
            integer :: i, j, matched=0
            character, dimension(1) :: dict
            character :: dict_letter
            ! anagram_rank = anagram(1:anagramLen)
            ! print *, anagram_rank
            ! print *, "anagramLen = ", anagramLen
            logical :: found = .false.


            do i = 1, lines
                matched = 0
                ! print *, "len of dic word: ", len(dictionary%list(i)%word)
                if(len(dictionary%list(i)%word) == anagramLen) then
                    do j = 1, len(dictionary%list(i)%word)
                        ! print *, "here in loop"
                        dict_letter = achar(iachar(dictionary%list(i)%word(j:j)))
                        ! print *, "dict: ", dict_letter
                        ! print *, "anagram: ", anagram(j)
                        if(dict_letter == anagram(j)) then
                            matched = matched + 1
                            ! print *, "matched: ", matched

                            if(anagramLen == matched .and. j == anagramLen) then
                                print *, dictionary%list(i)%word
                            end if
                        end if
                        if(matched == anagramLen .and. j == anagramLen) then ! exit
                            ! print *, "FOUND!!!! ", dictionary%list(i)%word
                            found = .true.
                        end if
                        ! if(matched == anagramLen .and. j == anagramLen) exit
                        if(found) exit
                    end do
                end if
            end do

            if(found) then
                ret = .true.
            else
                ret = .false.
            end if

        end subroutine findLex

        subroutine split(word, wordLen)

            integer, intent(in) :: wordLen
            character(len=wordLen), intent(in) :: word

        end subroutine

end module lexicon