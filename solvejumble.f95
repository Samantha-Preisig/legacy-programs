! for lexicon.mod: gfortran -c lexicon.f95
! gfortran -Wall solvejumble.f95 lexicon.f95

program solvejumble
    
    use lexicon
    implicit none

    character, dimension(1) :: inputWord
    integer :: lines, i
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

    ! do i = 1, lines
    !     print *, dictionary%list(i)%word
    ! end do

    call inputJumble()

    contains

        subroutine inputJumble()

            integer :: numWords, i, inputLen, j
            character(len=maxSize) :: input

            print *, "How many jumbled words?"
            read (*,*) numWords

            allocate(jumbledWords%list(numWords))

            print *, "Enter the ", numWords, " jumbled words:"
            do i = 1, numWords
                print *, "> "
                read (*,*) input
                inputLen = len(trim(input))
                allocate(character(len=inputLen) :: jumbledWords%list(i)%word)
                jumbledWords%list(i)%word = trim(input)

                do j = 1, inputLen
                    inputWord(j) = achar(iachar(input(j:j)))
                end do

                call generateAnagram(inputWord, 1, inputLen)
            end do

        end subroutine inputJumble

        recursive subroutine generateAnagram(word, left, right)
            character, dimension(1), intent(inout) :: word
            integer, intent(in) :: left, right
            integer :: i

            if(left == right) then
                print *, "anagram: ", word(1:right)
            else
                do i = left, right
                    call swap(word, left, i)
                    call generateAnagram(word, left+1, right)
                    call swap(word, left, i)
                end do
            end if

        end subroutine generateAnagram
        
        ! word: the current input (jumbled) word to create anagrams from
        ! wordLen: length of word
        ! i, j: positions to swap
        subroutine swap(word, i, j)
            character, dimension(1), intent(inout) :: word
            integer, intent(in) :: i, j
            character :: temp

            temp = word(i)
            word(i) = word(j)
            word(j) = temp

        end subroutine swap

        subroutine findAnagram()
            ! TODO
        end subroutine findAnagram

end program solvejumble