! for lexicon.mod: gfortran -c lexicon.f95
! gfortran -Wall solvejumble.f95 lexicon.f95

! write(*, "(A)") "I'M PRINTING A STRING"
! write(*, "(I0)") myInt

program solvejumble
    
    use lexicon
    implicit none

    character, dimension(1) :: inputWord
    integer :: solvedIndex
    logical :: foundWord

    call buildLexicon()
    call inputJumble()

    contains

        subroutine inputJumble()

            integer :: numWords, i, inputLen, j
            character(len=maxSize) :: input

            print *, "How many jumbled words?"
            read (*,*) numWords

            allocate(jumbledWords%list(numWords))
            allocate(solvedWords%list(numWords))

            print *, "Enter the ", numWords, " jumbled words:"
            do i = 1, numWords
                print *, "> "
                read (*,*) input
                inputLen = len(trim(input))
                allocate(character(len=inputLen) :: jumbledWords%list(i)%word)
                allocate(character(len=inputLen) :: solvedWords%list(i)%word)
                jumbledWords%list(i)%word = trim(input)

                do j = 1, inputLen
                    inputWord(j) = achar(iachar(input(j:j)))
                end do

                ! call generateAnagram(inputWord, 1, inputLen)
                ! print *, solvedWords%list(1)%word
            end do

            do i = 1, numWords
                solvedIndex = i
                foundWord = .false.
                call generateAnagram(jumbledWords%list(i)%word, 1, len(jumbledWords%list(i)%word))
            end do
            
            print *, "The following jumbles have been solved:"
            do i = 1, numWords
                print *, jumbledWords%list(i)%word, "   ", solvedWords%list(i)%word
            end do

        end subroutine inputJumble

        recursive subroutine generateAnagram(word, left, right)

            character, dimension(1), intent(inout) :: word
            integer, intent(in) :: left, right
            integer :: i
            logical :: anagramInDic=.false.

            if(left == right) then
                if(.not. foundWord) then
                    call findAnagram(word(1:right), right, anagramInDic)
                    if(anagramInDic) then
                        foundWord = .true.
                        return
                    end if
                end if
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

        subroutine findAnagram(word, wordLen, ret)

            character, dimension(1), intent(in) :: word
            integer, intent(in) :: wordLen
            logical, intent(inout) :: ret

            ret = .false.
            call findLex(word, wordLen, solvedIndex, ret)
            if(ret) then
                ret = .true.
            else
                ret = .false.
            end if

        end subroutine findAnagram

end program solvejumble