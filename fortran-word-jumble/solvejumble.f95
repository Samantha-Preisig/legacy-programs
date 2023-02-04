! for lexicon.mod: gfortran -c lexicon.f95
! gfortran -Wall solvejumble.f95 lexicon.f95

program solvejumble
    
    use lexicon
    implicit none

    character, dimension(1) :: inputWord

    call buildLexicon()
    call inputJumble()

    contains

        subroutine inputJumble()

            integer :: numWords, i, inputLen, j
            character(len=maxSize) :: input, unjumbled

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

                call generateAnagram(inputWord, 1, inputLen, unjumbled)
                ! print *, "anagram found in dictionary: ", unjumbled(1:len(unjumbled))
                print *, solvedWords%list(1)%word
            end do

        end subroutine inputJumble

        recursive subroutine generateAnagram(word, left, right, unjumbled)

            character, dimension(1), intent(inout) :: word
            integer, intent(in) :: left, right
            character, dimension(1), intent(out) :: unjumbled
            integer :: i, j=0
            logical :: anagramInDic=.false.

            if(left == right) then
                call findAnagram(word(1:right), right, anagramInDic)
                if(anagramInDic) then
                    ! print *, "anagram found in dictionary: ", word(1:right)
                    return
                end if
            else
                do i = left, right
                    call swap(word, left, i)
                    call generateAnagram(word, left+1, right, unjumbled)
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

            call findLex(word, wordLen, ret)
            if(ret) then
                ret = .true.
            else
                ret = .false.
            end if

        end subroutine findAnagram

end program solvejumble