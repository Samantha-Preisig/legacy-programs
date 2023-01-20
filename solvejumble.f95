program test
    
    use lexicon
    implicit none

    integer :: result ! For factorial example - delete after use
    character, dimension(1) :: inputWord

    call inputJumble()
    call factorial(5, result)
    print *, result

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
        integer, intent(in) :: left, right ! positions (left = current step, right = last letter)
        ! integer, intent(out) :: anagrams
        ! character, dimension(1) :: swappedWord
        integer :: i, j

        ! integer :: temp

        if(left == right) then
            print *, "anagram: ", word(1:right)
        else
            do i = left, right
                call swap(word, left, i)
                ! do j = 1, right
                !     print*, "shifted --> word(j) = ", word(j)
                ! end do
                call generateAnagram(word, left+1, right)
                call swap(word, left, i)
                ! print *, word
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
    end subroutine

end program test