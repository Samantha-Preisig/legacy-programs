! Word Jumble
! Authour: Samantha Preisig
! Date: Feb 5, 2023

program solvejumble
    
    use lexicon
    implicit none

    character, dimension(1) :: inputWord
    integer :: ierror, solvedIndex
    logical :: foundWord

    call buildLexicon() ! Build dictionary
    call inputJumble() ! Read jumbled words

    contains

        ! Subroutine inputJumble() gathers user input for the set of jumbles
        subroutine inputJumble()

            integer :: i, j, numWords, inputLen, indexCounter=1, numPos
            ! jumbleInput1 refers to user interface #1 and jumbleInput2 refers to user interface #2 (see README for distinction)
            character(len=maxSize) :: input, jumblePuzzleInput1, jumblePuzzleInput2
            character, dimension(maxSize) :: circledLetters
            integer, dimension(maxSize) :: circledPos

            ! Prompts user for the number of jumbled words they want to provide
            ! Error handling: input must be an integer
            do
                write (*, '(A)', advance='no') "How many jumbled words? "
                read (*,'(I10)',iostat=ierror) numWords

                if(ierror == 0) then
                    exit
                end if
                write (*, '(A)') "Error: please enter an integer"
            end do

            ! Allocating dynamic arrays which will store the jumbled and unjumbled words (respectively)
            allocate(jumbledWords%list(numWords))
            allocate(solvedWords%list(numWords+1)) ! numWords + 1 for the possible solved circled word jumble

            ! Asking user to decide between two interfaces (outlined in README)
            ! Error handling: user must answer with Y/y or N/n
            do while(.not. ((jumblePuzzleInput1 == "Y" .or. jumblePuzzleInput1 == "y") &
                     .or. (jumblePuzzleInput1 == "N" .or. jumblePuzzleInput1 == "n")))
                write (*, '(/,A)', advance='no') "Enter circled positions with each word [Y/y or N/n]? "
                read (*,*) jumblePuzzleInput1
            end do

            ! Gathering each jumbled word from user and storing into dynamic array called jumbledWords
            if(jumblePuzzleInput1 == "Y" .or. jumblePuzzleInput1 == "y") then
                write (*, '(/,A)') "Enter the jumbled word:"
            else
                write (*, '(/,A)', advance='no') "Enter the "
                write (*, '(I2)', advance='no') numWords
                write (*, '(A)') " jumbled words:"
            end if
            do i = 1, numWords
                write (*, '(A)', advance='no') "> "
                read (*,*) input
                inputLen = len(trim(input))
                
                ! Limitation: jumbled word length is capped at 8
                if(inputLen > 8) then
                    write (*, '(A)') "Error: only jumbled words of length 8 or less are accepted."
                    return
                end if
                
                ! Dynamically allocating ragged arrays and storing the given jumbled word
                allocate(character(len=inputLen) :: jumbledWords%list(i)%word)
                allocate(character(len=inputLen) :: solvedWords%list(i)%word)
                jumbledWords%list(i)%word = trim(input)
                solvedWords%list(i)%word = trim(input) ! Save jumbled word in solvedWords array incase there is no solution

                do j = 1, inputLen
                    inputWord(j) = achar(iachar(input(j:j)))
                end do

                ! If first user interface was chosen, each jumbled word is solved individually to obtain the circled
                ! letters given the position
                ! Circled positions are given based on the solved (unjumbled) word
                if(jumblePuzzleInput1 == "Y" .or. jumblePuzzleInput1 == "y") then
                    write (*, '(A)', advance='no') "Enter the circled positions: "
                    read (*,*) input

                    do j = 1, len(trim(input))
                        ! iachar adds 48 to the integer character, so we must subtract it to get the intended number
                        circledPos(j) = iachar(input(j:j))-48
                    end do
                    numPos = j-1 ! number of positions circled for the current jumbled word

                    ! Solve the current jumbled word by calling generateAnagram()
                    solvedIndex = i
                    foundWord = .false.
                    call generateAnagram(jumbledWords%list(i)%word, 1, len(jumbledWords%list(i)%word))

                    do j = 1, numPos
                        ! Obtaining the letters at each circled position using the solved word
                        circledLetters(indexCounter) = achar(iachar(solvedWords%list(i)%word(circledPos(j):circledPos(j))))
                        indexCounter = indexCounter + 1
                    end do
                end if
            end do

            ! If the first interface was not chosen, solve all jumbled words as a group
            if(jumblePuzzleInput1 == "N" .or. jumblePuzzleInput1 == "n") then
                do i = 1, numWords
                    solvedIndex = i
                    foundWord = .false.
                    call generateAnagram(jumbledWords%list(i)%word, 1, len(jumbledWords%list(i)%word))
                end do
            end if
            
            ! Print all solved jumbles
            write (*, '(/,A)') "The following jumbles have been solved:"
            do i = 1, numWords
                if(jumbledWords%list(i)%word == solvedWords%list(i)%word) then
                    write (*, '(4g0)') jumbledWords%list(i)%word, achar(9), "could not solve" ! achar(9) => tab character
                else
                    write (*, '(4g0)') jumbledWords%list(i)%word, achar(9), solvedWords%list(i)%word ! achar(9) => tab character
                end if
            end do

            ! If the first user interface was not chosen, ask the user (again) if they want to solve the circled word puzzle
            ! Error handling: user must answer with Y/y or N/n
            if(jumblePuzzleInput1 == "N" .or. jumblePuzzleInput1 == "n") then
                do while(.not. ((jumblePuzzleInput2 == "Y" .or. jumblePuzzleInput2 == "y") &
                        .or. (jumblePuzzleInput2 == "N" .or. jumblePuzzleInput2 == "n")))
                    write (*, '(/,A)', advance='no') "Solve word jumble puzzle [Y/y or N/n]? "
                    read (*,*) jumblePuzzleInput2
                end do
            end if

            ! If user wants to solve circled word puzzle, each solved word is given to prompt the user to provide letter(s)
            ! from the solved word
            ! Limitation: if the user is providing multiple letters, there must be no spaces between the letters. Otherwise,
            ! letters beyond the first space will be ignored
            if(jumblePuzzleInput2 == "Y" .or. jumblePuzzleInput2 == "y") then
                write(*,'(/,A)') "Select circled letters from word puzzle:"
                do i = 1, numWords
                    write (*, '(A)', advance='no') solvedWords%list(i)%word
                    write (*, '(A)', advance='no') ": "
                    read (*, *) input
                    do j = 1, len(trim(input))
                        circledLetters(indexCounter) = achar(iachar(input(j:j)))
                        indexCounter = indexCounter + 1
                    end do
                end do
            end if
            
            ! If user participated in circling letters/positions, the letters/positions form a new jumbled word to be
            ! solved by calling generateAnagram() and printing to stdout
            if(jumblePuzzleInput1 == "Y" .or. jumblePuzzleInput1 == "y" &
                .or. jumblePuzzleInput2 == "Y" .or. jumblePuzzleInput2 == "y") then
                write (*, '(A)', advance='no') "Jumbled word: "
                do i = 1, indexCounter-2
                    write (*, '(A)', advance='no') circledLetters(i)
                end do
                write (*, '(A)') circledLetters(i)

                solvedIndex = solvedIndex + 1
                foundWord = .false.
                call generateAnagram(circledLetters(1:indexCounter-1), 1, indexCounter-1) ! Solving circled jumble word
                write (*, '(/,A)', advance='no') "Solved jumble: "
                write (*, '(A)') solvedWords%list(solvedIndex)%word
            end if

        end subroutine inputJumble

        ! Recursive subroutine generateAnagram() recursively generates all permutations/anagrams of a given word
        ! Params:
            ! word: the jumbled word to generate anagrams for
            ! left: the left position of the word (always 1 for this assignment)
            ! right: the right most position of the word (length of word)
        ! While left and right params are not necessary, they are added for the sake of comprehensibility and the fact that
        ! anagrams can be generated given substrings of a jumbled word
        recursive subroutine generateAnagram(word, left, right)

            ! Function arguments
            character, dimension(1), intent(inout) :: word
            integer, intent(in) :: left, right

            ! Function variables
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
        
        ! Subroutine swap() swaps letters in positions i and j within the given word
        ! Params:
            ! word: the current input (jumbled) word to create anagrams from
            ! i, j: positions to swap
        subroutine swap(word, i, j)

            ! Function arguments
            character, dimension(1), intent(inout) :: word
            integer, intent(in) :: i, j

            ! Function variables
            character :: temp

            temp = word(i)
            word(i) = word(j)
            word(j) = temp

        end subroutine swap

        ! Subroutine findAnagram() calls findLex to search the dictionary with a given anagram
        ! Returns true if the anagram appears in the dictionary, false otherwise
        ! Params:
            ! word: the current anagram
            ! wordLen: length of anagram
            ! ret: return value of findLex (T/F)
        subroutine findAnagram(word, wordLen, ret)

            ! Function arguments
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