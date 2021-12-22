# # inits board variable

board_size = 3
board <- matrix(1:9,nrow = board_size)

for (i in 1:board_size){
  for (j in 1:board_size){
    board[i,j] <- ''
  }
}

# handles the game
Welcome <- 'Welcome to Tic Tac Toe, please select side'
Welcome

side_c <- 'not made'
# handles side selection
while (side_c == 'not made'){
  if (interactive()) {
  con <- stdin()
  } else {
  con <- "stdin"
  }
  cat("X or O? ")
  side <- readLines(con = con, n = 1)

  
#   makes sure input is valid
  if (side == 'X'){
    turn <- 'player'
    side_c <- 'made'
  } else if (side == '0'){
    turn <- 'comp'
    side_c <- 'made'
  } else {
    print('Please select either a capitol X or zero i.e.: X or 0')
  }
}

# initiates the player side variable
if (side == 'X'){
    comp_side <- '0'
  } else {
    comp_side <- 'X'
  }


active <- TRUE

while (active == TRUE){
  # handles player's turn
    while (turn == 'player'){
      print("Your turn")
        print('Choose coordinates of your move')

        # handles row selection
        r_c <- 'not made'
        while(r_c == 'not made') {if (interactive()) {
            con <- stdin()
        } else {
            con <- "stdin"
        }
        cat("Please Select Row ")
        row <- readLines(con = con, n=1)
            if (row == 1){
                print('Thank You')
                r_c <- 'made'
                } else if (row == 2) {
                print('Thank You')
                r_c <- 'made'
                } else if (row == 3){
                print('Thank You')
                r_c <- 'made'
                } else {
                # deals w/ non-standard input
                row <- " "
                print('Invalid input: please select different option')
                } 

        }

    
    # handles column selection
    c_c <- 'not made'
    while (c_c == 'not made')  {if (interactive()) {
        con <- stdin()
        } else {
        con <- "stdin"
        }
      cat("Please Select Column ")
      col <- readLines(con = con, n=1)
        if (col == 1){
        print('Thank You')

        c_c <- 'made'
        
        } else if (col == 2) {
        print('Thank You')

        c_c <- 'made'
        
        } else if (col == 3){
        print('Thank You')

        c_c <- 'made'
        
        } else {
        # handles non-standard input
        col <- " "
        print('Invalid input: please select different option')
        } 

      }
      
      if (board[as.integer(row),as.integer(col)] == ''){
        board[as.integer(row),as.integer(col)] <- side
        print(board)
        turn <- 'comp'
      # sprintf('You have selected the position: %s,%s ',row,col)
      } else {
        print("Invalid position: please choose another")
        c_c <- 'not made'
        r_c <- 'not made'
        print(board)
      }

    }

    # test if game is complete
    if (board[1,1] == board[2,2] && board[2,2] == board[3,3] && board[3,3] != '' ){
    print('Congratulations, you win!!')
    active <- FALSE
    turn <- 'N/A'
    } else if (board[3,1] == board[2,2] && board[2,2] == board[1,3] && board[2,2] != ''){
    print('Congratulations, you win!!')
    active <- FALSE
    turn <- 'N/A'
    } else{
        for (i in 1:board_size){
          if (board[i,1] == board[i,2] && board[i,2] == board[i,3] && board[i,3] != ''){
            print('Congratulations, you win!!')
            active <- FALSE
            turn <- 'N/A'
          } else if (board[1,i] == board[2,i] && board[2,i] == board[3,i] && board[3,i] != ''){
            print('Congratulations, you win!!')
            active <- FALSE
            turn <- 'N/A'
          }
          
        }
  }

  # handles computer moves
    while (turn == 'comp'){
      print("Computer's turn")
        if (board[2,2] == ''){
          board[2,2] <- comp_side
          turn <- 'player'
        } else {
          coordinates <- c(1,1,1,1,1,2,2,2,3,3,3,3,3)
          x_ind <- sample(1:length(coordinates),1)
          y_ind <- sample(1:length(coordinates),1)
          if (board[coordinates[x_ind],coordinates[y_ind]] == ''){
            board[coordinates[x_ind],coordinates[y_ind]] <- comp_side
            turn <- 'player'
          }
        }
      print(board)
    


  # test if game is complete
  if (board[1,1] == board[2,2] && board[2,2] == board[3,3] && board[3,3] != '' ){
    print('Computer wins!!')
    active <- FALSE
    } else if (board[3,1] == board[2,2] && board[2,2] == board[1,3] && board[2,2] != ''){
    print('Computer wins!!')
    active <- FALSE
    } else{
        for (i in 1:board_size){
          if (board[i,1] == board[i,2] && board[i,2] == board[i,3] && board[i,3] != ''){
            print('Computer wins!!')
            active <- FALSE
          } else if (board[1,i] == board[2,i] && board[2,i] == board[3,i] && board[3,i] != ''){
            print('Computer wins!!')
            active <- FALSE
          }
          
        }
    }
  }

  move_cnt <- 0
  for (i in 1:board_size){
    for (j in 1:board_size){
      if (board[i,j] != ''){
        move_cnt <- move_cnt +1
      }
    }
  }
  
  if( move_cnt == 9){
    print('A stalemate has been reached')
    active <- FALSE
    turn <- 'N/A'
  }
}
