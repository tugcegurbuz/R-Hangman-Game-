#Import necessary libraries
library(ggplot2)



wordReplacement <- function(target_word, letter, wanted_target_word){
  
  if (regexpr(letter, target_word)[1] > 0) {
    
    target_word <- base::tolower(target_word)
    letter <- base::tolower(letter)
    
    pozicije <- which(strsplit(target_word, "")[[1]]==letter)
    wanted_target_word[pozicije] <- letter
    
    message(paste(wanted_target_word, collapse =  " "))
    
    return(wanted_target_word)
  }
}

drawHead <- function(ori_pos = c(0,0),
                     dia = 1,
                     nof_points = 10,
                     group = 5){
  
  v <- seq(0,2*pi, length.out = nof_points)
  r <- dia/2
  x_head <- ori_pos[1] + r * cos(v)
  y_head <- ori_pos[2] + r * sin(v)
  return(data.frame (x = x_head, y = y_head, group = group))
}

drawHangman <- function(errors) {
    ggplot(levels[which(levels$group <= errors), ], aes(x = x, y = y, group = group)) +
    geom_path(size = 2.5) + 
    theme_void() +
    ggtitle('Hangman Game')
  
}


CheckDuplicate <- function(letter, selection){
  if (length(selection) == 0) {
    message('Zero length; adding...')
    selection <- rbind(selection, izb=letter)  
    selection[,1] <- as.character(selection[,1])
  } else {
    if (grepl(letter,selection) == TRUE) {
      selection[,1] <- as.character(selection[,1])
      message("exists")
    } else {
      selection <- rbind(selection, izb=letter)
      selection[,1] <- as.character(selection[,1])
      message("added...")
    }
  }
  return(selection)
}


#Graph coordinates: level1:horizontal line, level2:vertical line, level3: tinny horizontal line, level4: tinny vertical line, level5:head, level6:body, level7:arms, level8:legs

level1 <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8), 
                     y = c(1, 1, 1, 1, 1, 1, 1, 1), 
                     group = c(1, 1, 1, 1, 1, 1, 1, 1))
level2 <- data.frame(x = c(4, 4, 4, 4, 4), 
                     y = c(1, 2, 3, 4, 5),
                     group = c(2, 2, 2, 2, 2))
level3 <- data.frame(x = c(4, 5, 6), y= c (5, 5, 5), group = c(3, 3, 3))
level4 <- data.frame(x = c(6, 6), y = c(5, 4), group = c(4, 4))
level5 <- drawHead(c(6, 3.5), 1, 10, 5)
level6 <- data.frame(x = c(6, 6), 
                     y =c(3, 1.5), group = c(6, 6))
level7 <- data.frame(x = c(5.5, 6, 6.5), y = c(2, 2.5, 2), group = c(7, 7, 7))
level8 <- data.frame(x = c(5.5, 6, 6.5), y = c(1, 1.5, 1), group = c(8, 8, 8))
levels <- rbind(level1, level2, level3, level4, level5, level6, level7, level8)
rm(level1, level2, level3, level4, level5, level6, level7, level8)



#Variables

#suppressWarnings(rm(errors, selection, letter, goal, goal_n, wanted_target_word, target_word, i, active))
errors = 0
i = 0
selection = data.frame(izb=c(NULL))
goal = NULL
goal_n = data.frame(izb=c(NULL))
active = TRUE

#Hangman Game

StartNewGame <- function(sensitive.flag = TRUE) { # sensitive.flag: TRUE -> capital letters are available. 
  target_word <- readline(prompt = "Word: ")
  
  cat("\f") 
  graphics.off() 
  if (sensitive.flag == FALSE) {
    target_word <- base::tolower(target_word)
  }
  
  wanted_target_word <- replicate(nchar(target_word), '_')
  
  while (active == TRUE) {
    
    if (i == 0) {
      writeLines(paste(wanted_target_word, collapse = " "))
    }
    
    letter <- readline(prompt="Enter Letter: ")
    
    if (nchar(letter)>1) message("Taking first letter")
    letter <- substr(letter, 1, 1)
    
    selection <- CheckDuplicate(letter, selection)
    
    #wanted_target_word
    if (grepl(letter, target_word) == TRUE) {
      
      goal <- rbind(goal, letter)
      wanted_target_word <- wordReplacement(target_word, letter, wanted_target_word)
      
      message(paste("Yay!","Try N:",i+1)) 
      
      if (as.character(paste(base::tolower(wanted_target_word), collapse = "")) == base::tolower(target_word)) {
        active == FALSE
        message("Bravo, win!")
        break
      }
      
    } else {
      goal_n <- CheckDuplicate(letter=letter, selection=goal_n)
      message(paste("Nope!","Try N:",i + 1, "Wrong letters: {",(toString(paste0(goal_n[,1], sep = ","))),"}")) 
      
      #Graph
      errors <- as.integer(nrow(goal_n))
      print(drawHangman(errors = errors))
      
      if(as.integer(errors) == 8){
        active == FALSE
        break
        message("End Game")
      }
      
    }
    
    i= i + 1
    
    if(errors == 8){
      active == FALSE
      break
      message("End game")
    }
  }
}

### Start new Game


StartNewGame()