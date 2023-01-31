
#HangMan Assignment

#Set wd
getwd()
setwd("/Users/deweenaparija/Desktop/MSC2011/Assignments/Deweena-")

# Attaching dictionary of Words, with no headers and treating word after a space as a new coloumn
dic <- read.table("Dictionary.txt", header = F)


# Sampling a random element from 
secret <- sample(dic[,1], 1)

# Finding out many characters does the random word have.
chr <- nchar(secret)

# Making a another character variable for word to matching with input
secret.match <- unlist(strsplit(secret, ""))

# Prompt line 
Initiation <- readline(paste("Welcome to Hangman - cities edition! (Press enter)"))
Instructions <- readline(paste("Your city is", chr, "letters long. You've got", chr," + 1 tries, press enter!"))


      
# Read prompt line and give 10 tries as default.
tries = chr + 1
as.numeric(tries)

# Making a dash version of the random word.
missing.words <- replicate(nchar(secret), "_" )

# Begin the loop with 12 tries
while (tries > 0) {
  Guess <- readline("Guess a letter in your word:")
  tries <- tries - 1
 
  # Check if input is valid
  while (is.na(Guess) || nchar(Guess) > 1) {
    
    #Each guess will eat up a try
    print("Invalid entry: please enter one letter at a time")
    Guess <- readline("Guess a letter in your word:")
    tries <- tries - 1
  } # End while invalid entry

  # If the guessed letter is in the word:
  if (Guess %in% secret.match) {
    # Display the results
    print(paste(Guess, "is present in the word!"))
    
    # Identifying which position does the user input letter correspond to the secret word
    k <- which(Guess == secret.match)
    
    # Replacing  the correctly guessed letter into the dash word version
    missing.words <- replace(missing.words, k, Guess)
    print(missing.words)
    
    # If word is not present, return "not present input"
  } else {
    print(paste(Guess, "is not present in the word!")) 
  }
  
  # If the all the letters match then you get the following prompt.
  if (tries == 0) {
    print(paste("Game over. Your word was", secret))
  }
}
 break
  


