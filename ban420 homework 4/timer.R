#Loading packages 
library(tictoc)

#Clearing previous log before running the rest of the code:
tic.clearlog()

# Function to store the tictok log
printTicTocLog <-
  function() {
    tic.log() %>%
      unlist %>%
      tibble(logvals = .) %>%
      separate(logvals,
               sep = ":",
               into = c("Function type", "log")) %>%
      mutate(log = str_trim(log)) %>%
      separate(log,
               sep = " ",
               into = c("Seconds"),
               extra = "drop")
  }

#TIME OF ORIGINAL SCRIPT-------------

tic("Original Script")
source("scripts/originalscript.r")
toc(log = TRUE)




#TIME OF PARALLEL LOOP SCRIPT----------
tic("Parallel loop")

# Source the original script from the scripts folder
source("scripts/parallel-loop.r")

# Stop timing and store the result in the log
toc(log = TRUE)



#TIME OF PARALLEL LOOP SCRIPT 2-------
tic("Parallel loop2")

# Source the original script from the scripts folder
source("scripts/parallel-loop2.r")

# Stop timing and store the result in the log
toc(log = TRUE)


#Printing the log
printTicTocLog()
