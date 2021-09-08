
# The purpose of this code is that it's meant to extract the Fisher-converted effect size,
# as well as the statcheck data on whether the test has been reported properly.

# Not all these packages are needed right now, but I anticipate they'll all be required eventually
library(statcheck)
library(tidyverse)
library(data.table)

# You may wish to select different data columns from a different CSV
full_data <- fread("FINAL_Complete.csv")
# Making an additional column for statistical error and gross error with NAs
full_data$error <- rep(NA, nrow(full_data))
full_data$decisionError <- rep(NA, nrow(full_data))

chosen_data_columns <- fread("FINAL_Complete.csv", select = c("statistic","df1","df2","value","reported","p"))

final_row <- 10000 # You'll need to change this if you want more than 50 rows

# Here we loop through row 1 to whatever row you've chosen as list_length
# Style guides to R will sometimes suggest to avoid loops;
# however, they're much more human-readable than the alternatives and so
# are suitable for our purposes here.
statcheck_data <- data.frame()
for(i in 1:final_row) {
  # If this code is hard to understand, you might want to try setting i <- 1 and then
  # running each line one-by-one, to see how things fit together.
  
  # In this segment we extract things we'll need for to get our Fisher-converted effect size
  test_stat_type <- chosen_data_columns$statistic[i]
  df1 <- chosen_data_columns$df1[i]
  df2 <- chosen_data_columns$df2[i]
  test_stat <- chosen_data_columns$value[i]
  p_value <- chosen_data_columns$p[i]
  
  # Before we use statcheck replace all reported statistics with P with p
  chosen_data_columns$p <- tolower(chosen_data_columns$p)
  
  # Call Statcheck to check if the statistics have been correctly reported
  # Type ?statcheck for more information on Statcheck, we will use our constructed segment 
  reported_segment <- chosen_data_columns$reported[i]
  
  # Constructing our own string, by using different columns from the CSV file. 
  # This way we don't use the reported segmented in the dataset because it contains erroneous symbols
  
  if (test_stat_type == "t") {
    statcheck_type <- "t"
    # The next line should be correct. But the constructed segments for all other test types
    # are wrong
    constructed_segment <- str_c(test_stat_type,"(",df2,") = ",test_stat,", ",p_value)
  } else if (test_stat_type == "F") {
    statcheck_type <- "F"
    constructed_segment <- str_c(test_stat_type,"(",df1,",",df2,") = ",test_stat,", ",p_value)
  } else if (test_stat_type == "chi") {
    statcheck_type <- "chisq"
    constructed_segment <- str_c("chisq","(",df2,") = ",test_stat,", ",p_value)
  } else if (test_stat_type == "r") {
    statcheck_type <- "cor"
    constructed_segment <- str_c("cor","(",df2,") = ",test_stat,", ",p_value)
  } else {
    stop("end of extraction")
  }
  
  # Call Statcheck again, but using the text string we just constructed rather than the segment from the CSV file
  # I'm quite sure that will sometimes make a difference, but which is correct?
  second_statcheck <- statcheck(constructed_segment)
  print(second_statcheck)
  
  #We will replace the NAs with any error Statcheck was able to identify
  if (!is.null(second_statcheck)){
    full_data$error[i] <- full_data$Error[1]
    full_data$decisionError[i] <- second_statcheck$DecisionError[1]
  } else {
    statcheck_data[i,] <- NA
  }
  
  statcheck_data <- rbind(statcheck_data, second_statcheck)
  
  # Call Statcheck again, but also specify for Statcheck the test type we think it is.
  # Will that make a difference?
  third_statcheck <- statcheck(constructed_segment,stat=statcheck_type)
  
}

write.csv(full_data,'full_dataset_statchecked.csv')


