
# The purpose of this code is that it's meant to extract the Fisher-converted effect size,
# as well as the statcheck data on whether the test has been reported properly.

# Not all these packages are needed right now, but I anticipate they'll all be required eventually
library(statcheck)
library(tidyverse)
library(data.table)

# You may wish to select different data columns from a different CSV
chosen_data_columns <- fread("LucySample_final.csv", select = c("statistic","df1","df2","value","reported","p"))

final_row <- 50 # You'll need to change this if you want more than 50 rows

# For now we'll just save our effect sizes in a numeric vector. Later on we may change this.
saved_effect_sizes <- vector(mode = "numeric", length = final_row) 


# Here we loop through row 1 to whatever row you've chosen as list_length
# Style guides to R will sometimes suggest to avoid loops;
# however, they're much more human-readable than the alternatives and so
# are suitable for our purposes here.
all_the_second_statchecks <- data.frame()
for(i in 1:final_row) {
  
  # If this code is hard to understand, you might want to try setting i <- 1 and then
  # running each line one-by-one, to see how things fit together.
  
  # In this segment we extract things we'll need for to get our Fisher-converted effect size
  test_stat_type <- chosen_data_columns$statistic[i]
  df1 <- chosen_data_columns$df1[i]
  df2 <- chosen_data_columns$df2[i]
  test_stat <- chosen_data_columns$value[i]
  p_value <- chosen_data_columns$p[i]
  
  # Call the function which provides us with the Fisher Z-transformed correlation coefficient
  # I've called that function "fisher_convert"
  # Save the relevant effect sizes
  saved_effect_sizes[i] <- fisher_convert(test_stat_type,test_stat,df1,df2)
  
  # Call Statcheck to check if the statistics have been correctly reported
  # Type ?statcheck for more information on Statcheck
  # This first call to Statcheck just tests out the "Reported" column from the CSV file
  reported_segment <- chosen_data_columns$reported[i]
  first_statcheck <- statcheck(reported_segment)
  
  # For the second and third calls to Statcheck, instead of using the "Reported" column
  # We instead construct our own string, using different columns from the CSV file
  # It would be ideal if we'd get the same results as in the first call, but I doubt we will.
  if (test_stat_type == "t") {
    statcheck_type <- "t"
    # The next line should be correct. But the constructed segments for all other test types
    # are wrong
    constructed_segment <- str_c(test_stat_type,"(",df2,") = ",test_stat,", ",p_value)
  } else if (test_stat_type == "F") {
    statcheck_type <- "F"
    # Edit the next line
    constructed_segment <- str_c(test_stat_type,"(",df1,",",df2,") = ",test_stat,", ",p_value)
    # blablabla the effect was very significant (F(100,200)=1, p < 0.001)
  } else if (test_stat_type == "chi") {
    statcheck_type <- "chisq"
    # Edit the next line
    constructed_segment <- str_c("chisq","(",df2,") = ",test_stat,", ",p_value)
    #blablabla the effect was very not significant (X(33)=50, p = 0.501)
  } else if (test_stat_type == "r") {
    statcheck_type <- "cor"
    # Edit the next line
    constructed_segment <- str_c(test_stat_type,"(",df2,") = ",test_stat,", ",p_value)
    #blablabla the effect was extremely significant (r(112)=,.60, p = 0.012)
  } else {
    # edit error message?
    stop("end of extraction")
  }
  
  # Call Statcheck again, but using the text string we just constructed rather than the segment from the CSV file
  # I'm quite sure that will sometimes make a difference, but which is correct?
  second_statcheck <- statcheck(constructed_segment)
  all_the_second_statchecks <- rbind(all_the_second_statchecks, second_statcheck)
  
  # Call Statcheck again, but also specify for Statcheck the test type we think it is.
  # Will that make a difference?
  third_statcheck <- statcheck(constructed_segment,stat=statcheck_type)
  
  dataframe <- data.frame(
    to_code = c("t", "F", "chi", "cor")
  )
  
  for(i in seq_along(dataframe$to_code)){
    if (dataframe$to_code[i] == "t"){
      dataframe$dummy[i] <- 0
    } else if (dataframe$to_code[i] == "F"){
      dataframe$dummy[i] <- 1
    } else if (dataframe$to_code[i] == "chi"){
      dataframe$dummy[i] <- 2
    } else if (dataframe$to_code[i] == "cor"){
      dataframe$dummy[i] <- 3
  }
  
}

