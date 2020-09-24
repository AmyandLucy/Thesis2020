library(logging)

## This function will run Fisher Z Conversions on t-statistics, F-statistics and r-statistics per Singleton Thorn (2019).

## It will not run on Chi-Squared because there is no way to accurately reproduce the Chi-Squared to r conversion formula without knowing n

fisher_convert <- function(data_frame, test_stat_type_vector, test_stat_vector, df1_vector, df2_vector)
{
  # Calculate number of rows in dataframe
  
  final_row <- nrow(data_frame)
  saved_effect_sizes <- vector(mode = "numeric", length = final_row) 
  
  # Loop the entire function through the below
  for(i in 1:final_row) {
    test_stat_type <- test_stat_type_vector[i]
    test_stat <- test_stat_vector[i]
    df1 <- df1_vector[i]
    df2 <- df2_vector[i]
    
    # Run calculation on t-statistics
    if (test_stat_type == "t") {
      t_stat <- abs(test_stat)
      
      converted_corr <- sqrt(
        ( (t_stat^2) * (1/df2) ) /
          ( ( (t_stat^2)/df2 ) + 1 )
      )
    } 
    
    # Run the claculation on F-stats  
    else if (test_stat_type == "F") {
      F_stat <- test_stat
      
      converted_corr <- sqrt(
        (F_stat*(df1/df2))/
          (F_stat*(df1/df2)+1)
      )*sqrt(1/df1)
    } 
    
    # Rename r stats
    else if (test_stat_type == "r") {
      converted_corr <- abs(test_stat)
    } 
    
    # Error loop
    else {
      # This is a statistic that cannot be converted
      converted_corr <- NaN
    }
    
    # Calculating Z-converted correlations
    saved_effect_sizes[i] <- .5*log((1+converted_corr)/(1-converted_corr))
    
  }
  
  saved_effect_sizes
}
