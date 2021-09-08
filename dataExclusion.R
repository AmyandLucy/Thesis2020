# Data exlusion, p values reported with n.s is to be excluded 

## First convert all NS reported to lowercase
categorised_data_final$p <- tolower(categorised_data_final$p)

## Exclude all p reported with ns
categorised_data_final <- categorised_data_final[grep("ns", categorised_data_final$p, invert = TRUE),]

write.csv(categorised_data_final, "C:/Users/LU/Desktop/categorised_data_final.csv", )
