# Trying to merge files

merged_data_set <- merge(comprehensively_reported_tests, metadata_for_comprehensively_reported_tests, by = "PMCID")

# Saving merged file as .CSV

write.csv(merged_data_set, "merged_data_set.csv")

# Attempting to remove PeerJ

no_peerj <- subset(merged_data_set, journalID!="Peerj")

# Attempting to remove Royal Society Open Science

clean_data_set_complete <- subset(no_peerj, journalID!="Royal Society Open Science")

# Saving clean dataset

write.csv(clean_data_set_complete, "clean_data_set_complete.csv")