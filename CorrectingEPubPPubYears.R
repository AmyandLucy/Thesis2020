# Extracting years from ePub

metadata_for_comprehensively_reported_tests_2$ePubYr <- as.numeric(format(metadata_for_comprehensively_reported_tests_2$ePub,'%Y'))

# Extracting years from pPub

metadata_for_comprehensively_reported_tests_2$pPubYr <- as.numeric(format(metadata_for_comprehensively_reported_tests_2$pPub,'%Y'))

# Removing ePub column for ease of reading

metadata_for_comprehensively_reported_tests_2$ePub <- NULL

# Removing pPub column for ease of reading

metadata_for_comprehensively_reported_tests_2$pPub <- NULL

# Comparing ePub and pPub to determine which is lower

metadata_with_one_year_only <- transform(metadata_for_comprehensively_reported_tests_2, realYear = pmin(ePubYr, pPubYr, na.rm=TRUE))

# Removing ePubYr column for ease of reading

metadata_with_one_year_only$ePubYr <- NULL

# Removing pPub column for ease of reading

metadata_with_one_year_only$pPubYr <- NULL
