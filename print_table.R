#
# Function: prints dataframe to file of desired type
#
# June 2021
# lineb@met.no
#
# Example: print_table(df,"testfile","xlsx",";")
#
# --------------------------------------------------------------------------------------

print_table <- function(data_frame, filename_incl_path, file_format, seperator){
  write.table(data_frame, file = paste0(filename_incl_path, ".", file_format), row.names = FALSE, col.names = TRUE, sep = paste0(seperator), quote = FALSE)
}






