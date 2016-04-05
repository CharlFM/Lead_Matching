# Current Updated Model - To be used
save("Model", file = paste(Path, "/Active_Model.RData", sep = ""))

# Backup of model (incase of backtracking to use prior models)
save("Model", file = paste(Path, "/Model_Backup/Active_Model ", gsub("-", "_", gsub(":", "_", Sys.time())), ".RData", sep = ""))

