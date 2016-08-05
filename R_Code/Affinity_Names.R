##### Updates affinities selling dealer information.

data <- ManLead_DatOrig



  data$AFFINITY[substr(data$BRANCHNAME, start = 1, stop = 5) == "Mekor"]        <- "Mekor(Signio)"
  data$AFFINITY[substr(data$BRANCHNAME, start = 1, stop = 5) == "Group"]        <- "Group1"
  data$AFFINITY[data$BRANCHNAME == "WILLIAM SIMPSON CARS"]                      <- "WILLIAM SIMPSON"
  data$AFFINITY[data$BRANCHNAME == "Mazda Bloemfontein"]                        <- "Mazda Bloemfontein"
  data$AFFINITY[data$BRANCHNAME == "Lambons Peugeot"]                           <- "Lambons Peugeot"
  data$AFFINITY[data$BRANCHNAME == "Honda Claremont" |
                  data$BRANCHNAME == "Honda Amanzimtoti" |
                  data$BRANCHNAME == "HONDA AUTO UMHLANGA RIDGE"]               <- "Mekor(Signio)"
  data$AFFINITY[data$BRANCHNAME == "Citroen Richards Bay"]                      <- "Mekor(Signio)"
  data$AFFINITY[data$BRANCHNAME == "Lenasia Vw"]                                <- "Lenasia Vw"
  data$AFFINITY[data$BRANCHNAME == "Car Finance Company (Cfc)"]                 <- "CFC"
  data$AFFINITY[data$BRANCHNAME == "Melrose Nissan"]                            <- "Melrose Nissan"
  data$AFFINITY[data$BRANCHNAME == "Chicane Auto"]                              <- "Chicane Auto"
  data$AFFINITY[data$BRANCHNAME == "Peugeot Citroen Tygervalley"]               <- "Mekor(Signio)"
  data$AFFINITY[data$BRANCHNAME == "Peugeot Hatfield"]                          <- "Peugeot Hatfield"
  data$AFFINITY[data$BRANCHNAME == "Peugeot Northcliff"]                        <- "Peugeot Northcliff"
  data$AFFINITY[data$BRANCHNAME == "Judine Motors Boksburg"]                    <- "JUDINE MOTORS BOKSBURG"
  data$AFFINITY[data$BRANCHNAME == "Right Cars"]                                <- "Mekor(Signio)"
  data$AFFINITY[data$BRANCHNAME == "Ceres Toyota"]                              <- "Ceres"
  data$AFFINITY[data$BRANCHNAME == "Steven Johnson Cars"]                       <- "Steven Johnson"
  data$AFFINITY[data$BRANCHNAME == "Nellas Auto PTY LTD"]                       <- "Mekor(Signio)"

ManLead_DatOrig <- data
  




