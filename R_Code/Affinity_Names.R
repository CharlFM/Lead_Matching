# ##### Updates affinities selling dealer information.
# 
# data <- ManLead_DatOrig
# 
#   data$AFFINITY[substr(data$BRANCHNAME, start = 1, stop = 5) == "Mekor"]        <- "MEKOR(SIGNIO)"
#   data$AFFINITY[substr(data$BRANCHNAME, start = 1, stop = 5) == "Group"]        <- "Group1"
#   data$AFFINITY[data$BRANCHNAME == "WILLIAM SIMPSON CARS"]                      <- "WILLIAM SIMPSON"
#   data$AFFINITY[data$BRANCHNAME == "Mazda Bloemfontein"]                        <- "Mazda Bloemfontein"
#   data$AFFINITY[data$BRANCHNAME == "Lambons Peugeot"]                           <- "Lambons Peugeot"
#   data$AFFINITY[data$BRANCHNAME == "Honda Claremont" |
#                   data$BRANCHNAME == "Honda Amanzimtoti" |
#                   data$BRANCHNAME == "HONDA AUTO UMHLANGA RIDGE"]               <- "Mekor(Signio)"
#   data$AFFINITY[data$BRANCHNAME == "Citroen Richards Bay"]                      <- "Mekor(Signio)"
#   data$AFFINITY[data$BRANCHNAME == "Lenasia Vw"]                                <- "Lenasia Vw"
#   data$AFFINITY[data$BRANCHNAME == "Car Finance Company (Cfc)"]                 <- "CFC"
#   data$AFFINITY[data$BRANCHNAME == "Melrose Nissan"]                            <- "Melrose Nissan"
#   data$AFFINITY[data$BRANCHNAME == "Chicane Auto"]                              <- "Chicane Auto"
#   data$AFFINITY[data$BRANCHNAME == "Peugeot Citroen Tygervalley"]               <- "Mekor(Signio)"
#   data$AFFINITY[data$BRANCHNAME == "Peugeot Hatfield"]                          <- "Peugeot Hatfield"
#   data$AFFINITY[data$BRANCHNAME == "Peugeot Northcliff"]                        <- "Peugeot Northcliff"
#   data$AFFINITY[data$BRANCHNAME == "Judine Motors Boksburg"]                    <- "JUDINE MOTORS BOKSBURG"
#   data$AFFINITY[data$BRANCHNAME == "Right Cars"]                                <- "Mekor(Signio)"
#   data$AFFINITY[data$BRANCHNAME == "Ceres Toyota"]                              <- "Ceres"
#   data$AFFINITY[data$BRANCHNAME == "Steven Johnson Cars"]                       <- "Steven Johnson"
#   data$AFFINITY[data$BRANCHNAME == "Nellas Auto PTY LTD"]                       <- "Mekor(Signio)"
#   data$AFFINITY[is.na(data$BRANCHNAME)]                                         <- "Other"

# ManLead_DatOrig <- data
  


data <- ManLead_DatOrig
data$BRANCHNAME <- gsub(" ", "", toupper(data$BRANCHNAME))

  data$AFFINITY[substr(data$BRANCHNAME, start = 1, stop = 5) == "MEKOR"]        <- "MEKOR(SIGNIO)"
  data$AFFINITY[substr(data$BRANCHNAME, start = 1, stop = 5) == "GROUP"]        <- "GROUP1"
  data$AFFINITY[data$BRANCHNAME == "WILLIAMSIMPSONCARS"]                        <- "WILLIAMSIMPSON"
  data$AFFINITY[data$BRANCHNAME == "MAZDABLOEMFONTEIN"]                         <- "MAZDABLOEMFONTEIN"
  data$AFFINITY[data$BRANCHNAME == "LAMBONSPEUGEOT"]                            <- "LAMBONSPEUGEOT"
  data$AFFINITY[data$BRANCHNAME == "HONDACLAREMONT" |
                data$BRANCHNAME == "HONDAAMANZIMTOTI" |
                data$BRANCHNAME == "HONDAAUTOUMHLANGARIDGE"]                    <- "MEKOR(SIGNIO)"
  data$AFFINITY[data$BRANCHNAME == "CITROENRICHARDSBAY"]                        <- "MEKOR(SIGNIO)"
  data$AFFINITY[data$BRANCHNAME == "LENASIAVW"]                                 <- "LENASIAVW"
  data$AFFINITY[data$BRANCHNAME == "CARFINANCECOMPANY(CFC)"]                    <- "CFC"
  data$AFFINITY[substr(data$BRANCHNAME, start = 1, stop = 3) == "CFC"]          <- "CFC"
  data$AFFINITY[data$BRANCHNAME == "MELROSENISSAN"]                             <- "MELROSENISSAN"
  data$AFFINITY[data$BRANCHNAME == "CHICANEAUTO"]                               <- "CHICANEAUTO"
  data$AFFINITY[data$BRANCHNAME == "PEUGEOTCITROENTYGERVALLEY"]                 <- "MEKOR(SIGNIO)"
  data$AFFINITY[data$BRANCHNAME == "PEUGEOTHATFIELD"]                           <- "PEUGEOTHATFIELD"
  data$AFFINITY[data$BRANCHNAME == "PEUGEOTNORTHCLIFF"]                         <- "PEUGEOTNORTHCLIFF"
  data$AFFINITY[data$BRANCHNAME == "JUDINEMOTORSBOKSBURG"]                      <- "JUDINEMOTORSBOKSBURG"
  data$AFFINITY[data$BRANCHNAME == "RIGHTCARS"]                                 <- "MEKOR(SIGNIO)"
  data$AFFINITY[data$BRANCHNAME == "CERESTOYOTA"]                               <- "CERES"
  data$AFFINITY[data$BRANCHNAME == "STEVENJOHNSONCARS"]                         <- "STEVENJOHNSON"
  data$AFFINITY[data$BRANCHNAME == "NELLASAUTOPTYLTD"]                          <- "MEKOR(SIGNIO)"
  data$AFFINITY[is.na(data$BRANCHNAME)]                                         <- "OTHER"

ManLead_DatOrig <- data



