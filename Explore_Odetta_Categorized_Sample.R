# Explore the Odetta Sample sent on December 12 2018
#
library(dplyr)
library(stringr)
library(pandas)
library(tidyr)

setwd("/Volumes/GoogleDrive/My Drive/Projects/2018-10-Odetta_Categorizing_Program_Areas/SampleData/odetta_midpoint_sample")

sample <- readxl::read_xlsx(path = "Fluxx Work Done - Dec 11 2018.xlsx")

# clean up the n/as
sample$program_ntee[sample$program_ntee == "N/A"] <- NA
sample$subprogram_ntee[sample$subprogram_ntee == "N/A"] <- NA
sample$initiative_ntee[sample$initiative_ntee == "N/A"] <- NA
sample$subinitiative_ntee[sample$subinitiative_ntee == "N/A"] <- NA


psis <- c("program","subprogram","initiative","subinitiative")

#break the ntee codes into columns
sample2 <- separate(sample,program_ntee, c("program_ntee1", "program_ntee2","program_ntee3"),remove = FALSE)
sample3 <- separate(sample2,subprogram_ntee, c("subprogram_ntee1", "subprogram_ntee2","subprogram_ntee3"),remove = FALSE)
sample4 <- separate(sample3,initiative_ntee, c("initiative_ntee1", "initiative_ntee2","initiative_ntee3"),remove = FALSE)
sample5 <- separate(sample4,subinitiative_ntee, c("subinitiative_ntee1", "subinitiative_ntee2","subinitiative_ntee3"),remove = FALSE)


# how many geos have multiple entries
length(grep(",", sample5$program_geo))
length(grep(",", sample5$subprogram_geo))
length(grep(",", sample5$initiative_geo))
length(grep(",", sample5$subinitiative_geo))

# break multiple geos
sample6 <- separate(sample5,program_geo, c("program_geo1", "program_geo2"),remove = FALSE)
sample7 <- separate(sample6,subprogram_geo, c("subprogram_geo1", "subprogram_geo"),remove = FALSE)
sample8 <- separate(sample7,initiative_geo, c("initiative_geo1", "initiativem_geo"),remove = FALSE)
sample9 <- separate(sample8,subinitiative_geo, c("subinitiative_geo1", "subinitiative_geo2"),remove = FALSE)

rm(sample,sample2,sample3,sample4,sample5,sample6,sample7,sample8)

# number of rows with Z00s
sample9 %>% subset(sample9$program_ntee=="Z00") %>% nrow()
sample9 %>% subset(sample9$subprogram_ntee=="Z00") %>% nrow()
sample9 %>% subset(sample9$initiative_ntee=="Z00") %>% nrow()
sample9 %>% subset(sample9$subinitiative_ntee=="Z00") %>% nrow()

print((sum(sample9$program_ntee=="Z00")) / (nrow(sample9)))

sample9$program_ntee_dummy <- 1
sample9$program_ntee_dummy[is.na(sample9$program_ntee)] <- 0
sample9$program_ntee_dummy[sample9$program_ntee=="Z00"] <- 0

sample9$subprogram_ntee_dummy <- NULL
sample9$subprogram_ntee_dummy[sample9$most_granular=="Subprogram"] <- 1
sample9$subprogram_ntee_dummy[is.na(sample9$subprogram_ntee) & sample9$most_granular=="Subprogram"] <- 0
sample9$subprogram_ntee_dummy[sample9$subprogram_ntee1=="Z00" & sample9$most_granular=="Subprogram"] <- 0

sample9$initiative_ntee_dummy <- NULL
sample9$initiative_ntee_dummy[sample9$most_granular=="Initiative"] <- 1
sample9$initiative_ntee_dummy[is.na(sample9$initiative_ntee) & sample9$most_granular=="Initiative"] <- 0
sample9$initiative_ntee_dummy[sample9$initiative_ntee1=="Z00" & sample9$most_granular=="Initiative"] <- 0

sample9$subinitiative_ntee_dummy <- NULL
sample9$subinitiative_ntee_dummy[sample9$most_granular=="SubInitiative"] <- 1
sample9$subinitiative_ntee_dummy[is.na(sample9$subinitiative_ntee) & sample9$most_granular=="SubInitiative"] <- 0
sample9$subinitiative_ntee_dummy[sample9$subinitiative_ntee1=="Z00" & sample9$most_granular=="SubInitiative"] <- 0

CrossTable(sample9$program_ntee_dummy)
CrossTable(sample9$subprogram_ntee_dummy)
CrossTable(sample9$initiative_ntee_dummy)
CrossTable(sample9$subinitiative_ntee_dummy)

sample9$program_ntee_dummy <- ordered(sample9$program_ntee_dummy,
                     levels = c(0,1),
                     labels = c("No Category","Categorized"))

sample9$subprogram_ntee_dummy <- ordered(sample9$subprogram_ntee_dummy,
                                      levels = c(0,1),
                                      labels = c("No Category","Categorized"))

sample9$initiative_ntee_dummy <- ordered(sample9$initiative_ntee_dummy,
                                      levels = c(0,1),
                                      labels = c("No Category","Categorized"))

sample9$subinitiative_ntee_dummy <- ordered(sample9$subinitiative_ntee_dummy,
                                      levels = c(0,1),
                                      labels = c("No Category","Categorized"))

# clean the crosstab
pnd <- table(sample9$program_ntee_dummy)
pnd2<-prop.table(pnd)
print(pnd2)

prop.table(table(sample9$program_ntee_dummy))



tbl <- table(sample9$program_ntee_dummy)
cbind(tbl,prop.table(tbl))



View(sample9[sample9$most_granular=="SubInitiative",])

save(sample9, file="odetta_sample.RData")


program <- subset(sample9[grep("^program_", names(sample9))],sample9$most_granular=="Program")
subprogram <- subset(sample9[grep("^subprogram_", names(sample9))],sample9$most_granular=="Subprogram")
initiative <- subset(sample9[grep("^initiative_", names(sample9))],sample9$most_granular=="Initiative")
subinitiative <- subset(sample9[grep("^subinitiative_", names(sample9))],sample9$most_granular=="SubInitiative")

subprogram$id <- subprogram$subprogram_id
subprogram$name <- subprogram$subprogram_name
subprogram$nteecdcnt <- subprogram$subprogram_nteecdcnt
subprogram$ntee <- subprogram$subprogram_ntee
subprogram$ntee1 <- subprogram$subprogram_ntee1
subprogram$ntee2 <- subprogram$subprogram_ntee2
subprogram$ntee3 <- subprogram$subprogram_ntee3
subprogram$geo1 <- subprogram$subprogram_geo1
subprogram$geo <- subprogram$subprogram_geo
subprogram$ntee_dummy <- subprogram$subprogram_ntee_dummy
program$ntee_dummy <- program$subprogram_ntee_dummy
program$id <- program$program_id
program$name <- program$program_name
program$nteecdcnt <- program$program_nteecdcnt
program$ntee <- program$program_ntee
program$ntee1 <- program$program_ntee1
program$ntee2 <- program$program_ntee2
program$ntee3 <- program$program_ntee3
program$geo <- program$program_geo
program$geo1 <- program$program_geo1
program$geo2 <- program$program_geo2
program$ntee_dummy <- program$program_ntee_dummy
initiative$id <- initiative$initiative_id
initiative$name <- initiative$initiative_name
initiative$nteecdcnt <- initiative$initiative_nteecdcnt
initiative$ntee <- initiative$initiative_ntee
initiative$ntee1 <- initiative$initiative_ntee1
initiative$ntee2 <- initiative$initiative_ntee2
initiative$ntee3 <- initiative$initiative_ntee3
initiative$geo <- initiative$initiative_geo
initiative$geo1 <- initiative$initiative_geo1
initiative$ntee_dummy <- initiative$initiative_ntee_dummy
subinitiative$id <- subinitiative$subinitiative_id
subinitiative$name <- subinitiative$subinitiative_name
subinitiative$nteecdcnt <- subinitiative$subinitiative_nteecdcnt
subinitiative$ntee <- subinitiative$subinitiative_ntee
subinitiative$ntee1 <- subinitiative$subinitiative_ntee1
subinitiative$ntee2 <- subinitiative$subinitiative_ntee2
subinitiative$ntee3 <- subinitiative$subinitiative_ntee3
subinitiative$geo <- subinitiative$subinitiative_geo
subinitiative$geo1 <- subinitiative$subinitiative_geo1
subinitiative$geo2 <- subinitiative$subinitiative_geo2
subinitiative$ntee_dummy <- subinitiative$subinitiative_ntee_dummy

subprogram$subprogram_id <- NULL
subprogram$subprogram_name <- NULL
subprogram$subprogram_nteecdcnt <- NULL
subprogram$subprogram_ntee <- NULL
subprogram$subprogram_ntee1 <- NULL
subprogram$subprogram_ntee2 <- NULL
subprogram$subprogram_ntee3 <- NULL
subprogram$subprogram_geo1 <- NULL
subprogram$subprogram_geo <- NULL
subprogram$subprogram_ntee_dummy <- NULL
program$program_id <- NULL
program$program_name <- NULL
program$program_nteecdcnt <- NULL
program$program_ntee <- NULL
program$program_ntee1 <- NULL
program$program_ntee2 <- NULL
program$program_ntee3 <- NULL
program$program_geo <- NULL
program$program_geo1 <- NULL
program$program_geo2 <- NULL
program$program_ntee_dummy <- NULL
initiative$initiative_id <- NULL
initiative$initiative_name <- NULL
initiative$initiative_nteecdcnt <- NULL
initiative$initiative_ntee <- NULL
initiative$initiative_ntee1 <- NULL
initiative$initiative_ntee2 <- NULL
initiative$initiative_ntee3 <- NULL
initiative$initiative_geo <- NULL
initiative$initiative_geo1 <- NULL
initiative$initiative_ntee_dummy <- NULL
subinitiative$subinitiative_id <- NULL
subinitiative$subinitiative_name <- NULL
subinitiative$subinitiative_nteecdcnt <- NULL
subinitiative$subinitiative_ntee <- NULL
subinitiative$subinitiative_ntee1 <- NULL
subinitiative$subinitiative_ntee2 <- NULL
subinitiative$subinitiative_ntee3 <- NULL
subinitiative$subinitiative_geo <- NULL
subinitiative$subinitiative_geo1 <- NULL
subinitiative$subinitiative_geo2 <- NULL
subinitiative$subinitiative_ntee_dummy <- NULL

initiative$geo2 <- ""
subprogram$geo2 <- ""


stacked <- rbind(program,subprogram,initiative,subinitiative)


tbl <- table(stacked$ntee_dummy)
cbind(tbl,prop.table(tbl))
