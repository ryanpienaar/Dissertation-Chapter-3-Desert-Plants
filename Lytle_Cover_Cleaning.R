library(dplyr)
library(tidyr)

moj_cover <- read.csv("C:/Users/ryanp/Documents/Academia/Dissertation/Raw data/Mojave/Lytle_Cover/Lytle Cover All.csv")

moj_cover <- moj_cover |> 
  separate(col = X, into = c("third", "fourth"), sep = "/", fill = "right")

moj_cover$fourth <- as.factor(moj_cover$fourth)
summary(moj_cover$fourth)
moj_cover$fourth <- as.character(moj_cover$fourth)
head(moj_cover)

### How many unique species do we have
unique(moj_cover$X1ST.FOLIAR)
unique(moj_cover$X2ND.FOLIAR)
unique(moj_cover$third)
unique(moj_cover$fourth)
unique(moj_cover$BASAL)
unique(moj_cover$CANOPY)

# Lets go through em one by one,

moj_cover[moj_cover == "NF"] <- 0
moj_cover[moj_cover == "NC"] <- 0
moj_cover[moj_cover == ""] <- 0
moj_cover[moj_cover == "SS"] <- 0
moj_cover[moj_cover == "WS"] <- 0
moj_cover[moj_cover == "? (New Plant)"] <- 0
moj_cover[moj_cover == "DF"] <- 0
moj_cover[moj_cover == "DS"] <- 0
moj_cover[moj_cover == "WL"] <- 0
moj_cover[moj_cover == "DG"] <- 0
moj_cover[moj_cover == "BT"] <- 0
moj_cover[moj_cover == "R"] <- 0
moj_cover[moj_cover == "W"] <- 0
moj_cover[moj_cover == "LC"] <- 0
moj_cover[moj_cover == "PG"] <- 0
moj_cover[moj_cover == "BL"] <- 0
moj_cover[moj_cover == "AL"] <- 0
moj_cover[moj_cover == "CY"] <- 0
moj_cover[moj_cover == "M"] <- 0
moj_cover[moj_cover == "SW"] <- 0
moj_cover[moj_cover == "TT"] <- 0
moj_cover[moj_cover == "MCY"] <- 0
moj_cover[moj_cover == "CS"] <- 0
moj_cover[moj_cover == "-"] <- 0
moj_cover[moj_cover == "BG"] <- 0
moj_cover[moj_cover == "K"] <- 0
moj_cover[moj_cover == "C"] <- 0
moj_cover[moj_cover == "SL"] <- 0
moj_cover[moj_cover == "LI"] <- 0
moj_cover[moj_cover == "DW"] <- 0
moj_cover[moj_cover == "EL"] <- 0
moj_cover[moj_cover == "AS"] <- 0

moj_cover <- moj_cover %>%
  mutate(across(Canopy:Basal, ~ replace(., . == "BS", "0")))
moj_cover[moj_cover == "LL"] <- 0
moj_cover[moj_cover == "SLC"] <- 0
moj_cover[moj_cover == "S"] <- 0
moj_cover[moj_cover == "[DG]"] <- 0
moj_cover[moj_cover == "S "] <- 0
moj_cover[moj_cover == "[DF]"] <- 0
moj_cover[moj_cover == "[DS]"] <- 0
moj_cover[moj_cover == "BT(ant hill)"] <- 0
moj_cover[moj_cover == "WC"] <- 0
moj_cover[moj_cover == "Lichen"] <- 0
moj_cover[moj_cover == "Lichen White"] <- 0
moj_cover[moj_cover == "DC"] <- 0
moj_cover[moj_cover == "YL"] <- 0
moj_cover[moj_cover == "CT"] <- 0
moj_cover[moj_cover == "N"] <- 0
moj_cover[moj_cover == "WY"] <- 0
moj_cover[moj_cover == "LS"] <- 0
moj_cover[moj_cover == "F"] <- 0
moj_cover[moj_cover == "G"] <- 0
moj_cover[moj_cover == "T"] <- 0
moj_cover[moj_cover == "BIO* (ant hill)"] <- 0
moj_cover[moj_cover == "CYANOBACTE."] <- 0
moj_cover[moj_cover == "SOIL"] <- 0
moj_cover[moj_cover == "soil"] <- 0
moj_cover[moj_cover == "wood"] <- 0
moj_cover[moj_cover == "soil "] <- 0
moj_cover[moj_cover == "s"] <- 0
moj_cover[moj_cover == "AGCR (CRSTED WHEAT GRASS"] <- "AGCR"
moj_cover[moj_cover == "BTE"] <- 0
moj_cover[moj_cover == "FD"] <- 0
moj_cover[moj_cover == "AN"] <- 0
moj_cover[moj_cover == "B"] <- 0
moj_cover[moj_cover == "AG"] <- 0
moj_cover[moj_cover == "BIO* (spiderweb)"] <- 0
moj_cover[moj_cover == "l"] <- 0
moj_cover[moj_cover == "HNC"] <- 0
moj_cover[moj_cover == "Dead Wood"] <- 0
moj_cover[moj_cover == "BARE "] <- "BARE"
moj_cover[moj_cover == "LICHEN(WHITE)"] <- 0
moj_cover[moj_cover == "LICHEN(YELLOW)"] <- 0
moj_cover[moj_cover == "Orange lichen"] <- 0
moj_cover[moj_cover == "UK"] <- "Other"
moj_cover[moj_cover == "UKI"] <- "Other"
moj_cover[moj_cover == "UK1"] <- "Other"
moj_cover[moj_cover == "UK2"] <- "Other"
moj_cover[moj_cover == "UNKNOWN SHRUB 3"] <- "Other"
moj_cover[moj_cover == "UNKNOWN SHRUB"] <- "Other"
moj_cover[moj_cover == "UNKNOWN MUSTARD"] <- "Other"
moj_cover[moj_cover == "UNKNOWN SHRUB 3"] <- "Other"
moj_cover[moj_cover == "UNKNOWN SHRUB 1"] <- "Other"
moj_cover[moj_cover == "UNKNOWN SHRUB 2"] <- "Other"
moj_cover[moj_cover == "ABBYSHRUB"] <- "Other"
moj_cover[moj_cover == "FAKE CRESTED WHEATGRASS"] <- "Other"
moj_cover[moj_cover == "DODDER"] <- "Other"
moj_cover[moj_cover == "ASTER"] <- "Other"
moj_cover[moj_cover == "Greenbush"] <- "Other"
moj_cover[moj_cover == "UNK2"] <- "Other"
moj_cover[moj_cover == "UNKOWN SHRUB 2"] <- "Other"
moj_cover[moj_cover == "BOTTLEBRUSH"] <- "Other"
moj_cover[moj_cover == "dePI"] <- "DEPI"
moj_cover[moj_cover == "UNKNOWN FORB 1"] <- "Other"
moj_cover[moj_cover == "UNKNOWN FORB"] <- "Other"
moj_cover[moj_cover == "BOTTLEBUSH"] <- "Other"
moj_cover[moj_cover == "BRITTLEBUSH"] <- "Other"
moj_cover[moj_cover == "FALSE BRITTLE"] <- "Other"
moj_cover[moj_cover == "UNKNOWN FORB"] <- "Other"

moj_cover[moj_cover == "litter"] <- "LITTER"
moj_cover[moj_cover == "Litter"] <- "LITTER"
moj_cover[moj_cover == "L"] <- "LITTER" 
moj_cover[moj_cover == "L "] <- "LITTER"
moj_cover[moj_cover == "[L]"] <- "LITTER"
moj_cover[moj_cover == "LTTER"] <- "LITTER"
moj_cover[moj_cover == "LITER"] <- "LITTER"
moj_cover[moj_cover == "Dead wood"] <- "LITTER"
moj_cover[moj_cover == "SOIL/LITTER"] <- "LITTER"
moj_cover[moj_cover == "DS/BRTE"] <- "BRTE"
moj_cover[moj_cover == "BRT"] <- "BRTE"
moj_cover[moj_cover == "RTE"] <- "BRTE"
moj_cover[moj_cover == "BRET"] <- "BRTE"
moj_cover[moj_cover == "CETE 5"] <- "CETE"
moj_cover[moj_cover == "CETE "] <- "CETE"
moj_cover[moj_cover == "CETTE"] <- "CETE"
moj_cover[moj_cover == "ELEL 5"] <- "ELEL"
moj_cover[moj_cover == "[ELEL]"] <- "ELEL"
moj_cover[moj_cover == "[ARTR]"] <- "ARTR"
moj_cover[moj_cover == "sage bush"] <- "ARTR"
moj_cover[moj_cover == "sage bush "] <- "ARTR"
moj_cover[moj_cover == "SAIL"] <- "SIAL"
moj_cover[moj_cover == "EERCI"] <- "ERCI"
moj_cover[moj_cover == "BRR"] <- "BRRU"
moj_cover[moj_cover == "ASNUU"] <- "ASNU"
moj_cover[moj_cover == "LETE3"] <- "LETE"
moj_cover[moj_cover == "SS"] <- 0
moj_cover[moj_cover == "SR"] <- 0
moj_cover[moj_cover == "D"] <- 0
moj_cover[moj_cover == "MC"] <- 0
moj_cover[moj_cover == "CNC"] <- 0
moj_cover[moj_cover == "MNC"] <- 0
moj_cover[moj_cover == "SNC"] <- 0
moj_cover[moj_cover == "CN"] <- 0
moj_cover[moj_cover == "SDS"] <- 0
moj_cover[moj_cover == "NS"] <- 0
moj_cover[moj_cover == "V"] <- 0
moj_cover[moj_cover == "DD"] <- 0
moj_cover[moj_cover == "BA"] <- 0
moj_cover[moj_cover == "ER"] <- 0
moj_cover[moj_cover == "WF"] <- 0

moj_cover[moj_cover == "LATR (stump) soil?"] <- 0
moj_cover[moj_cover == "CORA(stump) soil?"] <- 0
moj_cover[moj_cover == "S (YUBR)"] <- "YUBR"
moj_cover[moj_cover == "YUBR "] <- "YUBR"
moj_cover[moj_cover == "ROCK"] <- 0
moj_cover[moj_cover == "DUNG"] <- 0
moj_cover[moj_cover == "CAT CLAW"] <- "SEGR"
moj_cover[moj_cover == "CYAC8/DW"] <- "CYAC"
moj_cover[moj_cover == "CYAC8"] <- "CYAC"
moj_cover[moj_cover == "DEAD SHRUB"] <- 0
moj_cover[moj_cover == "ATCA2"] <- "ATCA"
moj_cover[moj_cover == "SALTBUSH"] <- "ATCA"
moj_cover[moj_cover == "BLADDPOD"] <- "CLAR"
moj_cover[moj_cover == "BLADDERPOD"] <- "CLAR"
moj_cover[moj_cover == "TUBR"] <- "YUBR"
moj_cover[moj_cover == "UBR"] <- "YUBR"
moj_cover[moj_cover == "AMBDU"] <- "AMDU"
moj_cover[moj_cover == "BR"] <- 0
moj_cover[moj_cover == "ASN"] <- "ASNU"
moj_cover[moj_cover == "CSCAR"] <- "SCAR"
moj_cover[moj_cover == "Winter Fat"] <- "KRLA"
moj_cover[moj_cover == "SERCI"] <- "ERCI"
moj_cover[moj_cover == "SCAQR"] <- "SCAR"
moj_cover[moj_cover == "NRRU"] <- "BRRU"
moj_cover[moj_cover == "STPA4"] <- "STPA"
moj_cover[moj_cover == "BRUU"] <- "BRRU"
moj_cover[moj_cover == "SKRAME"] <- "KRAME"
moj_cover[moj_cover == "KRAMG"] <- "KRAME"
moj_cover[moj_cover == "GILIA"] <- "Other"
moj_cover[moj_cover == "GILEA"] <- "Other"
moj_cover[moj_cover == "DFF"] <- 0
moj_cover[moj_cover == "KA"] <- 0
moj_cover[moj_cover == "GD"] <- 0
moj_cover[moj_cover == "NC"] <- 0
moj_cover[moj_cover == "CRA"] <- 0
moj_cover[moj_cover == "ALAL'"] <- "ALAL"

# Replace 'moj_cover' with your actual dataframe name
moj_presence <- moj_cover %>%
  mutate(across(CANOPY:BASAL, as.character))   # Ensure species columns are characters


# Get all unique species (excluding NA values)
species <- unique(unlist(moj_presence[, c("CANOPY", "X1ST.FOLIAR", "X2ND.FOLIAR", "third","fourth", "BASAL")]))
species <- species[!is.na(species)]  # Remove NA values

# Add species columns and initialize with 0
moj_presence[species] <- 0

head(moj_presence)

moj_presence <- moj_presence |>
  mutate(PLOT = ifelse(PLOT== "RN", "BN", PLOT))|>
  mutate(PLOT = ifelse(PLOT== "RS", "BS", PLOT))|>
  mutate(PLOT = ifelse(PLOT== "B?", "BS", PLOT))


# Create a vector of species columns (from BRTE onward)
species_columns <- colnames(moj_presence)[which(colnames(moj_presence) == "SCAR"):ncol(moj_presence)]

# Define the range of columns for Canopy to Basal
canopy_to_basal_columns <- c("CANOPY", "X1ST.FOLIAR", "X2ND.FOLIAR", "third","fourth", "BASAL")

# Loop through each row
for(i in 1:nrow(moj_presence)) {
  # Loop through each species column (BRTE onward)
  for(species in species_columns) {
    # If the species column name exists in any of the columns from Canopy to Basal, set it to 1
    if(species %in% moj_presence[i, canopy_to_basal_columns]) {
      moj_presence[i, species] <- 1
    }
  }
}

summary(moj_presence)


# Group by DATE, Block, and Plot, then calculate the percentage of occurrences
moj_summary <- moj_presence %>%
  group_by(Year, BLOCK, PLOT) 

moj_summary <- moj_summary |> select(-`0`)

moj_summary[is.na(moj_summary)] = 0
moj_summary$Total <- rowSums(moj_summary[,c(11:90)])
moj_summary$Total[moj_summary$Total >= 1] <- 1

moj_summary <- moj_summary %>%
  group_by(Year, BLOCK, PLOT) %>%
  summarise(
    row_count = n(),  # Count the number of rows for each group
    across(SCAR:Total, sum, na.rm = TRUE)  # Sum species columns
  )

write.csv(moj_summary, "mojcovertotals.csv")


moj_new <- moj_summary

# Loop through columns from BRTE onward
for (col in colnames(moj_summary)[which(colnames(moj_summary) == "SCAR"):ncol(moj_summary)]) {
  # For each row, divide the column value by row_count, multiply by 100, and round to 2 decimal places
  moj_new[[col]] <- round((moj_new[[col]] / moj_new$row_count) * 100, 2)
}
summary(moj_new)
write.csv(moj_new, "mojcoverpercent.csv")


# I need to make Mojave cover combined

moj_tc <- moj_new |> select(Year, BLOCK, PLOT, Total)
colnames(mojdata)
colnames(moj_tc) <- c("Year", "Block", "Plot", "Total.Cover")

mcc <- merge(moj_tc, mojplotdata, by= c("Year", "Block", "Plot"))
write.csv(mcc, "Moj_Cover.csv")
