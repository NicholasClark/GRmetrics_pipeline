# Level 2 to Level 3 script

# Breast cancer density project
# Level 2: LDS-1261 or HMS dataset 20256
# Level 3: LDS-1262 or HMS dataset 20257
# Level 4: LDS-1263 or HMS dataset 20258

library(readr)
library(dplyr)

# will incorrectly guess "Small Mol Concentration" as integer column unless you increase "guess_max"
lvl2 = read_tsv("Breast Cancer Density/Level 2/20256.txt", guess_max = 55000)
lvl3 = read_tsv("Breast Cancer Density/Level 3/20257.txt")
#lvl4 = read_tsv("Breast Cancer Density/Level 4/20258.txt")

## Average over technical replicates
# Simply take the mean of the cell counts and GR values for each unique combination of cell line, small molecule,
# concentration, replicate, and seeding density

lvl2_gp = lvl2 %>% group_by(`Cell HMS LINCS ID`, `Cell Name`, `Small Molecule HMS LINCS ID`, `Small Molecule Name`, `Small Mol Concentration`, `Small Mol Conc Unit`, Replicate, `Seeding Density`) %>%
  dplyr::summarise(
    `Mean Total Cell Count Before Treatment` = round(mean(`Total Cell Count Before Treatment`),2),
    `Mean Total Cell Count After Treatment` = round(mean(`Total Cell Count After Treatment`),2),
    `Mean Total Control Cell Count` = round(mean(`Total Control Cell Count`),2),
    `Mean Relative Cell Count` = round(mean(`Relative Cell Count`),4),
    `Mean Normalized Growth Rate Inhibition Value` = round(mean(`Normalized Growth Rate Inhibition Value`),4) ) %>%
  dplyr::filter(`Small Mol Concentration` != 0) %>% dplyr::arrange(`Cell Name`, `Small Molecule Name`, Replicate, `Seeding Density`, `Small Mol Concentration`) %>% ungroup %>% as.data.frame() %>% `rownames<-`(seq_len(nrow(lvl3)))

# Compare this to the published Level 3 data

lvl3_sort = lvl3 %>% arrange(`Cell Name`, `Small Molecule Name`, Replicate, `Seeding Density`, `Small Mol Concentration`) %>% as.data.frame() %>% `rownames<-`(seq_len(nrow(lvl3)))

all.equal(lvl3_sort, lvl2_gp)
# [1] "Attributes: < Component “spec”: Component “cols”: Names: 5 string mismatches >"   
# [2] "Component “Mean Total Control Cell Count”: Mean relative difference: 9.130227e-07"
# [3] "Component “Mean Relative Cell Count”: Mean relative difference: 0.0001155902"

# All columns are exactly the same, except for these too - small differences.
# only different for huge values - rounding error