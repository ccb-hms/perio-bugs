library(readxl)
library(dplyr)

# Get all sheet names
overview_file <- 'Feres_PeriodontalMicrobiome/data/OVERVIEW_SHEET_RY.xlsx'
overview_sheets <- excel_sheets(overview_file)

# Read all sheets into a named list
overview <- lapply(overview_sheets, function(sheet) read_excel(overview_file, sheet = sheet))
names(overview) <- overview_sheets

# get correspondence between sheets
# need study number to connect different sheets
general_sheet <- overview$`General information`

perio_sheet <- overview$Periodontitis
perio_sheet <- perio_sheet[!is.na(perio_sheet$Number), ]
perio_sheet <- perio_sheet |> 
  rename(
    `Smokers (%)` = '...9',
    `Plaque (SD)` = '...17',
    `Bleeding on probing (SD)` = '...19',
    `Suppuration (SD)` = '...21',
    `Diagnosis (periodontitis)` = 'Diagnosis'
  ) 

health_sheet <- overview$`Periodontal health`
health_sheet <- health_sheet[!is.na(health_sheet$Number), ]
health_sheet$`Suppuration (SD)` <- NA

health_sheet <- health_sheet |> 
  rename(
    `Plaque (SD)`= '...17',
    `Bleeding on probing (SD)`= '...19',
    `PD OG (periodontal health)`= 'PD OG',
    # columns renamed to match perio_sheet
    `Clinical attachment loss/level (mm; mean+-SD)`= 'Cliniccal_Attaacchment_loss',
    `CAL. SD`= '...15',
    `Sample size (periodontal health)` = 'N',
    `Age mean` = 'Age',
    `Smokers (%)` = 'Smokers %'
  )

assessment_sheet <- overview$`Microbiological assessment`
assessment_sheet <- assessment_sheet[!is.na(assessment_sheet$Number), ]
assessment_sheet <- assessment_sheet |> 
  rename(
    `Elevated in health - Phylum`= "Species elevated in health (p<0.05)",
    `Elevated in health - Genera`= '...10',
    `Elevated in health - Species`= '...11',
    `Elevated in periodontitis - Phylum`= 'Species elevated in periodontitis (p<0.05)',
    `Elevated in periodontitis - Genera`= '...13',
    `Elevated in periodontitis - Species`= '...14',
    `Source`= '...20'
  )

# drop unused columns
assessment_sheet <- assessment_sheet[, -c(15:19)]

others_sheet <- overview$Others
others_sheet <- others_sheet[!is.na(others_sheet$Number), ]
others_sheet$...6 <- NULL

general_num <- general_sheet$Number

perio_num <- perio_sheet$Number
table(perio_num %in% general_num)

health_num <- health_sheet$Number
table(health_num %in% general_num)

assessment_num <- assessment_sheet$Number
table(assessment_num %in% general_num)

others_num <- others_sheet$Number
table(others_num %in% general_num)


# studies that are not in "General information" sheet but are in one of the other sheets
# NOTE: "Others" sheet documents these studies are excluded for various reasons
perio_num[!perio_num %in% general_num]
setequal(perio_num, health_num)
setequal(perio_num, others_num)

# have all studies that are in "General information" sheet
table(general_num %in% perio_num)

# get index to match between two sheets
idx_perio <- match(general_num, perio_num)
idx_health <- match(general_num, health_num)
idx_assessment <- match(general_num, assessment_num)
idx_others <- match(general_num, others_num)

all.equal(general_num, perio_num[idx_perio])
all.equal(general_num, health_num[idx_health])
all.equal(general_num, assessment_num[idx_assessment])
all.equal(general_num, others_num[idx_others])

# subset to same studies/order as "General information" sheet
perio_sheet <- perio_sheet[idx_perio,]
health_sheet <- health_sheet[idx_health,]
assessment_sheet <- assessment_sheet[idx_assessment,]
others_sheet <- others_sheet[idx_others,]

# drop columns that are duplicated across sheets
perio_sheet <- select(perio_sheet, -Number, -REFERENCE)
health_sheet <- select(health_sheet, -Number, -REFERENCE)
others_sheet <- select(others_sheet, -Number, -REFERENCE)
assessment_sheet <- select(assessment_sheet, -Number, -REFERENCE)

# disambiguate columns that are specific to periodontitis/health
common_cols <- intersect(colnames(perio_sheet), colnames(health_sheet))
disambiguate_cols <- function(col, group) paste0(col, ' (', group, ')')

perio_sheet <- rename_with(
  perio_sheet, 
  disambiguate_cols, 
  all_of(common_cols), 
  group = 'periodontitis')

health_sheet <- rename_with(
  health_sheet, 
  disambiguate_cols, 
  all_of(common_cols), 
  group = 'periodontal health')

# combine all sheets
df <- bind_cols(general_sheet, perio_sheet, health_sheet, assessment_sheet, others_sheet)

