# manually cleaned values for 2022-2025 studies
# used by 2-clean_overview.R in place of LLM prompts (see run_prompt.R)
# follows the conventions in the prompt notes/examples of run_prompt.R, e.g.:
# - subgroup sizes/counts are summed (e.g. '12+18'), subgroup means/SDs are 'NA'
# - fractions are percents for males/smokers/bop_perio ('0.7' -> '70')
# - a single reported median is used as the mean (SD 'NA')
# - 95% confidence intervals are not used as SDs
# - full-mouth (all sites) values are used over sampled sites

library(tibble)

manual_fixes <- list()

ids <- c(
  '2025-Embase-55', '2025-PubMed-14', '2025-PubMed-49', '2025-PubMed-55',
  '2025-PubMed-130', '2025-144', '2025-180', '2025-187', '2025-224', '2025-231',
  '2025-234', '2025-246', '2025-253', '2025-274', '2025-PubMed-357', '2025-PubMed-504'
)

# sample size ----

manual_fixes$group0_size <- tibble(
  Number = ids,
  clean_num = c('11', '23', '7', '24', '12', '17', '31', '20', '15', '14', '20', '60', '15', '20', '40', '10')
)

manual_fixes$group1_size <- tibble(
  Number = ids,
  clean_num = c('10', '16+24+32+9', '12+18', '21', '24', '33', '24', '20', '15', '8', '80', '120', '30', '40', '40', '10+10')
)

# sequencing ----

manual_fixes$seq_res <- tribble(
  ~Number, ~seq_type, ~`16s_regions`, ~seq_plat,
  '2025-Embase-55',  '16S', 'NA',        'NA',
  '2025-PubMed-14',  '16S', 'NA',        'NA',
  '2025-PubMed-49',  '16S', 'NA',        'NA',
  '2025-PubMed-55',  '16S', 'NA',        'NA',
  '2025-PubMed-130', '16S', '34',        'Illumina',
  '2025-144',        '16S', '3',         'Illumina',
  '2025-180',        '16S', '2346789',   'Ion Torrent',
  '2025-187',        '16S', '123456789', 'PacBio Vega (VS)/Revio (RS)/Sequel II',
  '2025-224',        '16S', 'NA',        'NA',
  '2025-231',        'WMS', 'NA',        'Illumina',
  '2025-234',        '16S', 'NA',        'NA',
  '2025-246',        'PCR', 'NA',        'RT-qPCR',
  '2025-253',        'WMS', 'NA',        'NA',
  '2025-274',        '16S', 'NA',        'NA',
  '2025-PubMed-357', 'WMS', 'NA',        'NA',
  '2025-PubMed-504', 'WMS', 'NA',        'Illumina'
)

# age ----

manual_fixes$age_overall <- tribble(
  ~Number, ~clean_num, ~clean_sd,
  '2025-Embase-55',  'NA',    'NA',
  '2025-PubMed-14',  '46.91', 'NA',
  '2025-PubMed-49',  'NA',    'NA',
  '2025-PubMed-55',  'NA',    'NA',
  '2025-PubMed-130', 'NA',    'NA',
  '2025-144',        '38.9',  'NA',
  '2025-180',        'NA',    'NA',
  '2025-187',        'NA',    'NA',
  '2025-224',        '45.73', '9.35',
  '2025-231',        '67.7',  '7.0',
  '2025-234',        '42.16', '7.91',
  '2025-246',        '45.53', '8.78',
  '2025-253',        '46.0',  '10.8',
  '2025-274',        '42.82', '8.71',
  '2025-PubMed-357', '43.81', 'NA',
  '2025-PubMed-504', 'NA',    'NA'
)

manual_fixes$age_health <- tribble(
  ~Number, ~clean_num, ~clean_sd,
  '2025-Embase-55',  '44.18', '8.14',
  '2025-PubMed-14',  '44.48', '8.57',
  '2025-PubMed-49',  '49.14', '6.49',
  '2025-PubMed-55',  '26.79', '5.56',
  '2025-PubMed-130', 'NA',    'NA',
  '2025-144',        '35.23', '2.07',
  '2025-180',        '25',    'NA',
  '2025-187',        '50.2',  'NA',
  '2025-224',        '44.46', '8.313',
  '2025-231',        '66.9',  '9.86',
  '2025-234',        '38.45', '5.82',
  '2025-246',        '45.4',  '9.66',
  '2025-253',        '46.6',  '11.3',
  '2025-274',        '42.90', '11.01',
  '2025-PubMed-357', '36.55', 'NA',
  '2025-PubMed-504', '25.70', '3.13'
)

manual_fixes$age_perio <- tribble(
  ~Number, ~clean_num, ~clean_sd,
  '2025-Embase-55',  '49.9',  '7.50',
  '2025-PubMed-14',  'NA',    'NA',
  '2025-PubMed-49',  'NA',    'NA',
  '2025-PubMed-55',  '43.28', '11.47',
  '2025-PubMed-130', 'NA',    'NA',
  '2025-144',        'NA',    'NA',
  '2025-180',        '53.5',  'NA',
  '2025-187',        '52.5',  'NA',
  '2025-224',        '47.2',  '10.798',
  '2025-231',        '68.4',  '6.95',
  '2025-234',        '43.09', '8.16',
  '2025-246',        '45.6',  '8.39',
  '2025-253',        '45.7',  '10.7',
  '2025-274',        '42.78', '7.66',
  '2025-PubMed-357', '51.45', 'NA',
  '2025-PubMed-504', 'NA',    'NA'
)

# males ----

manual_fixes$males_overall <- tribble(
  ~Number, ~clean_num, ~clean_percent,
  '2025-Embase-55',  '21', '51.2',
  '2025-PubMed-14',  '56', '45.1',
  '2025-PubMed-49',  '20', '39.2',
  '2025-PubMed-55',  '16', '35.5',
  '2025-PubMed-130', '10', '23.81',
  '2025-144',        'NA', 'NA',
  '2025-180',        '75', '66.96',
  '2025-187',        '18', '45',
  '2025-224',        '12', '40',
  '2025-231',        '23', '40.4',
  '2025-234',        '50', '50.0',
  '2025-246',        '81', '45.0',
  '2025-253',        '0',  '0',
  '2025-274',        '24', '40',
  '2025-PubMed-357', '38', '47.5',
  '2025-PubMed-504', '7',  '23.3'
)

manual_fixes$males_health <- tribble(
  ~Number, ~clean_num, ~clean_percent,
  '2025-Embase-55',  '6',  'NA',
  '2025-PubMed-14',  '14', '60.9',
  '2025-PubMed-49',  '1',  '14.3',
  '2025-PubMed-55',  '6',  '25',
  '2025-PubMed-130', '2',  '16.67',
  '2025-144',        '9',  '53',
  '2025-180',        '12', '38.7',
  '2025-187',        '8',  '40',
  '2025-224',        '8',  '53',
  '2025-231',        'NA', '28.6',
  '2025-234',        '11', '55',
  '2025-246',        '21', '35',
  '2025-253',        '0',  '0',
  '2025-274',        '6',  '30',
  '2025-PubMed-357', '17', '42.5',
  '2025-PubMed-504', '1',  '10.0'
)

manual_fixes$males_perio <- tribble(
  ~Number, ~clean_num, ~clean_percent,
  '2025-Embase-55',  '5',         '50',
  '2025-PubMed-14',  '7+10+16+5', '(7+10+16+5)/((7/.438)+(10/.417)+(16/.500)+(5/.556))*100',
  '2025-PubMed-49',  '7+12',      '(7+12)/((7/.583)+(12/.666))*100',
  '2025-PubMed-55',  '10',        '47',
  '2025-PubMed-130', '6',         '25.0',
  # stage I count missing ('???*')
  '2025-144',        'NA',        'NA',
  '2025-180',        '15',        '62.5',
  '2025-187',        '10',        '50',
  '2025-224',        '4',         '27',
  '2025-231',        'NA',        '37.5',
  '2025-234',        '39',        '48.75',
  '2025-246',        '60',        '50',
  '2025-253',        '0',         '0',
  '2025-274',        '18',        '45',
  '2025-PubMed-357', '21',        '52.5',
  '2025-PubMed-504', '6',         '30.0'
)

# smokers ----

manual_fixes$smokers_overall <- tribble(
  ~Number, ~clean_num, ~clean_percent,
  '2025-Embase-55',  '0',    '0',
  '2025-PubMed-14',  '29',   '23.4',
  '2025-PubMed-49',  '9',    '17.54',
  '2025-PubMed-55',  '9',    '20',
  '2025-PubMed-130', '0',    '0',
  '2025-144',        '0',    '0',
  # current and former smokers
  '2025-180',        '29+22', '(29+22)/((29/.2589)+(22/.1964))*100',
  '2025-187',        '8+5',   '(8+5)/((8/.20)+(5/.13))*100',
  '2025-224',        '0',    '0',
  '2025-231',        'NA',   'NA',
  '2025-234',        '0',    '0',
  '2025-246',        '38',   '21.1',
  '2025-253',        '0',    '0',
  '2025-274',        '11',   '18.33',
  '2025-PubMed-357', 'NA',   'NA',
  '2025-PubMed-504', '0',    '0'
)

manual_fixes$smokers_health <- tribble(
  ~Number, ~clean_num, ~clean_percent,
  '2025-Embase-55',  '0',  '0',
  '2025-PubMed-14',  '5',  '21.7',
  '2025-PubMed-49',  '0',  '0',
  '2025-PubMed-55',  '1',  '4',
  '2025-PubMed-130', '0',  '0',
  '2025-144',        '0',  '0',
  '2025-180',        '3',  '9.7',
  '2025-187',        '4',  '20',
  '2025-224',        '0',  '0',
  '2025-231',        'NA', 'NA',
  '2025-234',        '0',  '0',
  '2025-246',        '5',  '8.33',
  '2025-253',        '0',  '0',
  '2025-274',        '3',  '15',
  '2025-PubMed-357', 'NA', 'NA',
  '2025-PubMed-504', '0',  '0'
)

manual_fixes$smokers_perio <- tribble(
  ~Number, ~clean_num, ~clean_percent,
  '2025-Embase-55',  '0',       '0',
  '2025-PubMed-14',  '3+6+7+7', '(3+6+7+7)/((3/.188)+(6/.250)+(7/.219)+(7/.778))*100',
  # can't back-calculate subgroup size from 0 (0%) so use sizes (12 and 18)
  '2025-PubMed-49',  '0+9',     '(0+9)/(12+18)*100',
  '2025-PubMed-55',  '8',       '38',
  '2025-PubMed-130', '0',       '0',
  '2025-144',        '0',       '0',
  '2025-180',        '12',      '50.0',
  '2025-187',        '4',       '20',
  '2025-224',        '0',       '0',
  '2025-231',        'NA',      'NA',
  '2025-234',        '0',       '0',
  '2025-246',        '13',      '22.5',
  '2025-253',        '0',       '0',
  '2025-274',        '8',       '20',
  '2025-PubMed-357', 'NA',      'NA',
  '2025-PubMed-504', '0',       '0'
)

# bleeding on probing ----

manual_fixes$bop_health <- tribble(
  ~Number, ~clean_percent, ~clean_sd,
  '2025-Embase-55',  '0.82',  '0.25',
  '2025-PubMed-14',  '0.04',  '0.03',
  '2025-PubMed-49',  '18.10', '1.57',
  '2025-PubMed-55',  '23.17', '16.4',
  '2025-PubMed-130', '0',     'NA',
  '2025-144',        '0.05',  'NA',
  '2025-180',        '2.08',  'NA',
  '2025-187',        '4.45',  '2.93',
  '2025-224',        '11.10', '7.00',
  '2025-231',        'NA',    'NA',
  '2025-234',        '0.05',  'NA',
  '2025-246',        'NA',    'NA',
  '2025-253',        'NA',    'NA',
  '2025-274',        'NA',    'NA',
  '2025-PubMed-357', '22.5',  'NA',
  '2025-PubMed-504', '12.70', '2.31'
)

manual_fixes$bop_perio <- tribble(
  ~Number, ~clean_percent, ~clean_sd,
  '2025-Embase-55',  '3.10',  '0.39',
  '2025-PubMed-14',  'NA',    'NA',
  '2025-PubMed-49',  'NA',    'NA',
  '2025-PubMed-55',  '68.88', '17.22',
  '2025-PubMed-130', 'NA',    'NA',
  '2025-144',        'NA',    'NA',
  '2025-180',        '78.13', 'NA',
  '2025-187',        '52.40', '17.16',
  '2025-224',        '63.09', '28.40',
  '2025-231',        'NA',    'NA',
  '2025-234',        '97.50', '3.52',
  '2025-246',        'NA',    'NA',
  '2025-253',        'NA',    'NA',
  '2025-274',        'NA',    'NA',
  '2025-PubMed-357', '70',    'NA',
  '2025-PubMed-504', 'NA',    'NA'
)

# suppuration (not reported for any) ----

manual_fixes$supp_health <- tibble(Number = ids, clean_percent = 'NA', clean_sd = 'NA')
manual_fixes$supp_perio <- tibble(Number = ids, clean_percent = 'NA', clean_sd = 'NA')

# pocket depth ----

manual_fixes$pd_health <- tribble(
  ~Number, ~clean_num, ~clean_sd,
  '2025-Embase-55',  'NA',   'NA',
  '2025-PubMed-14',  '2.18', 'NA',
  '2025-PubMed-49',  '1.90', '0.05',
  '2025-PubMed-55',  '1.99', '0.16',
  '2025-PubMed-130', 'NA',   'NA',
  '2025-144',        '2.21', '0.27',
  '2025-180',        '2',    'NA',
  '2025-187',        '2.32', '0.29',
  '2025-224',        '2.34', '0.47',
  '2025-231',        'NA',   'NA',
  '2025-234',        '2.39', '0.27',
  '2025-246',        '2.2',  '0.25',
  '2025-253',        '1.13', '0.35',
  '2025-274',        'NA',   'NA',
  '2025-PubMed-357', '2.18', 'NA',
  '2025-PubMed-504', '2.25', '0.14'
)

manual_fixes$pd_perio <- tribble(
  ~Number, ~clean_num, ~clean_sd,
  '2025-Embase-55',  '4.51',  '0.42',
  '2025-PubMed-14',  'NA',    'NA',
  '2025-PubMed-49',  'NA',    'NA',
  '2025-PubMed-55',  '2.86',  '0.59',
  '2025-PubMed-130', 'NA',    'NA',
  '2025-144',        'NA',    'NA',
  '2025-180',        '6.33',  'NA',
  '2025-187',        '3.69',  '0.53',
  '2025-224',        '3.49',  '0.43',
  '2025-231',        'NA',    'NA',
  '2025-234',        '5.06',  '1.50',
  '2025-246',        '3.375', '0.75',
  '2025-253',        '7.47',  '0.86',
  '2025-274',        'NA',    'NA',
  '2025-PubMed-357', '5.67',  'NA',
  '2025-PubMed-504', 'NA',    'NA'
)

# clinical attachment loss ----

manual_fixes$cal_health <- tribble(
  ~Number, ~clean_num, ~clean_sd,
  '2025-Embase-55',  'NA',    'NA',
  '2025-PubMed-14',  '0.07',  'NA',
  '2025-PubMed-49',  '2.24',  '0.11',
  '2025-PubMed-55',  '1.01',  '0.85',
  '2025-PubMed-130', 'NA',    'NA',
  '2025-144',        '0',     'NA',
  '2025-180',        'NA',    'NA',
  '2025-187',        '2.38',  '0.34',
  '2025-224',        '0.29',  '0.52',
  '2025-231',        'NA',    'NA',
  '2025-234',        '0',     'NA',
  '2025-246',        '0.365', '0.255',
  '2025-253',        '1.0',   '0',
  '2025-274',        'NA',    'NA',
  '2025-PubMed-357', '1.52',  'NA',
  '2025-PubMed-504', '2.18',  '0.15'
)

manual_fixes$cal_perio <- tribble(
  ~Number, ~clean_num, ~clean_sd,
  '2025-Embase-55',  '4.64',  '0.61',
  '2025-PubMed-14',  'NA',    'NA',
  '2025-PubMed-49',  'NA',    'NA',
  '2025-PubMed-55',  '2.58',  '1.58',
  '2025-PubMed-130', 'NA',    'NA',
  '2025-144',        'NA',    'NA',
  '2025-180',        'NA',    'NA',
  '2025-187',        '4.45',  '0.91',
  '2025-224',        '3.31',  '0.73',
  '2025-231',        'NA',    'NA',
  '2025-234',        '3.68',  '1.70',
  '2025-246',        '3.725', '1.20',
  '2025-253',        '6.30',  '0.47',
  '2025-274',        'NA',    'NA',
  '2025-PubMed-357', '6.02',  'NA',
  '2025-PubMed-504', 'NA',    'NA'
)

# plaque ----

manual_fixes$plaque_health <- tribble(
  ~Number, ~clean_num, ~clean_sd,
  '2025-Embase-55',  'NA',    'NA',
  '2025-PubMed-14',  '0.45',  'NA',
  '2025-PubMed-49',  '0.21',  '0.04',
  '2025-PubMed-55',  'NA',    'NA',
  '2025-PubMed-130', 'NA',    'NA',
  '2025-144',        'NA',    'NA',
  '2025-180',        'NA',    'NA',
  '2025-187',        '6.85',  '2.52',
  '2025-224',        '12.87', '20.89',
  '2025-231',        'NA',    'NA',
  '2025-234',        'NA',    'NA',
  '2025-246',        'NA',    'NA',
  '2025-253',        'NA',    'NA',
  '2025-274',        'NA',    'NA',
  '2025-PubMed-357', 'NA',    'NA',
  '2025-PubMed-504', 'NA',    'NA'
)

manual_fixes$plaque_perio <- tribble(
  ~Number, ~clean_num, ~clean_sd,
  '2025-Embase-55',  'NA',    'NA',
  '2025-PubMed-14',  'NA',    'NA',
  '2025-PubMed-49',  'NA',    'NA',
  '2025-PubMed-55',  'NA',    'NA',
  '2025-PubMed-130', 'NA',    'NA',
  '2025-144',        'NA',    'NA',
  '2025-180',        'NA',    'NA',
  '2025-187',        '62.30', '16.80',
  '2025-224',        '52.50', '27.08',
  '2025-231',        'NA',    'NA',
  '2025-234',        'NA',    'NA',
  '2025-246',        'NA',    'NA',
  '2025-253',        'NA',    'NA',
  '2025-274',        'NA',    'NA',
  '2025-PubMed-357', 'NA',    'NA',
  '2025-PubMed-504', 'NA',    'NA'
)

stopifnot(all(sapply(manual_fixes, function(x) identical(x$Number, ids))))
