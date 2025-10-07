# asked to strip asterix and sum period separated (e.g. '9.163' → 9+163=172)
sample_size_volunteers <- c(
  92, 251, 30, 402, 56, 60, 60, 54, 29, 225, 164, 225, 172, 334, 39, 306, 54, 
  125, 164, 167, 240, 30, 16, 40, 90, 88, 100, 224, 97, 30, 24, NA, 32, 160, 172,
  31, 20, 84, 9, 105, 37, 31, 110, 620, 95, 77, 41, 222, 163, 121, 111, 53, 19, 
  19, 40, 154, 114, 20, 60, 90, 69, 20, 539, 60, 40, 49, 84, 77, 64, 8, 15, 135,
  210, 74, 101, 146, 32, 82, 38, 113, 205, 98, 197, 51, 20, 370, 69, 51, 56, 48,
  47, 10, 132, 19, 56, 76, 58, NA, 55, 100, 19, 67, 99, 40, 142, 30, 95, 79, 203,
  42, 27, 69, 50, 50, 13, 5, 824, 16, 80
)

# Age Mean and SD for overall study ---- 
age_mean <- c(
  30,     # 1   "average 30 (range 19-40)"
  NA,     # 2   "18 and 70"
  NA,     # 3   "26.2±4.1 (20–29)24.5±5.1 (20–28)"
  NA,     # 4   "≥18 years"
  NA,     # 5   "26 ± 3     25 ± 4"
  43.8,   # 6   "43.8 years"
  NA,     # 7   NA
  NA,     # 8   NA
  NA,     # 9   NA
  NA,     # 10  NA
  NA,     # 11  NA
  NA,     # 12  "over 18 years"
  NA,     # 13  NA
  NA,     # 14  "14–80 years"
  NA,     # 15  NA
  38.33,  # 16  "38.33 ± 13.19"
  NA,     # 17  "45.6 ± 10 [HIV -, ...], 42.8 ± 11 [...], ..."
  NA,     # 18  "15.2 ± 2.8 [...], 25.2 ± 3.2 [...], ..."
  NA,     # 19  "Not determined for all subjects combined"
  NA,     # 20  "≥30"
  NA,     # 21  "20-60 (range)"
  NA,     # 22  "≥18"
  NA,     # 23  "≥35"
  NA,     # 24  "ND"
  NA,     # 25  "ND"
  NA,     # 26  "range: 35 - 55"
  36.2,   # 27  "36.2±21.3 (range: 18–70)..."
  NA,     # 28  "≥18"
  NA,     # 29  "≥18"
  NA,     # 30  "≤35"
  NA,     # 31  "range: 18-71"
  NA,     # 32  NA
  NA,     # 33  "ND"
  NA,     # 34  "16.7 ;  16.4"
  NA,     # 35  "13–20 yr"
  NA,     # 36  "not determined for all subjects combined"
  NA,     # 37  "not determined for all subjects combined"
  NA,     # 38  "26,9 ± 6,0  35,3 ± 9,1"
  29.44,  # 39  "29.44±9.33* (mean±SD); ..."
  64.8,   # 40  "64.8 ± 7.0"
  NA,     # 41  "range: 20-70"
  47.2,   # 42  "mean (±SD): 47.2 ± 8.3*, range: 33-63*"
  NA,     # 43  NA
  37.4,   # 44  "37,4 ± 12,9"
  NA,     # 45  NA
  NA,     # 46  "ND"
  NA,     # 47  "not determined for all"
  NA,     # 48  "Not determined for all subjects combined"
  NA,     # 49  "34.2 ± 6.2   54.4 ± 12.1   66.7 ± 1.5"
  NA,     # 50  "51.0 ± 10.2 CP 66.7 ± 1.5 H"
  NA,     # 51  NA
  NA,     # 52  "20–69 years"
  54.53,  # 53  "mean*:54.53(±11.93), range*: 32-70; ..."
  54.53,  # 54  "mean±SD*: 54.53±11.93; range: 32-70* ..."
  NA,     # 55  "38 ± 13 H 55 ± 12 P"
  NA,     # 56  "not determined for all subjects combined"
  48.1,   # 57  "48.1 years (SD 5.7 years)"
  47.4,   # 58  "mean (±SD): 47.4 ± 12.4*, range: 17-73*"
  42,     # 59  "42 ± 8.543 (range 34-68)"
  NA,     # 60  "ND"
  45.6,   # 61  "45.6 (range: 35-65)"
  NA,     # 62  "ND"
  NA,     # 63  "≥20"
  NA,     # 64  "21.1 ± 1.8 [healthy], 51.3 ± 12.0 [periodontitis]"
  NA,     # 65  "41–80 , 22–29 year"
  NA,     # 66  NA
  NA,     # 67  "20–45 years"
  NA,     # 68  NA
  NA,     # 69  NA
  NA,     # 70  "<24 years old"
  NA,     # 71  NA
  NA,     # 72  "not determined for all"
  NA,     # 73  "not determined for all"
  NA,     # 74  "25-63 (range)"
  40.77,  # 75  "40.77±14.15"
  NA,     # 76  "not determined for all subjects combined"
  NA,     # 77  "range: 17-81"
  NA,     # 78  "range: 30–65"
  NA,     # 79  "ND"
  NA,     # 80  "31 to 40"
  NA,     # 81  NA
  NA,     # 82  "not determined for all subjects combined"
  NA,     # 83  "18 to 46 years"
  43.3,   # 84  "mean (±SD): 43.3 ± 20.3*, range: 16-83* ..."
  NA,     # 85  "ND"
  NA,     # 86  ">25"
  30.4,   # 87  "30.4 years"
  68,     # 88  "68 ± 4.5"
  NA,     # 89  "not determined for all"
  NA,     # 90  "55.33 (5.23)  47.92 (6.53)"
  NA,     # 91  NA
  NA,     # 92  "39-63"
  NA,     # 93  "not determined for all subjects combined"
  NA,     # 94  "ND"
  NA,     # 95  "not determined for all"
  40.8,   # 96  "40.8 ± 12.5"
  NA,     # 97  "not determined for all"
  NA,     # 98  NA
  NA,     # 99  "32,32 ± 9,34  44,88 ± 11,94"
  NA,     # 100 "18 yrs and older"
  NA,     # 101 "ND"
  NA,     # 102 "34 ± 12 H 51 ± 11 PR"
  NA,     # 103 NA
  NA,     # 104 NA
  NA,     # 105 "20 and 40 yrs"
  NA,     # 106 "42  to  80  years"
  NA,     # 107 NA
  NA,     # 108 "21 to 80 yrs (mean, 44)"
  NA,     # 109 "20  - 87 years"
  NA,     # 110 NA
  NA,     # 111 NA
  NA,     # 112 "≥19"
  NA,     # 113 "ND"
  NA,     # 114 "ND"
  NA,     # 115 ">18"
  NA,     # 116 "range: 28-45"
  NA,     # 117 "24-82"
  NA,     # 118 "ND"
  NA      # 119 "range: 30-60"
)

age_sd <- c(
  NA,     # 1
  NA,     # 2
  NA,     # 3
  NA,     # 4
  NA,     # 5
  NA,     # 6
  NA,     # 7
  NA,     # 8
  NA,     # 9
  NA,     # 10
  NA,     # 11
  NA,     # 12
  NA,     # 13
  NA,     # 14
  NA,     # 15
  13.19,  # 16
  NA,     # 17
  NA,     # 18
  NA,     # 19
  NA,     # 20
  NA,     # 21
  NA,     # 22
  NA,     # 23
  NA,     # 24
  NA,     # 25
  NA,     # 26
  21.3,   # 27
  NA,     # 28
  NA,     # 29
  NA,     # 30
  NA,     # 31
  NA,     # 32
  NA,     # 33
  NA,     # 34
  NA,     # 35
  NA,     # 36
  NA,     # 37
  NA,     # 38
  9.33,   # 39
  7.0,    # 40
  NA,     # 41
  8.3,    # 42
  NA,     # 43
  12.9,   # 44
  NA,     # 45
  NA,     # 46
  NA,     # 47
  NA,     # 48
  NA,     # 49
  NA,     # 50
  NA,     # 51
  NA,     # 52
  11.93,  # 53
  11.93,  # 54
  NA,     # 55
  NA,     # 56
  5.7,    # 57
  12.4,   # 58
  8.543,  # 59
  NA,     # 60
  NA,     # 61
  NA,     # 62
  NA,     # 63
  NA,     # 64
  NA,     # 65
  NA,     # 66
  NA,     # 67
  NA,     # 68
  NA,     # 69
  NA,     # 70
  NA,     # 71
  NA,     # 72
  NA,     # 73
  NA,     # 74
  14.15,  # 75
  NA,     # 76
  NA,     # 77
  NA,     # 78
  NA,     # 79
  NA,     # 80
  NA,     # 81
  NA,     # 82
  NA,     # 83
  20.3,   # 84
  NA,     # 85
  NA,     # 86
  NA,     # 87
  4.5,    # 88
  NA,     # 89
  NA,     # 90
  NA,     # 91
  NA,     # 92
  NA,     # 93
  NA,     # 94
  NA,     # 95
  12.5,   # 96
  NA,     # 97
  NA,     # 98
  NA,     # 99
  NA,     # 100
  NA,     # 101
  NA,     # 102
  NA,     # 103
  NA,     # 104
  NA,     # 105
  NA,     # 106
  NA,     # 107
  NA,     # 108
  NA,     # 109
  NA,     # 110
  NA,     # 111
  NA,     # 112
  NA,     # 113
  NA,     # 114
  NA,     # 115
  NA,     # 116
  NA,     # 117
  NA,     # 118
  NA      # 119
)
# Age Mean and SD for periodontal health ----

age_mean_health <- c(
  NA,            # 1  "NA"
  45.8,          # 2  "45.8 ± 10.5"
  24.5,          # 3  "24.5±5.1 (20–28)"
  30.6,          # 4  "30.6 ± 1.5"
  26,            # 5  "26 ± 3"
  42.8,          # 6  "42.8 years"
  41.10,         # 7  "41.10 ± 8.5"
  42.8,          # 8  "42.8±11"
  NA,            # 9  "27 1.5" (ambiguous)
  34.3,          # 10 "34,3 ± 12"
  42,            # 11 "42 ± 13"
  34.3,          # 12 "34.3  ± 12"
  34.4,          # 13 "34.4 ± 11"
  24.6,          # 14 "24.6 ± 5.4"
  34,            # 15 "34 ± 0.6"
  25.01,         # 16 "25.01"
  42.8,          # 17 "42.8"
  21.6,          # 18 "21.6"
  42,            # 19 "42"
  45.1,          # 20 "45.1 ± 5.9 (probably mean and SD)"
  29.9,          # 21 "29.9 ± 9.7 (27.0) (Mean ± SD (Median))"
  23.3,          # 22 "23.3 ± 0.8"
  45.9,          # 23 "45.9 ± 9.9 (age range: 35–59)"
  38.3,          # 24 "38.3±6.8"
  33.5,          # 25 "33.5 ± 11.0"
  43.4,          # 26 "43.4 ± 3.9"
  18.04,         # 27 "18.04 ± 2.12"
  31.1,          # 28 "31.1 ± 11"
  24.2,          # 29 "24.2 ± 6.9"
  26.6,          # 30 "26.6 ± 3.6"
  38.4,          # 31 "38.4 ± 4.1 (range: 18-56)"
  27.9,          # 32 "27.9± 6.7"
  34.1,          # 33 "34.1 ± 5.5"
  16.4,          # 34 "16.4"
  NA,            # 35 "age range 13–20 yr"
  36.7,          # 36 "36.700000000000003"
  54,            # 37 "54"
  26.9,          # 38 "26,9 ± 6,0"
  23.33,         # 39 "23.33±2.52* (mean±SD); ..."
  64.8,          # 40 "64.8 ± 6.7"
  NA,            # 41 "range: 20-70"
  39.4,          # 42 "mean (±SD): 39.4 ± 2.5*, ..."
  26.90,         # 43 "26.90 ±7.17"
  28.9,          # 44 "28,9 ± 10,3"
  40.8,          # 45 "40.8 ± 11.9"
  28.3,          # 46 "28.3 ± 7.9 (range: 24–52)"
  37.5,          # 47 "37.5"
  46.6,          # 48 "46.6"
  66.7,          # 49 "66.7 ± 1.5"
  66.7,          # 50 "66.7 ± 1.5"
  66.6,          # 51 "66.6 ± 1.5"
  NA,            # 52 "29 to 69 years"
  52.4,          # 53 "52.4 ± 12.3"
  52.4,          # 54 "52.4 ± 12.3"
  38,            # 55 "38 ± 13"
  46,            # 56 "46"
  NA,            # 57 "ND"
  54.0,          # 58 "mean (±SD): 54.0 ± 10.6*, ..."
  NA,            # 59 "NA"
  NA,            # 60 "ND"
  33.4,          # 61 "33.4 (range: 18-60)"
  NA,            # 62 "21‐63 (range)"
  NA,            # 63 "≥20"
  21.1,          # 64 "21.1"
  26,            # 65 "mean age 26 ± 1.8"
  NA,            # 66 "20 to 23 years"
  29.0,          # 67 "29.0±5.6"
  27.8,          # 68 "27.8  ± 1.4"
  27.5,          # 69 "27,5 ± 1,3"
  NA,            # 70 "17.5 years (range 16–23)"
  NA,            # 71 "ND"
  43.2,          # 72 "43.2"
  40.4,          # 73 "40.4"
  NA,            # 74 "25-28 (range)"
  NA,            # 75 "NA"
  43,            # 76 "43"
  55.6,          # 77 "55.6 ± 13.08"
  44.95,         # 78 "44.95 ± 13.04"
  24.40,         # 79 "24.40 ± 3.54"
  53.2,          # 80 "53,2 ± 3,0"
  49.3,          # 81 "49.3"
  38.2,          # 82 "38.200000000000003"
  29.4,          # 83 "29,4 ± 6,8"
  27.7,          # 84 "27.7 (range: 16-58), ..."
  NA,            # 85 "ND"
  NA,            # 86 "NAA"
  28.2,          # 87 "28.2 ± 9.4"
  68.53,         # 88 "68.53 ± 4.57"
  45,            # 89 "45"
  47.92,         # 90 "47.92 (6.53)"
  40.6,          # 91 "40.6"
  47.2,          # 92 "47.2"
  47.9,          # 93 "47.9"
  NA,            # 94 "NA"
  39.45,         # 95 "39.450000000000003"
  31.5,          # 96 "31.5"
  54.1,          # 97 "54.1"
  NA,            # 98 "ND"
  32.32,         # 99 "32,32 ± 9,34"
  46,            # 100 "46 (mean)"
  NA,            # 101 "ND"
  34,            # 102 "34 ± 12"
  45,            # 103 "45"
  NA,            # 104 "21 to 65 years"
  26.9,          # 105 "26.9 ± 1.03"
  60.2,          # 106 "60.2  ± 11.3"
  25.2,          # 107 "25.2 ± 3.9"
  35,            # 108 "35 (21-54)"
  36,            # 109 "36±9"
  32.8,          # 110 "32.799999999999997"
  NA,            # 111 "20-28"
  31.9,          # 112 "31.9 (range: 19-79)"
  56,            # 113 "56 ± 3"
  NA,            # 114 "ND"
  NA,            # 115 ">18"
  NA,            # 116 "ND"
  37,            # 117 "37"
  NA,            # 118 "ND"
  NA             # 119 "32.0 (31.0–33.8) (median, interquartile range)"
)

age_sd_health <- c(
  NA,            # 1  "NA"
  10.5,          # 2  "45.8 ± 10.5"
  5.1,           # 3  "24.5±5.1 (20–28)"
  1.5,           # 4  "30.6 ± 1.5"
  3,             # 5  "26 ± 3"
  NA,            # 6  "42.8 years"
  8.5,           # 7  "41.10 ± 8.5"
  11,            # 8  "42.8±11"
  NA,            # 9  "27 1.5" (ambiguous)
  12,            # 10 "34,3 ± 12"
  13,            # 11 "42 ± 13"
  12,            # 12 "34.3  ± 12"
  11,            # 13 "34.4 ± 11"
  5.4,           # 14 "24.6 ± 5.4"
  0.6,           # 15 "34 ± 0.6"
  5.96,          # 16 original SD col (not from mean string)
  11,            # 17 original SD col
  NA,            # 18 original SD col
  13.00,         # 19 original SD col
  5.9,           # 20 from mean string "45.1 ± 5.9 (probably mean and SD)"
  9.7,           # 21 from mean string "29.9 ± 9.7 (27.0) (Mean ± SD (Median))"
  0.8,           # 22 from mean string "23.3 ± 0.8"
  9.9,           # 23 from mean string "45.9 ± 9.9 (age range: 35–59)"
  6.8,           # 24 from mean string "38.3±6.8"
  11,            # 25 from mean string "33.5 ± 11.0"
  3.9,           # 26 from mean string "43.4 ± 3.9"
  2.12,          # 27 from mean string "18.04 ± 2.12"
  11,            # 28 from mean string "31.1 ± 11"
  6.9,           # 29 from mean string "24.2 ± 6.9"
  3.6,           # 30 from mean string "26.6 ± 3.6"
  4.1,           # 31 from mean string "38.4 ± 4.1 (range: 18-56)"
  6.7,           # 32 from mean string "27.9± 6.7"
  5.5,           # 33 from mean string "34.1 ± 5.5"
  NA,            # 34 from original SD col
  NA,            # 35 from original SD col (range)
  9.1,           # 36 from original SD col
  NA,            # 37 from original SD col
  6,             # 38 from mean string "26,9 ± 6,0"
  2.52,          # 39 from mean string "23.33±2.52* (mean±SD)..."
  6.7,           # 40 from mean string "64.8 ± 6.7"
  NA,            # 41
  2.5,           # 42 from mean string "mean (±SD): 39.4 ± 2.5*"
  7.17,          # 43 from mean string
  10.3,          # 44 from mean string
  11.9,          # 45 from mean string
  7.9,           # 46 from mean string
  10.4,          # 47 original SD col
  10.9,          # 48 original SD col
  1.5,           # 49 from mean string
  1.5,           # 50 from mean string
  1.5,           # 51 from mean string
  NA,            # 52 from both cols
  12.3,          # 53 from mean string
  12.3,          # 54 from mean string
  13,            # 55 from mean string
  8,             # 56 original SD col
  NA,            # 57 no SD
  10.6,          # 58 from mean string
  NA,            # 59 from original SD col
  NA,            # 60 from original SD col
  NA,            # 61 only mean reported
  NA,            # 62 only mean reported
  NA,            # 63 only mean reported
  1.8,           # 64 from original SD col
  1.8,           # 65 from mean string
  NA,            # 66 no SD
  5.6,           # 67 from mean string
  1.4,           # 68 from mean string
  1.3,           # 69 from mean string
  NA,            # 70 no SD
  NA,            # 71 from both cols
  12.6,          # 72 original SD col
  11.9,          # 73 original SD col
  NA,            # 74 from both cols
  NA,            # 75 from both cols
  4,             # 76 original SD col
  13.08,         # 77 from mean string
  13.04,         # 78 from mean string
  3.54,          # 79 from mean string
  3,             # 80 from mean string
  NA,            # 81 from original SD col
  8.8,           # 82 original SD col
  6.8,           # 83 from mean string
  NA,            # 84 from both cols
  NA,            # 85 from both cols
  NA,            # 86 from both cols
  9.4,           # 87 from mean string
  4.57,          # 88 from mean string
  15,            # 89 original SD col
  6.53,          # 90 from mean string
  NA,            # 91 no SD
  8.29,          # 92 original SD col
  13.1,          # 93 original SD col
  NA,            # 94 from both cols
  10.85,         # 95 original SD col
  9.5,           # 96 original SD col
  12.4,          # 97 original SD col
  NA,            # 98 from both cols
  9.34,          # 99 from mean string
  NA,            # 100 from both cols (mean value only)
  NA,            # 101 from both cols
  12,            # 102 from mean string
  NA,            # 103 only mean
  NA,            # 104 only mean
  1.03,          # 105 from mean string
  11.3,          # 106 from mean string
  3.9,           # 107 from mean string
  NA,            # 108 no SD
  9,             # 109 from mean string
  NA,            # 110 no SD
  NA,            # 111 no SD
  NA,            # 112 no SD
  3,             # 113 from mean string
  NA,            # 114
  NA,            # 115
  NA,            # 116
  10,            # 117 from original SD col
  NA,            # 118
  NA             # 119
)

# Age Mean and SD for periodontitis ----

age_mean_perio <- c(
  NA,          # 1  "not reported"
  37.5,        # 2  "37.5± 11.1"
  26.2,        # 3  "26.2±4.1 (20–29)"
  NA,          # 4  "≥18 years"
  25,          # 5  "25 ± 4"
  44.7,        # 6  "44.7 years"
  42.03,       # 7  "42.03 ± 6.2"
  45.6,        # 8  "45.6±10"
  29,          # 9  "29 ± 1.2"
  41,          #10  "41 ± 14"
  47,          #11  "47 ± 10"
  41,          #12  "41 ±  14"
  43.4,        #13  "43.4 ±  9"
  42.6,        #14  "42.6 ± 10.9"
  41,          #15  "41 ± 2.0"
  52.44,       #16  "52.44" (SD filled below)
  45.6,        #17  "45.6"
  NA,          #18  multiple
  47,          #19  "47"
  46.8,        #20  "46.8 ± 6.1 (probably mean and SD)"
  46.5,        #21  "46.5 ± 9.0 (47.0)..."
  49.8,        #22  "49.8 ± 2.9"
  46.2,        #23  "46.2 ± 10.6 (range: 35–59)"
  42.2,        #24  "42.2±6.4"
  NA,          #25  multiple
  42.5,        #26  "42.5 ± 6.5"
  33.01,       #27  "33.01 ± 1.06"
  40.2,        #28  "40.2 ± 14"
  NA,          #29  multiple
  27.4,        #30  "27.4 ± 4.3"
  46.8,        #31  "46.8 ± 4.0 (range: 25-71)"
  40.8,        #32  "40.8±10"
  42.5,        #33  "42.5 ± 3.3"
  16.7,        #34  "16.7"
  NA,          #35  NA
  49.1,        #36  "49.1"
  40.7,        #37  "40.700000000000003"
  35.3,        #38  "35,3 ± 9,1"
  NA,          #39  multiple
  63.4,        #40  "63.4 (7.0)"
  NA,          #41  "range: 20-70"
  46.5,        #42  "mean (±SD): 46.5 ± 7.7*, ..."
  33.91,       #43  "33.91 ±9.32"
  45.6,        #44  "45,6 ± 10,6"
  22.8,        #45  "22.8 ± 8.5"
  NA,          #46  multiple
  47.2,        #47  "47.2"
  NA,          #48  multiple
  NA,          #49  multiple
  51,          #50  "51.0 ± 10.2"
  34.4,        #51  "34.4 ± 6.5"
  39.39,       #52  "39.39±10.47"
  56.9,        #53  "56.9 ± 11.8"
  56.9,        #54  "56.9 ± 11.8"
  55,          #55  "55 ± 12"
  50,          #56  "50"
  NA,          #57  NA
  40.7,        #58  "mean (±SD): 40.7 ± 10.6*"
  NA,          #59  "not reported"
  NA,          #60  "ND"
  48.3,        #61  "48.3 (range: 20-65)"
  NA,          #62  multiple
  NA,          #63  "≥20"
  51.3,        #64  "51.3"
  62,          #65  "62 ± 9.9 years"
  NA,          #66  "20 to 23 years"
  35.3,        #67  "35.3±3.2"
  48.3,        #68  "48.3 ± 1.7"
  44.9,        #69  "44,9 ± 1,7"
  17.8,          #70  "17.8 years (range 15–23)"
  NA,          #71  NA
  NA,          #72  multiple "moderate: 43.7 ± 11.5 / severe: 43.0 ± 8.8"
  42.4,        #73  "42.4"
  NA,          #74  "42-60 (range)"
  NA,          #75  "not reported"
  48,          #76  "48"
  56,          #77  "56 ± 10.14"
  NA,          #78  multiple
  23.74,       #79  "23.74 ± 0.84 (range: 13 to 30), ..."
  NA,          #80  NA
  51.8,        #81  "51.8"
  NA,          #82  multiple
  NA,          #83  NA
  59.4,          #84  multiple
  NA,          #85  multiple
  NA,          #86  ">25"
  NA,          #87  multiple
  68.40,       #88  "68.40 ± 3.13"
  45,          #89  "45 (32-59)"
  55.33,       #90  "55.33 (5.23)"
  NA,          #91  "20  to  55  years"
  48.2,        #92  "48.2"
  50.6,        #93  "50.6"
  NA,          #94  "ND"
  NA,          #95  multiple
  NA,          #96  multiple
  51.3,        #97  "51.3"
  NA,          #98  NA
  44.88,       #99  "44,88 ± 11,94"
  NA,          #100 "18+"
  NA,          #101 NA
  51,          #102 "51 ± 11"
  50,          #103 "50"
  NA,          #104 "‡21 years of age"
  31.7,        #105 "31.7 ± 1.04"
  63.6,        #106 "63.6 years ± 9.1"
  24.6,          #107 "24.6 (4.6, 14-32)"
  50,          #108 "50 (22-80)"
  46,          #109 "46±11"
  33.1,        #110 "33.1"
  NA,          #111 "31-65"
  52,          #112 "52.0 (range 25-79)"
  58,          #113 "58 ± 2 (probably mean ± SD)"
  NA,          #114 "ND"
  NA,          #115 ">18"
  NA,          #116 "ND"
  50,          #117 "50"
  NA,          #118 "ND"
  41.5         #119 "41.5 (35–47) (median, interquartile range)"
)

age_sd_perio <- c(
  NA,          # 1   no SD
  11.1,        # 2   from mean string
  4.1,         # 3   from mean string
  NA,          # 4   no SD
  4,           # 5   from mean string
  NA,          # 6   no SD
  6.2,         # 7   from mean string
  10,          # 8   from mean string
  1.2,         # 9   from mean string
  14,          #10   from mean string
  10,          #11   from mean string
  14,          #12   from mean string
  9,           #13   from mean string
  10.9,        #14   from mean string
  2,           #15   from mean string
  11.37,       #16   orig SD col
  10,          #17   orig SD col
  NA,          #18   mult/group
  10,          #19   orig SD col
  6.1,         #20   from mean string
  9,           #21   from mean string
  2.9,         #22   from mean string
  10.6,        #23   from mean string
  6.4,         #24   from mean string
  NA,          #25   multiple
  6.5,         #26   from mean string
  1.06,        #27   from mean string
  14,          #28   from mean string
  NA,          #29   multiple
  4.3,         #30   from mean string
  4,           #31   from mean string
  10,          #32   from mean string
  3.3,         #33   from mean string
  NA,          #34   no SD
  NA,          #35   NA
  8.6,         #36   orig SD col
  10.6,        #37   orig SD col
  9.1,         #38   from mean string
  NA,          #39   multiple
  7.0,         #40   from mean string (in parentheses)
  NA,          #41   range
  7.7,         #42   from mean string
  9.32,        #43   from mean string
  10.6,        #44   from mean string
  8.5,         #45   from mean string
  NA,          #46   multiple
  11.3,          #47   no SD
  NA,          #48   multiple
  NA,          #49   multiple
  10.2,        #50   from mean string
  6.5,         #51   from mean string
  10.47,       #52   from mean string
  11.8,        #53   from mean string
  11.8,        #54   from mean string
  12,          #55   orig SD col
  10,          #56   no SD
  NA,          #57   NA
  10.6,        #58   from mean string
  NA,          #59   not reported
  NA,          #60   ND
  NA,          #61   only mean/range
  NA,          #62   multiple
  NA,          #63   only mean
  12,          #64   only mean
  9.9,         #65   from mean string
  NA,          #66   only mean
  3.2,         #67   from mean string
  1.7,         #68   from mean string
  1.7,         #69   from mean string
  NA,          #70   only mean/range
  NA,          #71   NA
  NA,          #72   multiple
  9.8,          #73   only mean
  NA,          #74   range
  NA,          #75   not reported
  11,          #76   only mean
  10.14,       #77   from mean string
  NA,          #78   multiple
  0.84,        #79   from mean string
  NA,          #80   NA
  NA,          #81   only mean
  NA,          #82   multiple
  NA,          #83   NA
  NA,          #84   multiple
  NA,          #85   multiple
  NA,          #86   no SD
  NA,          #87   multiple
  3.13,        #88   from mean string
  NA,          #89   only mean/range
  5.23,        #90   from mean string
  NA,          #91   only mean/range
  10.04,       #92   orig SD col
  16.2,        #93   orig SD col
  NA,          #94   ND
  NA,          #95   multiple
  NA,          #96   multiple
  12.6,        #97   orig SD col
  NA,          #98   NA
  11.94,       #99   from mean string
  NA,          #100  "18+"
  NA,          #101  NA
  11,          #102  from mean string
  NA,          #103  only mean
  NA,          #104  only mean
  1.04,        #105  from mean string
  9.1,         #106  from mean string
  4.6,          #107  only mean
  NA,          #108  only mean
  11,          #109  from mean string
  NA,          #110  only mean
  NA,          #111  only mean
  NA,          #112  range
  2,           #113  from mean string
  NA,          #114  ND
  NA,          #115  no SD
  NA,          #116  ND
  11,          #117  orig SD col
  NA,          #118  ND
  NA           #119  range
)

# Males number and percentage for study -----

males_num <- c(
  0,         # "0, 0%"                       (1)
  89,        # "89(35.5%)"                   (2)
  27,        # "27 ( 45%)"                   (3)
  NA,        # "NA"                          (4)
  NA,        # "0.45"                        (5)
  0,         # "0"                           (6)
  NA,        # "NA"                          (7)
  NA,        # "NA"                          (8)
  NA,        # "NA"                          (9)
  NA,        # "NA"                          (10)
  NA,        # "NA"                          (11)
  NA,        # "NA"                          (12)
  NA,        # "NA"                          (13)
  133,       # "n=133"                       (14)
  NA,        # "NA"                          (15)
  96,        # "96, 31.4%*"                  (16)
  23,        # "23 (42.59%)*"                (17)
  40,        # "40 (32.0%)*"                 (18)
  53,        # "53, 33%"                     (19)
  78,        # "78 (46.71%)"                 (20)
  NA,        # "0.4042"                      (21)
  10,        # "10 (33.33%)*"                (22)
  NA,        # "ND"                          (23)
  9,         # "9 (22.50%)"                  (24)
  39,        # "39 (43.33%)"                 (25)
  47,        # "47 (53.41%)*"                (26)
  39,        # "39 (39.00%)"                 (27)
  76,        # "76 (33.93%)*"                (28)
  30,        # "30 (30.93%)"                 (29)
  17,        # "17 (56.67%)"                 (30)
  9,         # "9 (37.5%)*"                  (31)
  NA,        # "NA"                          (32)
  12,        # "12 (37.5%)*"                 (33)
  NA,        # "38 ; 53 %"                   (34)
  72,        # "n=72"                        (35)
  17,        # "17, 54.8%"                   (36)
  6,         # "6, 30%"                      (37)
  NA,        # "41,7  75"                    (38)
  3,         # "3 (33.33%)"                  (39)
  71,        # "71 (67.62%)*"                (40)
  NA,        # "ND"                          (41)
  14,        # "14 (45.16%)*"                (42)
  NA,        # "NA"                          (43)
  NA,        # "42.6"                        (44)
  NA,        # "NA"                          (45)
  24,        # "24 (31.17%)*"                (46)
  18,        # "18 (43.9%)*"                 (47)
  93,        # "93 (42.0%)*"                 (48)
  NA,        # "24 (38.7%)36 (43.9%)7 (36.8%)" (49)
  NA,        # "N=15 CP N=7 H"               (50)
  NA,        # "NA"                          (51)
  NA,        # "30.2%"                       (52)
  7,         # "7 (36.84%)"                  (53)
  7,         # "7 (36.84%)*"                 (54)
  NA,        # "40% H 35% P"                 (55)
  89,        # "89, 57.79%"                  (56)
  57,        # "n=57"                        (57)
  6,         # "6 (30.00%)*"                 (58)
  40,        # "40, 66.7%*"                  (59)
  NA,        # "ND"                          (60)
  31,        # "31 (45%)"                    (61)
  6,         # "6 (30%)"                     (62)
  NA,        # "ND"                          (63)
  21,        # "21 (35%)*"                   (64)
  NA,        # "NA"                          (65)
  NA,        # "NA"                          (66)
  NA,        # "NA"                          (67)
  NA,        # "NA"                          (68)
  NA,        # "NA"                          (69)
  NA,        # "0.375"                       (70)
  NA,        # "NA"                          (71)
  59,        # "59 (43.4%)*"                 (72)
  88,        # "88 (41.9%)*"                 (73)
  33,        # "33 (44.59%)*"                (74)
  56,        # "56 (55.45%)*"                (75)
  73,        # "73, 50%"                     (76)
  9,         # "9 (28.13%)*"                 (77)
  32,        # "32 (39.02%)*"                (78)
  NA,        # "ND"                          (79)
  55,        # "n=55"                        (80)
  97,        # "n=97"                        (81)
  38,        # "38 (38.78%)*"                (82)
  0,         # "0"                           (83)
  19,        # "19 (37.25%)*"                (84)
  NA,        # "ND"                          (85)
  NA,        # "ND"                          (86)
  25,        # "n=25"                        (87)
  27,        # "27 (52.94%)"                 (88)
  NA,        # "not determined"              (89)
  NA,        # "9 (75.0) 6 (50.0)"           (90)
  NA,        # "NA"                          (91)
  4,         # "4 (40.0%)*"                  (92)
  60,        # "60 (45.46%)*"                (93)
  NA,        # "ND"                          (94)
  23,        # "23 (47.9%)"                  (95)
  38,        # "38 (50.0%)"                  (96)
  29,        # "29 (50.0%)*"                 (97)
  NA,        # "NA"                          (98)
  NA,        # "0.39"                        (99)
  NA,        # "60%  46% 35%"                (100)
  NA,        # "ND"                          (101)
  NA,        # "20% H 82 % PR"               (102)
  NA,
  NA,        # "NA"                          (103)
  64,        # "n=64"                        (104)
  NA,        # "NA"                          (105)
  47,        # "n=47"                        (106)
  NA,        # "NA"                          (107)
  NA,        # "NA"                          (108)
  18,        # "n=18"                        (109)
  NA,        # "NA"                          (110)
  38,        # "38 (55.07%)"                 (111)
  NA,        # "ND"                          (112)
  NA,        # "ND"                          (113)
  NA,        # "ND"                          (114)
  NA,        # "ND"                          (115)
  413,       # "413 (50.12%)*"               (116)
  NA,        # "ND"                          (117)
  NA         # "ND"                          (118)
)

males_percent <- c(
  0,         # "0, 0%"                       (1)
  35.5,      # "89(35.5%)"                   (2)
  45,        # "27 ( 45%)"                   (3)
  NA,        # "NA"                          (4)
  NA,        # "0.45"                        (5)
  NA,        # "0"                           (6)
  NA,        # "NA"                          (7)
  NA,        # "NA"                          (8)
  NA,        # "NA"                          (9)
  NA,        # "NA"                          (10)
  NA,        # "NA"                          (11)
  NA,        # "NA"                          (12)
  NA,        # "NA"                          (13)
  NA,        # "n=133"                       (14)
  NA,        # "NA"                          (15)
  31.4,      # "96, 31.4%*"                  (16)
  42.59,     # "23 (42.59%)*"                (17)
  32.0,      # "40 (32.0%)*"                 (18)
  33,        # "53, 33%"                     (19)
  46.71,     # "78 (46.71%)"                 (20)
  NA,        # "0.4042"                      (21)
  33.33,     # "10 (33.33%)*"                (22)
  NA,        # "ND"                          (23)
  22.5,      # "9 (22.50%)"                  (24)
  43.33,     # "39 (43.33%)"                 (25)
  53.41,     # "47 (53.41%)*"                (26)
  39.00,     # "39 (39.00%)"                 (27)
  33.93,     # "76 (33.93%)*"                (28)
  30.93,     # "30 (30.93%)"                 (29)
  56.67,     # "17 (56.67%)"                 (30)
  37.5,      # "9 (37.5%)*"                  (31)
  NA,        # "NA"                          (32)
  37.5,      # "12 (37.5%)*"                 (33)
  NA,        # "38 ; 53 %"                   (34)
  NA,        # "n=72"                        (35)
  54.8,      # "17, 54.8%"                   (36)
  30,        # "6, 30%"                      (37)
  NA,        # "41,7  75"                    (38)
  33.33,     # "3 (33.33%)"                  (39)
  67.62,     # "71 (67.62%)*"                (40)
  NA,        # "ND"                          (41)
  45.16,     # "14 (45.16%)*"                (42)
  NA,        # "NA"                          (43)
  NA,        # "42.6"                        (44)
  NA,        # "NA"                          (45)
  31.17,     # "24 (31.17%)*"                (46)
  43.9,      # "18 (43.9%)*"                 (47)
  42.0,      # "93 (42.0%)*"                 (48)
  NA,        # "24 (38.7%)36 (43.9%)7 (36.8%)" (49)
  NA,        # "N=15 CP N=7 H"               (50)
  NA,        # "NA"                          (51)
  30.2,      # "30.2%"                       (52)
  36.84,     # "7 (36.84%)"                  (53)
  36.84,     # "7 (36.84%)*"                 (54)
  NA,        # "40% H 35% P"                 (55)
  57.79,     # "89, 57.79%"                  (56)
  NA,        # "n=57"                        (57)
  30,        # "6 (30.00%)*"                 (58)
  66.7,      # "40, 66.7%*"                  (59)
  NA,        # "ND"                          (60)
  45,        # "31 (45%)"                    (61)
  30,        # "6 (30%)"                     (62)
  NA,        # "ND"                          (63)
  35,        # "21 (35%)*"                   (64)
  NA,        # "NA"                          (65)
  NA,        # "NA"                          (66)
  NA,        # "NA"                          (67)
  NA,        # "NA"                          (68)
  NA,        # "NA"                          (69)
  NA,        # "0.375"                       (70)
  NA,        # "NA"                          (71)
  43.4,      # "59 (43.4%)*"                 (72)
  41.9,      # "88 (41.9%)*"                 (73)
  44.59,     # "33 (44.59%)*"                (74)
  55.45,     # "56 (55.45%)*"                (75)
  50,        # "73, 50%"                     (76)
  28.13,     # "9 (28.13%)*"                 (77)
  39.02,     # "32 (39.02%)*"                (78)
  NA,        # "ND"                          (79)
  NA,        # "n=55"                        (80)
  NA,        # "n=97"                        (81)
  38.78,     # "38 (38.78%)*"                (82)
  NA,        # "0"                           (83)
  37.25,     # "19 (37.25%)*"                (84)
  NA,        # "ND"                          (85)
  NA,        # "ND"                          (86)
  NA,        # "n=25"                        (87)
  52.94,     # "27 (52.94%)"                 (88)
  NA,        # "not determined"              (89)
  NA,        # "9 (75.0) 6 (50.0)"           (90)
  NA,        # "NA"                          (91)
  40.0,      # "4 (40.0%)*"                  (92)
  45.46,     # "60 (45.46%)*"                (93)
  NA,        # "ND"                          (94)
  47.9,      # "23 (47.9%)"                  (95)
  50.0,      # "38 (50.0%)"                  (96)
  50.0,      # "29 (50.0%)*"                 (97)
  NA,        # "NA"                          (98)
  NA,        # "0.39"                        (99)
  NA,        # "60%  46% 35%"                (100)
  NA,        # "ND"                          (101)
  NA,        # "20% H 82 % PR"               (102)
  NA,
  NA,        # "NA"                          (103)
  NA,        # "n=64"                        (104)
  NA,        # "NA"                          (105)
  NA,        # "n=47"                        (106)
  NA,        # "NA"                          (107)
  NA,        # "NA"                          (108)
  NA,        # "n=18"                        (109)
  NA,        # "NA"                          (110)
  55.07,     # "38 (55.07%)"                 (111)
  NA,        # "ND"                          (112)
  NA,        # "ND"                          (113)
  NA,        # "ND"                          (114)
  NA,        # "ND"                          (115)
  50.12,     # "413 (50.12%)*"               (116)
  NA,        # "ND"                          (117)
  NA         # "ND"                          (118)
)


# Males number and percent for health ---

males_num_health = c(
  0,        # 1, (n, %) column, "0"
  46,       # 2, (n, %) column, "46 (31,5%)"
  13,       # 3, (n, %) column, "n=13"
  NA,       # 4, (n, %) column, "0.37"
  NA,       # 5, (n, %) column, "0.45"
  0,        # 6, (n, %) column, "0% (exclusion criteria)"
  6,        # 7, (n, %) column, "N=6"
  NA,       # 8, (n, %) column, "0.5"
  NA,       # 9, (n, %) column, "0.44"
  NA,       # 10, (n, %) column, "0.35499999999999998"
  41,       # 11, (n, %) column, "41"
  NA,       # 12, (n, %) column, "35.5"
  NA,       # 13, (n, %) column, "0.43099999999999999"
  NA,       # 14, (n, %) column, "ND"
  NA,       # 15, (n, %) column, "0.14000000000000001"
  17,       # 16, (n, %) column, "17"
  7,        # 17, (n, %) column, "7"
  16,       # 18, (n, %) column, "16"
  20,       # 19, (n, %) column, "20"
  41,       # 20, (n, %) column, "41 (45.05%)"
  33,       # 21, (n, %) column, "33 (34.74%)"
  3,        # 22, (n, %) column, "3 (20%)"
  NA,       # 23, (n, %) column, "ND"
  1,        # 24, (n, %) column, "1 (10.00%)"
  12,       # 25, (n, %) column, "12 (40.00%)"
  11,       # 26, (n, %) column, "11 (50.0%)"
  5,        # 27, (n, %) column, "5 (35.7%)"
  17,       # 28, (n, %) column, "17 (31%)*"
  6,        # 29, (n, %) column, "6 (22.2%)*"
  9,        # 30, (n, %) column, "9 (60%)"
  3,        # 31, (n, %) column, "3 (27.27%)*"
  10,       # 32, (n, %) column, "N=10"
  2,        # 33, (n, %) column, "2 (20.0%)*"
  NA,       # 34, (n, %) column, "0.53"
  72,       # 35, (n, %) column, "n 72"
  8,        # 36, (n, %) column, "8"
  3,        # 37, (n, %) column, "3"
  NA,       # 38, (n, %) column, "0.41699999999999998"
  1,        # 39, (n, %) column, "1 (33.33%)*"
  11,       # 40, (n, %) column, "11 (44.0%)"
  NA,       # 41, (n, %) column, "ND"
  3,        # 42, (n, %) column, "3 (60.00%)*"
  12,       # 43, (n, %) column, "n=12"
  NA,       # 44, (n, %) column, "38.700000000000003"
  11,       # 45, (n, %) column, "n=11"
  2,        # 46, (n, %) column, "2 (18.18%)*"
  8,        # 47, (n, %) column, "8"
  39,       # 48, (n, %) column, "39"
  7,        # 49, (n, %) column, "7 (36.8%)"
  7,        # 50, (n, %) column, "N=7"
  8,        # 51, (n, %) column, "8 (38.1)"
  NA,       # 52, (n, %) column, "0.30020000000000002"
  4,        # 53, (n, %) column, "4 (40%)*"
  4,        # 54, (n, %) column, "4 (40%)*"
  NA,       # 55, (n, %) column, "0.4"
  26,       # 56, (n, %) column, "26"
  NA,       # 57, (n, %) column, "ND"
  3,        # 58, (n, %) column, "3 (30.00%)*"
  NA,       # 59, (n, %) column, NA
  NA,       # 60, (n, %) column, "ND"
  8,        # 61, (n, %) column, "8 (48%)*"
  3,        # 62, (n, %) column, "3 (37.5%)*"
  NA,       # 63, (n, %) column, "ND"
  15,       # 64, (n, %) column, "15"
  NA,       # 65, (n, %) column, "ND"
  NA,       # 66, (n, %) column, "ND"
  NA,       # 67, (n, %) column, "ND"
  NA,       # 68, (n, %) column, "0.57899999999999996"
  NA,       # 69, (n, %) column, "0.55000000000000004"
  1,        # 70, (n, %) column, "n=1"
  NA,       # 71, (n, %) column, "ND"
  26,       # 72, (n, %) column, "26"
  44,       # 73, (n, %) column, "44"
  9,        # 74, (n, %) column, "9 (40.90%)"
  21,       # 75, (n, %) column, "21"
  15,       # 76, (n, %) column, "15"
  3,        # 77, (n, %) column, "3 (33.33%)*"
  8,        # 78, (n, %) column, "8 (36.36%)*"
  NA,       # 79, (n, %) column, "ND"
  14,       # 80, (n, %) column, "n=14"
  34,       # 81, (n, %) column, "n=34"
  3,        # 82, (n, %) column, "3"
  0,        # 83, (n, %) column, "exclusion criteria"
  8,        # 84, (n, %) column, "8 (30.77%)*"
  NA,       # 85, (n, %) column, "ND"
  NA,       # 86, (n, %) column, "ND"
  NA,       # 87, (n, %) column, "ND"
  10,       # 88, (n, %) column, "10 (58.82%)"
  NA,       # 89, (n, %) column, NA
  6,        # 90, (n, %) column, "6 (50.0)"
  10,       # 91, (n, %) column, "n=10"
  2,        # 92, (n, %) column, "2"
  21,       # 93, (n, %) column, "21"
  NA,       # 94, (n, %) column, "ND"
  NA,       # 95, (n, %) column, NA
  8,        # 96, (n, %) column, "8"
  10,       # 97, (n, %) column, "10"
  NA,       # 98, (n, %) column, "ND"
  NA,       # 99, (n, %) column, "0.39"
  NA,       # 100, (n, %) column, "0.6"
  NA,       # 101, (n, %) column, "ND"
  NA,       # 102, (n, %) column, "0.2"
  NA,       # 103, (n, %) column, "0.56000000000000005"
  NA,       # 104, (n, %) column, "ND"
  8,        # 105, (n, %) column, "8 (29%)"
  NA,       # 106, (n, %) column, "0.71"
  28,       # 107, (n, %) column, "n=28"
  6,        # 108, (n, %) column, "n=6"
  NA,       # 109, (n, %) column, "0.26"
  9,        # 110, (n, %) column, "n=9"
  NA,       # 111, (n, %) column, "ND"
  15,       # 112, (n, %) column, "15 (71.43%)"
  NA,       # 113, (n, %) column, "0.71"
  NA,       # 114, (n, %) column, "ND"
  NA,       # 115, (n, %) column, "ND"
  NA,       # 116, (n, %) column, "ND"
  70,       # 117, (n, %) column, "70"
  NA,       # 118, (n, %) column, "ND"
  NA        # 119, (n, %) column, "45.9% of all males were in Controls group"
)

males_percent_health = c(
  0,                                # 1, % column, "0"
  31.5,                             # 2, (n, %) column, "46 (31,5%)"
  NA,                               # 3, no percent in either column
  37,                               # 4, (n, %) column (fraction), "0.37"
  45,                               # 5, (n, %) column (fraction), "0.45"
  0,                                # 6, (n, %) column (fraction in string), "0% (exclusion criteria)"
  NA,                               # 7, no percent in either column
  50,                               # 8, (n, %) column (fraction), "0.5"
  44,                               # 9, (n, %) column (fraction), "0.44"
  35.5,                            # 10, (n, %) column (fraction), "0.35499999999999998"
  NA,                               # 11, no percent in either column
  35.5,                            # 12, (n, %) column (fraction), "35.5"
  43.1,                            # 13, (n, %) column (fraction), "0.43099999999999999"
  NA,                               # 14, no percent in either column
  14,                               # 15, (n, %) column (fraction), "0.14000000000000001"
  32.1,                             # 16, % column, "32.1%*"
  50,                               # 17, % column, "50"
  NA,                               # 18, no percent in either column
  41,                               # 19, % column, "41"
  45.05,                            # 20, (n, %) column, "41 (45.05%)"
  34.74,                            # 21, (n, %) column, "33 (34.74%)"
  20,                               # 22, (n, %) column, "3 (20%)"
  NA,                               # 23, no percent in either column
  10,                               # 24, (n, %) column, "1 (10.00%)"
  40,                               # 25, (n, %) column, "12 (40.00%)"
  50,                               # 26, (n, %) column, "11 (50.0%)"
  35.7,                             # 27, (n, %) column, "5 (35.7%)"
  31,                               # 28, (n, %) column, "17 (31%)*"
  22.2,                             # 29, (n, %) column, "6 (22.2%)*"
  60,                               # 30, (n, %) column, "9 (60%)"
  27.27,                            # 31, (n, %) column, "3 (27.27%)*"
  NA,                               # 32, no percent in either column
  20,                               # 33, (n, %) column, "2 (20.0%)*"
  53,                               # 34, (n, %) column (fraction), "0.53"
  NA,                               # 35, no percent in either column
  53.3,                             # 36, % column, "0.53300000000000003"
  30,                               # 37, % column, "0.3"
  41.7,                             # 38, (n, %) column (fraction), "0.41699999999999998"
  33.33,                            # 39, (n, %) column, "1 (33.33%)*"
  44,                               # 40, (n, %) column, "11 (44.0%)"
  NA,                               # 41, no percent in either column
  60,                               # 42, (n, %) column, "3 (60.00%)*"
  NA,                               # 43, no percent in either column
  38.7,                             # 44, (n, %) column (fraction), "38.700000000000003"
  NA,                               # 45, no percent in either column
  18.18,                            # 46, (n, %) column, "2 (18.18%)*"
  53.3,                             # 47, % column, "53.3"
  48.1,                             # 48, % column, "48.1"
  36.8,                             # 49, (n, %) column, "7 (36.8%)"
  NA,                               # 50, no percent in either column
  38.1,                             # 51, (n, %) column, "8 (38.1)"
  30,                               # 52, (n, %) column (fraction), "0.30020000000000002"
  40,                               # 53, (n, %) column, "4 (40%)*"
  40,                               # 54, (n, %) column, "4 (40%)*"
  40,                               # 55, (n, %) column (fraction), "0.4"
  52,                               # 56, % column, "0.52"
  NA,                               # 57, no percent in either column
  30,                               # 58, (n, %) column, "3 (30.00%)*"
  NA,                               # 59, no percent in either column
  NA,                               # 60, no percent in either column
  48,                               # 61, (n, %) column, "8 (48%)*"
  37.5,                             # 62, (n, %) column, "3 (37.5%)*"
  NA,                               # 63, no percent in either column
  33.33,                            # 64, % column, "33.33"
  NA,                               # 65, no percent in either column
  NA,                               # 66, no percent in either column
  NA,                               # 67, no percent in either column
  57.9,                             # 68, (n, %) column (fraction), "0.57899999999999996"
  55,                               # 69, (n, %) column (fraction), "0.55000000000000004"
  NA,                               # 70, no percent in either column
  NA,                               # 71, no percent in either column
  47,                               # 72, % column, "47"
  46.8,                             # 73, % column, "46.8"
  40.9,                             # 74, (n, %) column, "9 (40.90%)"
  54.4,                             # 75, % column, "0.54400000000000004"
  41.7,                             # 76, % column, "0.41699999999999998"
  33.33,                            # 77, (n, %) column, "3 (33.33%)*"
  36.36,                            # 78, (n, %) column, "8 (36.36%)*"
  NA,                               # 79, no percent in either column
  NA,                               # 80, no percent in either column
  NA,                               # 81, no percent in either column
  30,                               # 82, % column, "0.3"
  0,                                # 83, (n, %) column, "exclusion criteria"
  30.77,                            # 84, (n, %) column, "8 (30.77%)*"
  NA,                               # 85, no percent in either column
  NA,                               # 86, no percent in either column
  NA,                               # 87, no percent in either column
  58.82,                            # 88, (n, %) column, "10 (58.82%)"
  NA,                               # 89, no percent in either column
  50,                               # 90, (n, %) column, "6 (50.0)"
  NA,                               # 91, no percent in either column
  40,                               # 92, % column, "40"
  32,                               # 93, % column, "0.32"
  NA,                               # 94, no percent in either column
  NA,                               # 95, no percent in either column
  61.5,                             # 96, % column, "61.5"
  34.5,                             # 97, % column, "34.5"
  NA,                               # 98, no percent in either column
  39,                               # 99, (n, %) column (fraction), "0.39"
  60,                               # 100, (n, %) column (fraction), "0.6"
  NA,                               # 101, no percent in either column
  20,                               # 102, (n, %) column (fraction), "0.2"
  56,                               # 103, (n, %) column (fraction), "0.56000000000000005"
  NA,                               # 104, no percent in either column
  29,                               # 105, (n, %) column, "8 (29%)"
  71,                               # 106, (n, %) column (fraction), "0.71"
  NA,                               # 107, no percent in either column
  NA,                               # 108, no percent in either column
  26,                               # 109, (n, %) column (fraction), "0.26"
  NA,                               # 110, no percent in either column
  NA,                               # 111, no percent in either column
  71.43,                            # 112, (n, %) column, "15 (71.43%)"
  71,                               # 113, (n, %) column (fraction), "0.71"
  NA,                               # 114, no percent in either column
  NA,                               # 115, no percent in either column
  NA,                               # 116, no percent in either column
  37,                               # 117, % column, "37"
  NA,                               # 118, no percent in either column
  45.9                              # 119, (n, %) column, "45.9% of all males were in Controls group"
)

# Males number and percentage for perio ----
males_num_perio = c(
  0,       # 1, (n, %) column, "0"
  43,      # 2, (n, %) column, "43 (41%)"
  14,      # 3, (n, %) column, "n=14"
  40,      # 4, (n, %) column, "40; 33%"
  NA,      # 5, (n, %) column, "0.44"
  0,       # 6, (n, %) column, "0"
  8,       # 7, (n, %) column, "N=8"
  NA,      # 8, (n, %) column, "0.34"
  NA,      # 9, (n, %) column, "0.16"
  NA,      # 10, (n, %) column, "0.34200000000000003"
  29,      # 11, (n, %) column, "29"
  NA,      # 12, (n, %) column, "34.2"
  NA,      # 13, (n, %) column, "59.2"
  NA,      # 14, (n, %) column, "NA"
  NA,      # 15, (n, %) column, "0.52"
  NA,      # 16, (n, %) column, "0.13"
  6,       # 17, (n, %) column, "6"
  6+10+8,  # 18, (n, %) column, "6 (40.0%)* [localised aggressive periodontitis],..." 
  34,      # 19, (n, %) column, "34"
  37,      # 20, (n, %) column, "37 (48.68%)"
  38,      # 21, (n, %) column, "38 (56%)"
  7,       # 22, (n, %) column, "7 (46.7%)"
  NA,      # 23, (n, %) column, "ND"
  8,       # 24, (n, %) column, "8 (26.67%)"
  14+13,   # 25, (n, %) column, "GAgP: 14 (46.67%), GChP: 13 (43.33%)"
  12,      # 26, (n, %) column, "12 (54.5%)"
  14,      # 27, (n, %) column, "14 (36.8%)"
  59,      # 28, (n, %) column, "59 (35%)*"
  9+11,    # 29, (n, %) column, "ChP: 9 (25.5%)*, AgP: 11 (44.1%)*"
  8,       # 30, (n, %) column, "8 (53.33%)"
  6,       # 31, (n, %) column, "6 (46.15%)*"
  8,       # 32, (n, %) column, "N=8"
  10,      # 33, (n, %) column, "10 (45.5%)"
  NA,      # 34, (n, %) column, "0.38"
  NA,      # 35, (n, %) column, "NA"
  9,       # 36, (n, %) column, "9"
  3,       # 37, (n, %) column, "3"
  NA,      # 38, (n, %) column, "0.75"
  2,       # 39, (n, %) column, "2 (33.33%)*"
  19,      # 40, (n, %) column, "19 (76.0%)"
  NA,      # 41, (n, %) column, "ND"
  3,       # 42, (n, %) column, "3 (50.00%)*"
  23,      # 43, (n, %) column, "n=23"
  NA,      # 44, (n, %) column, "46.5"
  13,      # 45, (n, %) column, "n=13"
  13+11,   # 46, (n, %) column, "CP: 13 (43.33%)*, AgP: 11 (26.92%)*"
  10,      # 47, (n, %) column, "10"
  30+24,   # 48, (n, %) column, "Ap 30, 38.5% / Cp 24, 38.5%"
  24,      # 49, (n, %) column, "24 (38.7%)36 (43.9%"
  15,      # 50, (n, %) column, "N=15"
  19,      # 51, (n, %) column, "19 (43.2)"
  NA,      # 52, (n, %) column, "NA"
  3,       # 53, (n, %) column, "3/9(33.3%)*"
  3,       # 54, (n, %) column, "3 (33.3%)*"
  NA,      # 55, (n, %) column, "0.35"
  41,      # 56, (n, %) column, "41"
  NA,      # 57, (n, %) column, "NA"
  3,       # 58, (n, %) column, "3 (30.00%)*"
  NA,      # 59, (n, %) column, "not reported"
  NA,      # 60, (n, %) column, "ND"
  13,      # 61, (n, %) column, "13 (60%)"
  1+2,     # 62, (n, %) column, "PCnoS group: 1 (16.67%), PCS group: 2 (33.33%)"
  NA,      # 63, (n, %) column, "ND"
  6,       # 64, (n, %) column, "6"
  NA,      # 65, (n, %) column, "NA"
  14,      # 66, (n, %) column, "n=14"
  NA,      # 67, (n, %) column, "NA"
  NA,      # 68, (n, %) column, "35.9"
  NA,      # 69, (n, %) column, "38.6"
  2,       # 70, (n, %) column, "n=2"
  NA,      # 71, (n, %) column, "NA"
  19+14,   # 72, (n, %) column, "moderate: 19 (37%) / severe: 14 (48%)"
  44,      # 73, (n, %) column, "44"
  13,      # 74, (n, %) column, "13 (43.33%)"
  35,      # 75, (n, %) column, "35"
  17,      # 76, (n, %) column, "17"
  4,       # 77, (n, %) column, "4 (66.67%)*"
  8+16,    # 78, (n, %) column, "NS-Perio: 8 (28.57%) / S-Perio: 16 (57.14%)"
  4+2+2,   # 79, (n, %) column, "4 (21.05%), (GAgP: 2 (22%)*, LAgP: 2 (23%)*)"
  41,      # 80, (n, %) column, "n=41"
  63,      # 81, (n, %) column, "n=63"
  NA,      # 82, (n, %) column, "0.02"
  NA,      # 83, (n, %) column, "NA"
  11,      # 84, (n, %) column, "11 (44.00%)*"
  8,       # 85, (n, %) column, "8 (80.00%)"
  NA,      # 86, (n, %) column, "ND"
  NA,      # 87, (n, %) column, "NA"
  6,       # 88, (n, %) column, "6 (40.00%)"
  NA,      # 89, (n, %) column, "not determined"
  9,       # 90, (n, %) column, "9 (75.0)"
  9,       # 91, (n, %) column, "n=9"
  2,       # 92, (n, %) column, "2"
  39,      # 93, (n, %) column, "39"
  NA,      # 94, (n, %) column, "ND"
  5+3,     # 95, (n, %) column, "active: 5 (41.7%)* / recession: 3 (60%)"
  17+13,   # 96, (n, %) column, "17 (44.7%) early periodontitis / 13 (52%) advanced periodontitis*"
  19,      # 97, (n, %) column, "19"
  NA,      # 98, (n, %) column, "NA"
  NA,      # 99, (n, %) column, "0.35"
  NA,      # 100, (n, %) column, "46-35%"
  NA,      # 101, (n, %) column, "NA"
  NA,      # 102, (n, %) column, "0.82"
  NA,      # 103, (n, %) column, "0.38"
  NA,      # 104, (n, %) column, "NA"
  23,      # 105, (n, %) column, "23 (55%)"
  73,      # 106, (n, %) column, "73"
  19,      # 107, (n, %) column, "n=19"
  23,      # 108, (n, %) column, "n=23"
  NA,      # 109, (n, %) column, "0.51"
  9,       # 110, (n, %) column, "n=9"
  NA,      # 111, (n, %) column, "NA"
  23,      # 112, (n, %) column, "23 (47.92%)"
  NA,      # 113, (n, %) column, "0.72"
  NA,      # 114, (n, %) column, "ND"
  NA,      # 115, (n, %) column, "ND"
  NA,      # 116, (n, %) column, "ND"
  343,     # 117, (n, %) column, "343"
  NA,      # 118, (n, %) column, "ND"
  NA       # 119, (n, %) column, "54.1% of all males were in Cases group"
)

males_percent_perio = c(
  0,        # 1, % column, "0"
  41,       # 2, (n, %) column, "43 (41%)"
  NA,       # 3, no percent in either column
  33,       # 4, (n, %) column, "40; 33%"
  44,       # 5, (n, %) column (fraction), "0.44"
  0,        # 6, (n, %) column, "0"
  NA,       # 7, no percent in either column
  34,       # 8, (n, %) column (fraction), "0.34"
  16,       # 9, (n, %) column (fraction), "0.16"
  34.2,     # 10, (n, %) column (fraction), "0.34200000000000003"
  NA,       # 11, no percent in either column
  34.2,     # 12, (n, %) column (fraction), "34.2"
  59.2,     # 13, (n, %) column (fraction), "59.2"
  NA,       # 14, no percent in either column
  52,       # 15, (n, %) column (fraction), "0.52"
  26,       # 16, % column, "26"
  34,       # 17, % column, "34"
  (6+10+8)/((6/0.4)+(10/0.4)+(8/0.267))*100, # 18, calculated, "6 (40.0%)* ..." 
  29,       # 19, % column, "29"
  48.68,    # 20, (n, %) column, "37 (48.68%)"
  56,       # 21, (n, %) column, "38 (56%)"
  46.7,     # 22, (n, %) column, "7 (46.7%)"
  NA,       # 23, no percent in either column
  26.67,    # 24, (n, %) column, "8 (26.67%)"
  (14+13) / ((14/0.4667)+(13/0.4333)) * 100, # 25, calculated, "GAgP: 14 (46.67%), GChP: 13 (43.33%)"
  54.5,     # 26, (n, %) column, "12 (54.5%)"
  36.8,     # 27, (n, %) column, "14 (36.8%)"
  35,       # 28, (n, %) column, "59 (35%)*"
  (9+11) / ((9/0.255)+(11/0.441)) * 100, # 29, calculated, "ChP: 9 (25.5%)*, AgP: 11 (44.1%)*"
  53.33,    # 30, (n, %) column, "8 (53.33%)"
  46.15,    # 31, (n, %) column, "6 (46.15%)*"
  NA,       # 32, no percent in either column
  45.5,     # 33, (n, %) column, "10 (45.5%)"
  38,       # 34, (n, %) column (fraction), "0.38"
  NA,       # 35, no percent in either column
  56.25,    # 36, % column, "56.25"
  30,       # 37, % column, "30"
  75,       # 38, (n, %) column (fraction), "0.75"
  33.33,    # 39, (n, %) column, "2 (33.33%)*"
  76,       # 40, (n, %) column, "19 (76.0%)"
  NA,       # 41, no percent in either column
  50,       # 42, (n, %) column, "3 (50.00%)*"
  NA,       # 43, no percent in either column
  46.5,     # 44, (n, %) column (fraction), "46.5"
  NA,       # 45, no percent in either column
  (13+11)/((13/0.4333)+(11/0.2692))*100, # 46, calculated, "CP: 13 (43.33%)*, AgP: 11 (26.92%)*"
  38.5,     # 47, % column, "38.5"
  38.5,     # 48, (n, %) column, "Ap 30, 38.5% / Cp 24, 38.5%"
  38.7,     # 49, (n, %) column, "24 (38.7%)36 (43.9%"
  NA,       # 50, no percent in either column
  43.2,     # 51, (n, %) column, "19 (43.2)"
  NA,       # 52, no percent in either column
  33.3,     # 53, (n, %) column, "3/9(33.3%)*"
  33.3,     # 54, (n, %) column, "3 (33.3%)*"
  35,       # 55, (n, %) column (fraction), "0.35"
  79,       # 56, % column, "79"
  NA,       # 57, no percent in either column
  30,       # 58, (n, %) column, "3 (30.00%)*"
  NA,       # 59, no percent in either column
  NA,       # 60, no percent in either column
  60,       # 61, (n, %) column, "13 (60%)"
  (1+2)/((1/0.1667)+(2/0.3333))*100, # 62, calculated, "PCnoS group: 1 (16.67%), PCS group: 2 (33.33%)"
  NA,       # 63, no percent in either column
  40,       # 64, % column, "40"
  NA,       # 65, no percent in either column
  NA,       # 66, no percent in either column
  NA,       # 67, no percent in either column
  35.9,     # 68, (n, %) column (fraction), "35.9"
  38.6,     # 69, (n, %) column (fraction), "38.6"
  NA,       # 70, no percent in either column
  NA,       # 71, no percent in either column
  (19+14)/((19/0.37)+(14/0.48))*100, # 72, calculated, "moderate: 19 (37%) / severe: 14 (48%)"
  37.9,     # 73, % column, "37.9"
  43.33,    # 74, (n, %) column, "13 (43.33%)"
  55.6,     # 75, % column, "55.6"
  40.5,     # 76, % column, "40.5"
  66.67,    # 77, (n, %) column, "4 (66.67%)*"
  (8+16)/((8/0.2857)+(16/0.5714))*100, # 78, calculated, "NS-Perio: 8 (28.57%) / S-Perio: 16 (57.14%)"
  (4+2+2)/((4/0.2105)+(2/0.22)+(2/0.23))*100, # 79, calculated, "4 (21.05%), (GAgP: 2 (22%)*, LAgP: 2 (23%)*)"
  NA,       # 80, no percent in either column
  NA,       # 81, no percent in either column
  28.57,    # 82, % column, "28.57"
  NA,       # 83, no percent in either column
  44,       # 84, (n, %) column, "11 (44.00%)*"
  80,       # 85, (n, %) column, "8 (80.00%)"
  NA,       # 86, no percent in either column
  NA,       # 87, no percent in either column
  40,       # 88, (n, %) column, "6 (40.00%)"
  NA,       # 89, no percent in either column
  75,       # 90, (n, %) column, "9 (75.0)"
  NA,       # 91, no percent in either column
  40,       # 92, % column, "40"
  59,       # 93, % column, "59"
  NA,       # 94, no percent in either column
  (5+3)/((5/0.417)+(3/0.6))*100, # 95, calculated, "active: 5 (41.7%)* / recession: 3 (60%)"
  (17+13)/((17/0.447)+(13/0.52))*100, # 96, calculated, "17 (44.7%) early periodontitis / 13 (52%) advanced periodontitis*"
  65.5,     # 97, % column, "65.5"
  NA,       # 98, no percent in either column
  35,       # 99, (n, %) column (fraction), "0.35"
  NA,       # 100, no percent in either column
  NA,       # 101, no percent in either column
  82,       # 102, (n, %) column (fraction), "0.82"
  38,       # 103, (n, %) column (fraction), "0.38"
  NA,       # 104, no percent in either column
  55,       # 105, (n, %) column, "23 (55%)"
  NA,       # 106, no percent in either column
  NA,       # 107, no percent in either column
  NA,       # 108, no percent in either column
  51,       # 109, (n, %) column (fraction), "0.51"
  NA,       # 110, no percent in either column
  NA,       # 111, no percent in either column
  47.92,    # 112, (n, %) column, "23 (47.92%)"
  72,       # 113, (n, %) column (fraction), "0.72"
  NA,       # 114, no percent in either column
  NA,       # 115, no percent in either column
  NA,       # 116, no percent in either column
  54,       # 117, % column, "54"
  NA,       # 118, no percent in either column
  54.1      # 119, (n, %) column, "54.1% of all males were in Cases group"
)
# Smokers number and percentage ----
smokers_num = c(
  41,        # 1, "41, 44.6%*", taken as first number before comma
  44,        # 2, "44 (17.5%)", number before ( ) 
  NA,        # 3, "NA", NA in source
  NA,        # 4, "NA", NA in source
  NA,        # 5, "NA", NA in source
  NA,        # 6, "ND", ND treated as NA
  NA,        # 7, "NA", NA in source
  NA,        # 8, "NA", NA in source
  NA,        # 9, "NA", NA in source
  NA,        # 10, "NA", NA in source
  NA,        # 11, "NA", NA in source
  NA,        # 12, "NA", NA in source
  NA,        # 13, "NA", NA in source
  NA,        # 14, "NA", NA in source
  NA,        # 15, "NA", NA in source
  0,         # 16, "Excluded", treated as zero (excluded = no smokers)
  0,         # 17, "0 (0.0%)", explicit 0
  0,         # 18, "0 (0.0%)", explicit 0
  0,         # 19, "0 (0.0%)", explicit 0
  0,         # 20, "0", explicit 0
  0,         # 21, "0", explicit 0
  0,         # 22, "0", explicit 0
  0,         # 23, "0", explicit 0
  0,         # 24, "0", explicit 0
  0,         # 25, "0", explicit 0
  19,        # 26, "19 (21.59%)*", number before ( ) 
  6,         # 27, "6 (6.00%)", number before ( )
  45,        # 28, "45 (20.09%)*", number before ( )
  16,        # 29, "16 (16.49%)", number before ( )
  0,         # 30, "0", explicit 0
  7,         # 31, "7 (29.17%)*", number before ( )
  NA,        # 32, "NA", NA in source
  0,         # 33, "0", explicit 0
  NA,        # 34, "NA", NA in source
  NA,        # 35, "NA", NA in source
  0,         # 36, "Excluded", treated as zero
  0,         # 37, "Excluded", treated as zero
  12.5,      # 38, "12,5  25", first number (note: could be 12.5 or 12,5, let's take as 12.5)
  0,         # 39, "0", explicit 0
  40,        # 40, "40 (38.10%)*", number before ( )
  NA,        # 41, "ND", ND as NA
  0,         # 42, "0", explicit 0
  NA,        # 43, "NA", NA in source
  NA,        # 44, "0.156", fraction reported = percent only
  NA,        # 45, "NA", NA in source
  NA,        # 46, "ND", ND as NA
  2,         # 47, "2 (4.9%)*", number before ( )
  59,        # 48, "59 (27.0%)*", number before ( )
  NA,        # 49, "NA", NA in source
  NA,        # 50, "ND", ND as NA
  NA,        # 51, "NA", NA in source
  NA,        # 52, "37.7 %", percent only, no n
  NA,        # 53, "ND", ND as NA
  NA,        # 54, "ND", ND as NA
  NA,        # 55, "50% H 40% P", multiple percent, not number
  50,        # 56, "50, 32.47%", first number
  13,        # 57, "n=13", n= style
  NA,        # 58, "ND", ND as NA
  15,        # 59, "15, 25%", first number before comma
  NA,        # 60, "ND", ND as NA
  NA,        # 61, "ND", ND as NA
  6,         # 62, "6 (30%)", number before ( )
  0,         # 63, "0", explicit 0
  NA,        # 64, "ND", ND as NA
  NA,        # 65, "NA", NA in source
  NA,        # 66, "NA", NA in source
  NA,        # 67, "NA", NA in source
  NA,        # 68, "NA", NA in source
  NA,        # 69, "NA", NA in source
  NA,        # 70, "NA", NA in source
  NA,        # 71, "NA", NA in source
  62,        # 72, "62 (46.2%)*", number before ( )
  NA,        # 73, "not determined", not determined
  NA,        # 74, "ND", ND as NA
  0,         # 75, "Excluded", treated as zero
  43,        # 76, "43, 29.5%", first number before comma
  0,         # 77, "0", explicit 0
  32,        # 78, "32 (39.02%)*", number before ( )
  0,         # 79, "0", explicit 0
  32,        # 80, "n=32", n=style
  NA,        # 81, "NA", NA in source
  24,        # 82, "24 (24.49%)*", number before ( )
  NA,        # 83, "0.34100000000000003", fraction = percent only
  20,        # 84, "20 (39.22%)*", number before ( )
  0,         # 85, "0", explicit 0
  NA,        # 86, "ND", ND as NA
  NA,        # 87, "NA", NA in source
  0,         # 88, "0", explicit 0
  NA,        # 89, "not determined", not determined
  1,         # 90, "1 (8.3)", number before ( )
  NA,        # 91, "NA", NA in source
  0,         # 92, "0 (0.0%)", explicit 0
  NA,        # 93, "not reported", not reported
  NA,        # 94, "ND", ND as NA
  18,        # 95, "18 (37.6%)", number before ( )
  NA,        # 96, "not determined", not determined
  6,         # 97, "6 (10.3%)*", number before ( )
  NA,        # 98, "NA", NA in source
  NA,        # 99, "NA", NA in source
  NA,        # 100, "0 , 38 %, 80%", only percents
  NA,        # 101, "NA", NA in source
  NA,        # 102, "NA", NA in source
  NA,        # 103, "NA", NA in source
  NA,        # 104, "NA", NA in source
  23,        # 105, "n=23", n=style
  NA,        # 106, "NA", NA in source
  0,         # 107, "0", explicit 0
  NA,        # 108, "NA", NA in source
  NA,        # 109, "NA", NA in source
  NA,        # 110, "NA", NA in source
  NA,        # 111, "NA", NA in source
  NA,        # 112, "ND", ND as NA
  0,         # 113, "0", explicit 0
  NA,        # 114, "ND", ND as NA
  0,         # 115, "0", explicit 0
  NA,        # 116, "ND", ND as NA
  202,       # 117, "202, 25%*", first number before comma
  NA,        # 118, "ND", ND as NA
  NA         # 119, "ND", ND as NA
)

smokers_percent = c(
  44.6,       # 1, "41, 44.6%*", percent after comma
  17.5,       # 2, "44 (17.5%)", in ( )
  NA,         # 3, "NA", NA in source
  NA,         # 4, "NA", NA in source
  NA,         # 5, "NA", NA in source
  NA,         # 6, "ND", ND as NA
  NA,         # 7, "NA", NA in source
  NA,         # 8, "NA", NA in source
  NA,         # 9, "NA", NA in source
  NA,         # 10, "NA", NA in source
  NA,         # 11, "NA", NA in source
  NA,         # 12, "NA", NA in source
  NA,         # 13, "NA", NA in source
  NA,         # 14, "NA", NA in source
  NA,         # 15, "NA", NA in source
  0,          # 16, "Excluded", treat Excluded as 0% for smokers
  0,          # 17, "0 (0.0%)", 0 percent
  0,          # 18, "0 (0.0%)", 0 percent
  0,          # 19, "0 (0.0%)", 0 percent
  0,          # 20, "0", assumed 0 if count is 0
  0,          # 21, "0", assumed 0 if count is 0
  0,          # 22, "0", assumed 0 if count is 0
  0,          # 23, "0", assumed 0 if count is 0
  0,          # 24, "0", assumed 0 if count is 0
  0,          # 25, "0", assumed 0 if count is 0
  21.59,      # 26, "19 (21.59%)*", from ( )
  6,          # 27, "6 (6.00%)", from ( )
  20.09,      # 28, "45 (20.09%)*", from ( )
  16.49,      # 29, "16 (16.49%)", from ( )
  0,          # 30, "0", assumed 0
  29.17,      # 31, "7 (29.17%)*", from ( )
  NA,         # 32, "NA", NA in source
  0,          # 33, "0", 0
  NA,         # 34, "NA", NA in source
  NA,         # 35, "NA", NA in source
  0,          # 36, "Excluded", treat Excluded as 0% for smokers
  0,          # 37, "Excluded", treat Excluded as 0% for smokers
  25,         # 38, "12,5  25", percent last number (whitespace separated numbers, 25 taken as percent)
  NA,         # 39, "0", no percent but 0 smoker so 0
  38.10,      # 40, "40 (38.10%)*", from ( )
  NA,         # 41, "ND", ND as NA
  0,          # 42, "0", 0
  NA,         # 43, "NA", NA in source
  15.6,       # 44, "0.156", 0.156 as percent = 15.6
  NA,         # 45, "NA", NA in source
  NA,         # 46, "ND", ND as NA
  4.9,        # 47, "2 (4.9%)*", from ( )
  27.0,       # 48, "59 (27.0%)*", from ( )
  NA,         # 49, "NA", NA in source
  NA,         # 50, "ND", ND as NA
  NA,         # 51, "NA", NA in source
  37.7,       # 52, "37.7 %", percent only
  NA,         # 53, "ND", ND as NA
  NA,         # 54, "ND", ND as NA
  NA,         # 55, "50% H 40% P", two % not overall, leave NA
  32.47,      # 56, "50, 32.47%", take percent after comma
  NA,         # 57, "n=13", no percent
  NA,         # 58, "ND", ND as NA
  25,         # 59, "15, 25%", percent after comma
  NA,         # 60, "ND", ND as NA
  NA,         # 61, "ND", ND as NA
  30,         # 62, "6 (30%)", percent in ( )
  0,          # 63, "0", 0
  NA,         # 64, "ND", ND as NA
  NA,         # 65, "NA", NA in source
  NA,         # 66, "NA", NA in source
  NA,         # 67, "NA", NA in source
  NA,         # 68, "NA", NA in source
  NA,         # 69, "NA", NA in source
  NA,         # 70, "NA", NA in source
  NA,         # 71, "NA", NA in source
  46.2,       # 72, "62 (46.2%)*", from ( )
  NA,         # 73, "not determined", unknown
  NA,         # 74, "ND", ND as NA
  0,          # 75, "Excluded", treat Excluded as 0% for smokers
  29.5,       # 76, "43, 29.5%", percent after comma
  0,          # 77, "0", 0
  39.02,      # 78, "32 (39.02%)*", from ( )
  0,          # 79, "0", 0
  NA,         # 80, "n=32", no percent
  NA,         # 81, "NA", NA in source
  24.49,      # 82, "24 (24.49%)*", from ( )
  34.1,       # 83, "0.34100000000000003", fraction to percent
  39.22,      # 84, "20 (39.22%)*", from ( )
  0,          # 85, "0", 0
  NA,         # 86, "ND", ND as NA
  NA,         # 87, "NA", NA in source
  0,          # 88, "0", 0
  NA,         # 89, "not determined", unknown
  8.3,        # 90, "1 (8.3)", percent in ( )
  NA,         # 91, "NA", NA in source
  0,          # 92, "0 (0.0%)", 0
  NA,         # 93, "not reported", unknown
  NA,         # 94, "ND", ND as NA
  37.6,       # 95, "18 (37.6%)", percent in ( )
  NA,         # 96, "not determined", unknown
  10.3,       # 97, "6 (10.3%)*", percent in ( )
  NA,         # 98, "NA", NA in source
  NA,         # 99, "NA", NA in source
  NA,         # 100, "0 , 38 %, 80%", only percents, no n
  NA,         # 101, "NA", NA in source
  NA,         # 102, "NA", NA in source
  NA,         # 103, "NA", NA in source
  NA,         # 104, "NA", NA in source
  NA,         # 105, "n=23", no percent
  NA,         # 106, "NA", NA in source
  0,          # 107, "0", 0
  NA,         # 108, "NA", NA in source
  NA,         # 109, "NA", NA in source
  NA,         # 110, "NA", NA in source
  NA,         # 111, "NA", NA in source
  NA,         # 112, "ND", ND as NA
  0,          # 113, "0", 0
  NA,         # 114, "ND", ND as NA
  0,          # 115, "0", 0
  NA,         # 116, "ND", ND as NA
  25,         # 117, "202, 25%*", percent after comma
  NA,         # 118, "ND", ND as NA
  NA          # 119, "ND", ND as NA
)

# Smokers number and percentage (health) ----

smokers_num_health = c(
  7,        # 1, (n,%) column, "7"
  18,       # 2, (n,%) column, "18 (12.3%)"
  NA,       # 3, (n,%) column, "ND"
  NA,       # 4, (n,%) column, "0.11600000000000001", fraction only
  0,        # 5, (n,%) column, "exclusion criteria", exclusion criteria implies zero
  NA,       # 6, (n,%) column, "ND"
  0,        # 7, (n,%) column, "exclusion criteria", exclusion criteria implies zero
  0,        # 8, (n,%) column, "exclusion criteria", exclusion criteria implies zero
  0,        # 9, (n,%) column, "exclusion criteria", exclusion criteria implies zero
  0,        # 10, (n,%) column, "exclusion criteria", exclusion criteria implies zero
  0,        # 11, (n,%) column, "exclusion criteria", exclusion criteria implies zero
  0,        # 12, (n,%) column, "exclusion criteria", exclusion criteria implies zero
  0,        # 13, (n,%) column, "0"
  NA,       # 14, (n,%) column, "ND"
  NA,       # 15, (n,%) column, "0.15", fraction only
  0,        # 16, (n,%) column, "Excluded", Excluded implies zero
  0,        # 17, (n,%) column, "0"
  0,        # 18, (n,%) column, "0"
  0,        # 19, (n,%) column, "0"
  0,        # 20, (n,%) column, "0"
  0,        # 21, (n,%) column, "0"
  0,        # 22, (n,%) column, "0"
  0,        # 23, (n,%) column, "0"
  0,        # 24, (n,%) column, "0"
  0,        # 25, (n,%) column, "0"
  4,        # 26, (n,%) column, "4 (18.2%)"
  0,        # 27, (n,%) column, "0"
  3,        # 28, (n,%) column, "3 (6%)*"
  3,        # 29, (n,%) column, "3 (11.1%)*"
  0,        # 30, (n,%) column, "0"
  1,        # 31, (n,%) column, "1 (9.09%)*"
  NA,       # 32, (n,%) column, "ND"
  0,        # 33, (n,%) column, "0"
  NA,       # 34, (n,%) column, "ND"
  NA,       # 35, (n,%) column, "ND"
  0,        # 36, (n,%) column, "Excluded", Excluded implies zero
  0,        # 37, (n,%) column, "Excluded", Excluded implies zero
  12.5,     # 38, (n,%) column, "12.5"
  0,        # 39, (n,%) column, "0"
  5,        # 40, (n,%) column, "5 (20.0%)"
  NA,       # 41, (n,%) column, "ND"
  0,        # 42, (n,%) column, "0"
  4,        # 43, (n,%) column, "n=4"
  NA,       # 44, (n,%) column, "9.1999999999999998E-2", fraction only
  NA,       # 45, (n,%) column, "ND"
  NA,       # 46, (n,%) column, "ND"
  0,        # 47, (n,%) column, "0"
  18,       # 48, (n,%) column, "18"
  NA,       # 49, (n,%) column, "ND"
  NA,       # 50, (n,%) column, "ND"
  NA,       # 51, (n,%) column, "ND"
  NA,       # 52, (n,%) column, "37.65±10.88", not a count or percent
  NA,       # 53, (n,%) column, "ND"
  NA,       # 54, (n,%) column, "ND"
  NA,       # 55, (n,%) column, "0.5", fraction only
  12,       # 56, (n,%) column, "12"
  NA,       # 57, (n,%) column, "ND"
  NA,       # 58, (n,%) column, "ND"
  NA,       # 59, (n,%) column, "not reported"
  NA,       # 60, (n,%) column, "ND"
  NA,       # 61, (n,%) column, "ND"
  0,        # 62, (n,%) column, "0"
  0,        # 63, (n,%) column, "0"
  NA,       # 64, (n,%) column, "ND"
  NA,       # 65, (n,%) column, "ND"
  NA,       # 66, (n,%) column, "ND"
  0,        # 67, (n,%) column, "0"
  0,        # 68, (n,%) column, "0"
  0,        # 69, (n,%) column, "0"
  NA,       # 70, (n,%) column, "ND"
  NA,       # 71, (n,%) column, "ND"
  19,       # 72, (n,%) column, "19 (35%)*"
  NA,       # 73, (n,%) column, "not determined"
  NA,       # 74, (n,%) column, "ND"
  0,        # 75, (n,%) column, "Excluded", Excluded implies zero
  8,        # 76, (n,%) column, "8"
  0,        # 77, (n,%) column, "0"
  0,        # 78, (n,%) column, "0"
  0,        # 79, (n,%) column, "0"
  3,        # 80, (n,%) column, "n=3"
  NA,       # 81, (n,%) column, "17.8%", percent only not count
  0,        # 82, (n,%) column, "0"
  NA,       # 83, (n,%) column, "ND"
  8,        # 84, (n,%) column, "8 (30.77%)*"
  0,        # 85, (n,%) column, "0"
  NA,       # 86, (n,%) column, "ND"
  NA,       # 87, (n,%) column, "ND"
  0,        # 88, (n,%) column, "0"
  NA,       # 89, (n,%) column, "not determined"
  0,        # 90, (n,%) column, "0"
  NA,       # 91, (n,%) column, "ND"
  0,        # 92, (n,%) column, "0"
  NA,       # 93, (n,%) column, "not reported"
  NA,       # 94, (n,%) column, "ND"
  NA,       # 95, (n,%) column, "4 (31%) heatlh / 3 (17%) gingivitis", ambiguous, more than one group
  NA,       # 96, (n,%) column, "not determined"
  0,        # 97, (n,%) column, "0"
  NA,       # 98, (n,%) column, "ND"
  0,        # 99, (n,%) column, "exclusion criteria", exclusion criteria implies zero
  0,        # 100, (n,%) column, "0"
  NA,       # 101, (n,%) column, "ND"
  0,        # 102, (n,%) column, "0"
  0,        # 103, (n,%) column, "0"
  NA,       # 104, (n,%) column, "ND"
  2,        # 105, (n,%) column, "2 (7%)"
  0,        # 106, (n,%) column, "exclusion criteria", exclusion criteria implies zero
  0,        # 107, (n,%) column, "exclusion criteria", exclusion criteria implies zero
  NA,       # 108, (n,%) column, "ND"
  NA,       # 109, (n,%) column, "ND"
  NA,       # 110, (n,%) column, "ND"
  NA,       # 111, (n,%) column, "ND"
  NA,       # 112, (n,%) column, "ND"
  0,        # 113, (n,%) column, "0"
  NA,       # 114, (n,%) column, "ND"
  0,        # 115, (n,%) column, "0"
  NA,       # 116, (n,%) column, "ND"
  25,       # 117, (n,%) column, "25"
  0,        # 118, (n,%) column, "0"
  NA        # 119, (n,%) column, multi-group summary row, ambiguous
)
smokers_percent_health = c(
  29.2,      # 1, % column, "29.2"
  12.3,      # 2, (n,%) column, "18 (12.3%)"
  NA,        # 3, ND
  11.6,      # 4, (n,%) column as fraction, "0.11600000000000001" = 11.6%
  0,         # 5, (n,%) column, "exclusion criteria" implies 0%
  NA,        # 6, ND
  0,         # 7, (n,%) column, "exclusion criteria" implies 0%
  0,         # 8, (n,%) column, "exclusion criteria" implies 0%
  0,         # 9, (n,%) column, "exclusion criteria" implies 0%
  0,         # 10, (n,%) column, "exclusion criteria" implies 0%
  0,         # 11, (n,%) column, "exclusion criteria" implies 0%
  0,         # 12, (n,%) column, "exclusion criteria" implies 0%
  0,         # 13, (n,%) column, "0" = 0%
  NA,        # 14, ND
  15,        # 15, (n,%) column as fraction, "0.15" = 15%
  0,         # 16, (n,%) column, "Excluded" implies 0%
  0,         # 17, (n,%) column, "0"
  0,         # 18, (n,%) column, "0"
  0,         # 19, (n,%) column, "0"
  0,         # 20, (n,%) column, "0"
  0,         # 21, (n,%) column, "0"
  0,         # 22, (n,%) column, "0"
  0,         # 23, (n,%) column, "0"
  0,         # 24, (n,%) column, "0"
  0,         # 25, (n,%) column, "0"
  18.2,      # 26, (n,%) column, "4 (18.2%)"
  0,         # 27, (n,%) column, "0"
  6,         # 28, (n,%) column, "3 (6%)*"
  11.1,      # 29, (n,%) column, "3 (11.1%)*"
  0,         # 30, (n,%) column, "0"
  9.09,      # 31, (n,%) column, "1 (9.09%)*"
  NA,        # 32, ND
  0,         # 33, (n,%) column, "0"
  NA,        # 34, ND
  NA,        # 35, ND
  0,         # 36, (n,%) column, "Excluded" implies 0%
  0,         # 37, (n,%) column, "Excluded" implies 0%
  NA,        # 38, (n,%) column, "12.5", no % present
  0,         # 39, (n,%) column, "0"
  20,        # 40, (n,%) column, "5 (20.0%)"
  NA,        # 41, ND
  0,         # 42, (n,%) column, "0"
  NA,        # 43, (n,%) column, "n=4", no percent
  9.2,       # 44, (n,%) column, "9.1999999999999998E-2" = 9.2%
  NA,        # 45, ND
  NA,        # 46, ND
  0,         # 47, (n,%) column, "0"
  22.2,      # 48, % column, "22.2"
  NA,        # 49, ND
  NA,        # 50, ND
  NA,        # 51, ND
  NA,        # 52, (n,%) column, "37.65±10.88", not possible to extract percent
  NA,        # 53, ND
  NA,        # 54, ND
  50,        # 55, (n,%) column as fraction, "0.5" = 50%
  24,        # 56, % column, "24.0"
  NA,        # 57, ND
  NA,        # 58, ND
  NA,        # 59, (n,%) column, "not reported"
  NA,        # 60, ND
  NA,        # 61, ND
  0,         # 62, (n,%) column, "0"
  0,         # 63, (n,%) column, "0"
  NA,        # 64, ND
  NA,        # 65, ND
  NA,        # 66, ND
  0,         # 67, (n,%) column, "0"
  0,         # 68, (n,%) column, "0"
  0,         # 69, (n,%) column, "0"
  NA,        # 70, ND
  NA,        # 71, ND
  35,        # 72, % column, "35.0"
  NA,        # 73, (n,%) column, "not determined"
  NA,        # 74, ND
  0,         # 75, (n,%) column, "Excluded" implies 0%
  22.2,      # 76, % column, "22.2"
  0,         # 77, (n,%) column, "0"
  0,         # 78, (n,%) column, "0"
  0,         # 79, (n,%) column, "0"
  NA,        # 80, (n,%) column, "n=3", no percent
  17.8,      # 81, (n,%) column, "17.8%", percent only
  0,         # 82, % column, "0.0"
  NA,        # 83, ND
  30.77,     # 84, (n,%) column, "8 (30.77%)*"
  0,         # 85, (n,%) column, "0"
  NA,        # 86, ND
  NA,        # 87, ND
  0,         # 88, (n,%) column, "0"
  NA,        # 89, (n,%) column, "not determined"
  0,         # 90, (n,%) column, "0"
  NA,        # 91, ND
  0,         # 92, (n,%) column, "0"
  NA,        # 93, (n,%) column, "not reported"
  NA,        # 94, ND
  NA,        # 95, (n,%) column, "4 (31%) health / 3 (17%) gingivitis", ambiguous
  NA,        # 96, (n,%) column, "not determined"
  0,         # 97, % column, "0.0"
  NA,        # 98, ND
  0,         # 99, (n,%) column, "exclusion criteria" implies 0%
  0,         # 100, (n,%) column, "0"
  NA,        # 101, ND
  0,         # 102, (n,%) column, "0"
  0,         # 103, (n,%) column, "0"
  NA,        # 104, ND
  7,         # 105, (n,%) column, "2 (7%)" - % not given, so from n
  0,         # 106, (n,%) column, "exclusion criteria" implies 0%
  0,         # 107, (n,%) column, "exclusion criteria" implies 0%
  NA,        # 108, ND
  NA,        # 109, ND
  NA,        # 110, ND
  NA,        # 111, ND
  NA,        # 112, ND
  0,         # 113, (n,%) column, "0"
  NA,        # 114, ND
  0,         # 115, (n,%) column, "0"
  NA,        # 116, ND
  13,        # 117, % column, "13.0"
  0,         # 118, (n,%) column, "0"
  NA         # 119, (n,%) column, group summary, ambiguous
)

# Smokers number and percentage (perio) ----
smokers_num_perio = c(
  34,        # 1, (n,%) column, "34"
  26,        # 2, (n,%) column, "26 (24.8%)"
  NA,        # 3, (n,%) column, NA
  44,        # 4, (n,%) column, "44.7, 4,1 % *", first integer, likely count
  NA,        # 5, (n,%) column, NA
  NA,        # 6, (n,%) column, NA
  0,         # 7, (n,%) column, "0"
  NA,        # 8, (n,%) column, NA
  NA,        # 9, (n,%) column, NA
  NA,        # 10, (n,%) column, NA
  NA,        # 11, (n,%) column, NA
  NA,        # 12, (n,%) column, NA
  NA,        # 13, (n,%) column, "43.1" (this is a percent, not count; see instructions)
  NA,        # 14, (n,%) column, NA
  NA,        # 15, (n,%) column, "0.2", fraction is percent only
  0,         # 16, (n,%) column, "excluded", exclusion criteria (implies zero)
  0,         # 17, (n,%) column, "0"
  0,         # 18, (n,%) column, "0"
  0,         # 19, (n,%) column, "0"
  0,         # 20, (n,%) column, "0"
  0,         # 21, (n,%) column, "0"
  0,         # 22, (n,%) column, "0"
  0,         # 23, (n,%) column, "0"
  0,         # 24, (n,%) column, "0"
  0,         # 25, (n,%) column, "0"
  4,         # 26, (n,%) column, "4 (18.2%)"
  2,         # 27, (n,%) column, "2 (5.2%)"
  42,        # 28, (n,%) column, "42 (25%)*"
  10+1,      # 29, (n,%) column, "ChP: 10 (27.5%)*, AgP: 1 (2.9%)*", sum group sizes
  0,         # 30, (n,%) column, "0"
  6,         # 31, (n,%) column, "6 (46.15%)*"
  NA,        # 32, (n,%) column, NA
  0,         # 33, (n,%) column, "0"
  NA,        # 34, (n,%) column, NA
  NA,        # 35, (n,%) column, NA
  0,         # 36, (n,%) column, "excluded", exclusion criteria
  0,         # 37, (n,%) column, "excluded", exclusion criteria
  NA,        # 38, (n,%) column, "0.25", fraction is percent only
  0,         # 39, (n,%) column, "0"
  8,         # 40, (n,%) column, "8 (32.0%)"
  NA,        # 41, (n,%) column, "ND"
  0,         # 42, (n,%) column, "0"
  3,         # 43, (n,%) column, "n=3"
  NA,        # 44, (n,%) column, "0.187", fraction only, percent
  NA,        # 45, (n,%) column, NA
  NA,        # 46, (n,%) column, "ND"
  2,         # 47, (n,%) column, "2"
  27+14,     # 48, (n,%) column, "Ap 27, 34.6% / Cp 14, 22.2%", sum groups for smoker count
  NA,        # 49, (n,%) column, NA
  NA,        # 50, (n,%) column, NA
  17+4,      # 51, (n,%) column, "Current 17 (38.6) Former 4 (9.1)", sum as smoker count
  NA,        # 52, (n,%) column, NA
  NA,        # 53, (n,%) column, "ND"
  NA,        # 54, (n,%) column, "ND"
  NA,        # 55, (n,%) column, "0.4", fraction is percent only
  20,        # 56, (n,%) column, "20"
  NA,        # 57, (n,%) column, NA
  NA,        # 58, (n,%) column, "ND"
  15,        # 59, (n,%) column, "15"
  NA,        # 60, (n,%) column, "ND"
  NA,        # 61, (n,%) column, "ND"
  6,         # 62, (n,%) column, "PCS group: 6 (100%)"
  0,         # 63, (n,%) column, "0"
  NA,        # 64, (n,%) column, "ND"
  NA,        # 65, (n,%) column, NA
  NA,        # 66, (n,%) column, NA
  NA,        # 67, (n,%) column, NA
  NA,        # 68, (n,%) column, NA
  NA,        # 69, (n,%) column, NA
  NA,        # 70, (n,%) column, NA
  NA,        # 71, (n,%) column, NA
  NA,        # 72, (n,%) column, "not determined"
  NA,        # 73, (n,%) column, "not determined"
  NA,        # 74, (n,%) column, "ND"
  NA,        # 75, (n,%) column, "not reported"
  16,        # 76, (n,%) column, "16"
  0,         # 77, (n,%) column, "0"
  0+16,      # 78, (n,%) column, "NS-Perio: 0 (0.00%) / S-Perio: 16 (100.0%)", sum is 16
  0,         # 79, (n,%) column, "0"
  29,        # 80, (n,%) column, "n=29"
  NA,        # 81, (n,%) column, "0.40500000000000003", fraction only, percent
  2,         # 82, (n,%) column, "2"
  NA,        # 83, (n,%) column, NA
  11,        # 84, (n,%) column, "11 (44.00%)*"
  2,         # 85, (n,%) column, "2 (20.00%)"
  NA,        # 86, (n,%) column, "ND"
  NA,        # 87, (n,%) column, NA
  0,         # 88, (n,%) column, "0"
  NA,        # 89, (n,%) column, "not determined"
  1,         # 90, (n,%) column, "1 (8.3)"
  NA,        # 91, (n,%) column, NA
  0,         # 92, (n,%) column, "0"
  NA,        # 93, (n,%) column, "not reported"
  NA,        # 94, (n,%) column, "ND"
  10+1,      # 95, (n,%) column, "active: 10 (83%)* / recession: 1 (20%)", sum group counts
  NA,        # 96, (n,%) column, "not determined"
  6,         # 97, (n,%) column, "6"
  NA,        # 98, (n,%) column, NA
  NA,        # 99, (n,%) column, "0.28999999999999998", fraction is percent only
  NA,        # 100, (n,%) column, "38-80%", range only
  NA,        # 101, (n,%) column, NA
  NA,        # 102, (n,%) column, "0.59", fraction is percent only
  NA,        # 103, (n,%) column, "0.81", fraction is percent only
  NA,        # 104, (n,%) column, NA
  9,         # 105, (n,%) column, "9 (21%)"
  NA,        # 106, (n,%) column, NA
  0,         # 107, (n,%) column, "0"
  NA,        # 108, (n,%) column, NA
  NA,        # 109, (n,%) column, NA
  NA,        # 110, (n,%) column, NA
  NA,        # 111, (n,%) column, NA
  NA,        # 112, (n,%) column, "ND"
  0,         # 113, (n,%) column, "0"
  NA,        # 114, (n,%) column, "ND"
  0,         # 115, (n,%) column, "0"
  NA,        # 116, (n,%) column, "ND"
  178,       # 117, (n,%) column, "178"
  NA,        # 118, (n,%) column, "ND"
  NA         # 119, (n,%) column, summary multi-group, ambiguous
)
smokers_percent_perio = c(
  50,        # 1, % column, "50.00"
  24.8,      # 2, (n,%) column, "26 (24.8%)"
  NA,        # 3, % column, NA
  4.1,       # 4, (n,%) column, "44.7, 4,1 % *", after comma
  NA,        # 5, % column, NA
  NA,        # 6, % column, NA
  0,         # 7, (n,%) column, "0" = 0%
  NA,        # 8, % column, NA
  NA,        # 9, % column, NA
  NA,        # 10, % column, NA
  NA,        # 11, % column, NA
  NA,        # 12, % column, NA
  43.1,      # 13, (n,%) column, "43.1", fraction interpreted as percent
  NA,        # 14, % column, NA
  20,        # 15, (n,%) column, "0.2", fraction as percent
  0,         # 16, (n,%) column, "excluded" implies 0%
  0,         # 17, % column, "0.00"
  0,         # 18, % column, "0.00"
  0,         # 19, % column, "0.00"
  0,         # 20, (n,%) column, "0"
  0,         # 21, (n,%) column, "0"
  0,         # 22, (n,%) column, "0"
  0,         # 23, (n,%) column, "0"
  0,         # 24, (n,%) column, "0"
  0,         # 25, (n,%) column, "0"
  18.2,      # 26, (n,%) column, "4 (18.2%)"
  5.2,       # 27, (n,%) column, "2 (5.2%)"
  25,        # 28, (n,%) column, "42 (25%)*"
  (10+1)/((10/0.275)+(1/0.029))*100, # 29, (n,%), see example above, sum groups
  0,         # 30, (n,%) column, "0"
  46.15,     # 31, (n,%) column, "6 (46.15%)*"
  NA,        # 32, % column, NA
  0,         # 33, (n,%) column, "0"
  NA,        # 34, % column, NA
  NA,        # 35, % column, NA
  0,         # 36, (n,%) column, "excluded" implies 0%
  0,         # 37, (n,%) column, "excluded" implies 0%
  25,        # 38, (n,%) column, "0.25", fraction to percent
  0,         # 39, (n,%) column, "0"
  32,        # 40, (n,%) column, "8 (32.0%)"
  NA,        # 41, % column, "ND"
  0,         # 42, (n,%) column, "0"
  NA,        # 43, (n,%) column, "n=3", no percent
  18.7,      # 44, (n,%) column, "0.187", fraction to percent
  NA,        # 45, % column, NA
  NA,        # 46, % column, "ND"
  7.7,       # 47, % column, "7.70"
  (27+14)/((27/0.346)+(14/0.222))*100, # 48, (n,%) Ap/Cp, sum groups as in example
  NA,        # 49, % column, NA
  NA,        # 50, % column, NA
  (17+4)/((17/0.386)+(4/0.091))*100,   # 51, (n,%) Current/Former, sum groups as in example
  NA,        # 52, % column, NA
  NA,        # 53, % column, "ND"
  NA,        # 54, % column, "ND"
  40,        # 55, (n,%) column, "0.4", fraction to percent
  38,        # 56, % column, "38.00"
  NA,        # 57, % column, NA
  NA,        # 58, % column, "ND"
  50,        # 59, % column, "50.00"
  NA,        # 60, % column, "ND"
  NA,        # 61, % column, "ND"
  100,       # 62, (n,%), PCS group: 6 (100%) (percent is 100)
  0,         # 63, (n,%) column, "0"
  NA,        # 64, % column, "ND"
  NA,        # 65, % column, NA
  NA,        # 66, % column, NA
  NA,        # 67, % column, NA
  NA,        # 68, % column, NA
  NA,        # 69, % column, NA
  NA,        # 70, % column, NA
  NA,        # 71, % column, NA
  NA,        # 72, (n,%) column, "not determined" NA
  NA,        # 73, (n,%) column, "not determined" NA
  NA,        # 74, (n,%) column, "ND"
  NA,        # 75, (n,%) column, "not reported"
  38.1,      # 76, % column, "38.10"
  0,         # 77, (n,%) column, "0"
  (0+16)/((0/0.0)+(16/1.0))*100, # 78, (n,%), "NS-Perio: 0 (0.00%) / S-Perio: 16 (100.0%)", sum groups as in example
  0,         # 79, (n,%) column, "0"
  NA,        # 80, (n,%) column, "n=29", no percent
  40.5,      # 81, (n,%) column, "0.405", fraction as percent
  28.57,     # 82, % column, "28.57"
  NA,        # 83, % column, NA
  44,        # 84, (n,%) column, "11 (44.00%)*"
  20,        # 85, (n,%) column, "2 (20.00%)"
  NA,        # 86, (n,%) column, "ND"
  NA,        # 87, % column, NA
  0,         # 88, (n,%) column, "0"
  NA,        # 89, % column, "not determined"
  8.3,       # 90, (n,%) column, "1 (8.3)", percent in ()
  NA,        # 91, % column, NA
  0,         # 92, % column, "0.00"
  NA,        # 93, % column, "not reported"
  NA,        # 94, % column, "ND"
  (10+1)/((10/0.83)+(1/0.20))*100, # 95, (n,%), "active: 10 (83%)* / recession: 1 (20%)" per example
  NA,        # 96, % column, "not determined"
  20.7,      # 97, % column, "20.70"
  NA,        # 98, % column, NA
  29,        # 99, (n,%) column, "0.28999999999999998", fraction as percent
  NA,        # 100, (n,%) column, "38-80%", range only
  NA,        # 101, % column, NA
  59,        # 102, (n,%) column, "0.59", fraction as percent
  81,        # 103, (n,%) column, "0.81", fraction as percent
  NA,        # 104, % column, NA
  21,        # 105, (n,%) column, "9 (21%)"
  NA,        # 106, % column, NA
  0,         # 107, (n,%) column, "0"
  NA,        # 108, % column, NA
  NA,        # 109, % column, NA
  NA,        # 110, % column, NA
  NA,        # 111, % column, NA
  NA,        # 112, (n,%) column, "ND"
  0,         # 113, (n,%) column, "0"
  NA,        # 114, (n,%) column, "ND"
  0,         # 115, (n,%) column, "0"
  NA,        # 116, (n,%) column, "ND"
  28,        # 117, % column, "28.00"
  NA,        # 118, (n,%) column, "ND"
  NA         # 119, summary row, ambiguous
)