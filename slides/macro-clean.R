# Brute force ahoy

# ==============================================================================
# packages
# ==============================================================================

library(readxl)
library(tidyverse)
library(zoo)

make_vintage_seq <- function(start, end) {
  qseq <- seq(as.yearqtr(start, format = "%Y Q%q"),
              as.yearqtr(end,   format = "%Y Q%q"),
              by = 0.25)
  
  paste0("v", format(qseq, "%y"), quarters(qseq))
}

# ==============================================================================
# real GDP growth (YoY percent change - 100 * Δln)
# https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/routput
# ==============================================================================

ROUTPUTQvQd_2 <- read_excel("slides/data/ROUTPUTQvQd-2.xlsx", na = "#N/A")

rgdp_growth <- ROUTPUTQvQd_2 |>
  mutate(across(-DATE, ~ 100 * (log(.x) - log(dplyr::lag(.x, 4))))) |>
  slice(5:n()) |>
  mutate(
    DATE = as.yearqtr(DATE, format = "%Y:Q%q"),
    DATE = as.Date(DATE)
  ) |>
  rename(quarter = DATE)

colnames(rgdp_growth) <- sub("ROUTPUT", "v", colnames(rgdp_growth))

rm(ROUTPUTQvQd_2)

# ==============================================================================
# real consumption growth (YoY percent change - 100 * Δln)
# https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/rcon
# ==============================================================================

RCONQvQd <- read_excel("slides/data/RCONQvQd.xlsx", na = "#N/A")

rpce_growth <- RCONQvQd |>
  mutate(across(-DATE, ~ 100 * (log(.x) - log(dplyr::lag(.x, 4))))) |>
  slice(5:n()) |>
  mutate(
    DATE = as.yearqtr(DATE, format = "%Y:Q%q"),
    DATE = as.Date(DATE)
  ) |>
  rename(quarter = DATE)

colnames(rpce_growth) <- sub("RCON", "v", colnames(rpce_growth))

rm(RCONQvQd)

# ==============================================================================
# CPI inflation (YoY percent change - 100 * Δln)
# Monthly CPI level was first aggregated to a quarterly frequency by averaging
# https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/cpi
# ==============================================================================

CPI <- read_excel("slides/data/cpiQvMd.xlsx", na = "#N/A")

cpi_inflation <- CPI |>
  mutate(DATE = ymd(paste0(gsub(":", "-", DATE), "-01")),
         quarter = floor_date(DATE, "quarter")) |>
  relocate(quarter) |>
  group_by(quarter) |>
  summarize(across(CPI65Q4:CPI25Q4, ~ mean(.x))) |>
  mutate(across(-quarter, ~ 100 * (log(.x) - log(dplyr::lag(.x, 4))))) |>
  slice(5:n())

colnames(cpi_inflation) <- sub("CPI", "v", colnames(cpi_inflation))

cpi_inflation <- cpi_inflation |>
  mutate(v26Q1 = NA)

rm(CPI)

# ==============================================================================
# Real Gross Private Domestic Nonresidential Investment growth (YoY percent change - 100 * Δln)
# https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/rinvbf
# ==============================================================================

rinvbfQvQd <- read_excel("slides/data/rinvbfQvQd.xlsx", na = "#N/A")

investment_growth <- rinvbfQvQd |>
  mutate(across(-DATE, ~ 100 * (log(.x) - log(dplyr::lag(.x, 4))))) |>
  slice(5:n()) |>
  mutate(
    DATE = as.yearqtr(DATE, format = "%Y:Q%q"),
    DATE = as.Date(DATE)
  ) |>
  rename(quarter = DATE)

colnames(investment_growth) <- sub("rinvbf", "v", colnames(investment_growth))

rm(rinvbfQvQd)

# ==============================================================================
# Unemployment rate (no transformation)
# https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/ruc
# ==============================================================================

rucQvMd <- read_excel("slides/data/rucQvMd.xlsx", na = "#N/A")

unemployment_rate <- rucQvMd |>
  mutate(DATE = ymd(paste0(gsub(":", "-", DATE), "-01")),
         quarter = floor_date(DATE, "quarter")) |>
  relocate(quarter) |>
  group_by(quarter) |>
  summarize(across(RUC65Q4:RUC25Q4, ~ mean(.x))) |>
  slice(5:n())

colnames(unemployment_rate) <- sub("RUC", "v", colnames(unemployment_rate))

unemployment_rate <- unemployment_rate |>
  mutate(v26Q1 = NA)

rm(rucQvMd)

# ==============================================================================
# nonfarm payroll employment growth (YoY percent change - 100 * Δln)
# https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/employ
# ==============================================================================

employMvMd <- read_excel("slides/data/employMvMd.xlsx", na = "#N/A")

payroll_employment <- employMvMd |>
  mutate(DATE = ymd(paste0(gsub(":", "-", DATE), "-01")),
         quarter = floor_date(DATE, "quarter")) |>
  relocate(quarter) |>
  group_by(quarter) |>
  summarize(across(EMPLOY64M12:EMPLOY26M1, ~ mean(.x))) |>
  mutate(across(-quarter, ~ 100 * (log(.x) - log(dplyr::lag(.x, 4))))) |>
  filter(quarter >= min(rgdp_growth$quarter))

colnames(payroll_employment) <- sub("EMPLOY", "v", colnames(payroll_employment))

# Let chat do this nonsense -------->

# Identify all vintage columns
vintage_cols <- grep("^v.*M\\d{1,2}$", colnames(payroll_employment), value = TRUE)

# Extract months
months <- as.integer(sub(".*M", "", vintage_cols))

# Keep only last month of quarter
keep <- vintage_cols[months %in% c(3,6,9,12)]

# Subset data frame (including non-vintage columns if desired)
payroll_employment <- payroll_employment[, c("quarter", keep)]

# Extract year and month
year <- sub("v(\\d+)M\\d+", "\\1", keep)  # e.g., "66"
month <- as.integer(sub(".*M", "", keep)) # e.g., 3,6,9,12

# Construct new names
colnames(payroll_employment) <- c("quarter", paste0("v", year, "Q", (month - 1) %/% 3 + 1))

rm(employMvMd, vintage_cols, months, keep, year, month)

payroll_employment <- payroll_employment |>
  mutate(v26Q1 = NA) |>
  select(quarter, v65Q4:v26Q1)



# ==============================================================================
# production growth (YoY percent change - 100 * Δln)
# https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/ipt
# ==============================================================================

iptMvMd <- read_excel("slides/data/iptMvMd.xlsx", na = "#N/A")

production_growth <- iptMvMd |>
  mutate(DATE = ymd(paste0(gsub(":", "-", DATE), "-01")),
         quarter = floor_date(DATE, "quarter")) |>
  relocate(quarter) |>
  group_by(quarter) |>
  summarize(across(IPT62M11:IPT26M1, ~ mean(.x))) |>
  mutate(across(-quarter, ~ 100 * (log(.x) - log(dplyr::lag(.x, 4))))) |>
  filter(quarter >= min(rgdp_growth$quarter))

colnames(production_growth) <- sub("IPT", "v", colnames(production_growth))

# Let chat do this nonsense -------->

# Identify all vintage columns
vintage_cols <- grep("^v.*M\\d{1,2}$", colnames(production_growth), value = TRUE)

# Extract months
months <- as.integer(sub(".*M", "", vintage_cols))

# Keep only last month of quarter
keep <- vintage_cols[months %in% c(3,6,9,12)]

# Subset data frame (including non-vintage columns if desired)
production_growth <- production_growth[, c("quarter", keep)]

# Extract year and month
year <- sub("v(\\d+)M\\d+", "\\1", keep)  # e.g., "66"
month <- as.integer(sub(".*M", "", keep)) # e.g., 3,6,9,12

# Construct new names
colnames(production_growth) <- c("quarter", paste0("v", year, "Q", (month - 1) %/% 3 + 1))

rm(iptMvMd, vintage_cols, months, keep, year, month)

production_growth <- production_growth |>
  mutate(v26Q1 = NA) |>
  select(quarter, v65Q4:v26Q1)

# ==============================================================================
# labor productivity growth (YoY percent change - 100 * Δln)
# https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/oph
# ==============================================================================

OPH <- read_excel("slides/data/OPHQvQd.xlsx", na = "#N/A")

productivity_growth <- OPH |>
  mutate(across(-DATE, ~ 100 * (log(.x) - log(dplyr::lag(.x, 4))))) |>
  slice(5:n()) |>
  mutate(
    DATE = as.yearqtr(DATE, format = "%Y:Q%q"),
    DATE = as.Date(DATE)
  ) |>
  rename(quarter = DATE) 

colnames(productivity_growth) <- sub("OPH", "v", colnames(productivity_growth))

productivity_growth <- productivity_growth |>
  mutate(v26Q1 = NA)

padding <- make_vintage_seq("1965 Q4", "1998 Q3")

empty <- matrix(NA, nrow(productivity_growth), length(padding))
colnames(empty) <- padding

productivity_growth <- cbind(productivity_growth[, 1], 
                             empty, 
                             productivity_growth[, 2:ncol(productivity_growth)])

rm(OPH, padding, empty, make_vintage_seq)

# ==============================================================================
# labor productivity growth (YoY percent change - 100 * Δln)
# https://alfred.stlouisfed.org/series?seid=OPHNFB
# ==============================================================================

#OPHNFB_2 <- read_excel("slides/data/OPHNFB_2.xlsx", sheet = "Vintages Starting 1968-05-27")

#productivity_growth <- OPHNFB_2 |>
#  mutate(across(-observation_date, ~ 100 * (log(.x) - log(dplyr::lag(.x, 4))))) |>
#  mutate(quarter = as.Date(observation_date)) |>
#  select(-observation_date) |>
#  relocate(quarter) |>
#  filter(quarter >= min(rgdp_growth$quarter)) |>
#  add_row(quarter = as.Date("2025-10-01"))

#rm(OPHNFB_2)

#colnames(productivity_growth) <- sub("OPHNFB_", "v", colnames(productivity_growth))

#as.yearqtr(as.Date(sub("v", "", colnames(productivity_growth)), format = "%Y%m%d"))

# ==============================================================================
# basic check
# ==============================================================================

all(colnames(rgdp_growth) == colnames(production_growth))
all(rgdp_growth$quarter == production_growth$quarter)
all(colnames(rgdp_growth) == colnames(payroll_employment))
all(rgdp_growth$quarter == payroll_employment$quarter)
all(colnames(rgdp_growth) == colnames(cpi_inflation))
all(rgdp_growth$quarter == cpi_inflation$quarter)
all(colnames(rgdp_growth) == colnames(productivity_growth))
all(rgdp_growth$quarter == productivity_growth$quarter)

# ==============================================================================
# done
# ==============================================================================

save.image("macro-vintages.RData")
  
