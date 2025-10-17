# =============================================
# Yield Curve & Relative Value from ratesheet.pdf
# (pdftools-only version; no Java/tabulizer required)
# =============================================

# Required packages:
# install.packages(c("pdftools","dplyr","tidyr","stringr","readr","lubridate","zoo","chron"))

suppressPackageStartupMessages({
  library(pdftools)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(lubridate)
  library(zoo)
  library(chron)
})

# ------------- Utilities -------------

parse_num <- function(x) {
  # turn strings like "5.25%" or " 5.25 " into numeric 5.25
  x <- gsub(",", "", x)
  x <- gsub("%", "", x)
  suppressWarnings(readr::parse_number(x))
}

parse_maturity <- function(x){
  # handle dd/mm/yy and dd/mm/yyyy -> chron
  dt <- suppressWarnings(lubridate::dmy(x))
  chron::chron(format(dt, "%m/%d/%y"), format="m/d/y", out.format="m/d/y")
}

# yearsbefore: subtract integer years from a chron date
yearsbefore <- function(x, n_years){
  d <- as.Date(as.character(x), format = "%m/%d/%y")
  d <- d %m-% years(n_years)
  chron(format(d, "%m/%d/%y"), format="m/d/y", out.format="m/d/y")
}

# semiannual coupon dates from 20 years before to maturity (not required for plotting, but kept)
coupon_dates <- function(maturity){
  startdate <- yearsbefore(maturity, 20)
  s <- as.Date(as.character(startdate), format="%m/%d/%y")
  m <- as.Date(as.character(maturity),  format="%m/%d/%y")
  x <- seq(from = s, to = m, by = "month")
  x6 <- x[seq(1, length(x), by = 6)]
  chron(format(x6, "%m/%d/%y"), format="m/d/y", out.format="m/d/y")
}

# ------------- PDF -> tidy bonds -------------

# Parses lines from a PDF into a bond table with columns:
# Issuer | ISIN | Maturity | Coupon | Yield
read_ratesheet <- function(pdf_path){
  # 1) Extract token data per page
  pages <- pdftools::pdf_data(pdf_path)

  # 2) Collapse tokens into lines by approximate y-position
  lines_df <- dplyr::bind_rows(lapply(seq_along(pages), function(i){
    df <- pages[[i]]
    df %>%
      dplyr::mutate(y_round = round(y / 6)) %>%
      dplyr::group_by(y_round) %>%
      dplyr::summarise(line = paste(text, collapse = " "), .groups="drop") %>%
      dplyr::mutate(page = i) %>%
      dplyr::select(page, line)
  }))

  # 3) Use a regex to pull out: Issuer, ISIN, Maturity, Coupon, Yield
  #    - Issuer: anything (lazy) before ISIN
  #    - ISIN: 12 alphanumeric chars
  #    - Maturity: dd/mm/yy or dd/mm/yyyy
  #    - Coupon: number (with optional decimal) possibly with %
  #    - Yield: number (with optional decimal) possibly with %
  pat <- "^(.*?)\\s+([A-Z0-9]{12})\\s+(\\d{2}/\\d{2}/\\d{2,4})\\s+([0-9]+(?:\\.[0-9]+)?)%?\\s+([0-9]+(?:\\.[0-9]+)?)%?"

  m <- stringr::str_match(lines_df$line, pattern = pat)
  # m columns: [full, issuer, isin, maturity, coupon, yield]
  parsed <- tibble::tibble(
    Issuer_raw = m[,2],
    ISIN       = m[,3],
    Maturity_s = m[,4],
    Coupon_s   = m[,5],
    Yield_s    = m[,6]
  ) %>% 
    # keep only rows where we matched
    filter(!is.na(ISIN), !is.na(Maturity_s), !is.na(Coupon_s), !is.na(Yield_s)) %>%
    mutate(
      Issuer   = toupper(str_squish(Issuer_raw)),
      Coupon   = parse_num(Coupon_s),
      Yield    = parse_num(Yield_s),
      Maturity = parse_maturity(Maturity_s)
    ) %>%
    select(Issuer, ISIN, Maturity, Coupon, Yield) %>%
    distinct(Issuer, ISIN, Maturity, .keep_all = TRUE)

  parsed
}

# ------------- Curve + Plot -------------

get_par_curve <- function(data, issuer = NULL, type = NULL, titl, today, spar_in = 0.65){
  stopifnot(all(c("Issuer","Maturity","Yield") %in% names(data)))
  df <- data
  if (!is.null(issuer)) {
    df <- df %>% filter(Issuer %in% issuer)
  }
  if (!is.null(type)) {
    # No explicit Type column in PDF parse; keep for API compatibility.
    warning("`type` filter not applied (no Type column in parsed data).")
  }

  maturities <- df$Maturity
  bond_yld   <- df$Yield
  issuers    <- df$Issuer

  # de-duplicate by maturity; keep first
  keep <- !duplicated(maturities)
  maturities <- maturities[keep]
  bond_yld   <- bond_yld[keep]
  issuers    <- issuers[keep]

  # keep bonds > 1y from 'today'
  mask <- maturities > (today + 366)
  maturities <- maturities[mask]
  bond_yld   <- bond_yld[mask]
  issuers    <- issuers[mask]

  if (length(maturities) < 4) {
    warning("Not enough points to fit a spline for this selection.")
  }

  allrates <- zoo(bond_yld, maturities)
  rate  <- coredata(na.omit(allrates))

  # tenor in months from 'today'
  tenor <- (as.numeric(
              as.Date(as.character(index(na.omit(allrates))), "%m/%d/%y") -
              as.Date(as.character(today), "%m/%d/%y")
           )) / 30

  # plot and spline fit
  plot(tenor, rate,
       main = titl, ylab = "Yield (per cent)", xlab = "Tenor (months)")
  ycurve <- smooth.spline(tenor, rate, spar = spar_in)
  lines(predict(ycurve, seq(min(tenor), max(tenor), by = 1L)), lwd = 2.5, col = "blue")

  # labels: issuer + maturity date
  datalabels <- paste(issuers, as.character(index(allrates)))
  text(tenor, rate, datalabels, pos = 3, cex = .6, srt = 45, col = "darkgrey")

  ycurve.out <- predict(ycurve, tenor)$y
  relval <- round(100 * (rate - ycurve.out), 2)
  results <- data.frame(
    bond  = datalabels,
    yield = rate,
    model = round(ycurve.out, 4),
    relval = relval,
    tenor_months = tenor
  )
  invisible(results)
}

# ------------- Run -------------

# Put your PDF in the same folder as this script:
pdf_path <- "ratesheet.pdf"

# Parse PDF -> tidy bonds table
bonddata_raw <- read_ratesheet(pdf_path)

# 'today' = system date by default (change if you want to align with the sheet date)
today <- chron(format(Sys.Date(), "%m/%d/%y"), format="m/d/y", out.format="m/d/y")

# Examples — run any/all of these:
# ACGB (Commonwealth Gov)
try(get_par_curve(bonddata_raw, issuer = "ACGB",
                  titl = "ACGB yield curve", today = today, spar_in = 0.5), silent = TRUE)

# TCV
try(get_par_curve(bonddata_raw, issuer = "TCV",
                  titl = "TCV yield curve", today = today, spar_in = 0.1), silent = TRUE)

# Top-tier semis: QTC, NSWTC, TCV
try(get_par_curve(bonddata_raw, issuer = c("QTC","NSWTC","TCV"),
                  titl = "QTC / NSWTC / TCV yield curve", today = today, spar_in = 0.65), silent = TRUE)

# Lower-tier semis: SAFA, TASCOR, WATC  (Tas may appear as 'TASCOR' on some sheets)
try(get_par_curve(bonddata_raw, issuer = c("SAFA","TASCOR","WATC"),
                  titl = "SAFA / TASCOR / WATC yield curve", today = today, spar_in = 0.35), silent = TRUE)

# Supranationals (adjust list to what appears in your sheet)
supra_issuers <- c("IBRD","IFC","EIB","KFW","IADB","ASIA","AFDB","KBN","BNG","CAF","COE","EDC","AIIB")
try(get_par_curve(bonddata_raw, issuer = supra_issuers,
                  titl = "Supranational yield curve", today = today, spar_in = 0.9), silent = TRUE)

# Inspect what was parsed (optional):
# head(bonddata_raw)

