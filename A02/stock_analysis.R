# stock_analysis.R
# BDA400 Assignment 2 -- Technical Analysis using R, Preliminary Stage
#
# Reads data/portfolio.txt, imports daily OHLCV data for each symbol with
# quantmod, computes basic statistics (mean, median, mode, standard
# deviation, moving average) for each stock, and displays/plots the result.
#
# NOTE ON DATA SOURCE: load_stock_data() calls quantmod::getSymbols(src =
# "yahoo"), which requires live internet access to Yahoo Finance. The sandbox
# this script was authored and tested in has no outbound access to
# finance.yahoo.com (confirmed by a direct connection test, not assumed), so
# a synthetic-data fallback (generate_synthetic_stock()) is used automatically
# whenever the live fetch fails, so the rest of the pipeline can still be
# verified end-to-end. On a normal internet connection -- i.e. when you run
# this on your own machine for the real submission -- load_stock_data() will
# fetch real Yahoo Finance data and the fallback is never triggered.

required_packages <- c("quantmod", "TTR", "xts", "zoo")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
suppressPackageStartupMessages({
  library(quantmod)
  library(TTR)
})

# -----------------------------------------------------------------------
# Synthetic fallback -- only used if Yahoo Finance cannot be reached.
# Produces an xts object in the exact same shape as quantmod::getSymbols()
# (Open/High/Low/Close/Volume/Adjusted columns named SYMBOL.<field>), so
# every downstream function works identically on real or fallback data.
# -----------------------------------------------------------------------
generate_synthetic_stock <- function(symbol, n_days = 252, start_price = 150, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  dates <- seq(Sys.Date() - n_days + 1, Sys.Date(), by = "day")
  dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  n <- length(dates)

  daily_return <- rnorm(n, mean = 0.0004, sd = 0.015)
  close <- start_price * cumprod(1 + daily_return)
  open  <- close * (1 + rnorm(n, 0, 0.003))
  high  <- pmax(open, close) * (1 + abs(rnorm(n, 0, 0.004)))
  low   <- pmin(open, close) * (1 - abs(rnorm(n, 0, 0.004)))
  volume <- round(runif(n, 2e6, 9e6))

  m <- cbind(Open = open, High = high, Low = low, Close = close,
             Volume = volume, Adjusted = close)
  colnames(m) <- paste0(symbol, ".", colnames(m))
  xts::xts(round(m, 2), order.by = dates)
}

# -----------------------------------------------------------------------
# Step 5: load_stock_data() -- reads portfolio.txt, imports each symbol's
# data via quantmod, returns a named list of xts data frames (one per stock)
# -----------------------------------------------------------------------
load_stock_data <- function(portfolio_file = "portfolio.txt", use_synthetic_if_offline = TRUE) {
  symbols <- readLines(portfolio_file)
  symbols <- trimws(symbols)
  symbols <- symbols[nzchar(symbols)]

  stock_data <- list()
  for (i in seq_along(symbols)) {
    sym <- symbols[i]
    data <- tryCatch({
      quantmod::getSymbols(sym, src = "yahoo", auto.assign = FALSE,
                            from = Sys.Date() - 365, to = Sys.Date())
    }, error = function(e) {
      if (!use_synthetic_if_offline) stop(e)
      message(sprintf("[%s] Yahoo Finance unreachable (%s) -- using synthetic fallback data.",
                       sym, conditionMessage(e)))
      generate_synthetic_stock(sym, seed = i * 101)
    })
    stock_data[[sym]] <- data
  }
  stock_data
}

# -----------------------------------------------------------------------
# Step 6: calculate_statistics() -- mean, median, mode, sd, moving average
# -----------------------------------------------------------------------
get_mode <- function(x) {
  # R has no built-in mode() for data; bucket continuous prices to the
  # nearest dollar first so a meaningful mode exists at all.
  x <- round(x, 0)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

calculate_statistics <- function(stock_xts, ma_period = 20) {
  close_col <- grep("\\.Close$", colnames(stock_xts), value = TRUE)
  close <- as.numeric(stock_xts[, close_col])

  list(
    mean_close   = mean(close, na.rm = TRUE),
    median_close = median(close, na.rm = TRUE),
    mode_close   = get_mode(close),
    sd_close     = sd(close, na.rm = TRUE),
    ma_period    = ma_period,
    moving_avg   = TTR::SMA(close, n = ma_period)
  )
}

# -----------------------------------------------------------------------
# Step 7: display utilities
# -----------------------------------------------------------------------
display_stock_summary <- function(symbol, stock_xts, stats) {
  cat(strrep("=", 56), "\n")
  cat(sprintf("Stock: %s\n", symbol))
  cat(strrep("=", 56), "\n")
  cat("Last 5 trading days:\n")
  print(tail(stock_xts, 5))
  cat("\n")
  cat(sprintf("Mean Close:              $%.2f\n", stats$mean_close))
  cat(sprintf("Median Close:            $%.2f\n", stats$median_close))
  cat(sprintf("Mode Close:              $%.2f\n", stats$mode_close))
  cat(sprintf("Std Dev of Close:        $%.2f\n", stats$sd_close))
  latest_sma <- tail(na.omit(stats$moving_avg), 1)
  cat(sprintf("%d-day Simple Moving Avg: $%.2f (latest)\n\n", stats$ma_period, latest_sma))
}

plot_stock_chart <- function(symbol, stock_xts, stats, out_dir = "output") {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  close_col <- grep("\\.Close$", colnames(stock_xts), value = TRUE)
  close <- as.numeric(stock_xts[, close_col])

  png(file.path(out_dir, paste0(symbol, "_chart.png")), width = 900, height = 500, res = 120)
  plot(zoo::index(stock_xts), close, type = "l", col = "#1F4E78", lwd = 2,
       main = paste0(symbol, " - Closing Price & ", stats$ma_period, "-day SMA"),
       xlab = "Date", ylab = "Price ($)")
  lines(zoo::index(stock_xts), as.numeric(stats$moving_avg), col = "#D9534F", lwd = 2)
  legend("topleft", legend = c("Close", paste0(stats$ma_period, "-day SMA")),
         col = c("#1F4E78", "#D9534F"), lwd = 2, bty = "n")
  dev.off()
}

# -----------------------------------------------------------------------
# Main: run the full pipeline for every symbol in portfolio.txt
# -----------------------------------------------------------------------
portfolio <- load_stock_data("portfolio.txt")

all_stats <- list()
for (sym in names(portfolio)) {
  stats <- calculate_statistics(portfolio[[sym]])
  all_stats[[sym]] <- stats
  display_stock_summary(sym, portfolio[[sym]], stats)
  plot_stock_chart(sym, portfolio[[sym]], stats)
}

summary_table <- do.call(rbind, lapply(names(all_stats), function(sym) {
  s <- all_stats[[sym]]
  data.frame(
    Symbol       = sym,
    Mean         = round(s$mean_close, 2),
    Median       = round(s$median_close, 2),
    Mode         = round(s$mode_close, 2),
    SD           = round(s$sd_close, 2),
    Latest_SMA20 = round(tail(na.omit(s$moving_avg), 1), 2)
  )
}))

cat(strrep("=", 56), "\n")
cat("PORTFOLIO SUMMARY\n")
cat(strrep("=", 56), "\n")
print(summary_table, row.names = FALSE)

if (!dir.exists("output")) dir.create("output")
write.csv(summary_table, "output/summary_statistics.csv", row.names = FALSE)
cat("\nSaved output/summary_statistics.csv and one chart PNG per symbol in output/\n")
