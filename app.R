# ============================================================
# 0050 投資研究 Dashboard - 合併部署版
# 依目前需求重新設計後，由 R/00_config.R ~ R/06_server.R 合併
# ============================================================

# ============================================================
# 0050 Investment Research Dashboard - clean rebuild
# 00_config.R
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

options(scipen = 999)

DATA_FILE <- "data/stock_data.rds"
TRADING_DAYS_PER_YEAR <- 252
BENCHMARK_SYMBOL <- "0050"

# 全站共用時間尺度。所有頁面都只引用這一份定義。
PERIOD_DEFS <- data.frame(
  key = c("1w", "1m", "3m", "6m", "1y", "3y"),
  label = c("近 1 週", "近 1 個月", "近 3 個月", "近 6 個月", "近 1 年", "近 3 年"),
  days = c(5L, 21L, 63L, 126L, 252L, 756L),
  return_weight = c(0.05, 0.10, 0.15, 0.20, 0.25, 0.25),
  stringsAsFactors = FALSE
)

FORWARD_PERIOD_DEFS <- data.frame(
  key = c("1w", "1m", "3m", "6m", "1y", "2y", "3y", "5y"),
  label = c("未來 1 週", "未來 1 個月", "未來 3 個月", "未來 6 個月", "未來 1 年", "未來 2 年", "未來 3 年", "未來 5 年"),
  days = c(5L, 21L, 63L, 126L, 252L, 504L, 756L, 1260L),
  stringsAsFactors = FALSE
)

WEIGHT_SCHEMES <- list(
  stable = c(stability = 0.40, return = 0.25, risk = 0.20, sharpe = 0.15),
  balanced = c(stability = 0.35, return = 0.30, risk = 0.20, sharpe = 0.15),
  return_tilt = c(stability = 0.30, return = 0.35, risk = 0.20, sharpe = 0.15)
)

WEIGHT_SCHEME_LABELS <- c(
  stable = "穩定型 40 / 25 / 20 / 15",
  balanced = "均衡型 35 / 30 / 20 / 15",
  return_tilt = "報酬稍強型 30 / 35 / 20 / 15"
)

stock_info <- data.frame(
  symbol = c(
    "2330","2454","2308","2317","3711",
    "2303","2383","3037","2881","2891",
    "1303","3017","2882","2887","2345",
    "2382","2327","2360","2885","2884",
    "2059","2408","6669","2357","2883",
    "2886","3008","2890","3443","3231",
    "2301","2344","2412","3653","2880",
    "2892","3665","1216","4958","6446",
    "7769","2368","2449","2395","5880",
    "8046","2603","4904","3045","6505"
  ),
  name = c(
    "台積電","聯發科","台達電","鴻海","日月光投控",
    "聯電","台光電","欣興","富邦金","中信金",
    "南亞","奇鋐","國泰金","台新新光金","智邦",
    "廣達","國巨","致茂","元大金","玉山金",
    "川湖","南亞科","緯穎","華碩","凱基金",
    "兆豐金","大立光","永豐金","創意","緯創",
    "光寶科","華邦電","中華電","健策","華南金",
    "第一金","貿聯-KY","統一","臻鼎-KY","藥華藥",
    "鴻勁","金像電","京元電子","研華","合庫金",
    "南電","長榮","遠傳","台灣大","台塑化"
  ),
  industry = c(
    "半導體業","半導體業","電子零組件業","其他電子業","半導體業",
    "半導體業","電子零組件業","電子零組件業","金融保險","金融保險",
    "塑膠工業","電腦及週邊設備業","金融保險","金融保險","通信網路業",
    "電腦及週邊設備業","電子零組件業","其他電子業","金融保險","金融保險",
    "電子零組件業","半導體業","電腦及週邊設備業","電腦及週邊設備業","金融保險",
    "金融保險","其他電子業","金融保險","半導體業","電腦及週邊設備業",
    "電腦及週邊設備業","半導體業","通信網路業","電子零組件業","金融保險",
    "金融保險","其他電子業","食品工業","電子零組件業","生技醫療業",
    "半導體業","電子零組件業","半導體業","電腦及週邊設備業","金融保險",
    "電子零組件業","航運業","通信網路業","通信網路業","油電燃氣業"
  ),
  stringsAsFactors = FALSE
)

benchmark_info <- data.frame(
  symbol = BENCHMARK_SYMBOL,
  name = "元大台灣50",
  industry = "比較基準",
  stringsAsFactors = FALSE
)

all_info <- bind_rows(benchmark_info, stock_info)

industry_count <- table(stock_info$industry)
major_industries <- names(industry_count[industry_count >= 4])

# 0050 固定藍色。個股固定映射到代表色，跨圖表保持一致。
set.seed(50)
stock_palette <- grDevices::hcl(
  h = seq(15, 365, length.out = nrow(stock_info) + 1)[-1],
  c = 58,
  l = 55
)
STOCK_COLORS <- c(
  "0050" = "#4F8FC0",
  stats::setNames(stock_palette, stock_info$symbol)
)
UP_COLOR <- "#C65A5A"
DOWN_COLOR <- "#4D9A6A"
NEUTRAL_COLOR <- "#6F7F8F"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

convert_to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  out <- suppressWarnings(as.Date(x))
  if (all(is.na(out)) && is.numeric(x)) {
    out <- as.Date(x, origin = "1970-01-01")
  }
  out
}

get_stock_name <- function(symbol) {
  symbol <- as.character(symbol)
  idx <- match(symbol, all_info$symbol)
  out <- all_info$name[idx]
  out[is.na(out)] <- symbol[is.na(out)]
  out
}

get_stock_industry <- function(symbol) {
  symbol <- as.character(symbol)
  idx <- match(symbol, all_info$symbol)
  out <- all_info$industry[idx]
  out[is.na(out)] <- "其他"
  out
}

get_stock_label <- function(symbol) {
  paste0(get_stock_name(symbol), " (", symbol, ")")
}

get_stock_color <- function(symbol) {
  symbol <- as.character(symbol)[1]
  STOCK_COLORS[[symbol]] %||% "#6E8BA4"
}

period_days <- function(key) {
  x <- PERIOD_DEFS$days[match(key, PERIOD_DEFS$key)]
  ifelse(length(x) == 0 || is.na(x), NA_integer_, as.integer(x))
}

period_label <- function(key) {
  x <- PERIOD_DEFS$label[match(key, PERIOD_DEFS$key)]
  ifelse(length(x) == 0 || is.na(x), key, x)
}

forward_days <- function(key) {
  x <- FORWARD_PERIOD_DEFS$days[match(key, FORWARD_PERIOD_DEFS$key)]
  ifelse(length(x) == 0 || is.na(x), NA_integer_, as.integer(x))
}

forward_label <- function(key) {
  x <- FORWARD_PERIOD_DEFS$label[match(key, FORWARD_PERIOD_DEFS$key)]
  ifelse(length(x) == 0 || is.na(x), key, x)
}

horizons_up_to <- function(key) {
  n <- period_days(key)
  PERIOD_DEFS$key[PERIOD_DEFS$days <= n]
}

fmt_pct <- function(x, digits = 2, signed = FALSE) {
  ifelse(
    is.finite(x),
    sprintf(if (signed) paste0("%+.", digits, "f%%") else paste0("%.", digits, "f%%"), 100 * x),
    "—"
  )
}

fmt_num <- function(x, digits = 2) {
  ifelse(is.finite(x), sprintf(paste0("%.", digits, "f"), x), "—")
}

fmt_score <- function(x, digits = 1) {
  ifelse(is.finite(x), sprintf(paste0("%.", digits, "f"), x), "—")
}

fmt_date <- function(x) {
  ifelse(is.na(x), "—", format(as.Date(x), "%Y/%m/%d"))
}

format_period_info <- function(start_date, end_date, n_obs, label = NULL) {
  prefix <- if (!is.null(label) && nzchar(label)) paste0(label, "｜") else ""
  paste0(prefix, fmt_date(start_date), " ～ ", fmt_date(end_date), "｜有效觀測 ", n_obs, " 日")
}

make_info_strip <- function(text) {
  div(class = "info-strip", icon("calendar"), span(text))
}

make_kpi <- function(title, value, subtitle = NULL, tone = "blue") {
  div(
    class = paste("kpi-card", paste0("tone-", tone)),
    div(class = "kpi-title", title),
    div(class = "kpi-value", value),
    if (!is.null(subtitle)) div(class = "kpi-subtitle", subtitle)
  )
}

plain_help <- function(title, body) {
  div(
    class = "plain-help",
    tags$strong(title),
    span(body)
  )
}


# ============================================================
# 01_data_helpers.R
# ============================================================

prepare_stock_data <- function(raw_data) {
  if (!is.data.frame(raw_data)) stop("stock_data.rds 必須是 data.frame。")
  required <- c("date", "symbol")
  missing_cols <- setdiff(required, names(raw_data))
  if (length(missing_cols) > 0) {
    stop(paste0("資料缺少必要欄位：", paste(missing_cols, collapse = ", ")))
  }

  x <- raw_data
  x$date <- convert_to_date(x$date)
  if (is.numeric(x$symbol)) {
    x$symbol <- sprintf("%04d", as.integer(x$symbol))
  } else {
    x$symbol <- as.character(x$symbol)
  }

  if ("adjusted" %in% names(x)) {
    x$adjusted <- safe_numeric(x$adjusted)
  } else if ("price.adjusted" %in% names(x)) {
    x$adjusted <- safe_numeric(x$price.adjusted)
  } else if ("close" %in% names(x)) {
    x$adjusted <- safe_numeric(x$close)
  } else if ("price.close" %in% names(x)) {
    x$adjusted <- safe_numeric(x$price.close)
  } else {
    stop("找不到 adjusted / price.adjusted / close / price.close 欄位。")
  }

  if ("close" %in% names(x)) {
    x$close <- safe_numeric(x$close)
  } else if ("price.close" %in% names(x)) {
    x$close <- safe_numeric(x$price.close)
  } else {
    x$close <- x$adjusted
  }

  if ("volume" %in% names(x)) {
    x$volume <- safe_numeric(x$volume)
  } else if ("price.volume" %in% names(x)) {
    x$volume <- safe_numeric(x$price.volume)
  } else {
    x$volume <- NA_real_
  }

  x <- x %>%
    filter(!is.na(date), !is.na(symbol)) %>%
    arrange(symbol, date) %>%
    distinct(symbol, date, .keep_all = TRUE) %>%
    mutate(
      adjusted = ifelse(is.finite(adjusted) & adjusted > 0, adjusted, NA_real_),
      close = ifelse(is.finite(close) & close > 0, close, NA_real_),
      name = get_stock_name(symbol),
      industry = get_stock_industry(symbol),
      industry_group = ifelse(industry %in% major_industries, industry, "其他")
    ) %>%
    group_by(symbol) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      daily_return = adjusted / lag(adjusted) - 1,
      log_return = log(adjusted / lag(adjusted))
    ) %>%
    ungroup() %>%
    arrange(symbol, date)

  if (!BENCHMARK_SYMBOL %in% x$symbol) {
    stop("資料中找不到 0050，無法建立比較基準。")
  }
  x
}

safe_load_data <- function(path = DATA_FILE) {
  if (!file.exists(path)) {
    return(list(
      data = NULL,
      error = paste0("找不到資料檔：", path, "。請確認 GitHub 專案內仍有 data/stock_data.rds。")
    ))
  }
  out <- tryCatch(
    prepare_stock_data(readRDS(path)),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    return(list(data = NULL, error = conditionMessage(out)))
  }
  list(data = out, error = NULL)
}

benchmark_dates <- function(data, end_date = NULL) {
  x <- data %>%
    filter(symbol == BENCHMARK_SYMBOL, !is.na(date)) %>%
    arrange(date) %>%
    pull(date) %>%
    unique()
  if (!is.null(end_date)) x <- x[x <= as.Date(end_date)]
  x
}

trading_window <- function(data, end_date, n_days) {
  dates <- benchmark_dates(data, end_date)
  if (length(dates) == 0) return(list(start = as.Date(NA), end = as.Date(NA), dates = as.Date(character())))
  use <- tail(dates, min(length(dates), as.integer(n_days)))
  list(start = min(use), end = max(use), dates = use)
}

future_trading_window <- function(data, after_date, n_days) {
  dates <- benchmark_dates(data, NULL)
  use <- dates[dates > as.Date(after_date)]
  use <- head(use, min(length(use), as.integer(n_days)))
  if (length(use) == 0) return(list(start = as.Date(NA), end = as.Date(NA), dates = as.Date(character())))
  list(start = min(use), end = max(use), dates = use)
}

filter_window <- function(data, end_date, n_days, symbols = NULL) {
  w <- trading_window(data, end_date, n_days)
  if (length(w$dates) == 0) return(data[0, , drop = FALSE])
  out <- data %>% filter(date >= w$start, date <= w$end)
  if (!is.null(symbols)) out <- out %>% filter(symbol %in% symbols)
  out
}

filter_future_window <- function(data, after_date, n_days, symbols = NULL) {
  w <- future_trading_window(data, after_date, n_days)
  if (length(w$dates) == 0) return(data[0, , drop = FALSE])
  out <- data %>% filter(date >= w$start, date <= w$end)
  if (!is.null(symbols)) out <- out %>% filter(symbol %in% symbols)
  out
}

window_meta <- function(data, end_date, n_days) {
  w <- trading_window(data, end_date, n_days)
  list(
    start = w$start,
    end = w$end,
    expected = length(w$dates)
  )
}

max_drawdown_from_returns <- function(r) {
  r <- r[is.finite(r)]
  if (length(r) < 2) return(NA_real_)
  wealth <- exp(cumsum(r))
  peak <- cummax(wealth)
  dd <- wealth / peak - 1
  min(dd, na.rm = TRUE)
}

cvar_value <- function(x, alpha = 0.95) {
  x <- x[is.finite(x)]
  if (length(x) < 20) return(NA_real_)
  cutoff <- as.numeric(quantile(x, probs = 1 - alpha, na.rm = TRUE, names = FALSE, type = 7))
  tail_x <- x[x <= cutoff]
  if (length(tail_x) == 0) return(NA_real_)
  mean(tail_x, na.rm = TRUE)
}

portfolio_metrics_from_log_returns <- function(r) {
  r <- r[is.finite(r)]
  if (length(r) < 2) {
    return(list(return = NA_real_, volatility = NA_real_, max_drawdown = NA_real_, sharpe = NA_real_, n = length(r)))
  }
  ann_vol <- if (length(r) >= 20) sd(r, na.rm = TRUE) * sqrt(TRADING_DAYS_PER_YEAR) else NA_real_
  ann_mean <- mean(r, na.rm = TRUE) * TRADING_DAYS_PER_YEAR
  list(
    return = exp(sum(r, na.rm = TRUE)) - 1,
    volatility = ann_vol,
    max_drawdown = max_drawdown_from_returns(r),
    sharpe = ifelse(is.finite(ann_vol) && ann_vol > 0, ann_mean / ann_vol, NA_real_),
    n = length(r)
  )
}

rolling_correlation_fast <- function(x, y, window = 60) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  ok <- is.finite(x) & is.finite(y)
  x0 <- ifelse(ok, x, 0)
  y0 <- ifelse(ok, y, 0)
  roll_sum <- function(z) as.numeric(stats::filter(z, rep(1, window), sides = 1))
  n_obs <- roll_sum(as.numeric(ok))
  sx <- roll_sum(x0)
  sy <- roll_sum(y0)
  sxx <- roll_sum(x0 * x0)
  syy <- roll_sum(y0 * y0)
  sxy <- roll_sum(x0 * y0)
  cov_num <- sxy - sx * sy / n_obs
  vx <- sxx - sx * sx / n_obs
  vy <- syy - sy * sy / n_obs
  denom <- sqrt(vx * vy)
  out <- cov_num / denom
  out[n_obs < window | !is.finite(out)] <- NA_real_
  out
}

rsi_value <- function(price, n = 14) {
  price <- price[is.finite(price)]
  if (length(price) < n + 1) return(NA_real_)
  d <- diff(tail(price, n + 1))
  gain <- mean(pmax(d, 0), na.rm = TRUE)
  loss <- mean(pmax(-d, 0), na.rm = TRUE)
  if (!is.finite(gain) || !is.finite(loss)) return(NA_real_)
  if (loss == 0 && gain > 0) return(100)
  if (gain == 0 && loss == 0) return(50)
  rs <- gain / loss
  100 - 100 / (1 + rs)
}


# ============================================================
# 02_scoring.R
# ============================================================

cross_section_score <- function(x, higher_better = TRUE) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x)
  n_ok <- sum(ok)
  if (n_ok == 0) return(out)
  if (n_ok == 1) {
    out[ok] <- 50
    return(out)
  }
  r <- rank(x[ok], ties.method = "average")
  s <- (r - 1) / (n_ok - 1) * 100
  if (!higher_better) s <- 100 - s
  out[ok] <- s
  out
}

benchmark_percentile <- function(value, ref, higher_better = TRUE) {
  ref <- ref[is.finite(ref)]
  if (!is.finite(value) || length(ref) == 0) return(NA_real_)
  less <- mean(ref < value)
  equal <- mean(ref == value)
  p <- 100 * (less + 0.5 * equal)
  if (!higher_better) p <- 100 - p
  p
}

add_score_with_benchmark <- function(df, value_col, score_col, higher_better = TRUE) {
  vals <- df[[value_col]]
  stock_idx <- df$symbol != BENCHMARK_SYMBOL
  bench_idx <- df$symbol == BENCHMARK_SYMBOL
  out <- rep(NA_real_, nrow(df))
  out[stock_idx] <- cross_section_score(vals[stock_idx], higher_better)
  if (any(bench_idx)) {
    out[bench_idx] <- vapply(
      vals[bench_idx],
      benchmark_percentile,
      numeric(1),
      ref = vals[stock_idx],
      higher_better = higher_better
    )
  }
  df[[score_col]] <- out
  df
}

period_metrics <- function(data, end_date, horizon_keys = PERIOD_DEFS$key, min_coverage = 0.75) {
  end_date <- as.Date(end_date)
  symbols <- intersect(c(BENCHMARK_SYMBOL, stock_info$symbol), unique(data$symbol))

  blocks <- lapply(horizon_keys, function(key) {
    n_days <- period_days(key)
    w <- trading_window(data, end_date, n_days)
    expected <- length(w$dates)
    if (expected == 0) return(data.frame())

    x <- data %>%
      filter(symbol %in% symbols, date >= w$start, date <= w$end, is.finite(log_return))

    out <- x %>%
      group_by(symbol) %>%
      summarise(
        n_obs = n(),
        total_log_return = sum(log_return, na.rm = TRUE),
        mean_daily_log = mean(log_return, na.rm = TRUE),
        sd_daily_log = ifelse(n() >= 20, sd(log_return, na.rm = TRUE), NA_real_),
        max_drawdown = max_drawdown_from_returns(log_return),
        cvar_95 = cvar_value(log_return, 0.95),
        .groups = "drop"
      ) %>%
      mutate(
        simple_return = exp(total_log_return) - 1,
        annual_log_mean = mean_daily_log * TRADING_DAYS_PER_YEAR,
        annual_return = exp(annual_log_mean) - 1,
        annual_volatility = sd_daily_log * sqrt(TRADING_DAYS_PER_YEAR),
        sharpe = ifelse(
          is.finite(annual_volatility) & annual_volatility > 0,
          annual_log_mean / annual_volatility,
          NA_real_
        ),
        period = key,
        period_label = period_label(key),
        expected_days = expected,
        coverage = n_obs / expected,
        start_date = w$start,
        end_date = w$end
      )

    # 觀測不足時，不讓殘缺資料進入橫斷面比較。
    out <- out %>%
      mutate(
        total_log_return = ifelse(coverage >= min_coverage, total_log_return, NA_real_),
        simple_return = ifelse(coverage >= min_coverage, simple_return, NA_real_),
        annual_log_mean = ifelse(coverage >= min_coverage, annual_log_mean, NA_real_),
        annual_return = ifelse(coverage >= min_coverage, annual_return, NA_real_),
        annual_volatility = ifelse(coverage >= min_coverage, annual_volatility, NA_real_),
        max_drawdown = ifelse(coverage >= min_coverage, max_drawdown, NA_real_),
        cvar_95 = ifelse(coverage >= min_coverage, cvar_95, NA_real_),
        sharpe = ifelse(coverage >= min_coverage, sharpe, NA_real_)
      )

    out <- add_score_with_benchmark(out, "simple_return", "return_percentile", TRUE)
    out
  })

  bind_rows(blocks)
}

weighted_available_mean <- function(x, w, min_valid = 3) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (sum(ok) < min_valid) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

compute_stability <- function(period_long) {
  if (nrow(period_long) == 0) return(data.frame())

  raw <- period_long %>%
    group_by(symbol) %>%
    summarise(
      stability_return_periods = sum(is.finite(annual_log_mean)),
      stability_rank_periods = sum(is.finite(return_percentile)),
      stability_risk_periods = sum(is.finite(annual_volatility)),
      return_variation_raw = sd(annual_log_mean, na.rm = TRUE),
      rank_variation_raw = sd(return_percentile, na.rm = TRUE),
      risk_variation_raw = sd(annual_volatility, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      return_variation = ifelse(stability_return_periods >= 3, return_variation_raw, NA_real_),
      rank_variation = ifelse(stability_rank_periods >= 3, rank_variation_raw, NA_real_),
      risk_variation = ifelse(stability_risk_periods >= 2, risk_variation_raw, NA_real_)
    )

  raw <- add_score_with_benchmark(raw, "return_variation", "return_stability_score", FALSE)
  raw <- add_score_with_benchmark(raw, "rank_variation", "rank_stability_score", FALSE)
  raw <- add_score_with_benchmark(raw, "risk_variation", "risk_stability_score", FALSE)

  raw %>%
    mutate(
      stability_score = 0.40 * return_stability_score +
        0.30 * rank_stability_score +
        0.30 * risk_stability_score
    )
}

compute_scores <- function(
  data,
  end_date = max(data$date, na.rm = TRUE),
  horizon_keys = PERIOD_DEFS$key,
  risk_horizon = "1y",
  weight_scheme = WEIGHT_SCHEMES$stable,
  min_coverage = 0.75
) {
  end_date <- as.Date(end_date)
  horizon_keys <- intersect(PERIOD_DEFS$key, horizon_keys)
  if (length(horizon_keys) < 3) stop("評分至少需要三個時間尺度。")
  if (!risk_horizon %in% horizon_keys) {
    risk_horizon <- horizon_keys[which.max(PERIOD_DEFS$days[match(horizon_keys, PERIOD_DEFS$key)])]
  }

  p <- period_metrics(data, end_date, horizon_keys, min_coverage)
  if (nrow(p) == 0) return(list(scores = data.frame(), period_metrics = data.frame(), stability = data.frame()))

  stability <- compute_stability(p)

  return_weights <- PERIOD_DEFS$return_weight[match(p$period, PERIOD_DEFS$key)]
  p$return_weight <- return_weights

  multi_return <- p %>%
    group_by(symbol) %>%
    summarise(
      return_periods_used = sum(is.finite(return_percentile)),
      return_score = weighted_available_mean(return_percentile, return_weight, min_valid = 3),
      .groups = "drop"
    )

  risk_base <- p %>%
    filter(period == risk_horizon) %>%
    select(
      symbol, n_obs, expected_days, coverage, start_date, end_date,
      annual_return, annual_volatility, max_drawdown, cvar_95, sharpe
    )

  if (nrow(risk_base) == 0) return(list(scores = data.frame(), period_metrics = p, stability = stability))

  risk_base <- add_score_with_benchmark(risk_base, "annual_volatility", "volatility_score", FALSE)
  risk_base <- add_score_with_benchmark(risk_base, "max_drawdown", "drawdown_score", TRUE)
  risk_base <- add_score_with_benchmark(risk_base, "cvar_95", "cvar_score", TRUE)
  risk_base <- add_score_with_benchmark(risk_base, "sharpe", "sharpe_score", TRUE)

  risk_base <- risk_base %>%
    mutate(
      risk_score = 0.35 * volatility_score + 0.35 * drawdown_score + 0.30 * cvar_score
    )

  scores <- risk_base %>%
    left_join(stability, by = "symbol") %>%
    left_join(multi_return, by = "symbol") %>%
    mutate(
      composite_score = weight_scheme[["stability"]] * stability_score +
        weight_scheme[["return"]] * return_score +
        weight_scheme[["risk"]] * risk_score +
        weight_scheme[["sharpe"]] * sharpe_score,
      name = get_stock_name(symbol),
      industry = get_stock_industry(symbol),
      risk_horizon = risk_horizon,
      model_end_date = end_date
    )

  stock_rank <- scores %>%
    filter(symbol != BENCHMARK_SYMBOL, is.finite(composite_score)) %>%
    arrange(desc(composite_score)) %>%
    mutate(rank = row_number()) %>%
    select(symbol, rank)

  scores <- scores %>%
    left_join(stock_rank, by = "symbol") %>%
    arrange(ifelse(symbol == BENCHMARK_SYMBOL, Inf, rank))

  list(scores = scores, period_metrics = p, stability = stability)
}

score_explanation_table <- function() {
  data.frame(
    構面 = c("穩定性", "報酬", "風險控制", "風險報酬效率"),
    預設權重 = c("40%", "25%", "20%", "15%"),
    具體計算 = c(
      "報酬穩定 40% + 排名穩定 30% + 風險穩定 30%",
      "1週 5% + 1月 10% + 3月 15% + 6月 20% + 1年 25% + 3年 25% 的橫斷面報酬分數",
      "近1年波動度 35% + 最大回撤 35% + CVaR 30%，風險越可控分數越高",
      "近1年夏普比率轉成 0–100 橫斷面分數"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}


# ============================================================
# 03_pca_cluster.R
# ============================================================

average_silhouette <- function(x, cluster) {
  x <- as.matrix(x)
  if (nrow(x) < 3 || length(unique(cluster)) < 2) return(NA_real_)
  d <- as.matrix(dist(x))
  groups <- sort(unique(cluster))
  s <- rep(NA_real_, nrow(x))
  for (i in seq_len(nrow(x))) {
    same <- which(cluster == cluster[i] & seq_len(nrow(x)) != i)
    a <- if (length(same) > 0) mean(d[i, same]) else 0
    other <- groups[groups != cluster[i]]
    b <- min(vapply(other, function(g) mean(d[i, cluster == g]), numeric(1)))
    den <- max(a, b)
    s[i] <- if (is.finite(den) && den > 0) (b - a) / den else 0
  }
  mean(s, na.rm = TRUE)
}

cluster_label_rule <- function(z_row) {
  ret <- z_row[["annual_return"]] %||% 0
  vol <- z_row[["annual_volatility"]] %||% 0
  dd <- z_row[["max_drawdown"]] %||% 0
  cv <- z_row[["cvar_95"]] %||% 0
  sh <- z_row[["sharpe"]] %||% 0
  rs <- z_row[["return_stability_score"]] %||% 0
  rks <- z_row[["rank_stability_score"]] %||% 0
  vs <- z_row[["risk_stability_score"]] %||% 0

  # 注意：max_drawdown / CVaR 越大（越接近 0）代表越好。
  risk_bad <- mean(c(vol, -dd, -cv), na.rm = TRUE)
  stability_good <- mean(c(rs, rks, vs), na.rm = TRUE)

  if (ret > 0.35 && risk_bad > 0.35) return("高報酬高風險型")
  if (risk_bad < -0.35 && stability_good > 0.20) return("低波動穩定型")
  if (sh > 0.40 && ret > 0.15) return("報酬效率型")
  if (stability_good > 0.45) return("穩定均衡型")
  if (ret < -0.35 && risk_bad > 0.20) return("弱勢高風險型")
  "均衡型"
}

run_pca_clustering <- function(data, end_date, period_key = "1y", min_coverage = 0.80) {
  selected_days <- period_days(period_key)
  if (!is.finite(selected_days)) stop("PCA 分析期間設定錯誤。")

  horizons <- horizons_up_to(period_key)
  # 3 個月是最短可正式分群期間；若選更短則拒絕。
  if (selected_days < 63) stop("PCA / 分群至少使用近 3 個月資料。")

  risk_horizon <- period_key
  score_obj <- compute_scores(
    data = data,
    end_date = end_date,
    horizon_keys = horizons,
    risk_horizon = risk_horizon,
    weight_scheme = WEIGHT_SCHEMES$stable,
    min_coverage = min_coverage
  )

  scores <- score_obj$scores %>%
    filter(symbol != BENCHMARK_SYMBOL)

  features <- scores %>%
    transmute(
      symbol,
      name,
      industry,
      coverage,
      annual_return,
      annual_volatility,
      max_drawdown,
      cvar_95,
      sharpe,
      return_stability_score,
      rank_stability_score,
      risk_stability_score
    )

  feature_cols <- c(
    "annual_return", "annual_volatility", "max_drawdown", "cvar_95", "sharpe",
    "return_stability_score", "rank_stability_score", "risk_stability_score"
  )

  all_candidates <- features
  finite_rows <- features$coverage >= min_coverage & apply(
    features[, feature_cols, drop = FALSE],
    1,
    function(v) all(is.finite(as.numeric(v)))
  )
  features <- features[finite_rows, , drop = FALSE]

  if (nrow(features) < 8) stop("符合資料完整度條件的股票太少，無法穩定進行 PCA / 分群。")

  x <- as.matrix(features[, feature_cols, drop = FALSE])
  # 移除零變異欄位，避免 prcomp(scale.=TRUE) 失敗。
  keep <- apply(x, 2, function(v) is.finite(sd(v)) && sd(v) > 0)
  x <- x[, keep, drop = FALSE]
  if (ncol(x) < 3) stop("可用統計特徵不足，無法執行 PCA。")

  pca <- prcomp(x, center = TRUE, scale. = TRUE)
  var_ratio <- pca$sdev^2 / sum(pca$sdev^2)
  cum_var <- cumsum(var_ratio)
  n_pc <- which(cum_var >= 0.80)[1]
  n_pc <- max(2, min(n_pc, ncol(pca$x)))
  pc_x <- pca$x[, seq_len(n_pc), drop = FALSE]

  max_k <- min(6, nrow(pc_x) - 1)
  candidate_k <- 2:max_k
  sil <- rep(NA_real_, length(candidate_k))
  fits <- vector("list", length(candidate_k))
  set.seed(50)
  for (i in seq_along(candidate_k)) {
    k <- candidate_k[i]
    fit <- kmeans(pc_x, centers = k, nstart = 30, iter.max = 100)
    fits[[i]] <- fit
    sil[i] <- average_silhouette(pc_x, fit$cluster)
  }
  best_i <- if (all(!is.finite(sil))) 1 else which.max(sil)
  fit <- fits[[best_i]]
  best_k <- candidate_k[best_i]

  result <- features
  result$cluster <- fit$cluster
  result$PC1 <- pca$x[, 1]
  result$PC2 <- pca$x[, 2]

  # 以原始特徵的 Z 分數判讀每群型態。
  z <- scale(features[, feature_cols, drop = FALSE])
  z_df <- as.data.frame(z)
  z_df$cluster <- fit$cluster
  cluster_z <- z_df %>%
    group_by(cluster) %>%
    summarise(across(all_of(feature_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  labels <- vapply(seq_len(nrow(cluster_z)), function(i) {
    row <- as.list(cluster_z[i, feature_cols, drop = FALSE])
    cluster_label_rule(row)
  }, character(1))
  # 即使規則產生相同名稱，也保留群編號避免混淆。
  cluster_z$cluster_label <- paste0(labels, "（群 ", cluster_z$cluster, "）")
  result <- result %>% left_join(cluster_z %>% select(cluster, cluster_label), by = "cluster")

  # 加入目前正式綜合分數，方便使用者把「群組」和「排名」分開閱讀。
  current_scores <- compute_scores(data, end_date, PERIOD_DEFS$key, "1y", WEIGHT_SCHEMES$stable)$scores %>%
    select(symbol, composite_score)
  result <- result %>% left_join(current_scores, by = "symbol")

  excluded <- all_candidates %>%
    filter(!symbol %in% result$symbol) %>%
    mutate(
      cluster = NA_integer_,
      PC1 = NA_real_,
      PC2 = NA_real_,
      cluster_label = "未分群（資料不足）"
    ) %>%
    left_join(current_scores, by = "symbol")

  all_stocks <- bind_rows(result, excluded)

  loading_df <- data.frame(
    feature = rownames(pca$rotation),
    PC1 = pca$rotation[, 1],
    PC2 = pca$rotation[, 2],
    stringsAsFactors = FALSE
  )

  feature_names_cn <- c(
    annual_return = "年化報酬",
    annual_volatility = "年化波動",
    max_drawdown = "最大回撤",
    cvar_95 = "CVaR",
    sharpe = "夏普",
    return_stability_score = "報酬穩定",
    rank_stability_score = "排名穩定",
    risk_stability_score = "風險穩定"
  )

  top_loading_text <- function(pc_col) {
    vals <- loading_df[[pc_col]]
    ord <- order(abs(vals), decreasing = TRUE)
    top <- loading_df$feature[head(ord, 3)]
    paste(unname(feature_names_cn[top]), collapse = "、")
  }

  w <- trading_window(data, end_date, selected_days)

  list(
    stocks = result,
    all_stocks = all_stocks,
    pca = pca,
    variance = data.frame(
      component = paste0("PC", seq_along(var_ratio)),
      variance_ratio = var_ratio,
      cumulative = cum_var
    ),
    loadings = loading_df,
    retained_pc = n_pc,
    best_k = best_k,
    silhouette = data.frame(k = candidate_k, silhouette = sil),
    cluster_profile_z = cluster_z,
    pc1_plain = top_loading_text("PC1"),
    pc2_plain = top_loading_text("PC2"),
    start_date = w$start,
    end_date = w$end,
    expected_days = length(w$dates),
    included_count = nrow(result),
    period_key = period_key
  )
}


# ============================================================
# 04_validation.R
# 評分模型驗證 + Block Bootstrap
# ============================================================

rebalance_dates <- function(data, frequency = "quarterly") {
  dates <- benchmark_dates(data)
  if (length(dates) == 0) return(as.Date(character()))
  df <- data.frame(date = dates)
  if (frequency == "monthly") {
    df$key <- format(df$date, "%Y-%m")
  } else if (frequency == "annual") {
    df$key <- format(df$date, "%Y")
  } else {
    m <- as.integer(format(df$date, "%m"))
    q <- (m - 1) %/% 3 + 1
    df$key <- paste0(format(df$date, "%Y"), "-Q", q)
  }
  df %>% group_by(key) %>% summarise(date = max(date), .groups = "drop") %>% pull(date)
}

backtest_score_config <- function(lookback_key) {
  horizons <- horizons_up_to(lookback_key)
  risk_horizon <- if (period_days(lookback_key) >= 252) "1y" else lookback_key
  list(horizons = horizons, risk_horizon = risk_horizon)
}

future_stock_returns <- function(data, rebalance_date, forward_n, symbols) {
  fw <- future_trading_window(data, rebalance_date, forward_n)
  if (length(fw$dates) < max(3, floor(forward_n * 0.80))) return(data.frame())

  data %>%
    filter(symbol %in% symbols, date >= fw$start, date <= fw$end, is.finite(log_return)) %>%
    group_by(symbol) %>%
    summarise(
      n_obs = n(),
      sum_log_return = sum(log_return, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      forward_return = ifelse(n_obs >= floor(length(fw$dates) * 0.80), exp(sum_log_return) - 1, NA_real_)
    )
}

strategy_future_metrics <- function(data, rebalance_date, forward_n, symbols) {
  fw <- future_trading_window(data, rebalance_date, forward_n)
  if (length(fw$dates) < max(3, floor(forward_n * 0.80))) {
    return(list(return = NA_real_, volatility = NA_real_, max_drawdown = NA_real_, sharpe = NA_real_, n = 0))
  }

  x <- data %>%
    filter(symbol %in% symbols, date >= fw$start, date <= fw$end, is.finite(log_return)) %>%
    group_by(date) %>%
    summarise(
      n_assets = n_distinct(symbol),
      portfolio_log_return = mean(log_return, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_assets >= max(1, ceiling(length(symbols) * 0.70)))

  portfolio_metrics_from_log_returns(x$portfolio_log_return)
}

run_model_validation <- function(
  data,
  lookback_key = "1y",
  forward_key = "1y",
  frequency = "quarterly",
  weight_scheme = WEIGHT_SCHEMES$stable,
  progress_cb = NULL
) {
  cfg <- backtest_score_config(lookback_key)
  lookback_n <- period_days(lookback_key)
  forward_n <- forward_days(forward_key)
  dates <- rebalance_dates(data, frequency)
  all_dates <- benchmark_dates(data)
  if (length(dates) == 0 || length(all_dates) == 0) return(list(events = data.frame(), summary = data.frame(), rank_cor = data.frame()))

  first_eligible <- all_dates[min(length(all_dates), lookback_n)]
  last_possible_idx <- length(all_dates) - forward_n
  if (last_possible_idx <= 0) return(list(events = data.frame(), summary = data.frame(), rank_cor = data.frame()))
  last_eligible <- all_dates[last_possible_idx]
  dates <- dates[dates >= first_eligible & dates <= last_eligible]
  if (length(dates) == 0) return(list(events = data.frame(), summary = data.frame(), rank_cor = data.frame()))

  event_rows <- list()
  cor_rows <- list()
  event_i <- 0L
  cor_i <- 0L

  for (i in seq_along(dates)) {
    d <- dates[i]
    if (!is.null(progress_cb)) progress_cb(i, length(dates), d)

    score_obj <- tryCatch(
      compute_scores(
        data = data,
        end_date = d,
        horizon_keys = cfg$horizons,
        risk_horizon = cfg$risk_horizon,
        weight_scheme = weight_scheme,
        min_coverage = 0.75
      ),
      error = function(e) NULL
    )
    if (is.null(score_obj) || nrow(score_obj$scores) == 0) next

    rank_df <- score_obj$scores %>%
      filter(symbol != BENCHMARK_SYMBOL, is.finite(composite_score)) %>%
      arrange(desc(composite_score))
    if (nrow(rank_df) < 20) next

    top5 <- head(rank_df$symbol, 5)
    top10 <- head(rank_df$symbol, 10)
    bottom10 <- tail(rank_df$symbol, 10)

    strategies <- list(
      Top5 = top5,
      Top10 = top10,
      Bottom10 = bottom10,
      `0050` = BENCHMARK_SYMBOL
    )

    for (nm in names(strategies)) {
      m <- strategy_future_metrics(data, d, forward_n, strategies[[nm]])
      event_i <- event_i + 1L
      event_rows[[event_i]] <- data.frame(
        rebalance_date = d,
        strategy = nm,
        forward_return = m$return,
        volatility = m$volatility,
        max_drawdown = m$max_drawdown,
        sharpe = m$sharpe,
        n_obs = m$n,
        stringsAsFactors = FALSE
      )
    }

    # Spearman：當時的分數排名 vs 未來個股報酬排名。
    fut <- future_stock_returns(data, d, forward_n, rank_df$symbol)
    if (nrow(fut) > 5) {
      cc <- rank_df %>% select(symbol, composite_score) %>% inner_join(fut, by = "symbol") %>% filter(is.finite(forward_return))
      rho <- if (nrow(cc) >= 6) suppressWarnings(cor(cc$composite_score, cc$forward_return, method = "spearman", use = "complete.obs")) else NA_real_
      cor_i <- cor_i + 1L
      cor_rows[[cor_i]] <- data.frame(rebalance_date = d, spearman = rho, n_stocks = nrow(cc))
    }
  }

  events <- bind_rows(event_rows)
  rank_cor <- bind_rows(cor_rows)
  if (nrow(events) == 0) return(list(events = events, summary = data.frame(), rank_cor = rank_cor))

  bench <- events %>%
    filter(strategy == "0050") %>%
    select(rebalance_date, benchmark_return = forward_return)

  events <- events %>%
    left_join(bench, by = "rebalance_date") %>%
    mutate(beat_0050 = ifelse(strategy == "0050", NA, forward_return > benchmark_return))

  summary <- events %>%
    group_by(strategy) %>%
    summarise(
      validation_count = sum(is.finite(forward_return)),
      avg_return = mean(forward_return, na.rm = TRUE),
      median_return = median(forward_return, na.rm = TRUE),
      avg_volatility = mean(volatility, na.rm = TRUE),
      avg_max_drawdown = mean(max_drawdown, na.rm = TRUE),
      avg_sharpe = mean(sharpe, na.rm = TRUE),
      beat_0050_rate = ifelse(strategy == "0050", NA_real_, mean(beat_0050, na.rm = TRUE)),
      .groups = "drop"
    )

  list(events = events, summary = summary, rank_cor = rank_cor)
}

block_bootstrap_indices <- function(n, block_length = 10L) {
  if (n <= 1) return(seq_len(n))
  block_length <- max(2L, min(as.integer(block_length), n))
  starts <- seq_len(max(1L, n - block_length + 1L))
  idx <- integer(0)
  while (length(idx) < n) {
    s <- sample(starts, 1)
    idx <- c(idx, s:min(n, s + block_length - 1L))
  }
  idx[seq_len(n)]
}

score_return_matrix <- function(return_matrix, risk_days = 252L) {
  m <- as.matrix(return_matrix)
  symbols <- colnames(m)
  if (is.null(symbols) || ncol(m) < 3 || nrow(m) < 63) return(data.frame())

  defs <- PERIOD_DEFS[PERIOD_DEFS$days <= nrow(m), , drop = FALSE]
  if (nrow(defs) < 3) return(data.frame())

  # 各期間報酬與橫斷面分數
  period_list <- vector("list", nrow(defs))
  for (i in seq_len(nrow(defs))) {
    n <- defs$days[i]
    rows <- lapply(seq_along(symbols), function(j) {
      r <- tail(m[, j], min(n, nrow(m)))
      r <- r[is.finite(r)]
      ok <- length(r) >= floor(n * 0.75)
      data.frame(
        symbol = symbols[j],
        period = defs$key[i],
        return_weight = defs$return_weight[i],
        total_return = if (ok) exp(sum(r)) - 1 else NA_real_,
        annual_log_mean = if (ok) mean(r) * TRADING_DAYS_PER_YEAR else NA_real_,
        annual_volatility = if (ok && length(r) >= 20) sd(r) * sqrt(TRADING_DAYS_PER_YEAR) else NA_real_
      )
    })
    tmp <- bind_rows(rows)
    tmp <- add_score_with_benchmark(tmp, "total_return", "return_percentile", TRUE)
    period_list[[i]] <- tmp
  }
  p <- bind_rows(period_list)
  stability <- compute_stability(
    p %>% mutate(
      simple_return = total_return,
      n_obs = NA_integer_, expected_days = NA_integer_, coverage = NA_real_,
      start_date = as.Date(NA), end_date = as.Date(NA),
      period_label = period
    )
  )

  ret_score <- p %>%
    group_by(symbol) %>%
    summarise(return_score = weighted_available_mean(return_percentile, return_weight, 3), .groups = "drop")

  risk_days <- min(as.integer(risk_days), nrow(m))
  risk_rows <- lapply(seq_along(symbols), function(j) {
    r <- tail(m[, j], risk_days)
    r <- r[is.finite(r)]
    ann_vol <- if (length(r) >= 20) sd(r) * sqrt(TRADING_DAYS_PER_YEAR) else NA_real_
    ann_mean <- if (length(r) > 0) mean(r) * TRADING_DAYS_PER_YEAR else NA_real_
    data.frame(
      symbol = symbols[j],
      annual_volatility = ann_vol,
      max_drawdown = max_drawdown_from_returns(r),
      cvar_95 = cvar_value(r),
      sharpe = ifelse(is.finite(ann_vol) && ann_vol > 0, ann_mean / ann_vol, NA_real_)
    )
  })
  risk <- bind_rows(risk_rows)
  risk <- add_score_with_benchmark(risk, "annual_volatility", "volatility_score", FALSE)
  risk <- add_score_with_benchmark(risk, "max_drawdown", "drawdown_score", TRUE)
  risk <- add_score_with_benchmark(risk, "cvar_95", "cvar_score", TRUE)
  risk <- add_score_with_benchmark(risk, "sharpe", "sharpe_score", TRUE)
  risk <- risk %>% mutate(risk_score = .35 * volatility_score + .35 * drawdown_score + .30 * cvar_score)

  risk %>%
    left_join(stability, by = "symbol") %>%
    left_join(ret_score, by = "symbol") %>%
    mutate(composite_score = .40 * stability_score + .25 * return_score + .20 * risk_score + .15 * sharpe_score)
}

return_wide_matrix <- function(data, end_date, period_key = "1y") {
  x <- filter_window(data, end_date, period_days(period_key)) %>%
    filter(symbol %in% c(BENCHMARK_SYMBOL, stock_info$symbol)) %>%
    select(date, symbol, log_return) %>%
    distinct(date, symbol, .keep_all = TRUE) %>%
    pivot_wider(names_from = symbol, values_from = log_return) %>%
    arrange(date)
  if (nrow(x) == 0 || ncol(x) < 4) return(NULL)
  m <- as.matrix(x[, -1, drop = FALSE])
  storage.mode(m) <- "double"
  m
}

run_block_bootstrap <- function(data, symbol, end_date, period_key = "1y", reps = 100L, block_length = 10L, progress_cb = NULL) {
  m <- return_wide_matrix(data, end_date, period_key)
  if (is.null(m) || !symbol %in% colnames(m) || !BENCHMARK_SYMBOL %in% colnames(m)) {
    stop("目前期間沒有足夠資料進行區塊拔靴法。")
  }
  risk_days <- min(252L, nrow(m))
  reps <- as.integer(reps)
  set.seed(50)
  diffs <- rep(NA_real_, reps)
  for (b in seq_len(reps)) {
    if (!is.null(progress_cb)) progress_cb(b, reps)
    idx <- block_bootstrap_indices(nrow(m), block_length)
    s <- score_return_matrix(m[idx, , drop = FALSE], risk_days = risk_days)
    if (nrow(s) == 0) next
    a <- s$composite_score[s$symbol == symbol]
    z <- s$composite_score[s$symbol == BENCHMARK_SYMBOL]
    if (length(a) == 1 && length(z) == 1 && is.finite(a) && is.finite(z)) diffs[b] <- a - z
  }
  diffs <- diffs[is.finite(diffs)]
  if (length(diffs) < max(20, floor(reps * .5))) stop("有效拔靴樣本太少，請改用較長分析期間。")
  ci <- as.numeric(quantile(diffs, c(.025, .975), na.rm = TRUE, names = FALSE))
  p_above <- mean(diffs > 0)
  label <- if (ci[1] > 0) "相對 0050 的優勢較穩定" else if (ci[2] < 0) "相對 0050 偏弱" else "差異仍有不確定性"
  list(
    diffs = diffs,
    mean_diff = mean(diffs),
    lower = ci[1],
    upper = ci[2],
    p_above = p_above,
    label = label
  )
}


# ---- 啟動時安全載入資料 ----
STARTUP <- safe_load_data(DATA_FILE)
APP_DATA <- STARTUP$data
STARTUP_ERROR <- STARTUP$error


# ============================================================
# 05_ui.R
# ============================================================

app_css <- "
body { background:#F7F9FC; color:#24364B; font-family:-apple-system,BlinkMacSystemFont,'Segoe UI','Noto Sans TC',sans-serif; }
.navbar { background:linear-gradient(90deg,#EEF6FD,#F7F4FD)!important; border:0; border-bottom:1px solid #DCE6F0; box-shadow:0 2px 10px rgba(43,72,101,.05); }
.navbar-default .navbar-brand { color:#2D5278!important; font-weight:700; }
.navbar-default .navbar-nav>li>a { color:#4B6782!important; }
.navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:focus, .navbar-default .navbar-nav>.active>a:hover { background:#FFFFFF!important; color:#2E6FA3!important; }
.container-fluid { max-width:1600px; }
h2,h3,h4 { color:#2A4E72; font-weight:700; }
.page-intro { color:#65798D; margin-bottom:16px; line-height:1.7; }
.section-card { background:#FFFFFF; border:1px solid #E1E8F0; border-radius:14px; padding:18px; margin-bottom:18px; box-shadow:0 4px 16px rgba(34,62,90,.04); }
.info-strip { display:flex; gap:9px; align-items:center; background:#F0F6FC; border-left:4px solid #7AAAD8; padding:10px 13px; border-radius:10px; margin:10px 0 16px 0; color:#42617E; }
.plain-help { background:#F7F6FC; border-left:4px solid #9B88D3; padding:12px 14px; border-radius:10px; line-height:1.7; margin:10px 0 14px 0; }
.plain-help strong { margin-right:6px; color:#3A5570; }
.kpi-grid { display:grid; grid-template-columns:repeat(4,minmax(170px,1fr)); gap:14px; margin-bottom:18px; }
.kpi-card { background:#FFFFFF; border:1px solid #E2E9F1; border-top:4px solid #7FAED9; border-radius:14px; padding:15px 17px; min-height:116px; box-shadow:0 4px 14px rgba(34,62,90,.04); }
.kpi-card.tone-green { border-top-color:#79B18A; }
.kpi-card.tone-orange { border-top-color:#E1AD5A; }
.kpi-card.tone-purple { border-top-color:#9A82D0; }
.kpi-title { color:#698096; font-size:14px; font-weight:600; }
.kpi-value { color:#284D70; font-size:28px; font-weight:750; margin:5px 0; }
.kpi-subtitle { color:#8293A4; font-size:12px; line-height:1.4; }
.form-control, .selectize-input { border-color:#D6E0EA; border-radius:9px; box-shadow:none; }
.btn-primary { background:#6F9FCB; border-color:#6F9FCB; border-radius:9px; }
.btn-primary:hover { background:#5B8DBB; border-color:#5B8DBB; }
.radio-inline, .checkbox-inline { margin-right:9px; }
.shiny-options-group .radio-inline span { color:#425D77; }
table { background:#FFFFFF; }
.table>thead>tr>th { background:#F1F5F9; color:#3F5F7F; border-bottom:1px solid #D9E2EB; }
.table>tbody>tr>td { background:#FFFFFF; border-top:1px solid #E7EDF3; }
.table-hover>tbody>tr:hover>td { background:#F8FBFE; }
.warning-box { background:#FFF8ED; border-left:4px solid #D8A151; padding:12px 14px; border-radius:9px; color:#6B5840; line-height:1.7; }
.method-box { background:#FFFFFF; border:1px solid #E3EAF2; border-radius:12px; padding:16px; margin-bottom:14px; }
.small-note { color:#7B8D9F; font-size:12px; line-height:1.6; }
@media (max-width:900px){ .kpi-grid{grid-template-columns:repeat(2,1fr);} }
@media (max-width:560px){ .kpi-grid{grid-template-columns:1fr;} }
"

period_choices <- stats::setNames(PERIOD_DEFS$key, PERIOD_DEFS$label)
pca_period_choices <- stats::setNames(c("3m", "6m", "1y", "3y"), c("近 3 個月", "近 6 個月", "近 1 年", "近 3 年"))
available_stock_symbols <- if (!is.null(APP_DATA)) {
  intersect(stock_info$symbol, unique(APP_DATA$symbol))
} else {
  stock_info$symbol
}
if (length(available_stock_symbols) == 0) available_stock_symbols <- stock_info$symbol
stock_choices <- stats::setNames(available_stock_symbols, get_stock_label(available_stock_symbols))
default_stock_symbol <- if ("2330" %in% available_stock_symbols) "2330" else available_stock_symbols[1]

ui <- navbarPage(
  title = "0050 投資研究 Dashboard",
  id = "main_nav",
  header = tags$style(HTML(app_css)),

  tabPanel(
    "0050 總覽",
    fluidPage(
      h2("0050 總覽"),
      div(class = "page-intro", "先看 0050 本身目前的走勢、報酬與風險。價格拿來看走勢；統計分析主要使用日對數報酬率。"),
      uiOutput("startup_status"),
      div(class = "section-card",
          radioButtons("overview_period", "分析期間", choices = period_choices, selected = "1y", inline = TRUE),
          uiOutput("overview_period_info"),
          uiOutput("overview_kpis")
      ),
      fluidRow(
        column(7, div(class = "section-card", h3("0050 價格走勢"), plotOutput("overview_price_plot", height = 340))),
        column(5, div(class = "section-card", h3("日對數報酬率"), plotOutput("overview_return_plot", height = 340)))
      ),
      div(class = "section-card", h3("目前統計摘要"), tableOutput("overview_stats_table"))
    )
  ),

  tabPanel(
    "成分股探索",
    fluidPage(
      h2("成分股探索"),
      div(class = "page-intro", "把 0050 成分股放在相同時間尺度下比較，先看報酬，再看產業與近期狀態。"),
      div(class = "section-card",
          selectInput("explore_industry", "產業", choices = c("全部" = "ALL", sort(unique(stock_info$industry))), selected = "ALL"),
          uiOutput("explore_period_info")
      ),
      fluidRow(
        column(7, div(class = "section-card", h3("多期間報酬比較"), tableOutput("explore_table"))),
        column(5, div(class = "section-card", h3("近 1 年報酬分布"), plotOutput("explore_return_plot", height = 420)))
      ),
      div(class = "section-card", h3("產業概覽"), tableOutput("industry_table"))
    )
  ),

  tabPanel(
    "綜合排名",
    fluidPage(
      h2("成分股綜合排名"),
      div(class = "page-intro", "排名不是單純找漲最多的股票，而是把穩定程度、報酬、風險控制與風險報酬效率一起比較。0050 是比較基準，不參與成分股名次。"),
      plain_help("這個模型比較重視什麼？", "希望一檔股票在不同期間的表現不要只靠短期暴衝，同時兼顧報酬、回撤、極端損失與承擔風險後得到的報酬效率。"),
      div(class = "section-card",
          selectInput("ranking_weight_scheme", "評分權重", choices = WEIGHT_SCHEME_LABELS, selected = "stable"),
          uiOutput("ranking_period_info"),
          uiOutput("benchmark_score_info"),
          uiOutput("ranking_weight_cards")
      ),
      div(class = "section-card",
          h3("具體怎麼算？"),
          p("不同指標的單位不同，所以先把每個指標轉成相對 0–100 分，再依權重加總。"),
          tags$p(tags$strong("綜合分數 = 穩定性 × 權重 + 報酬 × 權重 + 風險控制 × 權重 + 風險報酬效率 × 權重")),
          tableOutput("score_explanation"),
          h4("本次各時間尺度實際資料範圍"),
          tableOutput("ranking_time_table")
      ),
      fluidRow(
        column(8, div(class = "section-card", h3("綜合排名"), tableOutput("ranking_table"))),
        column(4, div(class = "section-card", h3("Top 15 分數"), plotOutput("ranking_bar_plot", height = 520)))
      )
    )
  ),

  tabPanel(
    "個股分析",
    fluidPage(
      h2("個股分析"),
      div(class = "page-intro", "同一檔股票在全站使用固定代表色；可以切換 1週到3年的統一時間尺度。"),
      div(class = "section-card",
          selectInput("stock_symbol", "股票", choices = stock_choices, selected = default_stock_symbol),
          radioButtons("stock_period", "分析期間", choices = period_choices, selected = "1y", inline = TRUE),
          uiOutput("stock_period_info"),
          uiOutput("stock_kpis")
      ),
      fluidRow(
        column(7, div(class = "section-card", h3("價格與均線"), plotOutput("stock_price_plot", height = 350))),
        column(5, div(class = "section-card", h3("累積報酬"), plotOutput("stock_cum_return_plot", height = 350)))
      ),
      fluidRow(
        column(6, div(class = "section-card", h3("日對數報酬分布"), plotOutput("stock_return_hist", height = 320))),
        column(6, div(class = "section-card", h3("與 0050 的 60 日滾動相關"), plotOutput("stock_rolling_corr", height = 320)))
      ),
      fluidRow(
        column(6, div(class = "section-card", h3("風險與相關性"), tableOutput("stock_risk_table"))),
        column(6, div(class = "section-card", h3("趨勢輔助資訊"), tableOutput("stock_technical_table")))
      )
    )
  ),

  tabPanel(
    "風險與穩定性",
    fluidPage(
      h2("風險與穩定性"),
      div(class = "page-intro", "風險主要看近 1 年；穩定性則比較 1週、1月、3月、6月、1年、3年的結果是否一致。"),
      uiOutput("risk_period_info"),
      div(class = "section-card", h3("穩定性使用的實際時間範圍"), tableOutput("stability_time_table")),
      fluidRow(
        column(7, div(class = "section-card", h3("報酬－風險位置"), plotOutput("risk_return_plot", height = 430))),
        column(5, div(class = "section-card", h3("穩定性 Top 15"), plotOutput("stability_plot", height = 430)))
      ),
      div(class = "section-card", h3("風險與穩定性明細"), tableOutput("risk_stability_table"))
    )
  ),

  tabPanel(
    "模型驗證",
    fluidPage(
      h2("評分模型驗證"),
      div(class = "page-intro", "不是拿歷史資料做漂亮的投資模擬，而是檢查：如果當時真的只知道過去資料，高分股票之後是否比較好。"),
      tabsetPanel(
        tabPanel(
          "歷史滾動驗證",
          div(class = "section-card",
              fluidRow(
                column(3, selectInput("bt_lookback", "評分回看期間", choices = c("近 6 個月" = "6m", "近 1 年" = "1y", "近 3 年" = "3y"), selected = "1y")),
                column(3, selectInput("bt_forward", "往後驗證多久", choices = stats::setNames(FORWARD_PERIOD_DEFS$key, FORWARD_PERIOD_DEFS$label), selected = "1y")),
                column(3, selectInput("bt_frequency", "重新評分頻率", choices = c("每月" = "monthly", "每季" = "quarterly", "每年" = "annual"), selected = "quarterly")),
                column(3, selectInput("bt_weight_scheme", "權重版本", choices = WEIGHT_SCHEME_LABELS, selected = "stable"))
              ),
              actionButton("run_backtest", "執行模型驗證", class = "btn-primary"),
              actionButton("run_sensitivity", "比較三種權重"),
              br(), br(),
              uiOutput("backtest_info")
          ),
          div(class = "section-card", h3("模型驗證摘要"), tableOutput("backtest_summary")),
          div(class = "section-card",
              h3("權重敏感度比較"),
              p(class = "small-note", "使用上方相同的回看期間、未來驗證期間與重新評分頻率，分別驗證三組權重。此計算較重，只有按下『比較三種權重』才執行。"),
              tableOutput("sensitivity_table")
          ),
          fluidRow(
            column(7, div(class = "section-card", h3("各次驗證的後續報酬"), plotOutput("backtest_return_plot", height = 390))),
            column(5, div(class = "section-card", h3("分數 vs 未來表現：Spearman"), plotOutput("backtest_spearman_plot", height = 390)))
          ),
          div(class = "section-card", h3("逐次驗證結果"), tableOutput("backtest_events")),
          div(class = "warning-box", "限制：目前以目前可取得的 0050 成分股回溯，因此可能存在成分股變動造成的存活者偏差。模型驗證結果用來檢查區辨能力，不代表未來一定重現。")
        ),
        tabPanel(
          "區塊拔靴法",
          div(class = "section-card",
              fluidRow(
                column(4, selectInput("bootstrap_symbol", "股票", choices = stock_choices, selected = default_stock_symbol)),
                column(3, selectInput("bootstrap_period", "分析期間", choices = c("近 6 個月" = "6m", "近 1 年" = "1y", "近 3 年" = "3y"), selected = "1y")),
                column(2, selectInput("bootstrap_reps", "抽樣次數", choices = c(50, 100, 200), selected = 100)),
                column(3, selectInput("bootstrap_block", "連續區塊長度", choices = c("5 日" = 5, "10 日" = 10, "20 日" = 20), selected = 10))
              ),
              actionButton("run_bootstrap", "開始拔靴分析", class = "btn-primary"),
              br(), br(),
              uiOutput("bootstrap_period_info")
          ),
          uiOutput("bootstrap_kpis"),
          div(class = "section-card", h3("分數差的拔靴分布"), plotOutput("bootstrap_plot", height = 380)),
          plain_help("這是在做什麼？", "把歷史日對數報酬一小段一小段重新抽很多次，看看這檔股票相對 0050 的分數優勢會不會因為資料稍微改變就消失。它不是未來價格預測。")
        )
      )
    )
  ),

  tabPanel(
    "主成分與分群",
    fluidPage(
      h2("主成分與分群"),
      div(class = "page-intro", "一檔股票是一筆資料。PCA 與分群使用同一選定期間內的報酬、風險與穩定性特徵，不直接拿股票價格做分群。"),
      div(class = "section-card",
          selectInput("pca_period", "PCA / 分群分析期間", choices = pca_period_choices, selected = "1y"),
          actionButton("run_pca", "更新 PCA / 分群", class = "btn-primary"),
          br(), br(),
          uiOutput("pca_period_info")
      ),
      plain_help("使用哪些資料？", "年化報酬率、年化波動度、最大回撤、CVaR、夏普比率、報酬穩定性、排名穩定性、風險穩定性。所有特徵會先標準化，再做 PCA。"),
      uiOutput("pca_interpretation"),
      fluidRow(
        column(7, div(class = "section-card", h3("PCA 分群圖"), plotOutput("pca_scatter", height = 480))),
        column(5, div(class = "section-card", h3("群數選擇：輪廓係數"), plotOutput("pca_silhouette", height = 260), h3("主成分解釋率"), plotOutput("pca_variance", height = 210)))
      ),
      div(class = "section-card", h3("每一檔股票在哪一組？"), selectInput("cluster_filter", "篩選群組", choices = "全部", selected = "全部"), tableOutput("cluster_stock_table")),
      fluidRow(
        column(6, div(class = "section-card", h3("各群平均特徵"), tableOutput("cluster_profile_table"))),
        column(6, div(class = "section-card", h3("PC1 / PC2 主要負荷"), tableOutput("pca_loading_table")))
      )
    )
  ),

  tabPanel(
    "研究方法與說明",
    fluidPage(
      h2("研究方法與說明"),
      div(class = "page-intro", "這一頁專門交代資料期間、公式、模型定位與限制，避免不同頁面的時間定義不一致。"),
      div(class = "method-box", h3("全站統一時間尺度"), tableOutput("method_period_table")),
      div(class = "method-box",
          h3("核心原則"),
          tags$ul(
            tags$li(tags$strong("價格拿來看走勢；日對數報酬率拿來做統計。")),
            tags$li("報酬與穩定性同時看 1週、1月、3月、6月、1年、3年。"),
            tags$li("主風險與夏普預設看近 1 年，避免十年前資料把現在狀態沖淡。"),
            tags$li("PCA / 分群可切換 3月、6月、1年、3年，且同一次分析所有特徵一定使用同一時間區間。"),
            tags$li("歷史驗證會分開顯示：評分往回看多久，以及評分後往前驗證多久。")
          )
      ),
      div(class = "method-box",
          h3("日對數報酬率"),
          tags$p("rₜ = ln(Pₜ / Pₜ₋₁)"),
          p("相關性、波動度、CVaR、夏普、PCA 特徵、Bootstrap 與大部分模型計算都以日對數報酬為基礎。")
      ),
      div(class = "method-box",
          h3("評分模型"),
          tableOutput("method_score_table"),
          p("預設權重 40 / 25 / 20 / 15 只是研究起點，會透過歷史樣本外驗證與權重敏感度分析檢查，不為了讓結果漂亮而事後硬調。"),
          p("資料完整性規則：單一期間至少要有該視窗 75% 的有效日對數報酬才進入主評分；報酬至少要有 3 個有效時間尺度。缺少某個較長期間時，不硬補資料，而是只在有效期間中重新正規化權重。PCA / 分群則提高到 80% 完整度。")
      ),
      div(class = "method-box",
          h3("PCA / 分群"),
          p("PCA 的目的是把高度重複的統計指標整理成少數主要方向；分群則找出統計特徵相似的股票。群組名稱是依實際群內特徵判讀，不是先指定股票應該去哪一群。")
      ),
      div(class = "warning-box", "本 Dashboard 是統計研究與模型驗證工具，不提供個人化投資建議。若未來加入『統計配置傾向』，也會等評分模型完成歷史驗證後才啟用。")
    )
  )
)


# ============================================================
# 06_server.R
# ============================================================

server <- function(input, output, session) {

  data_r <- reactive({
    req(!is.null(APP_DATA))
    APP_DATA
  })

  latest_date_r <- reactive({ max(data_r()$date, na.rm = TRUE) })

  output$startup_status <- renderUI({
    if (!is.null(STARTUP_ERROR)) {
      return(div(class = "warning-box", tags$strong("資料載入失敗："), STARTUP_ERROR))
    }
    x <- APP_DATA
    div(
      class = "info-strip",
      icon("database"),
      span(paste0(
        "資料已載入｜", fmt_date(min(x$date, na.rm = TRUE)), " ～ ", fmt_date(max(x$date, na.rm = TRUE)),
        "｜", length(unique(x$symbol)), " 個代號"
      ))
    )
  })

  # ----------------------------------------------------------
  # 共用評分
  # ----------------------------------------------------------
  default_score_obj <- reactive({
    compute_scores(
      data_r(), latest_date_r(), PERIOD_DEFS$key, "1y", WEIGHT_SCHEMES$stable, 0.75
    )
  })

  ranking_score_obj <- reactive({
    scheme <- WEIGHT_SCHEMES[[input$ranking_weight_scheme %||% "stable"]]
    compute_scores(data_r(), latest_date_r(), PERIOD_DEFS$key, "1y", scheme, 0.75)
  })

  # ----------------------------------------------------------
  # 0050 總覽
  # ----------------------------------------------------------
  overview_window <- reactive({
    key <- input$overview_period %||% "1y"
    filter_window(data_r(), latest_date_r(), period_days(key), BENCHMARK_SYMBOL) %>% arrange(date)
  })

  output$overview_period_info <- renderUI({
    x <- overview_window()
    req(nrow(x) > 0)
    make_info_strip(format_period_info(min(x$date), max(x$date), sum(is.finite(x$log_return)), period_label(input$overview_period)))
  })

  output$overview_kpis <- renderUI({
    x <- overview_window() %>% filter(is.finite(log_return))
    req(nrow(x) >= 2)
    m <- portfolio_metrics_from_log_returns(x$log_return)
    latest <- tail(x$adjusted[is.finite(x$adjusted)], 1)
    div(
      class = "kpi-grid",
      make_kpi("最新調整後價格", ifelse(length(latest) == 1, sprintf("%.2f", latest), "—"), period_label(input$overview_period), "blue"),
      make_kpi("期間報酬", fmt_pct(m$return, signed = TRUE), "由日對數報酬累積", "green"),
      make_kpi("年化波動度", fmt_pct(m$volatility), "越高代表日常起伏越大", "orange"),
      make_kpi("最大回撤", fmt_pct(m$max_drawdown), "期間內高點到低點最深跌幅", "purple")
    )
  })

  output$overview_price_plot <- renderPlot({
    x <- overview_window() %>% filter(is.finite(adjusted))
    req(nrow(x) > 1)
    ggplot(x, aes(date, adjusted)) +
      geom_line(linewidth = 1, color = get_stock_color(BENCHMARK_SYMBOL)) +
      labs(x = NULL, y = "調整後價格") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })

  output$overview_return_plot <- renderPlot({
    x <- overview_window() %>% filter(is.finite(log_return))
    req(nrow(x) > 1)
    ggplot(x, aes(date, log_return)) +
      geom_col(aes(fill = log_return >= 0), width = 1) +
      scale_fill_manual(values = c(`TRUE` = UP_COLOR, `FALSE` = DOWN_COLOR), guide = "none") +
      scale_y_continuous(labels = function(v) sprintf("%.1f%%", 100 * v)) +
      labs(x = NULL, y = "日對數報酬率") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })

  output$overview_stats_table <- renderTable({
    x <- overview_window() %>% filter(is.finite(log_return))
    req(nrow(x) >= 2)
    m <- portfolio_metrics_from_log_returns(x$log_return)
    data.frame(
      指標 = c("期間累積報酬", "年化波動度", "最大回撤", "95% CVaR", "夏普比率", "有效日數"),
      數值 = c(
        fmt_pct(m$return, signed = TRUE),
        fmt_pct(m$volatility),
        fmt_pct(m$max_drawdown),
        fmt_pct(cvar_value(x$log_return)),
        fmt_num(m$sharpe),
        as.character(m$n)
      ),
      說明 = c(
        "這段期間總共漲跌多少",
        "日常價格起伏的年化程度",
        "曾經從高點跌到低點最深多少",
        "最差 5% 日報酬的平均",
        "承擔相同風險時換到多少報酬",
        "實際進入計算的日對數報酬筆數"
      ),
      check.names = FALSE
    )
  }, striped = FALSE, hover = TRUE, bordered = FALSE)

  # ----------------------------------------------------------
  # 成分股探索
  # ----------------------------------------------------------
  output$explore_period_info <- renderUI({
    d <- data_r()
    w <- trading_window(d, latest_date_r(), period_days("3y"))
    make_info_strip(paste0("多期間比較固定使用：1週、1月、3月、6月、1年、3年｜最長視窗：", fmt_date(w$start), " ～ ", fmt_date(w$end)))
  })

  explore_wide <- reactive({
    p <- default_score_obj()$period_metrics %>%
      filter(symbol != BENCHMARK_SYMBOL) %>%
      select(symbol, period, simple_return) %>%
      pivot_wider(names_from = period, values_from = simple_return)
    meta <- stock_info %>% select(symbol, name, industry)
    meta %>% left_join(p, by = "symbol")
  })

  output$explore_table <- renderTable({
    x <- explore_wide()
    if ((input$explore_industry %||% "ALL") != "ALL") x <- x %>% filter(industry == input$explore_industry)
    x %>%
      transmute(
        股票 = paste0(name, " (", symbol, ")"),
        產業 = industry,
        `1週` = fmt_pct(`1w`, signed = TRUE),
        `1月` = fmt_pct(`1m`, signed = TRUE),
        `3月` = fmt_pct(`3m`, signed = TRUE),
        `6月` = fmt_pct(`6m`, signed = TRUE),
        `1年` = fmt_pct(`1y`, signed = TRUE),
        `3年` = fmt_pct(`3y`, signed = TRUE)
      )
  }, striped = FALSE, hover = TRUE, bordered = FALSE)

  output$explore_return_plot <- renderPlot({
    x <- default_score_obj()$period_metrics %>%
      filter(symbol != BENCHMARK_SYMBOL, period == "1y", is.finite(simple_return)) %>%
      mutate(name = get_stock_name(symbol))
    if ((input$explore_industry %||% "ALL") != "ALL") x <- x %>% filter(get_stock_industry(symbol) == input$explore_industry)
    req(nrow(x) > 0)
    x <- x %>% arrange(simple_return) %>% tail(min(25, nrow(x)))
    ggplot(x, aes(reorder(name, simple_return), 100 * simple_return)) +
      geom_col(fill = "#80AEDD") +
      coord_flip() +
      labs(x = NULL, y = "近 1 年報酬 (%)") +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank())
  })

  output$industry_table <- renderTable({
    x <- default_score_obj()$period_metrics %>%
      filter(symbol != BENCHMARK_SYMBOL, period == "1y", is.finite(simple_return)) %>%
      mutate(industry = get_stock_industry(symbol)) %>%
      group_by(industry) %>%
      summarise(
        股票數 = n(),
        平均近1年報酬 = mean(simple_return, na.rm = TRUE),
        中位數近1年報酬 = median(simple_return, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(平均近1年報酬))
    x %>% transmute(產業 = industry, 股票數, `平均近1年報酬` = fmt_pct(平均近1年報酬, signed = TRUE), `中位數近1年報酬` = fmt_pct(中位數近1年報酬, signed = TRUE))
  }, striped = FALSE, hover = TRUE)

  # ----------------------------------------------------------
  # 綜合排名
  # ----------------------------------------------------------
  output$ranking_period_info <- renderUI({
    w <- trading_window(data_r(), latest_date_r(), period_days("1y"))
    make_info_strip(paste0("主風險 / 夏普：近 1 年（", fmt_date(w$start), " ～ ", fmt_date(w$end), "）｜報酬 / 穩定性：1週～3年多時間尺度滾動比較"))
  })

  output$benchmark_score_info <- renderUI({
    x <- ranking_score_obj()$scores %>% filter(symbol == BENCHMARK_SYMBOL)
    if (nrow(x) != 1 || !is.finite(x$composite_score)) return(NULL)
    make_info_strip(paste0(
      "0050 比較基準綜合分數：", fmt_score(x$composite_score),
      "｜穩定性 ", fmt_score(x$stability_score),
      "｜報酬 ", fmt_score(x$return_score),
      "｜風險控制 ", fmt_score(x$risk_score),
      "｜風險報酬效率 ", fmt_score(x$sharpe_score),
      "。0050 不參與成分股名次。"
    ))
  })

  output$ranking_time_table <- renderTable({
    d <- data_r(); end <- latest_date_r()
    rows <- lapply(seq_len(nrow(PERIOD_DEFS)), function(i) {
      w <- trading_window(d, end, PERIOD_DEFS$days[i])
      data.frame(
        時間尺度 = PERIOD_DEFS$label[i],
        起始日 = fmt_date(w$start),
        結束日 = fmt_date(w$end),
        基準交易日數 = length(w$dates),
        `報酬分數權重` = paste0(round(100 * PERIOD_DEFS$return_weight[i]), "%"),
        check.names = FALSE
      )
    })
    bind_rows(rows)
  }, striped = FALSE, hover = TRUE)

  output$ranking_weight_cards <- renderUI({
    w <- WEIGHT_SCHEMES[[input$ranking_weight_scheme %||% "stable"]]
    div(class = "kpi-grid",
        make_kpi("穩定性", paste0(round(100 * w[["stability"]]), "%"), "報酬穩定 40% + 排名穩定 30% + 風險穩定 30%", "blue"),
        make_kpi("報酬", paste0(round(100 * w[["return"]]), "%"), "1週～3年多期間報酬的加權相對分數", "green"),
        make_kpi("風險控制", paste0(round(100 * w[["risk"]]), "%"), "近1年波動、最大回撤、CVaR", "orange"),
        make_kpi("風險報酬效率", paste0(round(100 * w[["sharpe"]]), "%"), "近1年夏普比率", "purple")
    )
  })

  output$score_explanation <- renderTable({ score_explanation_table() }, striped = FALSE, hover = TRUE)

  output$ranking_table <- renderTable({
    x <- ranking_score_obj()$scores %>%
      filter(symbol != BENCHMARK_SYMBOL, is.finite(composite_score)) %>%
      arrange(rank)
    x %>% transmute(
      名次 = rank,
      股票 = paste0(name, " (", symbol, ")"),
      產業 = industry,
      綜合分數 = fmt_score(composite_score),
      穩定性 = fmt_score(stability_score),
      報酬 = fmt_score(return_score),
      風險控制 = fmt_score(risk_score),
      風險報酬效率 = fmt_score(sharpe_score),
      `近1年資料完整度` = fmt_pct(coverage),
      `有效報酬期間數` = return_periods_used
    )
  }, striped = FALSE, hover = TRUE, bordered = FALSE)

  output$ranking_bar_plot <- renderPlot({
    x <- ranking_score_obj()$scores %>%
      filter(symbol != BENCHMARK_SYMBOL, is.finite(composite_score)) %>%
      arrange(desc(composite_score)) %>%
      head(15) %>%
      mutate(label = paste0(name, " ", symbol))
    req(nrow(x) > 0)
    ggplot(x, aes(reorder(label, composite_score), composite_score)) +
      geom_col(fill = "#86ABD2") +
      coord_flip() +
      labs(x = NULL, y = "綜合分數") +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank())
  })

  # ----------------------------------------------------------
  # 個股分析
  # ----------------------------------------------------------
  stock_window <- reactive({
    key <- input$stock_period %||% "1y"
    filter_window(data_r(), latest_date_r(), period_days(key), c(input$stock_symbol, BENCHMARK_SYMBOL)) %>% arrange(symbol, date)
  })

  stock_only <- reactive({ stock_window() %>% filter(symbol == input$stock_symbol) %>% arrange(date) })

  output$stock_period_info <- renderUI({
    x <- stock_only()
    req(nrow(x) > 0)
    make_info_strip(format_period_info(min(x$date), max(x$date), sum(is.finite(x$log_return)), period_label(input$stock_period)))
  })

  output$stock_kpis <- renderUI({
    x <- stock_only() %>% filter(is.finite(log_return))
    req(nrow(x) >= 2)
    m <- portfolio_metrics_from_log_returns(x$log_return)
    latest <- tail(x$adjusted[is.finite(x$adjusted)], 1)
    div(class = "kpi-grid",
        make_kpi("最新價格", ifelse(length(latest) == 1, sprintf("%.2f", latest), "—"), get_stock_label(input$stock_symbol), "blue"),
        make_kpi("期間報酬", fmt_pct(m$return, signed = TRUE), period_label(input$stock_period), "green"),
        make_kpi("年化波動度", fmt_pct(m$volatility), "期間樣本足夠時年化", "orange"),
        make_kpi("最大回撤", fmt_pct(m$max_drawdown), "越接近 0 代表回撤越小", "purple")
    )
  })

  output$stock_price_plot <- renderPlot({
    x <- stock_only() %>% filter(is.finite(adjusted))
    req(nrow(x) > 1)
    x$MA20 <- as.numeric(stats::filter(x$adjusted, rep(1 / 20, 20), sides = 1))
    x$MA60 <- as.numeric(stats::filter(x$adjusted, rep(1 / 60, 60), sides = 1))
    col <- get_stock_color(input$stock_symbol)
    ggplot(x, aes(date, adjusted)) +
      geom_line(linewidth = 1.05, color = col) +
      geom_line(aes(y = MA20), linewidth = .7, color = "#E0A64B", na.rm = TRUE) +
      geom_line(aes(y = MA60), linewidth = .7, color = "#7A7FB6", na.rm = TRUE) +
      labs(x = NULL, y = "調整後價格", subtitle = "個股色＝價格；橘＝MA20；紫＝MA60") +
      theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
  })

  output$stock_cum_return_plot <- renderPlot({
    x <- stock_window() %>% filter(is.finite(log_return)) %>% group_by(symbol) %>% arrange(date, .by_group = TRUE) %>% mutate(cumulative = exp(cumsum(log_return)) - 1) %>% ungroup()
    req(nrow(x) > 2)
    cols <- c(get_stock_color(BENCHMARK_SYMBOL), get_stock_color(input$stock_symbol))
    names(cols) <- c(BENCHMARK_SYMBOL, input$stock_symbol)
    ggplot(x, aes(date, 100 * cumulative, color = symbol)) +
      geom_line(linewidth = .95) +
      scale_color_manual(values = cols, labels = get_stock_label) +
      labs(x = NULL, y = "累積報酬 (%)", color = NULL) +
      theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), legend.position = "bottom")
  })

  output$stock_return_hist <- renderPlot({
    x <- stock_only() %>% filter(is.finite(log_return))
    req(nrow(x) >= 5)
    ggplot(x, aes(100 * log_return)) +
      geom_histogram(bins = 30, fill = get_stock_color(input$stock_symbol), alpha = .80, color = "white") +
      geom_vline(xintercept = 0, linetype = 2, color = "#7A8793") +
      labs(x = "日對數報酬率 (%)", y = "天數") +
      theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
  })

  stock_pair <- reactive({
    x <- stock_window() %>% filter(symbol == input$stock_symbol, is.finite(log_return)) %>% select(date, stock_return = log_return)
    b <- stock_window() %>% filter(symbol == BENCHMARK_SYMBOL, is.finite(log_return)) %>% select(date, benchmark_return = log_return)
    inner_join(x, b, by = "date") %>% arrange(date)
  })

  output$stock_rolling_corr <- renderPlot({
    x <- stock_pair()
    validate(need(nrow(x) >= 60, "目前期間不足 60 個共同交易日；請改用 3 個月以上。"))
    x$rolling_corr <- rolling_correlation_fast(x$stock_return, x$benchmark_return, 60)
    ggplot(x, aes(date, rolling_corr)) +
      geom_hline(yintercept = 0, linetype = 2, color = "#A0AAB4") +
      geom_line(color = get_stock_color(input$stock_symbol), linewidth = .9, na.rm = TRUE) +
      coord_cartesian(ylim = c(-1, 1)) +
      labs(x = NULL, y = "60日相關係數") +
      theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
  })

  output$stock_risk_table <- renderTable({
    x <- stock_only() %>% filter(is.finite(log_return))
    pair <- stock_pair()
    req(nrow(x) >= 2)
    m <- portfolio_metrics_from_log_returns(x$log_return)
    corr <- if (nrow(pair) >= 5) cor(pair$stock_return, pair$benchmark_return, use = "complete.obs") else NA_real_
    beta <- if (nrow(pair) >= 5 && var(pair$benchmark_return, na.rm = TRUE) > 0) cov(pair$stock_return, pair$benchmark_return, use = "complete.obs") / var(pair$benchmark_return, na.rm = TRUE) else NA_real_
    data.frame(
      指標 = c("年化波動度", "最大回撤", "95% CVaR", "夏普比率", "與0050相關係數", "Beta"),
      數值 = c(fmt_pct(m$volatility), fmt_pct(m$max_drawdown), fmt_pct(cvar_value(x$log_return)), fmt_num(m$sharpe), fmt_num(corr), fmt_num(beta)),
      怎麼看 = c(
        "日常起伏越大，數值通常越高",
        "過去這段期間最深曾跌多少",
        "最差 5% 日子平均跌多少",
        "承擔同樣風險時換到多少報酬",
        "跟 0050 有多常一起漲跌",
        "0050 變動時，這檔股票通常有多敏感"
      ),
      check.names = FALSE
    )
  }, striped = FALSE, hover = TRUE)

  output$stock_technical_table <- renderTable({
    x <- stock_only() %>% filter(is.finite(adjusted)) %>% arrange(date)
    req(nrow(x) >= 5)
    latest <- tail(x$adjusted, 1)
    ma20 <- if (nrow(x) >= 20) mean(tail(x$adjusted, 20), na.rm = TRUE) else NA_real_
    ma60 <- if (nrow(x) >= 60) mean(tail(x$adjusted, 60), na.rm = TRUE) else NA_real_
    rsi <- rsi_value(x$adjusted, 14)
    vol20 <- if (sum(is.finite(x$volume)) >= 20) mean(tail(x$volume[is.finite(x$volume)], 20), na.rm = TRUE) else NA_real_
    latest_vol <- tail(x$volume[is.finite(x$volume)], 1)
    trend <- if (is.finite(ma20) && is.finite(ma60) && latest > ma20 && ma20 > ma60) "趨勢偏強" else if (is.finite(ma20) && is.finite(ma60) && latest < ma20 && ma20 < ma60) "趨勢偏弱" else "趨勢中性"
    momentum <- if (!is.finite(rsi)) "資料不足" else if (rsi >= 70) "動能偏熱" else if (rsi <= 30) "動能偏弱" else "動能中性"
    volume_state <- if (length(latest_vol) == 1 && is.finite(vol20) && vol20 > 0) sprintf("最新量 / 20日均量 = %.2f 倍", latest_vol / vol20) else "資料不足"
    data.frame(
      項目 = c("均線狀態", "RSI(14)", "成交量"),
      結果 = c(trend, ifelse(is.finite(rsi), paste0(sprintf("%.1f", rsi), "｜", momentum), momentum), volume_state),
      注意 = c("只做趨勢輔助，不是買賣訊號", "只反映近期動能，不直接決定排名", "量能放大不等於一定上漲"),
      check.names = FALSE
    )
  }, striped = FALSE, hover = TRUE)

  # ----------------------------------------------------------
  # 風險與穩定性
  # ----------------------------------------------------------
  output$risk_period_info <- renderUI({
    w <- trading_window(data_r(), latest_date_r(), 252)
    make_info_strip(paste0("風險主視窗：近 1 年｜", fmt_date(w$start), " ～ ", fmt_date(w$end), "；穩定性另外比較 1週～3年六種時間尺度。"))
  })

  output$stability_time_table <- renderTable({
    d <- data_r(); end <- latest_date_r()
    bind_rows(lapply(seq_len(nrow(PERIOD_DEFS)), function(i) {
      w <- trading_window(d, end, PERIOD_DEFS$days[i])
      data.frame(
        時間尺度 = PERIOD_DEFS$label[i],
        起始日 = fmt_date(w$start),
        結束日 = fmt_date(w$end),
        基準交易日數 = length(w$dates),
        check.names = FALSE
      )
    }))
  }, striped = FALSE, hover = TRUE)

  output$risk_return_plot <- renderPlot({
    x <- default_score_obj()$scores %>% filter(symbol != BENCHMARK_SYMBOL, is.finite(annual_return), is.finite(annual_volatility))
    req(nrow(x) > 0)
    ggplot(x, aes(100 * annual_volatility, 100 * annual_return)) +
      geom_point(aes(color = stability_score), size = 3, alpha = .85) +
      scale_color_gradient(low = "#D8E4EF", high = "#5E91C0") +
      labs(x = "近1年年化波動度 (%)", y = "近1年年化報酬率 (%)", color = "穩定性") +
      theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
  })

  output$stability_plot <- renderPlot({
    x <- default_score_obj()$scores %>% filter(symbol != BENCHMARK_SYMBOL, is.finite(stability_score)) %>% arrange(desc(stability_score)) %>% head(15) %>% mutate(label = paste0(name, " ", symbol))
    ggplot(x, aes(reorder(label, stability_score), stability_score)) +
      geom_col(fill = "#7FB08D") + coord_flip() +
      labs(x = NULL, y = "穩定性分數") +
      theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
  })

  output$risk_stability_table <- renderTable({
    x <- default_score_obj()$scores %>% filter(symbol != BENCHMARK_SYMBOL, is.finite(composite_score)) %>% arrange(desc(stability_score))
    x %>% transmute(
      股票 = paste0(name, " (", symbol, ")"),
      綜合分數 = fmt_score(composite_score),
      穩定性 = fmt_score(stability_score),
      報酬穩定 = fmt_score(return_stability_score),
      排名穩定 = fmt_score(rank_stability_score),
      風險穩定 = fmt_score(risk_stability_score),
      年化波動 = fmt_pct(annual_volatility),
      最大回撤 = fmt_pct(max_drawdown),
      CVaR = fmt_pct(cvar_95),
      夏普 = fmt_num(sharpe)
    )
  }, striped = FALSE, hover = TRUE)

  # ----------------------------------------------------------
  # 模型驗證
  # ----------------------------------------------------------
  backtest_result <- eventReactive(input$run_backtest, {
    req(APP_DATA)
    scheme <- WEIGHT_SCHEMES[[input$bt_weight_scheme %||% "stable"]]
    withProgress(message = "正在做歷史滾動驗證", value = 0, {
      run_model_validation(
        data_r(),
        lookback_key = input$bt_lookback,
        forward_key = input$bt_forward,
        frequency = input$bt_frequency,
        weight_scheme = scheme,
        progress_cb = function(i, n, d) {
          setProgress(i / n, detail = paste0(fmt_date(d), "｜", i, " / ", n))
        }
      )
    })
  }, ignoreInit = TRUE)

  output$backtest_info <- renderUI({
    if ((input$run_backtest %||% 0) == 0) {
      return(make_info_strip("請先選擇回看期間、未來驗證期間與重新評分頻率，再按「執行模型驗證」。"))
    }
    r <- backtest_result()
    if (is.null(r) || nrow(r$events) == 0) return(div(class = "warning-box", "這組期間沒有足夠完整的歷史樣本，請縮短回看或未來驗證期間。"))
    n_dates <- length(unique(r$events$rebalance_date))
    first_d <- min(r$events$rebalance_date, na.rm = TRUE)
    last_d <- max(r$events$rebalance_date, na.rm = TRUE)
    make_info_strip(paste0(
      "評分往回看：", period_label(input$bt_lookback),
      "｜往後驗證：", forward_label(input$bt_forward),
      "｜有效評分日期：", n_dates, " 次｜", fmt_date(first_d), " ～ ", fmt_date(last_d), "。"
    ))
  })

  output$backtest_summary <- renderTable({
    r <- backtest_result(); req(!is.null(r), nrow(r$summary) > 0)
    r$summary %>% transmute(
      策略 = strategy,
      有效驗證次數 = validation_count,
      平均後續報酬 = fmt_pct(avg_return, signed = TRUE),
      中位數後續報酬 = fmt_pct(median_return, signed = TRUE),
      平均年化波動 = fmt_pct(avg_volatility),
      平均最大回撤 = fmt_pct(avg_max_drawdown),
      平均夏普 = fmt_num(avg_sharpe),
      勝過0050比例 = ifelse(strategy == "0050", "—", fmt_pct(beat_0050_rate))
    )
  }, striped = FALSE, hover = TRUE)

  sensitivity_result <- eventReactive(input$run_sensitivity, {
    schemes <- names(WEIGHT_SCHEMES)
    rows <- list()
    withProgress(message = "正在比較三種評分權重", value = 0, {
      for (i in seq_along(schemes)) {
        nm <- schemes[i]
        setProgress((i - 1) / length(schemes), detail = WEIGHT_SCHEME_LABELS[[nm]])
        r <- run_model_validation(
          data_r(),
          lookback_key = input$bt_lookback,
          forward_key = input$bt_forward,
          frequency = input$bt_frequency,
          weight_scheme = WEIGHT_SCHEMES[[nm]],
          progress_cb = NULL
        )
        if (nrow(r$summary) == 0) next
        top10 <- r$summary %>% filter(strategy == "Top10")
        bottom10 <- r$summary %>% filter(strategy == "Bottom10")
        rows[[length(rows) + 1L]] <- data.frame(
          scheme = nm,
          top10_return = if (nrow(top10) == 1) top10$avg_return else NA_real_,
          top10_sharpe = if (nrow(top10) == 1) top10$avg_sharpe else NA_real_,
          top10_drawdown = if (nrow(top10) == 1) top10$avg_max_drawdown else NA_real_,
          top10_beat = if (nrow(top10) == 1) top10$beat_0050_rate else NA_real_,
          bottom10_return = if (nrow(bottom10) == 1) bottom10$avg_return else NA_real_,
          avg_spearman = if (nrow(r$rank_cor) > 0) mean(r$rank_cor$spearman, na.rm = TRUE) else NA_real_,
          validation_count = if (nrow(top10) == 1) top10$validation_count else 0
        )
      }
      setProgress(1)
    })
    bind_rows(rows)
  }, ignoreInit = TRUE)

  output$sensitivity_table <- renderTable({
    if ((input$run_sensitivity %||% 0) == 0) return(NULL)
    x <- sensitivity_result(); req(nrow(x) > 0)
    x %>% transmute(
      權重版本 = unname(WEIGHT_SCHEME_LABELS[scheme]),
      有效驗證次數 = validation_count,
      `Top10平均報酬` = fmt_pct(top10_return, signed = TRUE),
      `Top10平均夏普` = fmt_num(top10_sharpe),
      `Top10平均最大回撤` = fmt_pct(top10_drawdown),
      `Top10勝0050比例` = fmt_pct(top10_beat),
      `Bottom10平均報酬` = fmt_pct(bottom10_return, signed = TRUE),
      `平均Spearman` = fmt_num(avg_spearman, 3)
    )
  }, striped = FALSE, hover = TRUE)

  output$backtest_return_plot <- renderPlot({
    r <- backtest_result(); req(!is.null(r), nrow(r$events) > 0)
    x <- r$events %>% filter(is.finite(forward_return))
    cols <- c(Top5 = "#8E7CC3", Top10 = "#77A985", Bottom10 = "#D7A253", `0050` = get_stock_color(BENCHMARK_SYMBOL))
    ggplot(x, aes(rebalance_date, 100 * forward_return, color = strategy)) +
      geom_hline(yintercept = 0, linetype = 2, color = "#A4AFB9") +
      geom_line(linewidth = .8) + geom_point(size = 1.8) +
      scale_color_manual(values = cols) +
      labs(x = NULL, y = paste0(forward_label(input$bt_forward), "報酬 (%)"), color = NULL) +
      theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), legend.position = "bottom")
  })

  output$backtest_spearman_plot <- renderPlot({
    r <- backtest_result(); req(!is.null(r), nrow(r$rank_cor) > 0)
    x <- r$rank_cor %>% filter(is.finite(spearman))
    validate(need(nrow(x) > 0, "沒有足夠資料計算 Spearman。"))
    ggplot(x, aes(rebalance_date, spearman)) +
      geom_hline(yintercept = 0, linetype = 2, color = "#A4AFB9") +
      geom_line(color = "#8173B2", linewidth = .85) + geom_point(color = "#8173B2", size = 2) +
      coord_cartesian(ylim = c(-1, 1)) +
      labs(x = NULL, y = "Spearman 等級相關", subtitle = paste0("平均 = ", sprintf("%.3f", mean(x$spearman, na.rm = TRUE)))) +
      theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
  })

  output$backtest_events <- renderTable({
    r <- backtest_result(); req(!is.null(r), nrow(r$events) > 0)
    r$events %>%
      arrange(desc(rebalance_date), strategy) %>%
      transmute(
        評分日期 = fmt_date(rebalance_date),
        組別 = strategy,
        後續報酬 = fmt_pct(forward_return, signed = TRUE),
        年化波動 = fmt_pct(volatility),
        最大回撤 = fmt_pct(max_drawdown),
        夏普 = fmt_num(sharpe),
        是否勝0050 = ifelse(strategy == "0050", "—", ifelse(beat_0050, "是", "否"))
      )
  }, striped = FALSE, hover = TRUE)

  bootstrap_result <- eventReactive(input$run_bootstrap, {
    withProgress(message = "正在執行區塊拔靴法", value = 0, {
      run_block_bootstrap(
        data_r(), input$bootstrap_symbol, latest_date_r(), input$bootstrap_period,
        reps = as.integer(input$bootstrap_reps), block_length = as.integer(input$bootstrap_block),
        progress_cb = function(i, n) setProgress(i / n, detail = paste0(i, " / ", n))
      )
    })
  }, ignoreInit = TRUE)

  output$bootstrap_period_info <- renderUI({
    w <- trading_window(data_r(), latest_date_r(), period_days(input$bootstrap_period %||% "1y"))
    make_info_strip(paste0("拔靴資料期間：", period_label(input$bootstrap_period), "｜", fmt_date(w$start), " ～ ", fmt_date(w$end), "｜使用日對數報酬連續區塊抽樣。"))
  })

  output$bootstrap_kpis <- renderUI({
    if ((input$run_bootstrap %||% 0) == 0) return(NULL)
    r <- bootstrap_result(); req(!is.null(r))
    div(class = "kpi-grid",
        make_kpi("平均分數差", sprintf("%+.2f", r$mean_diff), "個股綜合分數 − 0050", "blue"),
        make_kpi("95%區間下界", sprintf("%+.2f", r$lower), "若仍 > 0，優勢較穩定", "green"),
        make_kpi("95%區間上界", sprintf("%+.2f", r$upper), "區間越寬代表不確定性越高", "orange"),
        make_kpi("高於0050比例", fmt_pct(r$p_above), r$label, "purple")
    )
  })

  output$bootstrap_plot <- renderPlot({
    r <- bootstrap_result(); req(!is.null(r), length(r$diffs) > 0)
    ggplot(data.frame(diff = r$diffs), aes(diff)) +
      geom_histogram(bins = 28, fill = get_stock_color(input$bootstrap_symbol), alpha = .78, color = "white") +
      geom_vline(xintercept = 0, linetype = 2, color = "#687887") +
      geom_vline(xintercept = c(r$lower, r$upper), linetype = 3, color = "#A374B6") +
      labs(x = "綜合分數差（個股 − 0050）", y = "抽樣次數") +
      theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
  })

  # ----------------------------------------------------------
  # PCA / 分群
  # ----------------------------------------------------------
  pca_result <- eventReactive(input$run_pca, {
    withProgress(message = "正在進行 PCA 與分群", value = .2, {
      out <- run_pca_clustering(data_r(), latest_date_r(), input$pca_period, 0.80)
      setProgress(1)
      out
    })
  }, ignoreInit = TRUE)

  output$pca_period_info <- renderUI({
    if ((input$run_pca %||% 0) == 0) {
      w <- trading_window(data_r(), latest_date_r(), period_days(input$pca_period %||% "1y"))
      return(make_info_strip(paste0("目前選擇：", period_label(input$pca_period), "｜", fmt_date(w$start), " ～ ", fmt_date(w$end), "。按下更新後才重新計算。")))
    }
    r <- pca_result(); req(!is.null(r))
    make_info_strip(paste0("PCA / 分群實際期間：", fmt_date(r$start_date), " ～ ", fmt_date(r$end_date), "｜納入 ", r$included_count, " 檔股票｜最佳群數 K = ", r$best_k, "。"))
  })

  output$pca_interpretation <- renderUI({
    if ((input$run_pca %||% 0) == 0) return(plain_help("怎麼看？", "先選期間並按「更新 PCA / 分群」。不同期間可能得到不同群組，這正是用來觀察目前特性是否改變。"))
    r <- pca_result(); req(!is.null(r))
    div(
      class = "plain-help",
      tags$strong("主成分白話解讀："),
      span(paste0("PC1 主要受 ", r$pc1_plain, " 影響；PC2 主要受 ", r$pc2_plain, " 影響。前 ", r$retained_pc, " 個主成分合計解釋至少 80% 的標準化資訊。"))
    )
  })

  observeEvent(pca_result(), {
    r <- pca_result()
    labs <- sort(unique(r$all_stocks$cluster_label))
    updateSelectInput(session, "cluster_filter", choices = c("全部", labs), selected = "全部")
  }, ignoreInit = TRUE)

  output$pca_scatter <- renderPlot({
    r <- pca_result(); req(!is.null(r))
    x <- r$stocks
    ggplot(x, aes(PC1, PC2, color = cluster_label)) +
      geom_point(size = 3, alpha = .85) +
      geom_text(aes(label = symbol), size = 3, vjust = -0.8, show.legend = FALSE) +
      labs(x = "PC1", y = "PC2", color = "群組") +
      theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), legend.position = "bottom")
  })

  output$pca_silhouette <- renderPlot({
    r <- pca_result(); req(!is.null(r))
    ggplot(r$silhouette, aes(k, silhouette)) +
      geom_line(color = "#7F78B5", linewidth = .85) + geom_point(color = "#7F78B5", size = 2.5) +
      geom_vline(xintercept = r$best_k, linetype = 2, color = "#D2A050") +
      scale_x_continuous(breaks = r$silhouette$k) +
      labs(x = "群數 K", y = "平均輪廓係數") +
      theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
  })

  output$pca_variance <- renderPlot({
    r <- pca_result(); req(!is.null(r))
    x <- head(r$variance, min(8, nrow(r$variance)))
    ggplot(x, aes(component, 100 * variance_ratio)) +
      geom_col(fill = "#7FAED9") +
      labs(x = NULL, y = "解釋變異 (%)") +
      theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
  })

  output$cluster_stock_table <- renderTable({
    r <- pca_result(); req(!is.null(r))
    x <- r$all_stocks
    if ((input$cluster_filter %||% "全部") != "全部") x <- x %>% filter(cluster_label == input$cluster_filter)
    x %>% arrange(is.na(cluster), cluster, desc(composite_score)) %>% transmute(
      股票 = paste0(name, " (", symbol, ")"),
      產業 = industry,
      群組 = ifelse(is.na(cluster), "—", as.character(cluster)),
      類型 = cluster_label,
      `目前綜合分數` = fmt_score(composite_score),
      `分析期年化報酬` = fmt_pct(annual_return, signed = TRUE),
      `分析期年化波動` = fmt_pct(annual_volatility),
      `分析期最大回撤` = fmt_pct(max_drawdown)
    )
  }, striped = FALSE, hover = TRUE)

  output$cluster_profile_table <- renderTable({
    r <- pca_result(); req(!is.null(r))
    r$stocks %>%
      group_by(cluster, cluster_label) %>%
      summarise(
        股票數 = n(),
        年化報酬 = mean(annual_return, na.rm = TRUE),
        年化波動 = mean(annual_volatility, na.rm = TRUE),
        最大回撤 = mean(max_drawdown, na.rm = TRUE),
        夏普 = mean(sharpe, na.rm = TRUE),
        穩定性 = mean(c(return_stability_score, rank_stability_score, risk_stability_score), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      transmute(群組 = cluster, 類型 = cluster_label, 股票數, 年化報酬 = fmt_pct(年化報酬, signed = TRUE), 年化波動 = fmt_pct(年化波動), 最大回撤 = fmt_pct(最大回撤), 夏普 = fmt_num(夏普), 穩定性平均 = fmt_score(穩定性))
  }, striped = FALSE, hover = TRUE)

  feature_cn <- c(
    annual_return = "年化報酬",
    annual_volatility = "年化波動",
    max_drawdown = "最大回撤",
    cvar_95 = "CVaR",
    sharpe = "夏普",
    return_stability_score = "報酬穩定",
    rank_stability_score = "排名穩定",
    risk_stability_score = "風險穩定"
  )

  output$pca_loading_table <- renderTable({
    r <- pca_result(); req(!is.null(r))
    x <- r$loadings
    x$importance <- pmax(abs(x$PC1), abs(x$PC2))
    x <- x %>% arrange(desc(importance))
    data.frame(
      指標 = unname(feature_cn[x$feature]),
      PC1負荷 = sprintf("%+.3f", x$PC1),
      PC2負荷 = sprintf("%+.3f", x$PC2),
      check.names = FALSE
    )
  }, striped = FALSE, hover = TRUE)

  # ----------------------------------------------------------
  # 研究方法
  # ----------------------------------------------------------
  output$method_period_table <- renderTable({
    PERIOD_DEFS %>% transmute(
      名稱 = label,
      約交易日 = days,
      `報酬分數權重` = paste0(round(100 * return_weight), "%"),
      主要用途 = c("超近期強弱", "短期狀態", "短中期狀態 / PCA 最短正式窗口", "中期風險", "主風險 / 夏普 / PCA 預設", "中長期背景")
    )
  }, striped = FALSE, hover = TRUE)

  output$method_score_table <- renderTable({ score_explanation_table() }, striped = FALSE, hover = TRUE)
}


shinyApp(ui = ui, server = server)
