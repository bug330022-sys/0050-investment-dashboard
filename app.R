# 0050 投資研究 Dashboard v4.2：分段合併＋啟動防呆＋統一時間尺度＋PCA分群＋模型驗證
# Connect Cloud auto-publish test
# ============================================================
# 0050 Investment Research Dashboard
# 第 1 段：資料、共用函數、UI
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

options(scipen = 999)
# ============================================================
# 1. 基本設定
# ============================================================

DATA_FILE <- "data/stock_data.rds"

RESEARCH_START_DATE <- as.Date("2016-01-04")
RESEARCH_END_DATE   <- Sys.Date()

TRADING_DAYS_PER_YEAR <- 252
MAX_SELECTED_STOCKS <- 5
TRADE_UNIT <- 1

# ============================================================
# 統一時間尺度（全站共用）
# ============================================================
# 所有頁面盡量使用同一組名稱與交易日定義，避免「1季 / 3個月 / 90天」混用。
TIME_WINDOWS <- c(
  `1週` = 5,
  `1個月` = 21,
  `3個月` = 63,
  `6個月` = 126,
  `1年` = 252,
  `3年` = 756
)

# 主評分的多期間報酬權重：近期要看得到，但不讓一週行情主導整體。
RETURN_WINDOW_WEIGHTS <- c(
  `1週` = 0.05,
  `1個月` = 0.10,
  `3個月` = 0.15,
  `6個月` = 0.20,
  `1年` = 0.30,
  `3年` = 0.20
)

# 四大構面的主模型權重。先保留目前版本，再由歷史驗證做敏感度比較。
MODEL_WEIGHTS <- c(stability = 0.40, ret = 0.25, risk = 0.20, sharpe = 0.15)
WEIGHT_PROFILES <- list(
  baseline = c(stability = 0.40, ret = 0.25, risk = 0.20, sharpe = 0.15),
  balanced = c(stability = 0.35, ret = 0.30, risk = 0.20, sharpe = 0.15),
  return_focus = c(stability = 0.30, ret = 0.35, risk = 0.20, sharpe = 0.15)
)
WEIGHT_PROFILE_LABELS <- c(
  baseline = "目前版｜40 / 25 / 20 / 15",
  balanced = "均衡版｜35 / 30 / 20 / 15",
  return_focus = "報酬稍強版｜30 / 35 / 20 / 15"
)

# 風險 / 夏普的主要比較視窗。
PRIMARY_RISK_WINDOW <- 252

# PCA / 分群只提供樣本量較足夠的期間。
PCA_WINDOWS <- c(`3個月` = 63, `6個月` = 126, `1年` = 252, `3年` = 756)

# 模型驗證：評分後往前觀察多久。
BACKTEST_HORIZONS <- c(
  `1週` = 5,
  `1個月` = 21,
  `3個月` = 63,
  `6個月` = 126,
  `1年` = 252,
  `2年` = 504,
  `3年` = 756,
  `5年` = 1260
)


# ============================================================
# 2. 0050 成分股資料
# ============================================================

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
  symbol = "0050",
  name = "元大台灣50",
  industry = "比較基準",
  stringsAsFactors = FALSE
)
# ============================================================
# 產業分類：少於 4 檔的產業合併為「其他」
# ============================================================

industry_count <- table(
  stock_info$industry
)

major_industries <- names(
  industry_count[
    industry_count >= 4
  ]
)

industry_group <- function(x) {
  
  ifelse(
    x %in% major_industries,
    x,
    "其他"
  )
  
}

# ============================================================
# 3. 共用函數
# ============================================================

# ============================================================
# 股票名稱 / 產業 / 顯示標籤
# 支援單一股票，也支援整個向量
# ============================================================

get_stock_name_one <- function(symbol) {
  
  symbol <- as.character(symbol)
  
  if (
    length(symbol) == 0 ||
    is.na(symbol)
  ) {
    return(NA_character_)
  }
  
  if (symbol == "0050") {
    return("元大台灣50")
  }
  
  result <- stock_info$name[
    match(
      symbol,
      stock_info$symbol
    )
  ]
  
  if (
    length(result) == 0 ||
    is.na(result)
  ) {
    return(symbol)
  }
  
  result
}


get_stock_name <- function(symbol) {
  
  vapply(
    as.character(symbol),
    get_stock_name_one,
    character(1)
  )
  
}


get_stock_industry_one <- function(symbol) {
  
  symbol <- as.character(symbol)
  
  if (
    length(symbol) == 0 ||
    is.na(symbol)
  ) {
    return(NA_character_)
  }
  
  if (symbol == "0050") {
    return("比較基準")
  }
  
  result <- stock_info$industry[
    match(
      symbol,
      stock_info$symbol
    )
  ]
  
  if (
    length(result) == 0 ||
    is.na(result)
  ) {
    return("其他")
  }
  
  result
}


get_stock_industry <- function(symbol) {
  
  vapply(
    as.character(symbol),
    get_stock_industry_one,
    character(1)
  )
  
}


get_stock_label <- function(symbol) {
  
  symbol <- as.character(symbol)
  
  paste0(
    get_stock_name(symbol),
    " (",
    symbol,
    ")"
  )
  
}


convert_to_date <- function(x) {
  
  if (inherits(x, "Date")) {
    return(x)
  }
  
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  
  if (is.numeric(x)) {
    
    finite_x <- x[is.finite(x)]
    
    if (
      length(finite_x) > 0 &&
      min(finite_x) >= 10000 &&
      max(finite_x) <= 60000
    ) {
      
      return(
        as.Date(
          x,
          origin = "1899-12-30"
        )
      )
    }
  }
  
  as.Date(x)
}


safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}


fmt_price <- function(x) {
  ifelse(
    is.finite(x),
    sprintf("%.2f", x),
    "NA"
  )
}


fmt_num <- function(x, digits = 0) {
  
  ifelse(
    is.finite(x),
    formatC(
      x,
      format = "f",
      digits = digits,
      big.mark = ","
    ),
    "NA"
  )
}

fmt_pct <- function(
    x,
    digits = 2,
    show_sign = TRUE
) {
  
  if (show_sign) {
    
    ifelse(
      is.finite(x),
      sprintf(
        paste0("%+.", digits, "f%%"),
        x * 100
      ),
      "NA"
    )
    
  } else {
    
    ifelse(
      is.finite(x),
      sprintf(
        paste0("%.", digits, "f%%"),
        x * 100
      ),
      "NA"
    )
  }
}


annualized_return <- function(
    start_value,
    end_value,
    trading_days
) {
  
  if (
    !is.finite(start_value) ||
    !is.finite(end_value) ||
    start_value <= 0 ||
    end_value <= 0 ||
    trading_days <= 1
  ) {
    
    return(NA_real_)
  }
  
  (
    end_value / start_value
  ) ^ (
    TRADING_DAYS_PER_YEAR /
      trading_days
  ) - 1
}


drawdown_value <- function(x) {
  
  x <- as.numeric(x)
  
  high <- cummax(x)
  
  x / high - 1
}


nearest_date <- function(
    dates,
    target
) {
  
  dates <- sort(unique(as.Date(dates)))
  target <- as.Date(target)
  
  if (
    length(dates) == 0 ||
    is.na(target)
  ) {
    return(NA)
  }
  
  dates[
    which.min(
      abs(
        as.numeric(dates - target)
      )
    )
  ]
}



# ============================================================
# 3-1. 統計評分與風險共用函數（融合版新增）
# ============================================================

# 條件風險值（CVaR / Expected Shortfall）
# alpha = 0.95 代表觀察最差 5% 日對數報酬的平均值。
cvar_value <- function(x, alpha = 0.95) {
  x <- x[is.finite(x)]
  if (length(x) < 20) return(NA_real_)
  cutoff <- as.numeric(
    quantile(
      x,
      probs = 1 - alpha,
      na.rm = TRUE,
      names = FALSE,
      type = 7
    )
  )
  tail_x <- x[x <= cutoff]
  if (length(tail_x) == 0) return(NA_real_)
  mean(tail_x, na.rm = TRUE)
}

# 將橫斷面指標轉成 0–100 分。
# higher_better = TRUE：數值越大分數越高。
# higher_better = FALSE：數值越小分數越高。
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

# 平均輪廓係數，不額外依賴其他套件。
average_silhouette <- function(x, cluster) {
  x <- as.matrix(x)
  if (nrow(x) < 3 || length(unique(cluster)) < 2) return(NA_real_)
  d <- as.matrix(dist(x))
  s <- rep(NA_real_, nrow(x))
  groups <- sort(unique(cluster))
  for (i in seq_len(nrow(x))) {
    same <- which(cluster == cluster[i] & seq_len(nrow(x)) != i)
    a <- if (length(same) > 0) mean(d[i, same]) else 0
    other_means <- sapply(
      groups[groups != cluster[i]],
      function(g) mean(d[i, cluster == g])
    )
    b <- min(other_means)
    denom <- max(a, b)
    s[i] <- if (is.finite(denom) && denom > 0) (b - a) / denom else 0
  }
  mean(s, na.rm = TRUE)
}

# 安全格式：0–100 分
fmt_score <- function(x, digits = 1) {
  ifelse(
    is.finite(x),
    sprintf(paste0("%.", digits, "f"), x),
    "NA"
  )
}

# 依「交易日數」取得共同分析起點。
# 用整體市場交易日而不是每檔股票各自倒數，可讓股票之間比較期間一致。
window_start_from_dates <- function(dates, n_days, end_date = NULL) {
  dates <- sort(unique(as.Date(dates)))
  if (!is.null(end_date)) dates <- dates[dates <= as.Date(end_date)]
  if (length(dates) == 0) return(as.Date(NA))
  n_days <- max(1, as.integer(n_days))
  idx <- max(1, length(dates) - n_days + 1)
  dates[idx]
}

window_label <- function(n_days) {
  hit <- names(TIME_WINDOWS)[TIME_WINDOWS == as.integer(n_days)]
  if (length(hit) > 0) return(hit[1])
  hit <- names(BACKTEST_HORIZONS)[BACKTEST_HORIZONS == as.integer(n_days)]
  if (length(hit) > 0) return(hit[1])
  paste0(n_days, " 個交易日")
}

# 快速滾動標準差；用於 PCA 的「風險穩定度」特徵。
rolling_sd_fast <- function(x, window = 21) {
  x <- as.numeric(x)
  ok <- is.finite(x)
  roll_sum <- function(z) as.numeric(stats::filter(z, rep(1, window), sides = 1))
  n_obs <- roll_sum(as.numeric(ok))
  x0 <- ifelse(ok, x, 0)
  sx <- roll_sum(x0)
  sxx <- roll_sum(x0 * x0)
  var_num <- (sxx - sx * sx / n_obs) / pmax(n_obs - 1, 1)
  out <- sqrt(pmax(var_num, 0))
  out[n_obs < window | !is.finite(out)] <- NA_real_
  out
}

# ============================================================
# 3-2. 技術狀態、區塊拔靴法與歷史回測共用函數（v4）
# ============================================================

# RSI（14 日）簡化計算：僅作為市場狀態輔助，不作買賣建議。
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

# 產生區塊拔靴法索引。區塊會保留連續數日的結構。
block_bootstrap_indices <- function(n, block_length = 10) {
  if (n <= 1) return(seq_len(n))
  block_length <- max(2, min(as.integer(block_length), n))
  starts <- seq_len(max(1, n - block_length + 1))
  idx <- integer(0)
  while (length(idx) < n) {
    s <- sample(starts, 1)
    idx <- c(idx, s:min(n, s + block_length - 1))
  }
  idx[seq_len(n)]
}

# 由「日期 × 股票」的日對數報酬矩陣計算研究分數。
# 這個函數用於區塊拔靴法與歷史回測，計算邏輯與主排名保持同一方向。
score_return_matrix <- function(return_matrix, model_weights = MODEL_WEIGHTS) {
  return_matrix <- as.matrix(return_matrix)
  if (nrow(return_matrix) < 63 || ncol(return_matrix) < 2) return(data.frame())
  symbols <- colnames(return_matrix)
  if (is.null(symbols)) symbols <- paste0("S", seq_len(ncol(return_matrix)))

  period_days <- TIME_WINDOWS
  period_weights <- RETURN_WINDOW_WEIGHTS

  # 風險、回撤、CVaR、Sharpe 固定用「最近 1 年」作主要比較視窗。
  # 這樣不會因為某檔股票上市 10 年、另一檔只有 3 年而混用不同長度。
  base_rows <- lapply(seq_along(symbols), function(j) {
    r_all <- return_matrix[, j]
    r_window <- tail(r_all, min(PRIMARY_RISK_WINDOW, nrow(return_matrix)))
    r <- r_window[is.finite(r_window)]
    if (length(r) < floor(PRIMARY_RISK_WINDOW * 0.75)) {
      return(data.frame(
        symbol=symbols[j], n_obs=length(r), annual_return=NA_real_,
        annual_volatility=NA_real_, max_drawdown=NA_real_, cvar_95=NA_real_,
        mean_daily_log=NA_real_, sharpe=NA_real_
      ))
    }
    wealth <- exp(cumsum(r))
    ann_vol <- sd(r, na.rm=TRUE) * sqrt(TRADING_DAYS_PER_YEAR)
    mean_daily <- mean(r, na.rm=TRUE)
    data.frame(
      symbol=symbols[j],
      n_obs=length(r),
      annual_return=exp(mean_daily * TRADING_DAYS_PER_YEAR) - 1,
      annual_volatility=ann_vol,
      max_drawdown=min(drawdown_value(wealth), na.rm=TRUE),
      cvar_95=cvar_value(r, 0.95),
      mean_daily_log=mean_daily,
      sharpe=ifelse(is.finite(ann_vol) && ann_vol > 0,
                    mean_daily * TRADING_DAYS_PER_YEAR / ann_vol, NA_real_),
      stringsAsFactors=FALSE
    )
  })
  base <- bind_rows(base_rows)

  period_long <- bind_rows(lapply(names(period_days), function(period_name) {
    n_day <- period_days[[period_name]]
    rows <- lapply(seq_along(symbols), function(j) {
      r_all <- return_matrix[, j]
      rr_window <- tail(r_all, min(n_day, nrow(return_matrix)))
      r <- rr_window[is.finite(rr_window)]
      min_required <- max(3, floor(n_day * 0.75))
      if (length(r) < min_required) {
        return(data.frame(
          symbol=symbols[j], period=period_name,
          period_log_return=NA_real_, annualized_log_mean=NA_real_,
          annualized_volatility=NA_real_
        ))
      }
      rr <- r
      data.frame(
        symbol=symbols[j],
        period=period_name,
        period_log_return=sum(rr, na.rm=TRUE),
        annualized_log_mean=mean(rr, na.rm=TRUE) * TRADING_DAYS_PER_YEAR,
        annualized_volatility=ifelse(length(rr) >= 20,
                                     sd(rr, na.rm=TRUE) * sqrt(TRADING_DAYS_PER_YEAR), NA_real_),
        stringsAsFactors=FALSE
      )
    })
    bind_rows(rows)
  })) %>%
    group_by(period) %>%
    mutate(return_percentile = cross_section_score(period_log_return, TRUE)) %>%
    ungroup()

  # 報酬與排名穩定性：六個統一時間尺度一起看。
  # 風險穩定性：1週與1月樣本太短，不納入；由3月、6月、1年、3年比較。
  stability_rank_return <- period_long %>%
    group_by(symbol) %>%
    summarise(
      rank_variation=sd(return_percentile, na.rm=TRUE),
      return_variation=sd(annualized_log_mean, na.rm=TRUE),
      valid_periods=sum(is.finite(period_log_return)),
      .groups="drop"
    )

  stability_risk <- period_long %>%
    filter(period %in% c("3個月", "6個月", "1年", "3年")) %>%
    group_by(symbol) %>%
    summarise(
      risk_variation=sd(annualized_volatility, na.rm=TRUE),
      valid_risk_periods=sum(is.finite(annualized_volatility)),
      .groups="drop"
    )

  stability_raw <- stability_rank_return %>%
    left_join(stability_risk, by="symbol") %>%
    mutate(
      rank_variation=ifelse(valid_periods >= 3, rank_variation, NA_real_),
      return_variation=ifelse(valid_periods >= 3, return_variation, NA_real_),
      risk_variation=ifelse(valid_risk_periods >= 2, risk_variation, NA_real_),
      rank_stability_score=cross_section_score(rank_variation, FALSE),
      return_stability_score=cross_section_score(return_variation, FALSE),
      risk_stability_score=cross_section_score(risk_variation, FALSE),
      stability_score=0.30 * rank_stability_score +
                      0.40 * return_stability_score +
                      0.30 * risk_stability_score
    )

  multi_return <- period_long %>%
    mutate(weight=period_weights[period]) %>%
    group_by(symbol) %>%
    summarise(
      multi_period_return_score=ifelse(
        sum(is.finite(return_percentile)) >= 3,
        weighted.mean(return_percentile, weight, na.rm=TRUE),
        NA_real_
      ),
      .groups="drop"
    )

  base %>%
    mutate(
      annual_return_score=cross_section_score(annual_return, TRUE),
      volatility_score=cross_section_score(annual_volatility, FALSE),
      drawdown_score=cross_section_score(max_drawdown, TRUE),
      cvar_score=cross_section_score(cvar_95, TRUE),
      sharpe_score=cross_section_score(sharpe, TRUE)
    ) %>%
    left_join(stability_raw, by="symbol") %>%
    left_join(multi_return, by="symbol") %>%
    mutate(
      return_score=0.70 * multi_period_return_score + 0.30 * annual_return_score,
      risk_score=0.35 * volatility_score + 0.35 * drawdown_score + 0.30 * cvar_score,
      composite_score=model_weights[["stability"]] * stability_score +
                      model_weights[["ret"]] * return_score +
                      model_weights[["risk"]] * risk_score +
                      model_weights[["sharpe"]] * sharpe_score
    )
}

# 將長表資料轉為日期 × 股票的日對數報酬矩陣。
return_wide_matrix <- function(x, symbols = NULL) {
  if (!is.null(symbols)) x <- x %>% filter(symbol %in% symbols)
  wide <- x %>%
    filter(is.finite(log_return)) %>%
    select(date, symbol, log_return) %>%
    distinct(date, symbol, .keep_all=TRUE) %>%
    pivot_wider(names_from=symbol, values_from=log_return) %>%
    arrange(date)
  if (nrow(wide) == 0 || ncol(wide) < 3) return(NULL)
  m <- as.matrix(wide[, -1, drop=FALSE])
  storage.mode(m) <- "double"
  m
}

# 60 日滾動相關性：使用移動加總公式，避免每一天都重新呼叫 cor()。
rolling_correlation_fast <- function(x, y, window = 60) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  ok <- is.finite(x) & is.finite(y)
  x[!ok] <- NA_real_
  y[!ok] <- NA_real_
  roll_sum <- function(z) as.numeric(stats::filter(z, rep(1, window), sides = 1))
  n_obs <- roll_sum(as.numeric(ok))
  x0 <- ifelse(ok, x, 0)
  y0 <- ifelse(ok, y, 0)
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

# ============================================================
# 4. 資料整理函數
# ============================================================

prepare_stock_data <- function(raw_data) {
  
  if (!is.data.frame(raw_data)) {
    stop("stock_data.rds 不是 data.frame。")
  }
  
  required <- c(
    "date",
    "symbol"
  )
  
  missing_cols <-
    setdiff(
      required,
      names(raw_data)
    )
  
  if (length(missing_cols) > 0) {
    
    stop(
      paste0(
        "資料缺少必要欄位：",
        paste(
          missing_cols,
          collapse = ", "
        )
      )
    )
  }
  
  x <- raw_data
  
  
  # ----------------------------------------------------------
  # 日期與股票代號
  # ----------------------------------------------------------
  
  x$date <- convert_to_date(
    x$date
  )
  
  if (is.numeric(x$symbol)) {
    
    x$symbol <- sprintf(
      "%04d",
      as.integer(x$symbol)
    )
    
  } else {
    
    x$symbol <- as.character(
      x$symbol
    )
  }
  
  
  # ----------------------------------------------------------
  # 調整後價格
  # ----------------------------------------------------------
  
  if ("adjusted" %in% names(x)) {
    
    x$adjusted <- safe_numeric(
      x$adjusted
    )
    
  } else if ("price.adjusted" %in% names(x)) {
    
    x$adjusted <- safe_numeric(
      x$price.adjusted
    )
    
  } else if ("close" %in% names(x)) {
    
    x$adjusted <- safe_numeric(
      x$close
    )
    
  } else if ("price.close" %in% names(x)) {
    
    x$adjusted <- safe_numeric(
      x$price.close
    )
    
  } else {
    
    stop(
      "找不到 adjusted / price.adjusted / close / price.close。"
    )
  }
  
  
  # ----------------------------------------------------------
  # 普通收盤價
  # ----------------------------------------------------------
  
  if ("close" %in% names(x)) {
    
    x$close <- safe_numeric(
      x$close
    )
    
  } else if ("price.close" %in% names(x)) {
    
    x$close <- safe_numeric(
      x$price.close
    )
    
  } else {
    
    x$close <- x$adjusted
  }
  
  
  # ----------------------------------------------------------
  # 成交量
  # ----------------------------------------------------------
  
  if ("volume" %in% names(x)) {
    
    x$volume <- safe_numeric(
      x$volume
    )
    
  } else if ("price.volume" %in% names(x)) {
    
    x$volume <- safe_numeric(
      x$price.volume
    )
    
  } else {
    
    x$volume <- NA_real_
  }
  
  
  # ----------------------------------------------------------
  # 股票名稱與產業
  # ----------------------------------------------------------
  
  x$name <- vapply(
    x$symbol,
    get_stock_name,
    character(1)
  )
  
  x$industry <- vapply(
    x$symbol,
    get_stock_industry,
    character(1)
  )
  
  x$industry_group <-
    ifelse(
      x$industry %in%
        major_industries,
      x$industry,
      "其他"
    )
  
  
  # ----------------------------------------------------------
  # 基本清理
  # ----------------------------------------------------------
  
  x <- x %>%
    filter(
      !is.na(date),
      !is.na(symbol)
    ) %>%
    arrange(
      symbol,
      date
    ) %>%
    distinct(
      symbol,
      date,
      .keep_all = TRUE
    ) %>%
    mutate(
      adjusted = ifelse(
        is.finite(adjusted) &
          adjusted > 0,
        adjusted,
        NA_real_
      ),
      close = ifelse(
        is.finite(close) &
          close > 0,
        close,
        NA_real_
      )
    )
  
  
  # ----------------------------------------------------------
  # 日報酬
  # ----------------------------------------------------------
  
  x <- x %>%
    group_by(symbol) %>%
    arrange(
      date,
      .by_group = TRUE
    ) %>%
    mutate(
      
      daily_return =
        adjusted /
        lag(adjusted) - 1,
      
      log_return =
        log(
          adjusted /
            lag(adjusted)
        )
      
    ) %>%
    ungroup() %>%
    arrange(
      symbol,
      date
    )
  
  
  if (!"0050" %in% x$symbol) {
    stop("資料中找不到 0050。")
  }
  
  x
}


# ============================================================
# 5. 載入初始資料
# ============================================================

# ------------------------------------------------------------
# 啟動防呆：資料檔或資料格式出錯時，不讓整個 Connect 內容直接 Startup Error。
# 會改為啟動一個診斷頁，直接顯示真正錯誤原因。
# ------------------------------------------------------------

startup_result <- if (!file.exists(DATA_FILE)) {
  list(
    data = NULL,
    error = paste0(
      "找不到資料檔：", DATA_FILE,
      "。請確認部署內容包含 data/stock_data.rds。"
    )
  )
} else {
  tryCatch(
    list(
      data = prepare_stock_data(readRDS(DATA_FILE)),
      error = NULL
    ),
    error = function(e) {
      list(
        data = NULL,
        error = paste0(
          "資料檔讀取或整理失敗：",
          conditionMessage(e)
        )
      )
    }
  )
}

if (!is.null(startup_result$error)) {
  ui <- fluidPage(
    lang = "zh-Hant",
    tags$head(
      tags$style(HTML("
        body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI','Microsoft JhengHei',sans-serif;background:#f6f9fc;color:#334155;}
        .startup-card{max-width:920px;margin:70px auto;background:#fff;border:1px solid #dbe5ef;border-radius:18px;padding:30px;box-shadow:0 8px 30px rgba(72,105,140,.10);}
        .startup-card h2{color:#29445f;margin-top:0;}
        .startup-error{background:#fff4f4;border-left:5px solid #d95b5b;padding:16px;border-radius:10px;line-height:1.8;margin:18px 0;}
        .startup-note{background:#f5f9fd;border:1px solid #dfe8f1;padding:14px 16px;border-radius:10px;line-height:1.8;}
        code{background:#eef3f8;padding:2px 6px;border-radius:5px;}
      "))
    ),
    div(
      class = "startup-card",
      h2("0050 Dashboard｜啟動診斷"),
      p("網站程式本身已啟動，但資料載入階段發現問題。"),
      div(class = "startup-error", strong("實際錯誤："), br(), startup_result$error),
      div(
        class = "startup-note",
        p(strong("目前工作目錄："), code(normalizePath(getwd(), winslash = "/", mustWork = FALSE))),
        p(strong("預期資料位置："), code(DATA_FILE)),
        p("如果你是把我提供的 app-only ZIP 直接部署到 Posit Connect，請不要這樣部署；應把 app.R 放回原本的 GitHub 專案根目錄，並保留既有的 data/、update_data.R 與 .github/。")
      )
    )
  )
  server <- function(input, output, session) {}
  shinyApp(ui = ui, server = server)
} else {

initial_data <- startup_result$data

data_min_date <- min(
  initial_data$date,
  na.rm = TRUE
)

data_max_date <- max(
  initial_data$date,
  na.rm = TRUE
)

available_symbols <- sort(
  unique(
    initial_data$symbol
  )
)

dashboard_symbols <- c(
  "0050",
  stock_info$symbol
)

dashboard_symbols <-
  dashboard_symbols[
    dashboard_symbols %in%
      available_symbols
  ]

dashboard_stock_choices <-
  setNames(
    dashboard_symbols,
    vapply(
      dashboard_symbols,
      get_stock_label,
      character(1)
    )
  )
portfolio_default_end_date <- data_max_date

portfolio_default_start_date <- max(
  data_min_date,
  data_max_date - 365
)

# ============================================================
# 共用顏色
# ============================================================

UP_COLOR <- "#d95b5b"
DOWN_COLOR <- "#4fa66a"
NEUTRAL_COLOR <- "#7b8794"

# ============================================================
# 代表色系統
# ============================================================

# 0050 固定使用中等藍色，其餘股票依固定順序產生代表色。
# 因此只要股票代號相同，同一檔股票在所有圖形都會維持相同顏色。

other_stock_symbols <-
  setdiff(
    dashboard_symbols,
    "0050"
  )

stock_colors <- c(
  "0050" = "#4f86c6"
)

if (length(other_stock_symbols) > 0) {
  # 刻意避開純紅 / 純綠，讓紅綠保留給台股漲跌語意。
  # 色相分成藍紫、紫紅、橘金、青藍四區，並固定依股票代號順序配置。
  n_stock_color <- length(other_stock_symbols)
  hue_pool <- c(
    seq(195, 255, length.out = 18),
    seq(270, 330, length.out = 14),
    seq(25, 55, length.out = 8),
    seq(165, 190, length.out = 10)
  )
  hue_pool <- hue_pool[seq_len(min(length(hue_pool), n_stock_color))]
  if (length(hue_pool) < n_stock_color) {
    hue_pool <- rep(hue_pool, length.out = n_stock_color)
  }
  lum <- rep(c(55, 64), length.out = n_stock_color)
  chroma <- rep(c(58, 46, 52), length.out = n_stock_color)
  stock_colors[other_stock_symbols] <- grDevices::hcl(
    h = hue_pool,
    c = chroma,
    l = lum,
    fixup = TRUE
  )
}


# ------------------------------------------------------------
# 產業分類
#
# 少於 4 檔成分股的產業全部合併為「其他」。
# ------------------------------------------------------------

stock_info$industry_group <- industry_group(
  stock_info$industry
)

available_industries <- sort(
  unique(
    stock_info$industry_group
  )
)

# ============================================================
# 產業重新分組
# 少於 4 檔的產業全部合併為「其他」
# ============================================================

industry_count <-
  table(
    stock_info$industry
  )

major_industries <-
  names(
    industry_count[
      industry_count >= 4
    ]
  )

stock_info$industry_group <-
  ifelse(
    stock_info$industry %in%
      major_industries,
    stock_info$industry,
    "其他"
  )

available_industries <-
  sort(
    unique(
      stock_info$industry_group
    )
  )


# ------------------------------------------------------------
# 產業代表色
# ------------------------------------------------------------

industry_colors <-
  setNames(
    hcl.colors(
      length(available_industries),
      palette = "Dynamic"
    ),
    available_industries
  )

if (
  "其他" %in% names(industry_colors)
) {
  
  industry_colors[
    "其他"
  ] <- "gray45"
  
}


# ============================================================
# 6. 共用產業
# ============================================================

available_industries <- sort(
  unique(
    stock_info$industry_group
  )
)

# ============================================================
# 7. UI
# ============================================================

ui <- fluidPage(
  
  lang = "zh-Hant",
  
  tags$head(
    
    tags$style(
      
      HTML(
        
        "
        :root {
          --blue:#5b8ecb;
          --blue2:#82b5e8;
          --purple:#9a7fd1;
          --green:#65a77b;
          --amber:#dca75a;
          --red:#d95b5b;
          --ink:#334155;
          --muted:#6b7b8d;
          --bg:#f6f9fc;
          --card:#ffffff;
          --line:#e1e8f0;
        }

        body {
          background:linear-gradient(180deg,#f3f8fd 0,#f8fafc 260px,#f6f9fc 100%);
          color:var(--ink);
          font-family:-apple-system,BlinkMacSystemFont,'Segoe UI','Microsoft JhengHei',sans-serif;
        }

        .container-fluid { padding:22px 32px 42px 32px; max-width:1600px; }

        .hero {
          background:linear-gradient(120deg,#eaf4ff 0%,#e7f1fb 55%,#edf0ff 100%);
          color:#29445f;
          border:1px solid #d5e4f2;
          border-radius:20px;
          padding:26px 30px;
          margin-bottom:18px;
          box-shadow:0 8px 24px rgba(72,105,140,.10);
        }
        .dashboard-title { font-size:32px; font-weight:800; letter-spacing:.3px; margin-bottom:6px; color:#24415f; }
        .dashboard-subtitle { color:#526b82; margin-bottom:0; font-size:16px; line-height:1.7; }
        .hero-badge { display:inline-block; margin-top:12px; padding:6px 11px; border-radius:999px; background:rgba(255,255,255,.78); color:#56708a; border:1px solid #d4e2ef; font-size:13px; }

        .nav-tabs {
          margin-bottom:22px; border-bottom:0; background:white; padding:8px;
          border-radius:14px; box-shadow:0 4px 14px rgba(72,105,140,.07);
        }
        .nav-tabs > li > a { font-weight:700; color:#5d6f81; border:0!important; border-radius:10px; margin:2px; }
        .nav-tabs > li > a:hover { background:#eef6fd; color:#477cad; }
        .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
          background:#6f9fd0; color:white; border:0!important;
          box-shadow:0 4px 10px rgba(91,142,203,.18);
        }

        .section-box {
          background:rgba(255,255,255,.98); border:1px solid var(--line); border-radius:18px;
          padding:26px; margin-bottom:22px; box-shadow:0 6px 20px rgba(72,105,140,.06);
        }
        .section-box h2 { font-weight:800; color:#29445f; margin-top:0; }
        .section-box h3 { font-weight:750; color:#3d5e7e; margin-top:26px; }
        .page-description { color:#60758a; line-height:1.8; font-size:15px; }

        .reading-guide {
          background:linear-gradient(90deg,#f1f7fd,#f5f3fb); border-left:5px solid #7fa9d3;
          padding:15px 18px; border-radius:10px; margin:15px 0 20px; line-height:1.75;
        }
        .plot-explanation {
          background:#fafbfc; border:1px solid #e4e9ef; border-left:4px solid #a18bd0;
          border-radius:10px; padding:13px 16px; margin:10px 0 20px; line-height:1.75; color:#56697a;
        }
        .method-note {
          background:#fffaf2; border:1px solid #f1dfc4; border-left:4px solid #dca75a;
          border-radius:10px; padding:14px 16px; margin:12px 0 18px; line-height:1.75;
        }

        .info-card {
          background:#ffffff; border:1px solid #e0e8f0;
          border-radius:15px; padding:17px 18px; min-height:112px; margin-bottom:15px;
          box-shadow:0 4px 13px rgba(72,105,140,.06); transition:transform .15s ease, box-shadow .15s ease;
        }
        .info-card:hover { transform:translateY(-2px); box-shadow:0 7px 17px rgba(72,105,140,.09); }
        .info-card-title { color:#718195; font-size:13px; font-weight:700; margin-bottom:8px; }
        .info-card-value { font-size:25px; font-weight:800; color:#385775; }
        .accent-blue { border-top:4px solid #78a7d5; }
        .accent-purple { border-top:4px solid #a28bd2; }
        .accent-green { border-top:4px solid #7caf8c; }
        .accent-amber { border-top:4px solid #dfb46f; }
        .accent-red { border-top:4px solid #de7777; }

        .data-status { background:#f9fbfd; border:1px solid #e0e8f0; border-radius:12px; padding:16px; line-height:1.85; }
        .warning-note { background:#fffaf0; border-left:5px solid #dfb46f; padding:13px 16px; border-radius:8px; margin:10px 0; line-height:1.7; }
        .quick-button-row { margin:10px 0 15px; }
        .quick-button-row .btn,
        .quick-button-row .action-button.btn-default {
          margin-right:6px; margin-bottom:6px; border-radius:9px;
          background:#ffffff; color:#53687b; border:1px solid #d8e1ea;
          box-shadow:none;
        }
        .quick-button-row .btn:hover,
        .quick-button-row .action-button.btn-default:hover {
          background:#f4f8fc; color:#365d81; border-color:#bdd0e2;
        }
        .btn-primary { border-radius:9px; background:#6f9fd0; border-color:#6f9fd0; color:white; }
        .btn-primary:hover { background:#5f8fbe; border-color:#5f8fbe; color:white; }
        .time-scope-bar {
          background:#f8fbfe; border:1px solid #e0e8f0; border-radius:11px;
          padding:11px 14px; margin:12px 0 18px; color:#596f84; line-height:1.7;
        }
        .time-scope-bar strong { color:#385775; }

        table { background:white; }
        .table > thead > tr > th { background:#f0f5fa; color:#49647e; border-bottom:2px solid #d6e1eb; }
        .table-striped > tbody > tr:nth-of-type(odd) { background:#ffffff; }
        .table-hover > tbody > tr:hover { background:#f7fafc; }

        details.method-details { background:#fbfcfd; border:1px solid #e1e8f0; border-radius:12px; padding:12px 16px; margin:10px 0; }
        details.method-details summary { cursor:pointer; font-weight:750; color:#527fae; }
        .term-chip { display:inline-block; padding:5px 9px; margin:3px; background:#f0f4fb; color:#6d6298; border-radius:999px; font-size:12px; font-weight:700; }

        .dashboard-footer { text-align:center; color:#8b99a8; font-size:13px; padding-top:22px; }
        "
        
      )
    )
  ),
  
  
  div(
    class = "hero",
    div(class = "dashboard-title", "0050 投資研究 Dashboard"),
    div(
      class = "dashboard-subtitle",
      "以日對數報酬為統計基礎，從報酬、風險、穩定性、不確定性與歷史驗證分析 0050 及其成分股。"
    ),
    span(
      class = "hero-badge",
      "統計研究｜非投資建議｜資料更新至：",
      textOutput("header_latest_date", inline = TRUE)
    )
  ),
  
  
  # ==========================================================
  # 主分頁
  # ==========================================================
  
  tabsetPanel(
    
    id = "main_tabs",
    
    
    # ========================================================
    # 1. 0050 總覽
    # ========================================================
    
    tabPanel(
      
      "0050 總覽",
      
      div(
        class = "section-box",
        
        h2("0050 總覽"),
        
        p(
          class = "page-description",
          "以 0050 為核心，快速掌握資料狀態、價格走勢、日對數報酬、風險與成分股近期表現。"
        ),
        
        div(
          class = "reading-guide",
          strong("怎麼看？ "),
          "先確認資料更新日期，再觀察 0050 的價格走勢與日對數報酬；下方成分股排名則用來了解近期相對表現。"
        ),
        
        h3("資料狀態"),
        
        div(
          class = "data-status",
          
          p(
            strong("目前最新日期："),
            textOutput(
              "global_latest_date",
              inline = TRUE
            )
          ),
          
          p(
            strong("最早日期："),
            textOutput(
              "global_earliest_date",
              inline = TRUE
            )
          ),
          
          p(
            strong("股票數量："),
            textOutput(
              "global_stock_count",
              inline = TRUE
            )
          ),
          
          p(
            strong("資料筆數："),
            textOutput(
              "global_observation_count",
              inline = TRUE
            )
          ),
          
          p(
            strong("檔案更新時間："),
            textOutput(
              "global_file_modified",
              inline = TRUE
            )
          )
        ),
        
        h3("市場觀察日期"),
        
        dateInput(
          "market_observation_date",
          "請選擇日期：",
          value = data_max_date,
          min = data_min_date,
          max = data_max_date,
          format = "yyyy-mm-dd",
          language = "zh-TW",
          weekstart = 1
        ),
        
        h3("0050 與市場概況"),
        
        fluidRow(
          
          column(
            3,
            div(
              class = "info-card",
              div(
                class = "info-card-title",
                "0050 調整後價格"
              ),
              h3(
                class = "info-card-value",
                textOutput(
                  "market_0050_price",
                  inline = TRUE
                )
              )
            )
          ),
          
          column(
            3,
            div(
              class = "info-card",
              div(
                class = "info-card-title",
                "最近一日日對數報酬率"
              ),
              h3(
                class = "info-card-value",
                textOutput(
                  "market_0050_daily_return",
                  inline = TRUE
                )
              )
            )
          ),
          
          column(
            3,
            div(
              class = "info-card",
              div(
                class = "info-card-title",
                "年初至今報酬"
              ),
              h3(
                class = "info-card-value",
                textOutput(
                  "market_ytd_return",
                  inline = TRUE
                )
              )
            )
          ),
          
          column(
            3,
            div(
              class = "info-card",
              div(
                class = "info-card-title",
                "50 檔漲跌家數"
              ),
              h3(
                class = "info-card-value",
                textOutput(
                  "market_breadth",
                  inline = TRUE
                )
              )
            )
          )
        ),
        
        h3("0050 統計研究摘要"),
        
        fluidRow(
          column(
            3,
            div(
              class = "info-card accent-blue",
              div(class = "info-card-title", "0050 綜合分數"),
              h3(class = "info-card-value", textOutput("market_0050_composite_score", inline = TRUE))
            )
          ),
          column(
            3,
            div(
              class = "info-card accent-purple",
              div(class = "info-card-title", "0050 穩定性分數"),
              h3(class = "info-card-value", textOutput("market_0050_stability_score", inline = TRUE))
            )
          ),
          column(
            3,
            div(
              class = "info-card accent-amber",
              div(class = "info-card-title", "0050 年化波動度"),
              h3(class = "info-card-value", textOutput("market_0050_volatility", inline = TRUE))
            )
          ),
          column(
            3,
            div(
              class = "info-card accent-red",
              div(class = "info-card-title", "0050 最大回撤"),
              h3(class = "info-card-value", textOutput("market_0050_max_drawdown", inline = TRUE))
            )
          )
        ),
        
        h3("0050 近期市場狀態"),
        
        fluidRow(
          column(4, div(class = "info-card accent-blue", div(class = "info-card-title", "均線趨勢"), h3(class = "info-card-value", textOutput("market_0050_trend", inline = TRUE)))),
          column(4, div(class = "info-card accent-purple", div(class = "info-card-title", "14 日相對強弱指標（RSI）"), h3(class = "info-card-value", textOutput("market_0050_rsi", inline = TRUE)))),
          column(4, div(class = "info-card accent-amber", div(class = "info-card-title", "近期波動狀態"), h3(class = "info-card-value", textOutput("market_0050_vol_state", inline = TRUE))))
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "技術狀態只用來描述近期趨勢與動能，不產生買進或賣出建議。排名與統計分析仍以日對數報酬、風險與穩定性為主。"
        ),
        
        hr(),
        
        h3("0050 調整後收盤價"),
        
        checkboxInput(
          "market_price_color_by_change",
          "啟用漲跌著色",
          value = FALSE
        ),
        
        dateRangeInput(
          "market_chart_date_range",
          "圖表時間範圍：",
          start = data_min_date,
          end = data_max_date,
          min = data_min_date,
          max = data_max_date,
          format = "yyyy-mm-dd",
          separator = " 至 "
        ),
        
        plotOutput(
          "market_0050_price_chart",
          height = "550px"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "主要觀察 0050 長期價格趨勢。曲線上升代表調整後價格增加，短期斜率越大代表該段價格變動越明顯。"
        ),
        
        hr(),
        
        h3("0050 近一年日對數報酬率"),
        
        checkboxInput(
          "market_return_color_by_change",
          "啟用漲跌幅著色",
          value = FALSE
        ),
        
        plotOutput(
          "market_0050_daily_return_timeseries",
          height = "500px"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "高於 0% 代表上漲，低於 0% 代表下跌；離 0% 越遠代表當日價格變動幅度越大。"
        ),
        
        hr(),
        
        h3("近 30 個交易日成分股漲跌幅"),
        
        plotOutput(
          "market_30d_return_ranking",
          height = "700px"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "往右代表近 30 個交易日累積對數報酬較高，往左代表較低。此圖描述過去期間，不代表未來表現。"
        ),
        
        hr(),
        
        h3("最新行情"),
        
        tableOutput(
          "realtime_monitor_table"
        )
      )
    ),
    
    
    # ========================================================
    # 2. 成分股探索
    # ========================================================
    
    tabPanel(
      
      "成分股探索",
      
      div(
        class = "section-box",
        
        h2("成分股探索"),
        
        p(
          class = "page-description",
          "瀏覽 0050 與 50 檔成分股的指定日期行情，作為後續排名比較、個股分析與產業分析的入口。"
        ),
        
        dateInput(
          "monitor_date",
          "行情觀察日期：",
          value = data_max_date,
          min = data_min_date,
          max = data_max_date,
          format = "yyyy-mm-dd",
          language = "zh-TW",
          weekstart = 1
        ),
        
        div(
          class = "reading-guide",
          strong("怎麼看？ "),
          "日對數報酬率是本研究主要報酬指標；可先從個股行情了解當日變化，再進一步前往綜合排名與個股分析。"
        ),
        
        tableOutput(
          "monitor_table"
        )
      )
    ),
    
    
    # ========================================================
    # 3. 綜合排名
    # ========================================================
    
    tabPanel(
      
      "綜合排名",
      
      div(
        class = "section-box",
        
        h2("成分股綜合排名"),
        
        p(
          class = "page-description",
          "這個排名不是單純找『漲最多』的股票，而是把長期穩定程度、報酬、風險控制與風險報酬效率一起比較。0050 是比較基準，不參與成分股名次。"
        ),
        
        div(
          class = "reading-guide",
          strong("這個模型比較重視什麼？ "),
          "我們偏好在不同期間都能維持相對一致表現、報酬合理，而且不需要承擔過度波動或深度回撤的股票。因此短期大漲但風險很高的股票，不一定會排在前面。"
        ),
        uiOutput("model_time_scope_ui"),
        fluidRow(
          column(3, div(class="info-card accent-blue", div(class="info-card-title", "穩定性｜40%"), div("不同期間的報酬、排名與風險是否一致。"))),
          column(3, div(class="info-card accent-green", div(class="info-card-title", "報酬｜25%"), div("兼顧近期與中長期報酬，而不是只看單一期間。"))),
          column(3, div(class="info-card accent-amber", div(class="info-card-title", "風險控制｜20%"), div("波動、最大回撤與極端損失是否相對可控。"))),
          column(3, div(class="info-card accent-purple", div(class="info-card-title", "風險報酬效率｜15%"), div("承擔相同風險時，是否換到較好的報酬。")))
        ),
        
        details(
          class = "method-details",
          tags$summary("查看綜合分數具體怎麼算"),
          p("第一步：每個原始指標先在 0050 成分股之間轉成 0–100 分，避免百分比、回撤與夏普比率直接相加。"),
          p("第二步：穩定性 = 30% 排名穩定 + 40% 報酬穩定 + 30% 風險穩定。排名與報酬跨 1週、1月、3月、6月、1年、3年比較；風險從 3個月以上開始比較。"),
          p("第三步：報酬分數 = 70% 多時間尺度報酬 + 30% 近 1 年年化報酬。多時間尺度內部權重為：1週 5%、1月 10%、3月 15%、6月 20%、1年 30%、3年 20%。"),
          p("第四步：風險控制分數 = 35% 波動度控制 + 35% 最大回撤控制 + 30% CVaR 尾端風險控制；這三項主要使用近 1 年資料。"),
          p("第五步：綜合分數 = 40% 穩定性 + 25% 報酬 + 20% 風險控制 + 15% 風險報酬效率。"),
          p("例：穩定性 80、報酬 70、風險控制 75、風險報酬效率 85，則綜合分數 = 80×0.40 + 70×0.25 + 75×0.20 + 85×0.15 = 77.25。")
        ),
        h3("綜合分數前 10 名"),
        plotOutput("composite_ranking_chart", height = "520px"),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "分數越高，代表這檔股票在我們重視的『穩定、報酬、風險控制、風險報酬效率』四個面向整體較均衡。高分不代表未來一定上漲，也不是買進建議。"
        ),
        
        h3("完整成分股排名"),
        tableOutput("composite_ranking_table")
      )
    ),
    
    
    # ========================================================
    # 4. 風險與穩定性
    # ========================================================
    
    tabPanel(
      
      "風險與穩定性",
      
      div(
        class = "section-box",
        
        h2("風險與穩定性分析"),
        
        p(
          class = "page-description",
          "這一頁想回答兩件事：過去承受的風險有多大？表現是不是只好在某一段時間，還是不同期間都相對一致？"
        ),
        
        div(
          class = "reading-guide",
          strong("穩定性是什麼？ "),
          "我們同時看 1 週、1 個月、3 個月、6 個月、1 年與 3 年。報酬與排名使用六個時間尺度；風險穩定性則從 3 個月以上的期間比較，避免用只有幾天的資料硬算風險。高穩定不等於報酬最高，而是代表結果比較不依賴某一段剛好特別好的行情。"
        ),
        uiOutput("model_time_scope_ui_risk"),
        
        h3("穩定性分數前 10 名"),
        plotOutput("stability_ranking_chart", height = "520px"),
        
        h3("風險與穩定性摘要"),
        tableOutput("risk_stability_table")
      )
    ),
    
    
    # ========================================================
    # 5. 主成分與分群
    # ========================================================
    
    tabPanel(
      
      "主成分與分群",
      
      div(
        class = "section-box",
        
        h2("主成分分析與分群"),

        p(
          class = "page-description",
          "這一頁不是拿股價本身分群，而是把每一檔股票在同一段時間內的報酬、波動、回撤、尾端風險與風險報酬效率整理成統計特徵，再找出特性相近的股票。"
        ),

        fluidRow(
          column(
            4,
            selectInput(
              "pca_period",
              "PCA／分群使用多久資料：",
              choices = PCA_WINDOWS,
              selected = 252
            )
          ),
          column(8, uiOutput("pca_period_info"))
        ),

        div(
          class = "reading-guide",
          strong("這頁實際拿什麼資料？ "),
          "一檔股票就是一筆資料。每檔股票都只使用上方選定的同一段時間，計算：年化報酬、年化波動、最大回撤、CVaR、夏普比率、上漲日比例、21 日報酬變動程度與 21 日波動變動程度。先標準化後才做 PCA，因此不會因為某個指標數字本來比較大就支配結果。"
        ),

        div(
          class = "method-note",
          strong("為什麼不提供 1 週／1 個月 PCA？ "),
          "PCA 與分群需要同時估計多個風險特徵；1 週只有約 5 個交易日、1 個月約 21 個交易日，CVaR、夏普與風險穩定度會非常不穩。因此本頁從 3 個月開始，預設近 1 年，3 年只作較長期對照。"
        ),

        h3("PCA 在整理什麼資訊？"),
        uiOutput("pca_interpretation_ui"),

        h3("主成分解釋變異"),
        tableOutput("pca_variance_table"),

        h3("主成分負荷量"),
        tableOutput("pca_loading_table"),

        h3("分群數比較"),
        tableOutput("cluster_quality_table"),

        h3("主成分分群圖"),
        plotOutput("pca_cluster_chart", height = "600px"),
        div(class="plot-explanation", strong("怎麼看？ "), "每一個點是一檔 0050 成分股；距離較近代表統計特徵比較相似。顏色代表 K-means 分到的群組，群組只用來描述股票特性，不直接決定綜合排名。"),

        h3("各群平均特徵"),
        tableOutput("cluster_summary_table"),

        h3("每一檔股票屬於哪一群？"),
        uiOutput("cluster_filter_ui"),
        tableOutput("cluster_member_table")
      )
    ),
    
    
    # ========================================================
    # 6. 個股分析
    # ========================================================
    
    tabPanel(
      
      "個股分析",
      
      div(
        class = "section-box",
        
        h2("個股分析"),
        
        p(
          class = "page-description",
          "最多選擇 5 檔股票，以指定期間比較價格、累積報酬、日對數報酬與風險。"
        ),
        
        div(
          class = "reading-guide",
          strong("分析原則："),
          "研究分析使用調整後價格；日對數報酬率為 ln(Pt / Pt-1)。"
        ),
        
        selectizeInput(
          "selected_stock",
          "請選擇股票（最多 5 檔）：",
          choices = dashboard_stock_choices,
          selected = "0050",
          multiple = TRUE,
          options = list(
            maxItems = MAX_SELECTED_STOCKS,
            plugins = list("remove_button")
          )
        ),
        
        
        
        div(
          class = "quick-button-row",

          strong("快速期間："),
          actionButton("period_1w", "1 週"),
          actionButton("period_1m", "1 個月"),
          actionButton("period_1q", "3 個月"),
          actionButton("period_6m", "6 個月"),
          actionButton("period_1y", "1 年"),
          actionButton("period_3y", "3 年"),
          actionButton("period_all", "全部")
        ),

        dateRangeInput(
          "date_range",
          "分析期間：",
          start = RESEARCH_START_DATE,
          end = min(
            RESEARCH_END_DATE,
            data_max_date
          ),
          min = data_min_date,
          max = data_max_date,
          format = "yyyy-mm-dd",
          separator = " 至 "
        ),
        
        textOutput(
          "analysis_period_text"
        ),
        
        hr(),
        
        h3("所選股票實際資料期間"),
        
        tableOutput(
          "selected_stock_ranges"
        ),
        
        uiOutput(
          "stock_warning"
        ),
        
        hr(),
        
        h3("調整後收盤價"),
        
        plotOutput(
          "price_plot",
          height = "500px"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "價格圖主要看趨勢。不同股票價格尺度不同，因此比較績效時應搭配累積報酬率。"
        ),
        
        uiOutput(
          "cumulative_return_display"
        ),
        
        
        
        hr(),
        
        h3("日對數報酬描述統計"),
        
        tableOutput(
          "return_summary"
        ),
        
        hr(),
        
        h3("日對數報酬分布"),
        
        uiOutput(
          "return_distribution_ui"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "直方圖呈現實際日對數報酬；深色曲線為依該股票樣本平均數與標準差估計的常態機率密度函數。若實際分布與曲線差異較大，表示資料未必符合常態分布。"
        ),
        
        
        hr(),
        
        h3("日對數報酬箱型圖"),
        
        uiOutput(
          "return_boxplot_ui"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "箱體代表中間 50% 的報酬分布。箱體越高通常表示報酬波動範圍越大。"
        ),
        
        hr(),
        
        h3("日對數報酬時間序列"),
        
        uiOutput(
          "log_return_series_ui"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "每檔股票各自一張圖，可以直接觀察高波動與極端報酬發生在哪些日期。"
        ),
        
        hr(),
        
        h3("期間績效"),
        
        tableOutput(
          "period_performance"
        ),
        
        hr(),
        
        h3("價格描述統計"),
        
        tableOutput(
          "price_summary"
        ),
        
        hr(),
        
        h3("風險摘要"),
        
        tableOutput(
          "risk_summary"
        ),
        
        hr(),
        
        h3("回撤"),
        
        uiOutput(
          "drawdown_charts_ui"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "回撤表示價格從之前高點下跌多少。越接近 0% 表示越接近高點，負值越深表示回落幅度越大。"
        ),
        
        hr(),
        
        h3("報酬與風險關係"),
        
        plotOutput(
          "return_risk_chart",
          height = "550px"
        ),
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "越往右代表年化波動度越高；越往上代表年化報酬率越高。右上方代表報酬與波動都較高，左下方則代表報酬與波動都較低。這張圖主要用來觀察報酬與風險的分布關係。"
        ),
        
        hr(),
        h3("與 0050 的關係"),
        div(
          class = "reading-guide",
          strong("這裡在看什麼？ "),
          "相關係數看這檔股票和 0050 是否常常一起漲跌；60 日滾動相關性則看這種關係有沒有隨時間改變；Beta 則描述股票對 0050 漲跌的敏感程度。"
        ),
        tableOutput("individual_correlation_table"),
        h4("60 日滾動相關性"),
        plotOutput("individual_rolling_correlation_chart", height = "420px"),
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "越接近 1，代表最近約 60 個交易日和 0050 的漲跌較同步；接近 0 表示同步程度較弱。這個數值會隨市場狀況改變，所以比只看一個長期相關係數更能看出關係是否穩定。"
        )
      )
    ),
    
    
    # ========================================================
    # 7. 產業比較
    # ========================================================
    
    tabPanel(
      
      "產業比較",
      
      div(
        class = "section-box",
        
        h2("產業比較"),
        
        p(
          class = "page-description",
          "產業比較中，原始產業分類若少於 4 檔成分股，統一合併為「其他」。"
        ),
        
        div(
          class = "reading-guide",
          strong("怎麼看？ "),
          "先在上方選擇一個產業查看其個別結果，再往下觀察所有產業的綜合比較。每個產業都有固定代表色。"
        ),
        
        # ========================================================
        # 單一產業
        # ========================================================
        
        h3("單一產業分析"),
        
        selectInput(
          "selected_industry",
          "請選擇產業：",
          choices = available_industries,
          selected = available_industries[1]
        ),
        
        h3("產業成分股"),
        
        tableOutput(
          "industry_stock_table"
        ),
        
        h3("產業報酬摘要"),
        
        tableOutput(
          "industry_summary"
        ),
        
        h3("產業累積報酬"),
        
        plotOutput(
          "industry_cumulative_chart",
          height = "550px"
        ),
        
        h3("產業風險與報酬"),
        
        plotOutput(
          "industry_risk_return_chart",
          height = "500px"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "越往右代表波動度越高；越往上代表年化報酬率越高。可用來觀察該產業內各成分股的報酬與風險分布。"
        ),
        
        hr(),
        
        # ========================================================
        # 所有產業
        # ========================================================
        
        h3("所有產業綜合比較"),
        
        h4("產業累積報酬比較"),
        
        plotOutput(
          "industry_all_cumulative_chart",
          height = "600px"
        ),
        
        h4("產業日對數報酬分布"),
        
        uiOutput(
          "industry_all_distribution_ui"
        ),
        
        h4("產業報酬與風險"),
        
        plotOutput(
          "industry_all_risk_return_chart",
          height = "550px"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "所有產業圖形使用各自代表色。累積報酬圖比較不同產業期間變化，分布圖比較日報酬型態，風險報酬圖則比較產業的年化報酬率與年化波動度。"
        )
        
      )
      
    ),
    
    
    # ========================================================
    # 8. 模型驗證：區塊拔靴法＋歷史回測
    # ========================================================
    
    tabPanel(
      "模型驗證",
      div(
        class = "section-box",
        h2("模型驗證｜結果穩不穩？過去有沒有用？"),
        p(
          class = "page-description",
          "綜合排名先告訴我們誰的分數較高；模型驗證再從兩個方向檢查：這個差異會不會只是剛好，以及過去真的使用這套排名時，下一期是否有區辨能力。"
        ),
        tabsetPanel(
          id = "validation_tabs",
          type = "pills",
          tabPanel(
            "結果穩不穩｜區塊拔靴法",
            br(),
            div(
              class = "reading-guide",
              strong("白話版： "),
              "把歷史日對數報酬『一小段一小段』重新抽很多次，看看成分股相對 0050 的分數優勢會不會一下就消失。如果重新抽樣後大多仍高於 0050，代表這個差異相對穩定。"
            ),
            fluidRow(
              column(3, selectInput("bootstrap_stock", "比較成分股：", choices = setNames(stock_info$symbol, paste0(stock_info$name, " (", stock_info$symbol, ")")), selected = "2330")),
              column(3, selectInput("bootstrap_window", "抽樣使用期間：", choices = c("近 1 年" = 252, "近 3 年" = 756), selected = 252)),
              column(3, selectInput("bootstrap_block", "每個區塊多長：", choices = c("5 個交易日" = 5, "10 個交易日" = 10, "20 個交易日" = 20), selected = 10)),
              column(3, selectInput("bootstrap_reps", "重新抽幾次：", choices = c("50 次｜最快" = 50, "100 次｜建議" = 100, "200 次｜較完整" = 200), selected = 100))
            ),
            actionButton("run_bootstrap", "開始快速檢查", class = "btn-primary"),
            uiOutput("bootstrap_period_info"),
            div(class = "method-note", "網站版以速度為優先，建議先用 100 次。正式報告若需要更穩定的區間，可在離線或每日更新流程中使用 1000 次以上，不需要讓每位使用者現場等待。"),
            h3("結果摘要"),
            tableOutput("bootstrap_summary_table"),
            h3("相對 0050 的分數差異分布"),
            plotOutput("bootstrap_diff_chart", height = "500px"),
            div(class = "plot-explanation", strong("怎麼看？ "), "0 代表和 0050 差不多。大部分結果都在 0 的右邊，代表這檔股票在重新抽樣後通常仍高於 0050；如果分布橫跨 0，代表目前看到的優勢還不夠穩。")
          ),
          tabPanel(
            "過去有沒有用｜歷史回測",
            br(),
            div(
              class = "reading-guide",
              strong("這裡到底在驗證什麼？ "),
              "假裝回到過去的某一天，只使用那一天以前能看到的資料，依照『現在網站同一套評分規則』重新計分與排名；接著往後觀察指定期間，看高分組是否真的比低分組更好。"
            ),
            fluidRow(
              column(3, selectInput(
                "backtest_horizon",
                "評分後往前看多久：",
                choices = BACKTEST_HORIZONS,
                selected = 252
              )),
              column(3, selectInput(
                "backtest_weight_profile",
                "要驗證哪組權重：",
                choices = setNames(names(WEIGHT_PROFILE_LABELS), WEIGHT_PROFILE_LABELS),
                selected = "baseline"
              )),
              column(3, numericInput("backtest_start_year", "最早從哪一年開始驗證：", value=2019, min=2019, max=max(2019, as.integer(format(data_max_date, "%Y"))), step=1)),
              column(3, br(), actionButton("run_backtest", "執行評分模型驗證", class="btn-primary"))
            ),
            div(
              class="time-scope-bar",
              strong("評分時使用的資料： "),
              "與目前主模型一致──報酬／穩定性同時看近 1週、1月、3月、6月、1年、3年；風險控制與夏普主要使用當時往回近 1 年。最長只看到當時以前 3 年，不會使用未來資料。"
            ),
            div(class = "warning-note", strong("研究限制： "), "目前回測仍以現有成分股名單往回分析，因此較早年份可能有『存活者偏差』。此外，2～5 年的後續驗證因資料長度較長，有效驗證次數會自然變少，結果旁邊會直接顯示樣本數。"),
            uiOutput("backtest_status"),
            h3("每次驗證的後續報酬"),
            plotOutput("backtest_cumulative_chart", height="520px"),
            h3("Top／Bottom 與 0050 比較"),
            tableOutput("backtest_summary_table"),
            h3("分數排名與未來表現是否同方向？"),
            tableOutput("backtest_rank_corr_table"),
            h3("各次驗證結果"),
            tableOutput("backtest_yearly_table"),
            div(class="plot-explanation", strong("怎麼看？ "), "如果模型具有區辨能力，理想上 Top 組的後續報酬或風險調整後表現應較常優於 Bottom 組；Spearman 等級相關若長期偏正，也代表分數越高的股票，之後越傾向排在前面。")
          )
        )
      )
    ),
    
    
    # ========================================================
    # 11. 研究方法與資料說明
    # ========================================================
    
    tabPanel(
      "研究方法與說明",
      div(
        class = "section-box",
        h2("研究方法、指標與資料說明"),
        p(class="page-description", "如果看到某個名詞不知道在說什麼，可以回到這一頁。每個重要指標都會先用白話說明『它是什麼、怎麼看、要注意什麼』，需要時再看計算方式。"),
        div(class="reading-guide", strong("核心原則： "), "價格用來觀察走勢；日對數報酬率用來進行主要統計分析。網站結果為統計研究與歷史資料分析，不構成投資建議。"),
        tags$details(class="method-details", open=NA,
          tags$summary("全站時間規則：每個結果到底用了多久資料？"),
          p("統一時間尺度為：1週約5個交易日、1個月21日、3個月63日、6個月126日、1年252日、3年756日。"),
          p("主排名：風險控制與夏普主要使用近1年；報酬與穩定性同時比較1週、1月、3月、6月、1年、3年，避免只看太久以前的資料。"),
          p("PCA／分群：使用者可選3月、6月、1年、3年；同一次分析的所有特徵都使用相同起訖期間，預設近1年。"),
          p("區塊拔靴法：明確顯示抽樣使用的近1年或近3年。"),
          p("評分模型驗證：『評分時』只使用當時以前最多3年資料；『評分後』再依你選的1週～5年觀察未來，兩個時間完全分開。")
        ),
        tags$details(class="method-details", open=NA,
          tags$summary("日對數報酬率：是什麼、怎麼算、怎麼看？"),
          p("定義：相鄰兩個交易日價格變化的對數形式。"),
          p("計算：ln(今日調整後價格 / 昨日調整後價格)。"),
          p("解讀：正值代表上漲、負值代表下跌；本研究的波動、相關性、條件風險值、主成分與拔靴法主要都從這個報酬序列出發。")
        ),
        tags$details(class="method-details",
          tags$summary("風險指標：平常會晃多大？最慘曾跌多少？"),
          p("年化波動度：可以把它想成『平常價格晃動有多大』。數值越高，通常代表漲跌幅度比較大。"),
          p("最大回撤：過去曾經從一個高點一路跌到低點時，最嚴重跌了多少。例如 -30% 代表曾經從高點最多跌約 30%。"),
          p("95% 條件風險值：專門看最差的一小段行情。如果只挑最差約 5% 的日子，平均大概會損失多少，用來補充一般波動度看不到的極端風險。")
        ),
        tags$details(class="method-details",
          tags$summary("我們的綜合評分到底在挑什麼？"),
          p("我們不是單純推薦『過去漲最多』的股票，而是偏好長期表現比較一致、報酬合理、風險不要過度偏高，而且承擔風險後仍能換到不錯報酬的股票。"),
          p("穩定性分數：同時比較 1 週、1 個月、3 個月、6 個月、1 年與 3 年。如果報酬、排名和風險在不同期間都相對一致，分數就會比較高。它代表『表現是否穩定』，不是『報酬是否最高』。"),
          p("風險控制分數：把波動度、最大回撤與極端損失一起看。分數越高，代表這些風險相對比較可控。"),
          p("風險報酬效率：以夏普比率為主。簡單說，就是承擔同樣風險時，誰換到的報酬比較多。"),
          p("綜合分數：目前權重為穩定性 40%、報酬 25%、風險控制 20%、風險報酬效率 15%。0050 也會得到自己的分數，但只當比較基準，不參與成分股名次。")
        ),
        tags$details(class="method-details",
          tags$summary("區塊拔靴法：這個優勢會不會只是剛好？"),
          p("做法：把歷史日對數報酬一小段一小段重新抽很多次，每次重新計算成分股與 0050 的分數差。"),
          p("怎麼看：如果重新抽很多次，大部分結果仍然高於 0050，就表示這個優勢比較不容易因資料稍微改變就消失；如果結果常常跨過 0，就代表還有不小的不確定性。")
        ),
        tags$details(class="method-details",
          tags$summary("歷史回測：如果以前真的照這個排名看，後來呢？"),
          p("做法：假裝回到過去不同時間點，只使用當時以前的資料重新評分；再依選定的1週、1月、3月、6月、1年、2年、3年或5年觀察後續結果。"),
          p("怎麼看：除了 Top／Bottom 與 0050 的報酬、波動、回撤、夏普與勝率，也計算 Spearman 等級相關，直接檢查『分數越高，未來是否越傾向表現較好』。"),
          p("權重敏感度：可切換目前版40/25/20/15、均衡版35/30/20/15、報酬稍強版30/35/20/15，用同一套歷史驗證比較，而不是憑感覺挑權重。")
        ),
        tags$details(class="method-details",
          tags$summary("主成分分析與分群：把複雜指標整理成比較好懂的類型"),
          p("主成分分析：每檔股票在同一選定期間內計算年化報酬、波動、最大回撤、CVaR、夏普、上漲日比例，以及21日報酬／波動變動程度；標準化後再做PCA，避免尺度不同。"),
          p("分群：使用能解釋約80%資訊的前幾個主成分進行K-means，再用輪廓係數比較群數。頁面會直接列出每一檔股票屬於哪一群，以及每群的平均特徵。"),
          p("時間：可選近3月、6月、1年或3年，預設近1年；不使用1週／1月做正式PCA，因為風險指標樣本太少。")
        ),
        h3("目前資料清單與有效期間"),
        tableOutput("stock_table"),
        h3("研究限制"),
        div(class="warning-note", "0050 成分股會隨時間調整；若以目前成分股回溯過去，可能產生存活者偏差。此外，所有統計結果都會受到研究期間、資料品質與市場狀態影響。")
      )
    )
  ),
  
  
  div(
    class = "dashboard-footer",
    "0050 投資研究 Dashboard｜統計研究用途・非投資建議"
  )
)


# ============================================================
# 第 1 段結束
# ============================================================

# ============================================================
# 第 2 段：Server
# ============================================================

server <- function(input, output, session) {
  
  # ==========================================================
  # 1. 自動讀取資料
  # ==========================================================
  #
  # 每 60 秒檢查一次 stock_data.rds。
  # 如果 update_data.R 更新了 RDS，
  # Dashboard 會重新讀取資料。
  #
  # 不需要重新啟動網站。
  # ==========================================================
  
  raw_data <- reactiveFileReader(
    intervalMillis = 60000,
    session = session,
    filePath = DATA_FILE,
    readFunc = readRDS
  )
  
  data <- reactive({
    prepare_stock_data(
      raw_data()
    )
  })
  
  trading_dates <- reactive({
    sort(
      unique(
        data()$date
      )
    )
  })
  
  latest_date <- reactive({
    max(
      data()$date,
      na.rm = TRUE
    )
  })
  
  earliest_date <- reactive({
    min(
      data()$date,
      na.rm = TRUE
    )
  })
  
  
  # ==========================================================
  # 1-1. 融合版研究指標與綜合評分
  # ==========================================================
  
  research_metrics <- reactive({

    x <- data() %>%
      filter(
        symbol %in% dashboard_symbols,
        is.finite(log_return),
        is.finite(adjusted)
      ) %>%
      arrange(symbol, date)

    req(nrow(x) > 0)

    # --------------------------------------------------------
    # 近 1 年：風險、回撤、CVaR、Sharpe 的主要比較視窗
    # --------------------------------------------------------
    risk_end_date <- max(x$date, na.rm = TRUE)
    risk_start_date <- window_start_from_dates(
      sort(unique(x$date)),
      PRIMARY_RISK_WINDOW,
      risk_end_date
    )

    base <- x %>%
      filter(date >= risk_start_date, date <= risk_end_date) %>%
      group_by(symbol) %>%
      summarise(
        n_obs = n(),
        analysis_start = min(date, na.rm = TRUE),
        analysis_end = max(date, na.rm = TRUE),
        annual_return = ifelse(
          n_obs >= floor(PRIMARY_RISK_WINDOW * 0.75),
          exp(mean(log_return, na.rm = TRUE) * TRADING_DAYS_PER_YEAR) - 1,
          NA_real_
        ),
        annual_volatility = ifelse(
          n_obs >= floor(PRIMARY_RISK_WINDOW * 0.75),
          sd(log_return, na.rm = TRUE) * sqrt(TRADING_DAYS_PER_YEAR),
          NA_real_
        ),
        max_drawdown = ifelse(
          n_obs >= floor(PRIMARY_RISK_WINDOW * 0.75),
          min(drawdown_value(adjusted), na.rm = TRUE),
          NA_real_
        ),
        cvar_95 = ifelse(n_obs >= floor(PRIMARY_RISK_WINDOW * 0.75), cvar_value(log_return, 0.95), NA_real_),
        mean_daily_log = ifelse(n_obs >= floor(PRIMARY_RISK_WINDOW * 0.75), mean(log_return, na.rm = TRUE), NA_real_),
        .groups = "drop"
      ) %>%
      mutate(
        sharpe = ifelse(
          is.finite(annual_volatility) & annual_volatility > 0,
          mean_daily_log * TRADING_DAYS_PER_YEAR / annual_volatility,
          NA_real_
        )
      )

    # --------------------------------------------------------
    # 統一六個滾動時間尺度：1週 / 1月 / 3月 / 6月 / 1年 / 3年
    # --------------------------------------------------------
    period_days <- TIME_WINDOWS
    period_weights <- RETURN_WINDOW_WEIGHTS

    period_list <- lapply(
      names(period_days),
      function(period_name) {
        n_day <- period_days[[period_name]]
        period_end_date <- max(x$date, na.rm = TRUE)
        period_start_date <- window_start_from_dates(
          sort(unique(x$date)),
          n_day,
          period_end_date
        )
        x %>%
          filter(date >= period_start_date, date <= period_end_date) %>%
          group_by(symbol) %>%
          arrange(date, .by_group = TRUE) %>%
          summarise(
            period = period_name,
            n = n(),
            period_start = min(date, na.rm = TRUE),
            period_end = max(date, na.rm = TRUE),
            period_log_return = ifelse(
              n >= max(3, floor(n_day * 0.75)),
              sum(log_return, na.rm = TRUE),
              NA_real_
            ),
            annualized_log_mean = ifelse(
              n >= max(3, floor(n_day * 0.75)),
              mean(log_return, na.rm = TRUE) * TRADING_DAYS_PER_YEAR,
              NA_real_
            ),
            annualized_volatility = ifelse(
              n >= max(20, floor(n_day * 0.75)),
              sd(log_return, na.rm = TRUE) * sqrt(TRADING_DAYS_PER_YEAR),
              NA_real_
            ),
            .groups = "drop"
          )
      }
    )

    period_long <- bind_rows(period_list) %>%
      group_by(period) %>%
      mutate(return_percentile = cross_section_score(period_log_return, TRUE)) %>%
      ungroup()

    # --------------------------------------------------------
    # 跨期間穩定性
    # 排名 / 報酬：六個尺度；風險：從 3 個月起算，避免短樣本失真。
    # --------------------------------------------------------
    stability_rank_return <- period_long %>%
      group_by(symbol) %>%
      summarise(
        rank_variation = sd(return_percentile, na.rm = TRUE),
        return_variation = sd(annualized_log_mean, na.rm = TRUE),
        valid_periods = sum(is.finite(period_log_return)),
        .groups = "drop"
      )

    stability_risk <- period_long %>%
      filter(period %in% c("3個月", "6個月", "1年", "3年")) %>%
      group_by(symbol) %>%
      summarise(
        risk_variation = sd(annualized_volatility, na.rm = TRUE),
        valid_risk_periods = sum(is.finite(annualized_volatility)),
        .groups = "drop"
      )

    stability_raw <- stability_rank_return %>%
      left_join(stability_risk, by = "symbol") %>%
      mutate(
        rank_variation = ifelse(valid_periods >= 3, rank_variation, NA_real_),
        return_variation = ifelse(valid_periods >= 3, return_variation, NA_real_),
        risk_variation = ifelse(valid_risk_periods >= 2, risk_variation, NA_real_),
        rank_stability_score = cross_section_score(rank_variation, FALSE),
        return_stability_score = cross_section_score(return_variation, FALSE),
        risk_stability_score = cross_section_score(risk_variation, FALSE),
        stability_score =
          0.30 * rank_stability_score +
          0.40 * return_stability_score +
          0.30 * risk_stability_score
      )

    # --------------------------------------------------------
    # 多時間尺度報酬分數
    # --------------------------------------------------------
    multi_return <- period_long %>%
      mutate(weight = period_weights[period]) %>%
      group_by(symbol) %>%
      summarise(
        multi_period_return_score = ifelse(
          sum(is.finite(return_percentile)) >= 3,
          weighted.mean(return_percentile, weight, na.rm = TRUE),
          NA_real_
        ),
        .groups = "drop"
      )

    # 各期間轉寬，供近期轉弱標籤使用。
    period_wide <- period_long %>%
      select(symbol, period, period_log_return, return_percentile) %>%
      pivot_wider(
        names_from = period,
        values_from = c(period_log_return, return_percentile),
        names_sep = "_"
      )

    # --------------------------------------------------------
    # 四大面向分數
    # --------------------------------------------------------
    score <- base %>%
      mutate(
        annual_return_score = cross_section_score(annual_return, TRUE),
        volatility_score = cross_section_score(annual_volatility, FALSE),
        drawdown_score = cross_section_score(max_drawdown, TRUE),
        cvar_score = cross_section_score(cvar_95, TRUE),
        sharpe_score = cross_section_score(sharpe, TRUE)
      ) %>%
      left_join(stability_raw, by = "symbol") %>%
      left_join(multi_return, by = "symbol") %>%
      left_join(period_wide, by = "symbol") %>%
      mutate(
        return_score = 0.70 * multi_period_return_score + 0.30 * annual_return_score,
        risk_score = 0.35 * volatility_score + 0.35 * drawdown_score + 0.30 * cvar_score,
        composite_score =
          MODEL_WEIGHTS[["stability"]] * stability_score +
          MODEL_WEIGHTS[["ret"]] * return_score +
          MODEL_WEIGHTS[["risk"]] * risk_score +
          MODEL_WEIGHTS[["sharpe"]] * sharpe_score
      )

    # --------------------------------------------------------
    # 0050 比較基準
    # --------------------------------------------------------
    benchmark_score <- score %>%
      filter(symbol == "0050") %>%
      slice_head(n = 1)

    if (nrow(benchmark_score) == 1) {
      score <- score %>%
        mutate(
          score_diff_0050 = composite_score - benchmark_score$composite_score,
          relative_0050 = case_when(
            score_diff_0050 >= 3 ~ "高於 0050",
            score_diff_0050 <= -3 ~ "低於 0050",
            TRUE ~ "接近 0050"
          )
        )
    } else {
      score <- score %>%
        mutate(
          score_diff_0050 = NA_real_,
          relative_0050 = "無法比較"
        )
    }

    # --------------------------------------------------------
    # 固定標籤庫：同時參考 0050 與成分股分布
    # --------------------------------------------------------
    component <- score %>% filter(symbol %in% stock_info$symbol)
    q_return_80 <- quantile(component$annual_return, 0.80, na.rm = TRUE)
    q_stability_80 <- quantile(component$stability_score, 0.80, na.rm = TRUE)
    q_vol_80 <- quantile(component$annual_volatility, 0.80, na.rm = TRUE)
    q_dd_20 <- quantile(component$max_drawdown, 0.20, na.rm = TRUE)
    q_cvar_20 <- quantile(component$cvar_95, 0.20, na.rm = TRUE)

    b_ann_return <- if (nrow(benchmark_score) == 1) benchmark_score$annual_return else NA_real_
    b_stability <- if (nrow(benchmark_score) == 1) benchmark_score$stability_score else NA_real_
    b_vol <- if (nrow(benchmark_score) == 1) benchmark_score$annual_volatility else NA_real_
    b_dd <- if (nrow(benchmark_score) == 1) benchmark_score$max_drawdown else NA_real_
    b_cvar <- if (nrow(benchmark_score) == 1) benchmark_score$cvar_95 else NA_real_

    score$labels <- vapply(
      seq_len(nrow(score)),
      function(i) {
        row <- score[i, ]
        lab <- character(0)
        if (is.finite(row$annual_return) && row$annual_return >= q_return_80 && (!is.finite(b_ann_return) || row$annual_return > b_ann_return)) lab <- c(lab, "高報酬")
        if (is.finite(row$stability_score) && row$stability_score >= q_stability_80 && (!is.finite(b_stability) || row$stability_score > b_stability)) lab <- c(lab, "穩定性佳")
        if (is.finite(row$annual_volatility) && row$annual_volatility >= q_vol_80 && (!is.finite(b_vol) || row$annual_volatility > b_vol)) lab <- c(lab, "高波動")
        if (is.finite(row$max_drawdown) && row$max_drawdown <= q_dd_20 && (!is.finite(b_dd) || row$max_drawdown < b_dd)) lab <- c(lab, "深度回撤")
        if (is.finite(row$cvar_95) && row$cvar_95 <= q_cvar_20 && (!is.finite(b_cvar) || row$cvar_95 < b_cvar)) lab <- c(lab, "尾端風險偏高")
        if (
          "return_percentile_1週" %in% names(row) &&
          "return_percentile_1年" %in% names(row) &&
          is.finite(row$return_percentile_1週) &&
          is.finite(row$return_percentile_1年) &&
          row$return_percentile_1週 + 30 < row$return_percentile_1年
        ) lab <- c(lab, "近期轉弱")
        if (length(lab) == 0) "一般" else paste(lab, collapse = "｜")
      },
      character(1)
    )

    score %>%
      mutate(
        name = get_stock_name(symbol),
        industry = get_stock_industry(symbol)
      )
  })

  output$model_time_scope_ui <- renderUI({
    d <- sort(unique(data()$date))
    end_d <- max(d, na.rm = TRUE)
    one_year_start <- window_start_from_dates(d, PRIMARY_RISK_WINDOW, end_d)
    three_year_start <- window_start_from_dates(d, TIME_WINDOWS[["3年"]], end_d)
    div(
      class = "time-scope-bar",
      strong("目前評分使用的時間： "),
      paste0(
        "風險控制與夏普主要使用近 1 年（", format(one_year_start, "%Y-%m-%d"), " ～ ", format(end_d, "%Y-%m-%d"), "）；",
        "報酬與穩定性同時比較 1週、1月、3月、6月、1年、3年。最長背景從 ",
        format(three_year_start, "%Y-%m-%d"), " 起，不使用 2016 至今全部歷史。"
      )
    )
  })

  output$model_time_scope_ui_risk <- renderUI({
    d <- sort(unique(data()$date))
    end_d <- max(d, na.rm = TRUE)
    one_year_start <- window_start_from_dates(d, PRIMARY_RISK_WINDOW, end_d)
    div(
      class = "time-scope-bar",
      strong("風險指標期間： "),
      paste0(
        "表格中的年化波動、最大回撤、CVaR、夏普主要取近 1 年：",
        format(one_year_start, "%Y-%m-%d"), " ～ ", format(end_d, "%Y-%m-%d"),
        "。穩定性則跨 1週～3年比較。"
      )
    )
  })

  # ==========================================================
  # 1-2. 主成分與分群
  # ==========================================================
  
  pca_feature_data <- reactive({
    n_day <- as.integer(if (is.null(input$pca_period)) 252 else input$pca_period)
    d <- sort(unique(data()$date))
    end_date <- max(d, na.rm = TRUE)
    start_date <- window_start_from_dates(d, n_day, end_date)

    x <- data() %>%
      filter(
        symbol %in% stock_info$symbol,
        date >= start_date,
        date <= end_date,
        is.finite(log_return),
        is.finite(adjusted)
      ) %>%
      arrange(symbol, date)

    out <- x %>%
      group_by(symbol) %>%
      group_modify(~{
        r <- .x$log_return
        px <- .x$adjusted
        n_obs <- sum(is.finite(r))
        required_obs <- max(20, floor(n_day * 0.80))
        ann_vol <- if (n_obs >= required_obs) sd(r, na.rm=TRUE) * sqrt(TRADING_DAYS_PER_YEAR) else NA_real_
        roll21_ret <- as.numeric(stats::filter(r, rep(1, 21), sides = 1))
        roll21_vol <- rolling_sd_fast(r, 21) * sqrt(TRADING_DAYS_PER_YEAR)
        data.frame(
          n_obs = n_obs,
          period_start = min(.x$date, na.rm=TRUE),
          period_end = max(.x$date, na.rm=TRUE),
          coverage_ratio = n_obs / n_day,
          annual_return = ifelse(n_obs >= required_obs, exp(mean(r, na.rm=TRUE) * TRADING_DAYS_PER_YEAR) - 1, NA_real_),
          annual_volatility = ann_vol,
          max_drawdown = ifelse(n_obs >= required_obs, min(drawdown_value(px), na.rm=TRUE), NA_real_),
          cvar_95 = ifelse(n_obs >= required_obs, cvar_value(r, 0.95), NA_real_),
          sharpe = ifelse(is.finite(ann_vol) && ann_vol > 0,
                          mean(r, na.rm=TRUE) * TRADING_DAYS_PER_YEAR / ann_vol, NA_real_),
          positive_day_ratio = ifelse(n_obs >= required_obs, mean(r > 0, na.rm=TRUE), NA_real_),
          return_variability_21d = ifelse(sum(is.finite(roll21_ret)) >= 10, sd(roll21_ret, na.rm=TRUE), NA_real_),
          volatility_variability_21d = ifelse(sum(is.finite(roll21_vol)) >= 10, sd(roll21_vol, na.rm=TRUE), NA_real_)
        )
      }) %>%
      ungroup() %>%
      mutate(
        name = get_stock_name(symbol),
        industry = get_stock_industry(symbol)
      )

    # 加入主排名結果只是顯示參考，不放進 PCA / K-means。
    out %>%
      left_join(
        research_metrics() %>%
          select(symbol, composite_score, stability_score, return_score, risk_score),
        by = "symbol"
      )
  })

  pca_cluster_result <- reactive({
    m <- pca_feature_data()

    feature_names <- c(
      "annual_return",
      "annual_volatility",
      "max_drawdown",
      "cvar_95",
      "sharpe",
      "positive_day_ratio",
      "return_variability_21d",
      "volatility_variability_21d"
    )

    ok <- complete.cases(m[, feature_names])
    m2 <- m[ok, , drop = FALSE]
    req(nrow(m2) >= 8)

    # 移除極少數可能出現的零變異欄位，避免 prcomp(scale.=TRUE) 失敗。
    feature_sd <- vapply(m2[, feature_names, drop=FALSE], sd, numeric(1), na.rm=TRUE)
    use_features <- feature_names[is.finite(feature_sd) & feature_sd > 1e-12]
    req(length(use_features) >= 3)

    pca <- prcomp(
      m2[, use_features, drop=FALSE],
      center = TRUE,
      scale. = TRUE
    )

    # 分群使用能解釋至少 80% 變異的主成分，至少 2 個、最多 5 個。
    variance <- (pca$sdev ^ 2) / sum(pca$sdev ^ 2)
    cum_var <- cumsum(variance)
    pc_count <- which(cum_var >= 0.80)[1]
    if (!is.finite(pc_count)) pc_count <- min(3, ncol(pca$x))
    pc_count <- max(2, min(pc_count, 5, ncol(pca$x)))
    cluster_x <- pca$x[, seq_len(pc_count), drop = FALSE]

    max_k <- min(6, nrow(m2) - 1)
    k_candidates <- 2:max_k
    quality <- lapply(
      k_candidates,
      function(k) {
        set.seed(50 + k)
        km <- kmeans(cluster_x, centers = k, nstart = 30)
        data.frame(
          k = k,
          total_withinss = km$tot.withinss,
          silhouette = average_silhouette(cluster_x, km$cluster),
          stringsAsFactors = FALSE
        )
      }
    ) %>% bind_rows()

    best_k <- quality$k[which.max(quality$silhouette)]
    set.seed(50)
    km <- kmeans(cluster_x, centers = best_k, nstart = 50)

    m2$cluster <- km$cluster
    m2$PC1 <- pca$x[, 1]
    m2$PC2 <- if (ncol(pca$x) >= 2) pca$x[, 2] else 0

    # 依群組「實際平均統計特徵」命名，不預先指定第幾群是哪種類型。
    global_medians <- list(
      ret = median(m2$annual_return, na.rm=TRUE),
      vol = median(m2$annual_volatility, na.rm=TRUE),
      dd = median(m2$max_drawdown, na.rm=TRUE),
      cvar = median(m2$cvar_95, na.rm=TRUE),
      sharpe = median(m2$sharpe, na.rm=TRUE),
      retvar = median(m2$return_variability_21d, na.rm=TRUE),
      volvar = median(m2$volatility_variability_21d, na.rm=TRUE)
    )

    group_character <- m2 %>%
      group_by(cluster) %>%
      summarise(
        annual_return = mean(annual_return, na.rm=TRUE),
        annual_volatility = mean(annual_volatility, na.rm=TRUE),
        max_drawdown = mean(max_drawdown, na.rm=TRUE),
        cvar_95 = mean(cvar_95, na.rm=TRUE),
        sharpe = mean(sharpe, na.rm=TRUE),
        return_variability_21d = mean(return_variability_21d, na.rm=TRUE),
        volatility_variability_21d = mean(volatility_variability_21d, na.rm=TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        type = case_when(
          annual_volatility <= global_medians$vol &
            max_drawdown >= global_medians$dd &
            return_variability_21d <= global_medians$retvar ~ "低波動穩定型",
          annual_return >= global_medians$ret &
            sharpe >= global_medians$sharpe &
            annual_volatility <= global_medians$vol ~ "報酬效率型",
          annual_return >= global_medians$ret &
            annual_volatility > global_medians$vol ~ "高報酬高風險型",
          max_drawdown < global_medians$dd & cvar_95 < global_medians$cvar ~ "深度回撤型",
          TRUE ~ "均衡型"
        ),
        cluster_label = paste0("第", cluster, "群｜", type)
      )

    m2 <- m2 %>%
      left_join(group_character %>% select(cluster, cluster_label), by = "cluster")

    list(
      data = m2,
      pca = pca,
      quality = quality,
      best_k = best_k,
      pc_count = pc_count,
      use_features = use_features,
      group_character = group_character,
      period_days = as.integer(if (is.null(input$pca_period)) 252 else input$pca_period)
    )
  })

  pca_feature_labels <- c(
    annual_return = "期間年化報酬",
    annual_volatility = "年化波動度",
    max_drawdown = "最大回撤",
    cvar_95 = "95% CVaR",
    sharpe = "夏普比率",
    positive_day_ratio = "上漲日比例",
    return_variability_21d = "21日報酬變動程度",
    volatility_variability_21d = "21日波動變動程度"
  )

  output$pca_period_info <- renderUI({
    n_day <- as.integer(if (is.null(input$pca_period)) 252 else input$pca_period)
    d <- sort(unique(data()$date))
    end_date <- max(d, na.rm=TRUE)
    start_date <- window_start_from_dates(d, n_day, end_date)
    div(
      class="time-scope-bar",
      strong("目前 PCA／分群期間： "),
      paste0(window_label(n_day), "｜", format(start_date, "%Y-%m-%d"), " ～ ", format(end_date, "%Y-%m-%d"), "｜約 ", n_day, " 個交易日｜股票至少需有 80% 有效觀測")
    )
  })

  output$pca_interpretation_ui <- renderUI({
    r <- pca_cluster_result()
    rot <- r$pca$rotation
    pcs <- seq_len(min(2, ncol(rot)))
    blocks <- lapply(pcs, function(j) {
      vals <- abs(rot[, j])
      top <- names(sort(vals, decreasing=TRUE))[seq_len(min(3, length(vals)))]
      labels <- unname(pca_feature_labels[top])
      div(
        class="reading-guide",
        strong(paste0("PC", j, " 主要由：")),
        paste(labels, collapse="、"),
        "。負荷量正負號本身可以整體翻轉，因此這裡重點看『哪些指標影響最大』，不把正負號直接解讀成好或壞。"
      )
    })
    tagList(blocks)
  })

  
  # ==========================================================
  # 2. 共用日期工具
  # ==========================================================
  
  selected_market_date <- reactive({
    
    req(
      input$market_observation_date
    )
    
    nearest_date(
      trading_dates(),
      input$market_observation_date
    )
  })
  
  selected_monitor_date <- reactive({
    
    req(
      input$monitor_date
    )
    
    nearest_date(
      trading_dates(),
      input$monitor_date
    )
  })
  
  
  output$header_latest_date <- renderText({
    format(latest_date(), "%Y-%m-%d")
  })
  
  # ==========================================================
  # 3. 市場總覽
  # ==========================================================
  
  output$global_latest_date <- renderText({
    format(
      latest_date(),
      "%Y-%m-%d"
    )
  })
  
  output$global_earliest_date <- renderText({
    format(
      earliest_date(),
      "%Y-%m-%d"
    )
  })
  
  output$global_stock_count <- renderText({
    n_distinct(
      data()$symbol
    )
  })
  
  output$global_observation_count <- renderText({
    fmt_num(
      nrow(data())
    )
  })
  
  output$global_file_modified <- renderText({
    
    info <- file.info(
      DATA_FILE
    )
    
    if (
      nrow(info) == 0 ||
      is.na(info$mtime)
    ) {
      
      return("無法取得")
      
    }
    
    format(
      info$mtime,
      "%Y-%m-%d %H:%M:%S"
    )
  })
  
  
  # ----------------------------------------------------------
  # 市場觀察日期自動同步
  # ----------------------------------------------------------
  
  observe({
    
    req(
      latest_date()
    )
    
    updateDateInput(
      session,
      "market_observation_date",
      value = latest_date(),
      min = earliest_date(),
      max = latest_date()
    )
    
  })
  
  
  observe({
    
    req(
      latest_date()
    )
    
    updateDateInput(
      session,
      "monitor_date",
      value = latest_date(),
      min = earliest_date(),
      max = latest_date()
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 0050 最新價格
  # ----------------------------------------------------------
  
  output$market_0050_price <- renderText({
    
    d <- selected_market_date()
    
    x <- data() %>%
      filter(
        symbol == "0050",
        date <= d,
        is.finite(adjusted)
      ) %>%
      arrange(
        desc(date)
      ) %>%
      slice_head(
        n = 1
      )
    
    req(
      nrow(x) > 0
    )
    
    fmt_price(
      x$adjusted[1]
    )
  })
  
  
  # ----------------------------------------------------------
  # 0050 最近一日普通報酬
  # ----------------------------------------------------------
  
  output$market_0050_daily_return <- renderText({
    
    d <- selected_market_date()
    
    x <- data() %>%
      filter(
        symbol == "0050",
        date <= d,
        is.finite(log_return)
      ) %>%
      arrange(
        desc(date)
      ) %>%
      slice_head(
        n = 1
      )
    
    req(
      nrow(x) > 0
    )
    
    fmt_pct(
      x$log_return[1]
    )
  })
  
  
  # ----------------------------------------------------------
  # 0050 YTD
  # ----------------------------------------------------------
  
  output$market_ytd_return <- renderText({
    
    d <- selected_market_date()
    
    year_now <-
      format(
        d,
        "%Y"
      )
    
    x <- data() %>%
      filter(
        symbol == "0050",
        date <= d,
        format(date, "%Y") == year_now,
        is.finite(adjusted)
      ) %>%
      arrange(date)
    
    req(
      nrow(x) >= 2
    )
    
    ytd <-
      tail(x$adjusted, 1) /
      head(x$adjusted, 1) - 1
    
    fmt_pct(
      ytd
    )
  })
  
  
  # ----------------------------------------------------------
  # 50 檔成分股漲跌家數
  # ----------------------------------------------------------
  
  output$market_breadth <- renderText({
    
    d <- selected_market_date()
    
    x <- data() %>%
      filter(
        symbol %in% stock_info$symbol,
        date <= d,
        is.finite(daily_return)
      ) %>%
      group_by(symbol) %>%
      arrange(
        desc(date)
      ) %>%
      slice_head(n = 1) %>%
      ungroup()
    
    up <- sum(
      x$daily_return > 0,
      na.rm = TRUE
    )
    
    down <- sum(
      x$daily_return < 0,
      na.rm = TRUE
    )
    
    flat <- sum(
      x$daily_return == 0,
      na.rm = TRUE
    )
    
    paste0(
      up,
      " ↑ / ",
      down,
      " ↓ / ",
      flat,
      " –"
    )
  })
  
  
  # ----------------------------------------------------------
  # 0050 歷史價格
  # ----------------------------------------------------------
  
  output$market_0050_price_chart <- renderPlot({
    
    req(
      input$market_chart_date_range
    )
    
    x <- data() %>%
      filter(
        symbol == "0050",
        date >= as.Date(
          input$market_chart_date_range[1]
        ),
        date <= as.Date(
          input$market_chart_date_range[2]
        ),
        is.finite(adjusted)
      ) %>%
      arrange(date)
    
    req(
      nrow(x) > 1
    )
    
    # ----------------------------------------------------------
    # 未勾選：
    # 整條線維持黑色
    # ----------------------------------------------------------
    
    if (
      !isTRUE(
        input$market_price_color_by_change
      )
    ) {
      
      return(
        
        ggplot(
          x,
          aes(
            x = date,
            y = adjusted
          )
        ) +
          
          geom_line(
            color = stock_colors["0050"],
            linewidth = 1.0
          ) +
          
          labs(
            title = "0050 調整後收盤價",
            x = "日期",
            y = "調整後價格"
          ) +
          
          theme_minimal() +
          
          theme(
            plot.title =
              element_text(
                hjust = 0.5,
                face = "bold"
              ),
            
            axis.text.x =
              element_text(
                angle = 45,
                hjust = 1
              )
          )
        
      )
    }
    
    
    # ----------------------------------------------------------
    # 勾選：
    # 依當日漲跌把線段染色
    #
    # 紅 = 上漲
    # 綠 = 下跌
    # 灰 = 持平
    # ----------------------------------------------------------
    
    x <- x %>%
      mutate(
        next_date = lead(date),
        next_price = lead(adjusted),
        
        direction = case_when(
          lead(adjusted) > adjusted ~ "上漲",
          lead(adjusted) < adjusted ~ "下跌",
          TRUE ~ "持平"
        )
      ) %>%
      filter(
        is.finite(next_price)
      )
    
    
    ggplot() +
      
      geom_segment(
        data = x,
        aes(
          x = date,
          y = adjusted,
          xend = next_date,
          yend = next_price,
          color = direction
        ),
        linewidth = 0.85
      ) +
      
      scale_color_manual(
        values = c(
          "上漲" = UP_COLOR,
          "下跌" = DOWN_COLOR,
          "持平" = NEUTRAL_COLOR
        )
      ) +
      
      labs(
        title = "0050 調整後收盤價",
        x = "日期",
        y = "調整後價格",
        color = NULL
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          ),
        
        legend.position =
          "bottom",
        
        axis.text.x =
          element_text(
            angle = 45,
            hjust = 1
          )
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 0050 近一年每日對數報酬
  # ----------------------------------------------------------
  
  output$market_0050_daily_return_timeseries <- renderPlot({
    
    d <- selected_market_date()
    
    dates <- trading_dates()
    
    end_index <- max(
      which(
        dates <= d
      )
    )
    
    start_index <- max(
      1,
      end_index - 251
    )
    
    x <- data() %>%
      filter(
        symbol == "0050",
        date >= dates[start_index],
        date <= d,
        is.finite(log_return)
      ) %>%
      arrange(date)
    
    req(
      nrow(x) > 1
    )
    
    # --------------------------------------------------------
    # 不啟用著色：黑色
    # --------------------------------------------------------
    
    if (
      !isTRUE(
        input$market_return_color_by_change
      )
    ) {
      
      return(
        
        ggplot(
          x,
          aes(
            x = date,
            y = log_return
          )
        ) +
          
          geom_hline(
            yintercept = 0,
            linetype = "dashed"
          ) +
          
          geom_line(
            color = stock_colors["0050"],
            linewidth = 0.8
          ) +
          
          labs(
            title = "0050 近一年每日對數報酬率",
            x = "日期",
            y = "日對數報酬率"
          ) +
          
          scale_y_continuous(
            labels = function(z) {
              paste0(
                round(
                  z * 100,
                  2
                ),
                "%"
              )
            }
          ) +
          
          theme_minimal() +
          
          theme(
            plot.title =
              element_text(
                hjust = 0.5,
                face = "bold"
              )
          )
        
      )
    }
    
    # --------------------------------------------------------
    # 啟用著色
    # --------------------------------------------------------
    
    x <- x %>%
      mutate(
        next_date =
          lead(date),
        
        next_return =
          lead(log_return),
        
        direction =
          case_when(
            next_return > log_return ~ "上升",
            next_return < log_return ~ "下降",
            TRUE ~ "持平"
          )
      ) %>%
      filter(
        is.finite(next_return)
      )
    
    ggplot(
      x
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_segment(
        aes(
          x = date,
          y = log_return,
          xend = next_date,
          yend = next_return,
          color = direction
        ),
        linewidth = 0.8
      ) +
      
      scale_color_manual(
        values = c(
          "上升" = UP_COLOR,
          "下降" = DOWN_COLOR,
          "持平" = NEUTRAL_COLOR
        )
      ) +
      
      labs(
        title = "0050 近一年每日對數報酬率",
        x = "日期",
        y = "日對數報酬率",
        color = NULL
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              2
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          ),
        
        legend.position =
          "bottom"
      )
    
  })
  
  # ----------------------------------------------------------
  # 近 30 個交易日排名
  # ----------------------------------------------------------
  
  output$market_30d_return_ranking <- renderPlot({
    
    d <- selected_market_date()
    
    x <- data() %>%
      filter(
        symbol %in% stock_info$symbol,
        date <= d,
        is.finite(log_return)
      ) %>%
      group_by(symbol) %>%
      arrange(date, .by_group = TRUE) %>%
      slice_tail(n = 30) %>%
      summarise(
        n = n(),
        return_30d =
          ifelse(
            n >= 20,
            sum(log_return, na.rm = TRUE),
            NA_real_
          ),
        .groups = "drop"
      ) %>%
      filter(
        is.finite(return_30d)
      )
    
    req(
      nrow(x) > 0
    )
    
    x <- x %>%
      arrange(return_30d) %>%
      mutate(
        label = paste0(
          symbol,
          " ",
          get_stock_name(symbol)
        )
      )
    
    x <- x %>%
      mutate(
        direction = case_when(
          return_30d > 0 ~ "上漲",
          return_30d < 0 ~ "下跌",
          TRUE ~ "持平"
        )
      )
    
    ggplot(
      x,
      aes(
        x = return_30d,
        y = reorder(label, return_30d),
        fill = direction
      )
    ) +
      
      geom_col(
        width = 0.7
      ) +
      
      scale_fill_manual(
        values = c(
          "上漲" = UP_COLOR,
          "下跌" = DOWN_COLOR,
          "持平" = NEUTRAL_COLOR
        )
      )  +
      geom_vline(
        xintercept = 0,
        linetype = "dashed"
      ) +
      labs(
        title = "50 檔成分股近 30 個交易日累積對數報酬",
        x = "累積對數報酬",
        y = NULL
      ) +
      scale_x_continuous(
        labels = function(z) {
          paste0(
            round(z * 100, 1),
            "%"
          )
        }
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold"
        )
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 最新行情表
  # ----------------------------------------------------------
  
  latest_market_table <- reactive({
    
    d <- selected_market_date()
    
    symbols <- c(
      "0050",
      stock_info$symbol
    )
    
    result <- lapply(
      symbols,
      function(s) {
        
        x <- data() %>%
          filter(
            symbol == s,
            date <= d
          ) %>%
          arrange(
            desc(date)
          ) %>%
          slice_head(n = 1)
        
        if (nrow(x) == 0) {
          return(NULL)
        }
        
        data.frame(
          股票代號 = s,
          股票名稱 = get_stock_name(s),
          日期 = x$date[1],
          調整後價格 = x$adjusted[1],
          日對數報酬率 = x$log_return[1],
          成交量 = x$volume[1],
          stringsAsFactors = FALSE
        )
      }
    )
    
    bind_rows(result)
    
  })
  
  
  output$realtime_monitor_table <- renderTable({
    
    latest_market_table() %>%
      mutate(
        日期 = format(
          日期,
          "%Y-%m-%d"
        ),
        調整後價格 = fmt_price(
          調整後價格
        ),
        日對數報酬率 = fmt_pct(
          日對數報酬率
        ),
        成交量 = fmt_num(
          成交量
        )
      )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
 
  # ==========================================================
  # 4. 51 檔行情監控
  # ==========================================================
  
  monitor_data <- reactive({
    
    d <- selected_monitor_date()
    
    symbols <- c(
      "0050",
      stock_info$symbol
    )
    
    result <- lapply(
      symbols,
      function(s) {
        
        x <- data() %>%
          filter(
            symbol == s,
            date <= d
          ) %>%
          arrange(
            desc(date)
          ) %>%
          slice_head(
            n = 1
          )
        
        if (
          nrow(x) == 0
        ) {
          return(NULL)
        }
        
        data.frame(
          股票代號 = s,
          股票名稱 = get_stock_name(s),
          日期 = x$date[1],
          收盤價 = x$close[1],
          調整後價格 = x$adjusted[1],
          日對數報酬率 = x$log_return[1],
          成交量 = x$volume[1],
          stringsAsFactors = FALSE
        )
        
      }
    )
    
    bind_rows(
      result
    )
    
  })
  
  
  output$monitor_table <- renderTable({
    
    monitor_data() %>%
      mutate(
        日期 = format(
          日期,
          "%Y-%m-%d"
        ),
        收盤價 = fmt_price(
          收盤價
        ),
        調整後價格 = fmt_price(
          調整後價格
        ),
        日對數報酬率 = fmt_pct(
          日對數報酬率
        ),
        成交量 = fmt_num(
          成交量
        )
      )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ==========================================================
  # 5. 個股分析
  # ==========================================================
  
  analysis_range <- reactive({
    
    req(
      input$date_range
    )
    
    as.Date(
      input$date_range
    )
    
  })
  
  
  analysis_symbols <- reactive({
    
    s <- input$selected_stock
    
    if (
      is.null(s) ||
      length(s) == 0
    ) {
      return("0050")
    }
    
    head(
      unique(s),
      MAX_SELECTED_STOCKS
    )
    
  })
  
  
  analysis_data <- reactive({
    
    r <- analysis_range()
    
    data() %>%
      filter(
        symbol %in% analysis_symbols(),
        date >= r[1],
        date <= r[2]
      )
    
  })
  
  
  output$analysis_period_text <- renderText({
    
    r <- analysis_range()
    
    paste0(
      "目前分析期間：",
      format(r[1], "%Y-%m-%d"),
      " 至 ",
      format(r[2], "%Y-%m-%d")
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 快速期間
  # ----------------------------------------------------------
  
  update_analysis_range <- function(
    start_date,
    end_date
  ) {
    
    start_date <- nearest_date(
      trading_dates(),
      start_date
    )
    
    end_date <- nearest_date(
      trading_dates(),
      end_date
    )
    
    if (
      start_date > end_date
    ) {
      tmp <- start_date
      start_date <- end_date
      end_date <- tmp
    }
    
    updateDateRangeInput(
      session,
      "date_range",
      start = start_date,
      end = end_date
    )
  }
  
  
  observeEvent(
    input$period_research,
    {
      update_analysis_range(
        RESEARCH_START_DATE,
        min(
          RESEARCH_END_DATE,
          latest_date()
        )
      )
    }
  )
  
  
  observeEvent(
    input$period_all,
    {
      update_analysis_range(
        earliest_date(),
        latest_date()
      )
    }
  )
  
  
  observeEvent(
    input$period_1w,
    {
      dates <- trading_dates()
      n <- length(dates)
      if (n >= 5) {
        update_analysis_range(dates[n - 4], dates[n])
      }
    }
  )

  observeEvent(
    input$period_1m,
    {
      dates <- trading_dates()
      n <- length(dates)
      
      if (n >= 21) {
        update_analysis_range(
          dates[n - 20],
          dates[n]
        )
      }
    }
  )
  
  
  observeEvent(
    input$period_1q,
    {
      dates <- trading_dates()
      n <- length(dates)
      
      if (n >= 63) {
        update_analysis_range(
          dates[n - 62],
          dates[n]
        )
      }
    }
  )
  
  
  observeEvent(
    input$period_6m,
    {
      dates <- trading_dates()
      n <- length(dates)
      
      if (n >= 126) {
        update_analysis_range(
          dates[n - 125],
          dates[n]
        )
      }
    }
  )
  
  
  observeEvent(
    input$period_1y,
    {
      dates <- trading_dates()
      n <- length(dates)
      
      if (n >= 252) {
        update_analysis_range(
          dates[n - 251],
          dates[n]
        )
      }
    }
  )
  
  
  observeEvent(
    input$period_3y,
    {
      dates <- trading_dates()
      n <- length(dates)
      if (n >= 756) {
        update_analysis_range(dates[n - 755], dates[n])
      } else if (n >= 2) {
        update_analysis_range(dates[1], dates[n])
      }
    }
  )

  observeEvent(
    input$period_ytd,
    {
      d <- latest_date()
      
      y <- format(
        d,
        "%Y"
      )
      
      y_dates <- trading_dates()[
        format(
          trading_dates(),
          "%Y"
        ) == y &
          trading_dates() <= d
      ]
      
      if (length(y_dates) >= 2) {
        
        update_analysis_range(
          min(y_dates),
          max(y_dates)
        )
      }
    }
  )
  
  
  # ----------------------------------------------------------
  # 個股資料期間
  # ----------------------------------------------------------
  
  output$selected_stock_ranges <- renderTable({
    
    result <- lapply(
      analysis_symbols(),
      function(s) {
        
        x <- data() %>%
          filter(
            symbol == s,
            is.finite(adjusted)
          )
        
        if (nrow(x) == 0) {
          return(NULL)
        }
        
        data.frame(
          股票代號 = s,
          股票名稱 = get_stock_name(s),
          最早日期 = format(
            min(x$date),
            "%Y-%m-%d"
          ),
          最新日期 = format(
            max(x$date),
            "%Y-%m-%d"
          ),
          有效觀測數 = nrow(x),
          stringsAsFactors = FALSE
        )
      }
    )
    
    bind_rows(result)
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 資料期間警告
  # ----------------------------------------------------------
  
  output$stock_warning <- renderUI({
    
    r <- analysis_range()
    
    selected <- analysis_symbols()
    
    warnings <- lapply(
      selected,
      function(s) {
        
        x <- data() %>%
          filter(
            symbol == s,
            is.finite(adjusted)
          )
        
        if (nrow(x) == 0) {
          return(
            div(
              class = "warning-note",
              paste0(
                get_stock_label(s),
                " 沒有有效價格資料。"
              )
            )
          )
        }
        
        actual_start <- min(x$date)
        actual_end <- max(x$date)
        
        if (
          actual_start > r[1] ||
          actual_end < r[2]
        ) {
          
          div(
            class = "warning-note",
            paste0(
              get_stock_label(s),
              " 的實際資料範圍為 ",
              format(actual_start, "%Y-%m-%d"),
              " 至 ",
              format(actual_end, "%Y-%m-%d"),
              "，與目前分析期間不完全重疊。"
            )
          )
          
        } else {
          
          NULL
        }
      }
    )
    
    do.call(
      tagList,
      warnings
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 個股調整後收盤價
  # 所選股票全部合併在同一張圖
  # ----------------------------------------------------------
  
  output$price_plot <- renderPlot({
    
    symbols <- analysis_symbols()
    
    req(
      length(symbols) >= 1
    )
    
    x <- analysis_data() %>%
      filter(
        symbol %in% symbols,
        is.finite(adjusted)
      ) %>%
      arrange(
        date
      )
    
    req(
      nrow(x) > 0
    )
    
    # --------------------------------------------------------
    # 建立目前選擇股票的代表色
    # --------------------------------------------------------
    
    plot_colors <- stock_colors[
      symbols
    ]
    
    # --------------------------------------------------------
    # 建立股票名稱
    # --------------------------------------------------------
    
    plot_labels <- get_stock_label(
      symbols
    )
    
    names(plot_labels) <- symbols
    
    # --------------------------------------------------------
    # 繪圖
    # --------------------------------------------------------
    
    ggplot(
      x,
      aes(
        x = date,
        y = adjusted,
        color = symbol,
        group = symbol
      )
    ) +
      
      geom_line(
        linewidth = 0.9,
        na.rm = TRUE
      ) +
      
      scale_color_manual(
        values = plot_colors,
        breaks = symbols,
        labels = plot_labels,
        drop = FALSE
      ) +
      
      labs(
        title = "個股調整後收盤價",
        x = "日期",
        y = "調整後價格",
        color = NULL
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          ),
        
        legend.position =
          "bottom",
        
        legend.text =
          element_text(
            size = 10
          ),
        
        axis.text.x =
          element_text(
            angle = 45,
            hjust = 1
          )
      )
    
  })
  

  # ----------------------------------------------------------
  # 累積報酬（補回舊版 UI 原本缺少的 Server 輸出）
  # ----------------------------------------------------------
  
  output$cumulative_return_display <- renderUI({
    req(length(analysis_symbols()) > 0)
    plotOutput("cumulative_return_plot", height = "500px")
  })
  
  output$cumulative_return_plot <- renderPlot({
    symbols <- analysis_symbols()
    x <- analysis_data() %>%
      filter(symbol %in% symbols, is.finite(log_return)) %>%
      group_by(symbol) %>%
      arrange(date, .by_group = TRUE) %>%
      mutate(cumulative_return = exp(cumsum(log_return)) - 1) %>%
      ungroup()
    req(nrow(x) > 0)
    ggplot(x, aes(x=date, y=cumulative_return, color=symbol, group=symbol)) +
      geom_hline(yintercept=0, linetype="dashed", color="#94a3b8") +
      geom_line(linewidth=1.0) +
      scale_color_manual(
        values=stock_colors[symbols],
        breaks=symbols,
        labels=setNames(get_stock_label(symbols), symbols),
        drop=FALSE
      ) +
      scale_y_continuous(labels=function(z) paste0(round(z*100,1), "%")) +
      labs(title="期間累積報酬", x="日期", y="累積報酬率", color=NULL) +
      theme_minimal() +
      theme(plot.title=element_text(hjust=0.5, face="bold"), legend.position="bottom")
  })
  
  # ----------------------------------------------------------
  # 日對數報酬描述統計
  # ----------------------------------------------------------
  
  output$return_summary <- renderTable({
    
    x <- analysis_data() %>%
      filter(
        is.finite(log_return)
      ) %>%
      group_by(symbol) %>%
      summarise(
        有效交易日 = n(),
        平均數 = mean(
          log_return,
          na.rm = TRUE
        ),
        標準差 = sd(
          log_return,
          na.rm = TRUE
        ),
        最大值 = max(
          log_return,
          na.rm = TRUE
        ),
        最小值 = min(
          log_return,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    req(
      nrow(x) > 0
    )
    
    x %>%
      mutate(
        股票代號 = symbol,
        股票名稱 = vapply(
          symbol,
          get_stock_name,
          character(1)
        ),
        平均數 = fmt_pct(平均數, 3),
        標準差 = fmt_pct(
          標準差,
          3,
          show_sign = FALSE
        ),
        最大值 = fmt_pct(最大值, 3),
        最小值 = fmt_pct(最小值, 3)
      ) %>%
      select(
        股票代號,
        股票名稱,
        有效交易日,
        平均數,
        標準差,
        最大值,
        最小值
      )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 日對數報酬分布
  # ----------------------------------------------------------
  
  output$return_distribution_ui <- renderUI({
    
    symbols <- analysis_symbols()
    
    req(
      length(symbols) > 0
    )
    
    lapply(
      
      seq(
        1,
        length(symbols),
        by = 3
      ),
      
      function(i) {
        
        ids <-
          symbols[
            i:min(
              i + 2,
              length(symbols)
            )
          ]
        
        fluidRow(
          
          lapply(
            
            ids,
            
            function(symbol) {
              
              local({
                
                s <- symbol
                
                plot_id <-
                  paste0(
                    "return_distribution_plot_",
                    s
                  )
                
                output[[plot_id]] <- renderPlot({
                  
                  x <- analysis_data() %>%
                    filter(
                      symbol == s,
                      is.finite(log_return)
                    )
                  
                  req(
                    nrow(x) > 10
                  )
                  
                  base_col <-
                    stock_colors[s]
                  
                  light_col <-
                    adjustcolor(
                      base_col,
                      alpha.f = 0.50
                    )
                  
                  hist_col <-
                    adjustcolor(
                      base_col,
                      alpha.f = 0.35
                    )
                  
                  mu <-
                    mean(
                      x$log_return,
                      na.rm = TRUE
                    )
                  
                  sig <-
                    sd(
                      x$log_return,
                      na.rm = TRUE
                    )
                  
                  p <- ggplot(
                    
                    x,
                    
                    aes(
                      x = log_return
                    )
                    
                  ) +
                    
                    geom_histogram(
                      
                      aes(
                        y =
                          after_stat(
                            density
                          )
                      ),
                      
                      bins = 40,
                      
                      fill =
                        light_col,
                      
                      color =
                        hist_col,
                      
                      linewidth = 0.3
                      
                    ) +
                    
                    labs(
                      
                      title =
                        get_stock_label(s),
                      
                      x =
                        "日對數報酬率",
                      
                      y =
                        "密度"
                      
                    ) +
                    
                    scale_x_continuous(
                      
                      labels =
                        function(z) {
                          
                          paste0(
                            round(
                              z * 100,
                              1
                            ),
                            "%"
                          )
                          
                        }
                      
                    ) +
                    
                    theme_minimal() +
                    
                    theme(
                      
                      plot.title =
                        element_text(
                          hjust = 0.5,
                          face = "bold"
                        )
                      
                    )
                  
                  if (
                    is.finite(sig) &&
                    sig > 0
                  ) {
                    
                    p <- p +
                      
                      stat_function(
                        
                        fun = dnorm,
                        
                        args = list(
                          mean = mu,
                          sd = sig
                        ),
                        
                        color =
                          base_col,
                        
                        linewidth =
                          1.2
                        
                      )
                    
                  }
                  
                  p
                  
                })
                
                column(
                  
                  width = 4,
                  
                  plotOutput(
                    plot_id,
                    height = "350px"
                  )
                  
                )
                
              })
              
            }
            
          )
          
        )
        
      }
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 日對數報酬箱型圖
  # 固定 1 × 5 排版
  # 即使只選 1 檔，也保留 5 個固定位置
  # ----------------------------------------------------------
  
  output$return_boxplot_ui <- renderUI({
    
    symbols <- analysis_symbols()
    
    req(
      length(symbols) > 0
    )
    
    div(
      style = "
      display:flex;
      width:100%;
      align-items:flex-start;
    ",
      
      lapply(
        seq_len(5),
        function(i) {
          
          if (i <= length(symbols)) {
            
            local({
              
              s <- symbols[i]
              
              plot_id <- paste0(
                "return_boxplot_",
                s
              )
              
              output[[plot_id]] <- renderPlot({
                
                x <- analysis_data() %>%
                  filter(
                    symbol == s,
                    is.finite(log_return)
                  )
                
                req(
                  nrow(x) > 0
                )
                
                current_color <-
                  stock_colors[s]
                
                ggplot(
                  x,
                  aes(
                    x = "",
                    y = log_return
                  )
                ) +
                  
                  geom_hline(
                    yintercept = 0,
                    linetype = "dashed"
                  ) +
                  
                  geom_boxplot(
                    width = 0.55,
                    fill = current_color,
                    color = current_color,
                    outlier.alpha = 0.35
                  ) +
                  
                  labs(
                    title = get_stock_label(s),
                    x = NULL,
                    y = "日對數報酬率"
                  ) +
                  
                  scale_y_continuous(
                    labels = function(z) {
                      paste0(
                        round(
                          z * 100,
                          2
                        ),
                        "%"
                      )
                    }
                  ) +
                  
                  theme_minimal() +
                  
                  theme(
                    plot.title =
                      element_text(
                        hjust = 0.5,
                        face = "bold",
                        size = 12
                      ),
                    
                    axis.text.x =
                      element_blank(),
                    
                    axis.ticks.x =
                      element_blank(),
                    
                    plot.margin =
                      margin(
                        10,
                        5,
                        10,
                        5
                      )
                  )
                
              })
              
              div(
                style = "
                flex:0 0 20%;
                width:20%;
                padding:0 5px;
              ",
                
                plotOutput(
                  plot_id,
                  height = "400px"
                )
              )
              
            })
            
          } else {
            
            div(
              style = "
              flex:0 0 20%;
              width:20%;
              padding:0 5px;
              height:400px;
            "
            )
            
          }
          
        }
      )
    )
    
  })
  
  # ----------------------------------------------------------
  # 每檔股票獨立日對數報酬時間序列
  # ----------------------------------------------------------
  
  output$log_return_series_ui <- renderUI({
    
    symbols <- analysis_symbols()
    
    lapply(
      
      seq(
        1,
        length(symbols),
        by = 2
      ),
      
      function(i) {
        
        ids <-
          symbols[
            i:min(
              i + 1,
              length(symbols)
            )
          ]
        
        fluidRow(
          
          lapply(
            
            ids,
            
            function(symbol) {
              
              local({
                
                s <- symbol
                
                plot_id <-
                  paste0(
                    "log_return_plot_",
                    s
                  )
                
                output[[plot_id]] <- renderPlot({
                  
                  x <- analysis_data() %>%
                    filter(
                      symbol == s,
                      is.finite(log_return)
                    )
                  
                  req(
                    nrow(x) > 1
                  )
                  
                  ggplot(
                    
                    x,
                    
                    aes(
                      x = date,
                      y = log_return
                    )
                    
                  ) +
                    
                    geom_hline(
                      yintercept = 0,
                      linetype = "dashed"
                    ) +
                    
                    geom_line(
                      color =
                        stock_colors[s],
                      linewidth = 0.7
                    ) +
                    
                    labs(
                      title =
                        get_stock_label(s),
                      x = "日期",
                      y = "日對數報酬率"
                    ) +
                    
                    scale_y_continuous(
                      labels = function(z) {
                        paste0(
                          round(
                            z * 100,
                            2
                          ),
                          "%"
                        )
                      }
                    ) +
                    
                    theme_minimal() +
                    
                    theme(
                      plot.title =
                        element_text(
                          hjust = 0.5,
                          face = "bold"
                        ),
                      axis.text.x =
                        element_text(
                          angle = 45,
                          hjust = 1
                        )
                    )
                  
                })
                
                column(
                  
                  width = 6,
                  
                  plotOutput(
                    plot_id,
                    height = "270px"
                  )
                  
                )
                
              })
              
            }
            
          )
          
        )
        
      }
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 期間績效
  # ----------------------------------------------------------
  
  output$period_performance <- renderTable({
    
    x <- analysis_data() %>%
      filter(
        is.finite(adjusted)
      ) %>%
      group_by(symbol) %>%
      arrange(date, .by_group = TRUE) %>%
      summarise(
        起始日期 = first(date),
        結束日期 = last(date),
        起始價格 = first(adjusted),
        結束價格 = last(adjusted),
        有效交易日 = n(),
        .groups = "drop"
      ) %>%
      mutate(
        累積報酬率 =
          結束價格 /
          起始價格 - 1,
        年化報酬率 =
          mapply(
            annualized_return,
            起始價格,
            結束價格,
            有效交易日
          )
      )
    
    x %>%
      mutate(
        股票代號 = symbol,
        股票名稱 = vapply(
          symbol,
          get_stock_name,
          character(1)
        ),
        起始日期 = format(
          起始日期,
          "%Y-%m-%d"
        ),
        結束日期 = format(
          結束日期,
          "%Y-%m-%d"
        ),
        起始價格 = fmt_price(
          起始價格
        ),
        結束價格 = fmt_price(
          結束價格
        ),
        累積報酬率 = fmt_pct(
          累積報酬率
        ),
        年化報酬率 = fmt_pct(
          年化報酬率
        )
      ) %>%
      select(
        股票代號,
        股票名稱,
        起始日期,
        起始價格,
        結束日期,
        結束價格,
        有效交易日,
        累積報酬率,
        年化報酬率
      )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 價格摘要
  # ----------------------------------------------------------
  
  output$price_summary <- renderTable({
    
    x <- analysis_data() %>%
      filter(
        is.finite(adjusted)
      ) %>%
      group_by(symbol) %>%
      summarise(
        平均價格 = mean(
          adjusted,
          na.rm = TRUE
        ),
        價格標準差 = sd(
          adjusted,
          na.rm = TRUE
        ),
        期間最低價 = min(
          adjusted,
          na.rm = TRUE
        ),
        期間最高價 = max(
          adjusted,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    x %>%
      mutate(
        股票代號 = symbol,
        股票名稱 = vapply(
          symbol,
          get_stock_name,
          character(1)
        ),
        平均價格 = fmt_price(
          平均價格
        ),
        價格標準差 = fmt_price(
          價格標準差
        ),
        期間最低價 = fmt_price(
          期間最低價
        ),
        期間最高價 = fmt_price(
          期間最高價
        )
      ) %>%
      select(
        股票代號,
        股票名稱,
        平均價格,
        價格標準差,
        期間最低價,
        期間最高價
      )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 風險摘要
  # ----------------------------------------------------------
  
  risk_data <- reactive({
    
    x <- analysis_data() %>%
      filter(
        is.finite(log_return),
        is.finite(adjusted)
      ) %>%
      group_by(symbol) %>%
      arrange(
        date,
        .by_group = TRUE
      ) %>%
      summarise(
        
        annual_volatility =
          sd(
            log_return,
            na.rm = TRUE
          ) *
          sqrt(
            TRADING_DAYS_PER_YEAR
          ),
        
        max_drawdown =
          min(
            drawdown_value(adjusted),
            na.rm = TRUE
          ),
        
        mean_daily =
          mean(
            log_return,
            na.rm = TRUE
          ),
        
        .groups = "drop"
      ) %>%
      mutate(
        
        sharpe =
          ifelse(
            annual_volatility > 0,
            mean_daily *
              TRADING_DAYS_PER_YEAR /
              annual_volatility,
            NA_real_
          )
        
      )
    
    x
    
  })
  
  
  output$risk_summary <- renderTable({
    
    x <- risk_data()
    
    x %>%
      mutate(
        股票代號 = symbol,
        股票名稱 = vapply(
          symbol,
          get_stock_name,
          character(1)
        ),
        年化波動度 = fmt_pct(
          annual_volatility,
          2,
          FALSE
        ),
        最大回撤 = fmt_pct(
          max_drawdown
        ),
        夏普比率 =
          ifelse(
            is.finite(sharpe),
            sprintf(
              "%.3f",
              sharpe
            ),
            "NA"
          )
      ) %>%
      select(
        股票代號,
        股票名稱,
        年化波動度,
        最大回撤,
        夏普比率
      )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 回撤圖
  # ----------------------------------------------------------
  
  output$drawdown_charts_ui <- renderUI({
    
    symbols <- analysis_symbols()
    
    lapply(
      
      seq(
        1,
        length(symbols),
        by = 2
      ),
      
      function(i) {
        
        ids <- symbols[
          i:min(
            i + 1,
            length(symbols)
          )
        ]
        
        fluidRow(
          
          lapply(
            
            ids,
            
            function(symbol) {
              
              local({
                
                s <- symbol
                
                plot_id <-
                  paste0(
                    "drawdown_plot_",
                    s
                  )
                
                output[[plot_id]] <- renderPlot({
                  
                  x <- analysis_data() %>%
                    filter(
                      symbol == s,
                      is.finite(adjusted)
                    ) %>%
                    arrange(date)
                  
                  req(
                    nrow(x) > 1
                  )
                  
                  x$drawdown <-
                    drawdown_value(
                      x$adjusted
                    )
                  
                  ggplot(
                    x,
                    aes(
                      x = date,
                      y = drawdown
                    )
                  ) +
                    
                    geom_hline(
                      yintercept = 0,
                      linetype = "dashed"
                    ) +
                    
                    geom_line(
                      color =
                        stock_colors[s],
                      linewidth = 0.9
                    ) +
                    
                    labs(
                      title =
                        get_stock_label(s),
                      x = "日期",
                      y = "回撤"
                    ) +
                    
                    scale_y_continuous(
                      labels = function(z) {
                        paste0(
                          round(
                            z * 100,
                            1
                          ),
                          "%"
                        )
                      }
                    ) +
                    
                    theme_minimal() +
                    
                    theme(
                      plot.title =
                        element_text(
                          hjust = 0.5,
                          face = "bold"
                        ),
                      
                      axis.text.x =
                        element_text(
                          angle = 45,
                          hjust = 1
                        )
                    )
                  
                })
                
                column(
                  6,
                  plotOutput(
                    plot_id,
                    height = "320px"
                  )
                )
                
              })
              
            }
            
          )
          
        )
        
      }
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 報酬與風險圖
  # ----------------------------------------------------------
  
  output$return_risk_chart <- renderPlot({
    
    x <- analysis_data() %>%
      filter(
        is.finite(log_return),
        is.finite(adjusted)
      ) %>%
      group_by(symbol) %>%
      arrange(
        date,
        .by_group = TRUE
      ) %>%
      summarise(
        
        annual_return =
          annualized_return(
            first(adjusted),
            last(adjusted),
            n()
          ),
        
        annual_volatility =
          sd(
            log_return,
            na.rm = TRUE
          ) *
          sqrt(
            TRADING_DAYS_PER_YEAR
          ),
        
        .groups = "drop"
        
      )
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      
      x,
      
      aes(
        x = annual_volatility,
        y = annual_return,
        color = symbol,
        label = symbol
      )
      
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_point(
        size = 3
      ) +
      
      geom_text(
        vjust = -0.8
      ) +
      
      scale_color_manual(
        values = stock_colors,
        guide = "none"
      ) +
      
      labs(
        title = "報酬與風險關係",
        x = "年化波動度",
        y = "年化報酬率"
      ) +
      
      scale_x_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          )
      )
    
  })
  
  
  
  # ----------------------------------------------------------
  # 個股 vs 0050：整體相關係數、Beta、60 日滾動相關性
  # ----------------------------------------------------------
  individual_relation_data <- reactive({
    symbols <- setdiff(analysis_symbols(), "0050")
    if (length(symbols) == 0) return(data.frame())
    r <- analysis_range()
    bench <- data() %>%
      filter(symbol == "0050", date >= r[1], date <= r[2], is.finite(log_return)) %>%
      select(date, benchmark_return = log_return)
    bind_rows(lapply(symbols, function(s) {
      z <- data() %>%
        filter(symbol == s, date >= r[1], date <= r[2], is.finite(log_return)) %>%
        select(date, stock_return = log_return) %>%
        inner_join(bench, by = "date") %>%
        arrange(date)
      if (nrow(z) < 20) return(NULL)
      bvar <- var(z$benchmark_return, na.rm = TRUE)
      beta <- if (is.finite(bvar) && bvar > 0) cov(z$stock_return, z$benchmark_return, use = "complete.obs") / bvar else NA_real_
      data.frame(
        symbol = s,
        name = get_stock_name(s),
        observations = nrow(z),
        correlation = cor(z$stock_return, z$benchmark_return, use = "complete.obs"),
        beta = beta,
        stringsAsFactors = FALSE
      )
    }))
  })
  
  output$individual_correlation_table <- renderTable({
    x <- individual_relation_data()
    if (nrow(x) == 0) {
      return(data.frame(說明 = "請至少再選 1 檔成分股，才能和 0050 比較相關性與 Beta。"))
    }
    x %>% transmute(
      股票 = paste0(name, " (", symbol, ")"),
      有效交易日 = observations,
      與0050相關係數 = sprintf("%.3f", correlation),
      Beta = sprintf("%.3f", beta),
      白話解讀 = case_when(
        correlation >= 0.8 ~ "和 0050 的漲跌通常很同步",
        correlation >= 0.5 ~ "和 0050 有中度同步",
        correlation > 0 ~ "有同向關係，但同步程度較弱",
        TRUE ~ "和 0050 的同向關係不明顯"
      )
    )
  }, striped = FALSE, bordered = TRUE, hover = TRUE)
  
  output$individual_rolling_correlation_chart <- renderPlot({
    symbols <- setdiff(analysis_symbols(), "0050")
    req(length(symbols) > 0)
    r <- analysis_range()
    bench <- data() %>%
      filter(symbol == "0050", date >= r[1], date <= r[2], is.finite(log_return)) %>%
      select(date, benchmark_return = log_return)
    long <- bind_rows(lapply(symbols, function(s) {
      z <- data() %>%
        filter(symbol == s, date >= r[1], date <= r[2], is.finite(log_return)) %>%
        select(date, stock_return = log_return) %>%
        inner_join(bench, by = "date") %>%
        arrange(date)
      if (nrow(z) < 60) return(NULL)
      z$rolling_correlation <- rolling_correlation_fast(z$stock_return, z$benchmark_return, 60)
      z$symbol <- s
      z
    })) %>% filter(is.finite(rolling_correlation))
    req(nrow(long) > 0)
    ggplot(long, aes(x = date, y = rolling_correlation, color = symbol)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#b4bec8") +
      geom_line(linewidth = 0.9) +
      scale_color_manual(values = stock_colors, labels = function(z) get_stock_label(z)) +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = "與 0050 的 60 日滾動相關性", x = "日期", y = "相關係數", color = NULL) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom")
  })
  
  # ==========================================================
  # 6. 融合版：0050 摘要、綜合排名、風險穩定性、主成分與分群
  # ==========================================================
  
  output$market_0050_composite_score <- renderText({
    x <- research_metrics() %>% filter(symbol == "0050")
    req(nrow(x) == 1)
    paste0(fmt_score(x$composite_score), " / 100")
  })
  
  output$market_0050_stability_score <- renderText({
    x <- research_metrics() %>% filter(symbol == "0050")
    req(nrow(x) == 1)
    paste0(fmt_score(x$stability_score), " / 100")
  })
  
  output$market_0050_volatility <- renderText({
    x <- research_metrics() %>% filter(symbol == "0050")
    req(nrow(x) == 1)
    fmt_pct(x$annual_volatility, 2, FALSE)
  })
  
  output$market_0050_max_drawdown <- renderText({
    x <- research_metrics() %>% filter(symbol == "0050")
    req(nrow(x) == 1)
    fmt_pct(x$max_drawdown, 2, TRUE)
  })
  
  output$market_0050_trend <- renderText({
    x <- data() %>% filter(symbol == "0050", is.finite(adjusted)) %>% arrange(date)
    req(nrow(x) >= 60)
    p <- tail(x$adjusted, 1)
    ma20 <- mean(tail(x$adjusted, 20), na.rm=TRUE)
    ma60 <- mean(tail(x$adjusted, 60), na.rm=TRUE)
    if (p > ma20 && ma20 > ma60) "偏強" else if (p < ma20 && ma20 < ma60) "偏弱" else "整理"
  })
  
  output$market_0050_rsi <- renderText({
    x <- data() %>% filter(symbol == "0050", is.finite(adjusted)) %>% arrange(date)
    value <- rsi_value(x$adjusted, 14)
    if (!is.finite(value)) return("NA")
    state <- if (value >= 70) "動能偏熱" else if (value <= 30) "動能偏弱" else "中性"
    paste0(sprintf("%.1f", value), "｜", state)
  })
  
  output$market_0050_vol_state <- renderText({
    x <- data() %>% filter(symbol == "0050", is.finite(log_return)) %>% arrange(date)
    req(nrow(x) >= 63)
    recent <- sd(tail(x$log_return, 21), na.rm=TRUE)
    base <- sd(tail(x$log_return, min(252, nrow(x))), na.rm=TRUE)
    ratio <- recent / base
    if (!is.finite(ratio)) "NA" else if (ratio >= 1.20) "波動偏高" else if (ratio <= 0.80) "波動偏低" else "接近一年水準"
  })
  
  output$composite_ranking_chart <- renderPlot({
    x <- research_metrics() %>%
      filter(symbol %in% stock_info$symbol, is.finite(composite_score)) %>%
      arrange(desc(composite_score)) %>%
      slice_head(n = 10) %>%
      mutate(label = paste0(symbol, " ", name))
    req(nrow(x) > 0)
    ggplot(x, aes(x = composite_score, y = reorder(label, composite_score), fill = symbol)) +
      geom_col(width = 0.72) +
      scale_fill_manual(values = stock_colors, guide = "none") +
      geom_vline(
        xintercept = research_metrics() %>% filter(symbol == "0050") %>% pull(composite_score),
        linetype = "dashed"
      ) +
      labs(
        title = "成分股綜合分數前 10 名",
        subtitle = "虛線為 0050 綜合分數",
        x = "綜合分數（0–100）",
        y = NULL
      ) +
      xlim(0, 100) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$composite_ranking_table <- renderTable({
    x <- research_metrics() %>%
      filter(symbol %in% stock_info$symbol, is.finite(composite_score)) %>%
      arrange(desc(composite_score)) %>%
      mutate(
        排名 = row_number(),
        股票代號 = symbol,
        股票名稱 = name,
        綜合分數 = fmt_score(composite_score),
        穩定性分數 = fmt_score(stability_score),
        報酬分數 = fmt_score(return_score),
        風險控制分數 = fmt_score(risk_score),
        風險報酬效率 = fmt_score(sharpe_score),
        相對0050 = relative_0050,
        分數差 = ifelse(is.finite(score_diff_0050), sprintf("%+.1f", score_diff_0050), "NA"),
        標籤 = labels
      ) %>%
      select(排名, 股票代號, 股票名稱, 綜合分數, 穩定性分數, 報酬分數, 風險控制分數, 風險報酬效率, 相對0050, 分數差, 標籤)
    x
  }, striped = FALSE, bordered = TRUE, hover = TRUE)
  
  output$stability_ranking_chart <- renderPlot({
    x <- research_metrics() %>%
      filter(symbol %in% stock_info$symbol, is.finite(stability_score)) %>%
      arrange(desc(stability_score)) %>%
      slice_head(n = 10) %>%
      mutate(label = paste0(symbol, " ", name))
    req(nrow(x) > 0)
    ggplot(x, aes(x = stability_score, y = reorder(label, stability_score), fill = symbol)) +
      geom_col(width = 0.72) +
      scale_fill_manual(values = stock_colors, guide = "none") +
      labs(
        title = "成分股穩定性分數前 10 名",
        x = "穩定性分數（0–100）",
        y = NULL
      ) +
      xlim(0, 100) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$risk_stability_table <- renderTable({
    research_metrics() %>%
      filter(symbol %in% c("0050", stock_info$symbol)) %>%
      arrange(desc(stability_score)) %>%
      transmute(
        股票代號 = symbol,
        股票名稱 = name,
        穩定性分數 = fmt_score(stability_score),
        年化波動度 = fmt_pct(annual_volatility, 2, FALSE),
        最大回撤 = fmt_pct(max_drawdown, 2, TRUE),
        `95%條件風險值（日對數報酬）` = fmt_pct(cvar_95, 2, TRUE),
        夏普比率 = ifelse(is.finite(sharpe), sprintf("%.3f", sharpe), "NA"),
        標籤 = labels
      )
  }, striped = FALSE, bordered = TRUE, hover = TRUE)
  
  output$pca_variance_table <- renderTable({
    r <- pca_cluster_result()
    p <- r$pca
    variance <- (p$sdev ^ 2) / sum(p$sdev ^ 2)
    data.frame(
      主成分 = paste0("PC", seq_along(variance)),
      解釋變異比例 = paste0(round(variance * 100, 2), "%"),
      累積比例 = paste0(round(cumsum(variance) * 100, 2), "%"),
      分群是否使用 = ifelse(seq_along(variance) <= r$pc_count, "是", "否"),
      stringsAsFactors = FALSE
    )
  }, striped = FALSE, bordered = TRUE, hover = TRUE)

  output$pca_loading_table <- renderTable({
    p <- pca_cluster_result()$pca
    loading <- as.data.frame(round(p$rotation[, seq_len(min(3, ncol(p$rotation))), drop = FALSE], 3))
    loading$feature_key <- rownames(loading)
    loading$原始指標 <- unname(pca_feature_labels[loading$feature_key])
    loading <- loading %>% select(原始指標, everything(), -feature_key)
    loading
  }, striped = FALSE, bordered = TRUE, hover = TRUE)

  output$cluster_quality_table <- renderTable({
    r <- pca_cluster_result()
    x <- r$quality %>%
      mutate(
        群數 = k,
        群內平方和 = round(total_withinss, 2),
        平均輪廓係數 = round(silhouette, 3),
        建議 = ifelse(k == r$best_k, "目前較佳", "")
      ) %>%
      select(群數, 群內平方和, 平均輪廓係數, 建議)
    x
  }, striped = FALSE, bordered = TRUE, hover = TRUE)

  output$pca_cluster_chart <- renderPlot({
    r <- pca_cluster_result()
    x <- r$data
    req(nrow(x) > 0)
    ggplot(x, aes(x = PC1, y = PC2, color = cluster_label, label = symbol)) +
      geom_point(size = 3) +
      geom_text(vjust = -0.7, size = 3, show.legend = FALSE) +
      labs(
        title = paste0("主成分分群圖｜", window_label(r$period_days), "（", r$best_k, " 群）"),
        subtitle = paste0("分群使用前 ", r$pc_count, " 個主成分，累積解釋至少約 80% 的資訊"),
        x = "第一主成分（PC1）",
        y = "第二主成分（PC2）",
        color = "分群"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
  })

  output$cluster_summary_table <- renderTable({
    r <- pca_cluster_result()
    r$data %>%
      group_by(cluster_label) %>%
      summarise(
        股票數 = n(),
        平均年化報酬 = mean(annual_return, na.rm = TRUE),
        平均年化波動 = mean(annual_volatility, na.rm = TRUE),
        平均最大回撤 = mean(max_drawdown, na.rm = TRUE),
        平均CVaR = mean(cvar_95, na.rm = TRUE),
        平均夏普 = mean(sharpe, na.rm = TRUE),
        平均綜合分數 = mean(composite_score, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        平均年化報酬 = fmt_pct(平均年化報酬, 2, TRUE),
        平均年化波動 = fmt_pct(平均年化波動, 2, FALSE),
        平均最大回撤 = fmt_pct(平均最大回撤, 2, TRUE),
        平均CVaR = fmt_pct(平均CVaR, 2, TRUE),
        平均夏普 = ifelse(is.finite(平均夏普), sprintf("%.3f", 平均夏普), "NA"),
        平均綜合分數 = fmt_score(平均綜合分數)
      ) %>%
      rename(分群類型 = cluster_label)
  }, striped = FALSE, bordered = TRUE, hover = TRUE)

  output$cluster_filter_ui <- renderUI({
    x <- pca_cluster_result()$data
    groups <- sort(unique(x$cluster_label))
    selectInput(
      "cluster_filter",
      "篩選群組：",
      choices = c("全部群組" = "ALL", setNames(groups, groups)),
      selected = "ALL"
    )
  })

  output$cluster_member_table <- renderTable({
    r <- pca_cluster_result()
    x <- r$data
    if (!is.null(input$cluster_filter) && input$cluster_filter != "ALL") {
      x <- x %>% filter(cluster_label == input$cluster_filter)
    }
    x %>%
      arrange(cluster_label, desc(composite_score)) %>%
      transmute(
        股票代號 = symbol,
        股票名稱 = name,
        產業 = industry,
        所屬群組 = cluster_label,
        `期間年化報酬` = fmt_pct(annual_return, 2, TRUE),
        `期間年化波動` = fmt_pct(annual_volatility, 2, FALSE),
        最大回撤 = fmt_pct(max_drawdown, 2, TRUE),
        夏普比率 = ifelse(is.finite(sharpe), sprintf("%.3f", sharpe), "NA"),
        `主模型綜合分數（參考）` = fmt_score(composite_score)
      )
  }, striped = FALSE, bordered = TRUE, hover = TRUE)


  # ==========================================================
  # 6-1. 不確定性分析：區塊拔靴法
  # ==========================================================
  
  output$bootstrap_period_info <- renderUI({
    n_day <- as.integer(if (is.null(input$bootstrap_window)) 252 else input$bootstrap_window)
    d <- sort(unique(data()$date))
    end_date <- max(d, na.rm=TRUE)
    start_date <- window_start_from_dates(d, n_day, end_date)
    div(
      class="time-scope-bar",
      strong("這次抽樣使用的資料： "),
      paste0(window_label(n_day), "｜", format(start_date, "%Y-%m-%d"), " ～ ", format(end_date, "%Y-%m-%d"), "｜約 ", n_day, " 個交易日")
    )
  })

  bootstrap_result <- eventReactive(input$run_bootstrap, {
    req(input$bootstrap_stock)
    set.seed(20261023)
    n_day <- as.integer(input$bootstrap_window)
    block_length <- as.integer(input$bootstrap_block)
    reps <- as.integer(input$bootstrap_reps)
    d <- sort(unique(data()$date))
    end_date <- max(d, na.rm=TRUE)
    start_date <- window_start_from_dates(d, n_day, end_date)

    x <- data() %>%
      filter(
        date >= start_date,
        date <= end_date,
        symbol %in% dashboard_symbols,
        is.finite(log_return)
      )
    m <- return_wide_matrix(x, dashboard_symbols)
    req(!is.null(m), nrow(m) >= 63, "0050" %in% colnames(m), input$bootstrap_stock %in% colnames(m))

    diffs <- rep(NA_real_, reps)
    withProgress(message = "正在重新抽樣並檢查結果…", value = 0, {
      for (b in seq_len(reps)) {
        idx <- block_bootstrap_indices(nrow(m), block_length)
        s <- score_return_matrix(m[idx, , drop=FALSE])
        if (nrow(s) > 0) {
          stock_score <- s$composite_score[s$symbol == input$bootstrap_stock]
          bench_score <- s$composite_score[s$symbol == "0050"]
          if (length(stock_score) == 1 && length(bench_score) == 1 && is.finite(stock_score) && is.finite(bench_score)) {
            diffs[b] <- stock_score - bench_score
          }
        }
        if (b %% max(1, floor(reps / 20)) == 0 || b == reps) {
          setProgress(value = b / reps, detail = paste0("已完成 ", b, " / ", reps, " 次"))
        }
      }
    })
    diffs <- diffs[is.finite(diffs)]
    req(length(diffs) >= max(20, reps * 0.4))
    ci <- as.numeric(quantile(diffs, c(0.025, 0.975), na.rm=TRUE, names=FALSE))
    prob_above <- mean(diffs > 0, na.rm=TRUE)
    label <- if (ci[1] > 0) {
      "多數重新抽樣後仍高於 0050，結果相對穩定"
    } else if (ci[2] < 0) {
      "多數重新抽樣後仍低於 0050"
    } else {
      "結果有時高、有時低，暫時無法說優勢穩定"
    }
    list(
      diffs=diffs,
      ci=ci,
      prob_above=prob_above,
      label=label,
      reps=length(diffs),
      stock=input$bootstrap_stock,
      period_days=n_day,
      start_date=start_date,
      end_date=end_date,
      block=block_length
    )
  }, ignoreInit=TRUE)

  output$bootstrap_summary_table <- renderTable({
    r <- bootstrap_result()
    data.frame(
      比較標的=get_stock_label(r$stock),
      使用期間=window_label(r$period_days),
      實際日期=paste0(format(r$start_date, "%Y-%m-%d"), " ～ ", format(r$end_date, "%Y-%m-%d")),
      有效重複次數=r$reps,
      區塊長度=paste0(r$block, " 個交易日"),
      平均分數差=sprintf("%+.2f", mean(r$diffs)),
      `95%區間下限`=sprintf("%+.2f", r$ci[1]),
      `95%區間上限`=sprintf("%+.2f", r$ci[2]),
      `高於0050比例`=paste0(round(r$prob_above * 100, 1), "%"),
      結論=r$label,
      check.names=FALSE,
      stringsAsFactors=FALSE
    )
  }, striped=FALSE, bordered=TRUE, hover=TRUE)

  output$bootstrap_diff_chart <- renderPlot({
    r <- bootstrap_result()
    x <- data.frame(diff=r$diffs)
    ggplot(x, aes(x=diff)) +
      geom_histogram(aes(y=after_stat(density)), bins=36, fill="#a28bd2", color="white", alpha=0.85) +
      geom_density(color="#5b8ecb", linewidth=1.1) +
      geom_vline(xintercept=0, linetype="dashed", color="#d95b5b", linewidth=1) +
      geom_vline(xintercept=r$ci, linetype="dotted", color="#dca75a", linewidth=1) +
      labs(
        title=paste0(get_stock_label(r$stock), " 相對 0050 的區塊拔靴分布｜", window_label(r$period_days)),
        subtitle=paste0(format(r$start_date, "%Y-%m-%d"), " ～ ", format(r$end_date, "%Y-%m-%d"), "｜95% 區間：", sprintf("%+.2f", r$ci[1]), " ～ ", sprintf("%+.2f", r$ci[2])),
        x="綜合分數差（成分股－0050）",
        y="密度"
      ) +
      theme_minimal() +
      theme(plot.title=element_text(hjust=0.5, face="bold"), plot.subtitle=element_text(hjust=0.5))
  })


  # ==========================================================
  # 6-2. 歷史回測：排名模型驗證
  # ==========================================================
  
  backtest_result <- eventReactive(input$run_backtest, {
    tryCatch({
      horizon <- as.integer(input$backtest_horizon)
      start_year <- as.integer(input$backtest_start_year)
      weight_profile <- if (is.null(input$backtest_weight_profile)) "baseline" else input$backtest_weight_profile
      model_weights <- WEIGHT_PROFILES[[weight_profile]]
      if (is.null(model_weights)) model_weights <- MODEL_WEIGHTS
      all_dates <- sort(unique(data()$date))
      req(length(all_dates) > 0)

      # 為了驗證「目前同一套模型」，每個歷史評分點最多往回 3 年。
      history_days <- TIME_WINDOWS[["3年"]]
      min_cutoff_idx <- history_days
      requested_idx <- which(as.integer(format(all_dates, "%Y")) >= start_year)[1]
      if (!is.finite(requested_idx)) {
        return(list(error = "找不到你指定起始年度之後的交易資料。"))
      }
      first_idx <- max(min_cutoff_idx, requested_idx)
      last_idx <- length(all_dates) - horizon
      if (first_idx > last_idx) {
        return(list(error = paste0(
          "資料長度不足：目前選擇往後驗證 ", window_label(horizon),
          "，但還需要保留完整的未來資料。請縮短驗證期間或把起始年度往前。"
        )))
      }

      # 1週驗證若每週都重算太密、也高度重疊，因此最密只每月取一個排名點。
      # 其他期間使用不重疊或近似不重疊的步長。
      step_days <- max(horizon, 21)
      cutoff_indices <- seq(first_idx, last_idx, by = step_days)
      if (length(cutoff_indices) > 120) {
        cutoff_indices <- cutoff_indices[round(seq(1, length(cutoff_indices), length.out=120))]
      }

      strategy_rows <- list()
      detail_rows <- list()
      corr_rows <- list()
      counter <- 0

      portfolio_stats <- function(fut_data, symbols) {
        z <- fut_data %>%
          filter(symbol %in% symbols, is.finite(log_return)) %>%
          group_by(date) %>%
          summarise(
            portfolio_log_return = mean(log_return, na.rm=TRUE),
            n_stock = sum(is.finite(log_return)),
            .groups="drop"
          ) %>%
          filter(n_stock >= max(1, ceiling(length(symbols) * 0.5))) %>%
          arrange(date)
        if (nrow(z) < max(3, floor(horizon * 0.6))) {
          return(c(total_return=NA_real_, annual_volatility=NA_real_, max_drawdown=NA_real_, sharpe=NA_real_))
        }
        r <- z$portfolio_log_return
        wealth <- exp(cumsum(r))
        av <- if (length(r) >= 5) sd(r, na.rm=TRUE) * sqrt(TRADING_DAYS_PER_YEAR) else NA_real_
        c(
          total_return = exp(sum(r, na.rm=TRUE)) - 1,
          annual_volatility = av,
          max_drawdown = min(drawdown_value(wealth), na.rm=TRUE),
          sharpe = ifelse(is.finite(av) && av > 0, mean(r, na.rm=TRUE) * TRADING_DAYS_PER_YEAR / av, NA_real_)
        )
      }

      withProgress(message = "正在回到過去重新計分並驗證…", value = 0, {
        for (ii in seq_along(cutoff_indices)) {
          cutoff_idx <- cutoff_indices[ii]
          cutoff_date <- all_dates[cutoff_idx]
          hist_start_idx <- max(1, cutoff_idx - history_days + 1)
          hist_start_date <- all_dates[hist_start_idx]
          future_end_date <- all_dates[min(length(all_dates), cutoff_idx + horizon)]

          hist <- data() %>%
            filter(
              date >= hist_start_date,
              date <= cutoff_date,
              symbol %in% dashboard_symbols,
              is.finite(log_return)
            )
          m <- return_wide_matrix(hist, dashboard_symbols)

          if (!is.null(m) && nrow(m) >= 252) {
            scores <- score_return_matrix(m, model_weights = model_weights) %>%
              filter(symbol %in% stock_info$symbol, is.finite(composite_score)) %>%
              arrange(desc(composite_score))

            if (nrow(scores) >= 15) {
              top5 <- head(scores$symbol, 5)
              top10 <- head(scores$symbol, min(10, nrow(scores)))
              bottom10 <- tail(scores$symbol, min(10, nrow(scores)))

              fut <- data() %>%
                filter(
                  date > cutoff_date,
                  date <= future_end_date,
                  symbol %in% unique(c("0050", scores$symbol)),
                  is.finite(log_return)
                )

              s_top5 <- portfolio_stats(fut, top5)
              s_top10 <- portfolio_stats(fut, top10)
              s_bottom10 <- portfolio_stats(fut, bottom10)
              s_0050 <- portfolio_stats(fut, "0050")

              counter <- counter + 1
              for (nm in c("Top5", "Top10", "Bottom10", "0050")) {
                st <- switch(nm, Top5=s_top5, Top10=s_top10, Bottom10=s_bottom10, `0050`=s_0050)
                strategy_rows[[length(strategy_rows)+1]] <- data.frame(
                  驗證序號 = counter,
                  排名日期 = cutoff_date,
                  評估截止 = future_end_date,
                  策略 = nm,
                  後續報酬 = unname(st["total_return"]),
                  年化波動 = unname(st["annual_volatility"]),
                  最大回撤 = unname(st["max_drawdown"]),
                  夏普比率 = unname(st["sharpe"]),
                  stringsAsFactors=FALSE
                )
              }

              detail_rows[[length(detail_rows)+1]] <- data.frame(
                驗證序號 = counter,
                排名日期 = cutoff_date,
                評估截止 = future_end_date,
                Top5 = unname(s_top5["total_return"]),
                Top10 = unname(s_top10["total_return"]),
                Bottom10 = unname(s_bottom10["total_return"]),
                `0050` = unname(s_0050["total_return"]),
                stringsAsFactors=FALSE,
                check.names=FALSE
              )

              # Spearman：現在分數越高，未來個股報酬是否也傾向越高？
              indiv_future <- fut %>%
                filter(symbol %in% scores$symbol) %>%
                group_by(symbol) %>%
                summarise(
                  future_return = exp(sum(log_return, na.rm=TRUE)) - 1,
                  n_obs = n(),
                  .groups="drop"
                ) %>%
                filter(n_obs >= max(3, floor(horizon * 0.6)), is.finite(future_return))

              corr_df <- scores %>%
                select(symbol, composite_score) %>%
                inner_join(indiv_future, by="symbol")

              rho <- if (nrow(corr_df) >= 10) {
                suppressWarnings(cor(corr_df$composite_score, corr_df$future_return, method="spearman", use="complete.obs"))
              } else NA_real_

              corr_rows[[length(corr_rows)+1]] <- data.frame(
                驗證序號=counter,
                排名日期=cutoff_date,
                評估截止=future_end_date,
                Spearman=rho,
                個股數=nrow(corr_df),
                stringsAsFactors=FALSE
              )
            }
          }

          setProgress(value = ii / length(cutoff_indices), detail = paste0("排名日期：", format(cutoff_date, "%Y-%m-%d")))
        }
      })

      strategy_long <- bind_rows(strategy_rows) %>% filter(is.finite(後續報酬))
      detail <- bind_rows(detail_rows)
      corr_detail <- bind_rows(corr_rows) %>% filter(is.finite(Spearman))

      if (nrow(strategy_long) == 0 || nrow(detail) == 0) {
        return(list(error = "目前條件下找不到足夠資料完成模型驗證。可以縮短未來驗證期間，或把起始年度往前調。"))
      }

      benchmark_by_test <- strategy_long %>%
        filter(策略 == "0050") %>%
        select(驗證序號, benchmark_return = 後續報酬)

      summary <- strategy_long %>%
        left_join(benchmark_by_test, by="驗證序號") %>%
        group_by(策略) %>%
        summarise(
          有效驗證次數 = n(),
          平均後續報酬 = mean(後續報酬, na.rm=TRUE),
          中位數後續報酬 = median(後續報酬, na.rm=TRUE),
          平均年化波動 = mean(年化波動, na.rm=TRUE),
          平均最大回撤 = mean(最大回撤, na.rm=TRUE),
          平均夏普 = mean(夏普比率, na.rm=TRUE),
          勝過0050比例 = ifelse(first(策略) == "0050", NA_real_, mean(後續報酬 > benchmark_return, na.rm=TRUE)),
          .groups="drop"
        )

      corr_summary <- if (nrow(corr_detail) > 0) {
        data.frame(
          有效驗證次數 = nrow(corr_detail),
          平均Spearman = mean(corr_detail$Spearman, na.rm=TRUE),
          中位數Spearman = median(corr_detail$Spearman, na.rm=TRUE),
          正相關比例 = mean(corr_detail$Spearman > 0, na.rm=TRUE),
          stringsAsFactors=FALSE
        )
      } else {
        data.frame(
          有效驗證次數 = 0,
          平均Spearman = NA_real_,
          中位數Spearman = NA_real_,
          正相關比例 = NA_real_
        )
      }

      list(
        error=NULL,
        strategy_long=strategy_long,
        detail=detail,
        summary=summary,
        corr_detail=corr_detail,
        corr_summary=corr_summary,
        horizon=horizon,
        step_days=step_days,
        history_days=history_days,
        weight_profile=weight_profile,
        model_weights=model_weights
      )
    }, error = function(e) {
      list(error = paste0("模型驗證暫時無法完成：", conditionMessage(e)))
    })
  }, ignoreInit=TRUE)

  output$backtest_status <- renderUI({
    r <- backtest_result()
    if (!is.null(r$error)) {
      div(class = "warning-note", strong("目前無法完成驗證： "), r$error)
    } else {
      n_test <- nrow(r$detail)
      low_sample <- n_test < 5
      div(
        class = if (low_sample) "warning-note" else "reading-guide",
        strong(if (low_sample) "樣本數偏少： " else "模型驗證完成： "),
        paste0(
          "權重：", WEIGHT_PROFILE_LABELS[[r$weight_profile]], "；後續觀察期間：", window_label(r$horizon),
          "；共有 ", n_test, " 次有效驗證。",
          if (r$horizon == 5) "1週驗證每月取一個排名點，避免結果高度重疊。" else "",
          if (low_sample) " 長期驗證只有少量樣本，勝率請勿單獨解讀。" else ""
        )
      )
    }
  })

  output$backtest_cumulative_chart <- renderPlot({
    r <- backtest_result()
    req(is.null(r$error), nrow(r$strategy_long) > 0)
    ggplot(r$strategy_long, aes(x=排名日期, y=後續報酬, color=策略, group=策略)) +
      geom_hline(yintercept=0, linetype="dashed", color="#94a3b8") +
      geom_line(linewidth=0.8, alpha=0.75) +
      geom_point(size=2.0) +
      scale_color_manual(values=c("Top5"="#6f9fd0", "Top10"="#9a7fd1", "Bottom10"="#d6a45f", "0050"="#4f86c6")) +
      scale_y_continuous(labels=function(z) paste0(round(z*100,1), "%")) +
      labs(
        title=paste0("評分模型驗證｜往後 ", window_label(r$horizon), " 的實際報酬"),
        subtitle="每一個點都先用當時以前的資料重新評分，再觀察之後；不是事後挑選買賣日期",
        x="當時的排名日期",
        y="之後實際報酬",
        color="比較組別"
      ) +
      theme_minimal() +
      theme(plot.title=element_text(hjust=0.5, face="bold"), plot.subtitle=element_text(hjust=0.5), legend.position="bottom")
  })

  output$backtest_summary_table <- renderTable({
    r <- backtest_result()
    req(is.null(r$error), nrow(r$summary) > 0)
    r$summary %>%
      mutate(
        平均後續報酬=fmt_pct(平均後續報酬,2,TRUE),
        中位數後續報酬=fmt_pct(中位數後續報酬,2,TRUE),
        平均年化波動=fmt_pct(平均年化波動,2,FALSE),
        平均最大回撤=fmt_pct(平均最大回撤,2,TRUE),
        平均夏普=ifelse(is.finite(平均夏普), sprintf("%.3f", 平均夏普), "NA"),
        勝過0050比例=ifelse(is.finite(勝過0050比例), paste0(round(勝過0050比例*100,1), "%"), "—")
      )
  }, striped=FALSE, bordered=TRUE, hover=TRUE)

  output$backtest_rank_corr_table <- renderTable({
    r <- backtest_result()
    req(is.null(r$error))
    x <- r$corr_summary
    x %>% mutate(
      平均Spearman = ifelse(is.finite(平均Spearman), sprintf("%.3f", 平均Spearman), "NA"),
      中位數Spearman = ifelse(is.finite(中位數Spearman), sprintf("%.3f", 中位數Spearman), "NA"),
      正相關比例 = ifelse(is.finite(正相關比例), paste0(round(正相關比例*100,1), "%"), "NA"),
      解讀 = case_when(
        is.finite(r$corr_summary$平均Spearman) & r$corr_summary$平均Spearman > 0.10 ~ "整體偏正：高分股票之後較常排前面",
        is.finite(r$corr_summary$平均Spearman) & r$corr_summary$平均Spearman < -0.10 ~ "整體偏負：需要檢查評分方向或權重",
        TRUE ~ "接近零：目前沒有明顯排序關係"
      )
    )
  }, striped=FALSE, bordered=TRUE, hover=TRUE)

  output$backtest_yearly_table <- renderTable({
    r <- backtest_result()
    req(is.null(r$error), nrow(r$detail) > 0)
    r$detail %>%
      mutate(
        排名日期=format(as.Date(排名日期), "%Y-%m-%d"),
        評估截止=format(as.Date(評估截止), "%Y-%m-%d"),
        Top5=fmt_pct(Top5,2,TRUE),
        Top10=fmt_pct(Top10,2,TRUE),
        Bottom10=fmt_pct(Bottom10,2,TRUE),
        `0050`=fmt_pct(`0050`,2,TRUE)
      )
  }, striped=FALSE, bordered=TRUE, hover=TRUE)


  # ==========================================================
  # 7. 產業比較
  # ==========================================================
  
  selected_industry_stocks <- reactive({
    
    stock_info %>%
      filter(
        industry_group ==
          input$selected_industry
      ) %>%
      pull(symbol)
    
  })
  
  
  selected_industry_data <- reactive({
    
    data() %>%
      filter(
        symbol %in%
          selected_industry_stocks(),
        date >= analysis_range()[1],
        date <= analysis_range()[2],
        is.finite(log_return)
      )
    
  })
  
  
  industry_daily <- reactive({
    
    selected_industry_data() %>%
      group_by(date) %>%
      summarise(
        log_return =
          mean(
            log_return,
            na.rm = TRUE
          ),
        .groups = "drop"
      ) %>%
      arrange(date)
    
  })
  
  
  output$industry_stock_table <- renderTable({
    
    stock_info %>%
      filter(
        industry_group ==
          input$selected_industry
      ) %>%
      transmute(
        股票代號 = symbol,
        股票名稱 = name,
        產業 = industry_group
      )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  output$industry_summary <- renderTable({
    
    x <- industry_daily()
    
    req(
      nrow(x) > 1
    )
    
    data.frame(
      
      產業 =
        input$selected_industry,
      
      成分股數 =
        length(
          selected_industry_stocks()
        ),
      
      平均日對數報酬 =
        fmt_pct(
          mean(
            x$log_return,
            na.rm = TRUE
          ),
          3
        ),
      
      日對數報酬標準差 =
        fmt_pct(
          sd(
            x$log_return,
            na.rm = TRUE
          ),
          3,
          FALSE
        ),
      
      累積報酬率 =
        fmt_pct(
          exp(
            sum(
              x$log_return,
              na.rm = TRUE
            )
          ) - 1
        ),
      
      有效交易日 =
        nrow(x),
      
      stringsAsFactors = FALSE
      
    )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 單一產業累積報酬
  # ----------------------------------------------------------
  
  output$industry_cumulative_chart <- renderPlot({
    
    industry_x <- industry_daily() %>%
      mutate(
        cumulative =
          exp(
            cumsum(
              log_return
            )
          ) - 1
      )
    
    benchmark <- data() %>%
      filter(
        symbol == "0050",
        date >= analysis_range()[1],
        date <= analysis_range()[2],
        is.finite(log_return)
      ) %>%
      arrange(date) %>%
      mutate(
        cumulative =
          exp(
            cumsum(
              log_return
            )
          ) - 1,
        series = "0050"
      )
    
    req(
      nrow(industry_x) > 1,
      nrow(benchmark) > 1
    )
    
    ggplot() +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_line(
        data = industry_x,
        aes(
          x = date,
          y = cumulative
        ),
        color =
          industry_colors[
            input$selected_industry
          ],
        linewidth = 1
      ) +
      
      geom_line(
        data = benchmark,
        aes(
          x = date,
          y = cumulative
        ),
        color = "red",
        linewidth = 1,
        linetype = "dashed"
      ) +
      
      labs(
        title =
          paste0(
            input$selected_industry,
            " 與 0050 累積報酬比較"
          ),
        x = "日期",
        y = "累積報酬率"
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          )
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 單一產業風險報酬
  # ----------------------------------------------------------
  
  output$industry_risk_return_chart <- renderPlot({
    
    x <- selected_industry_data() %>%
      group_by(symbol) %>%
      arrange(
        date,
        .by_group = TRUE
      ) %>%
      summarise(
        
        annual_return =
          exp(
            mean(
              log_return,
              na.rm = TRUE
            ) *
              TRADING_DAYS_PER_YEAR
          ) - 1,
        
        annual_volatility =
          sd(
            log_return,
            na.rm = TRUE
          ) *
          sqrt(
            TRADING_DAYS_PER_YEAR
          ),
        
        .groups = "drop"
        
      )
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      x,
      aes(
        x = annual_volatility,
        y = annual_return,
        label = symbol
      )
    ) +
      
      geom_point(
        size = 3,
        color =
          industry_colors[
            input$selected_industry
          ]
      ) +
      
      geom_text(
        vjust = -0.8,
        color =
          industry_colors[
            input$selected_industry
          ]
      ) +
      
      labs(
        title =
          paste0(
            input$selected_industry,
            " 成分股風險與報酬"
          ),
        x = "年化波動度",
        y = "年化報酬率"
      ) +
      
      scale_x_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          )
      )
    
  })
  
  
  # ==========================================================
  # 所有產業綜合比較
  # ==========================================================
  
  all_industry_daily <- reactive({
    
    data() %>%
      filter(
        symbol %in%
          stock_info$symbol,
        date >= analysis_range()[1],
        date <= analysis_range()[2],
        is.finite(log_return)
      ) %>%
      group_by(
        industry_group,
        date
      ) %>%
      summarise(
        log_return =
          mean(
            log_return,
            na.rm = TRUE
          ),
        .groups = "drop"
      ) %>%
      arrange(
        industry_group,
        date
      )
    
  })
  
  
  output$industry_all_cumulative_chart <- renderPlot({
    
    x <- all_industry_daily() %>%
      group_by(
        industry_group
      ) %>%
      arrange(
        date,
        .by_group = TRUE
      ) %>%
      mutate(
        cumulative =
          exp(
            cumsum(
              log_return
            )
          ) - 1
      ) %>%
      ungroup()
    
    ggplot(
      x,
      aes(
        x = date,
        y = cumulative,
        color = industry_group
      )
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_line(
        linewidth = 0.9
      ) +
      
      scale_color_manual(
        values =
          industry_colors
      ) +
      
      labs(
        title =
          "所有產業累積報酬比較",
        x = "日期",
        y = "累積報酬率",
        color = "產業"
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          ),
        legend.position =
          "bottom"
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 所有產業報酬分布
  # 3 × N
  # ----------------------------------------------------------
  
  output$industry_all_distribution_ui <- renderUI({
    
    industries <-
      sort(
        unique(
          all_industry_daily()$industry_group
        )
      )
    
    lapply(
      
      seq(
        1,
        length(industries),
        by = 3
      ),
      
      function(i) {
        
        ids <-
          industries[
            i:min(
              i + 2,
              length(industries)
            )
          ]
        
        fluidRow(
          
          lapply(
            
            ids,
            
            function(industry) {
              
              local({
                
                ind <- industry
                
                plot_id <-
                  paste0(
                    "industry_distribution_",
                    make.names(ind)
                  )
                
                output[[plot_id]] <- renderPlot({
                  
                  x <- all_industry_daily() %>%
                    filter(
                      industry_group == ind,
                      is.finite(log_return)
                    )
                  
                  req(
                    nrow(x) > 10
                  )
                  
                  col <-
                    industry_colors[ind]
                  
                  light <-
                    adjustcolor(
                      col,
                      alpha.f = 0.50
                    )
                  
                  edge <-
                    adjustcolor(
                      col,
                      alpha.f = 0.35
                    )
                  
                  mu <-
                    mean(
                      x$log_return,
                      na.rm = TRUE
                    )
                  
                  sig <-
                    sd(
                      x$log_return,
                      na.rm = TRUE
                    )
                  
                  p <- ggplot(
                    x,
                    aes(
                      x = log_return
                    )
                  ) +
                    
                    geom_histogram(
                      aes(
                        y =
                          after_stat(
                            density
                          )
                      ),
                      bins = 40,
                      fill = light,
                      color = edge
                    ) +
                    
                    labs(
                      title = ind,
                      x = "平均日對數報酬率",
                      y = "密度"
                    ) +
                    
                    scale_x_continuous(
                      labels = function(z) {
                        paste0(
                          round(
                            z * 100,
                            1
                          ),
                          "%"
                        )
                      }
                    ) +
                    
                    theme_minimal() +
                    
                    theme(
                      plot.title =
                        element_text(
                          hjust = 0.5,
                          face = "bold"
                        )
                    )
                  
                  if (
                    is.finite(sig) &&
                    sig > 0
                  ) {
                    
                    p <- p +
                      stat_function(
                        fun = dnorm,
                        args = list(
                          mean = mu,
                          sd = sig
                        ),
                        color = col,
                        linewidth = 1.1
                      )
                    
                  }
                  
                  p
                  
                })
                
                column(
                  width = 4,
                  plotOutput(
                    plot_id,
                    height = "340px"
                  )
                )
                
              })
              
            }
            
          )
          
        )
        
      }
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 所有產業風險報酬
  # ----------------------------------------------------------
  
  output$industry_all_risk_return_chart <- renderPlot({
    
    x <- all_industry_daily() %>%
      group_by(
        industry_group
      ) %>%
      summarise(
        
        annual_return =
          exp(
            mean(
              log_return,
              na.rm = TRUE
            ) *
              TRADING_DAYS_PER_YEAR
          ) - 1,
        
        annual_volatility =
          sd(
            log_return,
            na.rm = TRUE
          ) *
          sqrt(
            TRADING_DAYS_PER_YEAR
          ),
        
        .groups = "drop"
        
      )
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      
      x,
      
      aes(
        x = annual_volatility,
        y = annual_return,
        color = industry_group,
        label = industry_group
      )
      
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_point(
        size = 4
      ) +
      
      geom_text(
        vjust = -0.8
      ) +
      
      scale_color_manual(
        values =
          industry_colors
      ) +
      
      labs(
        title =
          "所有產業報酬與風險比較",
        x = "年化波動度",
        y = "年化報酬率",
        color = "產業"
      ) +
      
      scale_x_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          ),
        legend.position =
          "bottom"
      )
    
  })
  
  
  # ==========================================================
  # 7. 相關性分析
  # ==========================================================
  
  correlation_data <- reactive({
    
    req(
      input$cor_stock
    )
    
    data() %>%
      filter(
        symbol %in% input$cor_stock,
        date >= analysis_range()[1],
        date <= analysis_range()[2],
        is.finite(log_return)
      ) %>%
      select(
        date,
        symbol,
        log_return
      ) %>%
      pivot_wider(
        names_from = symbol,
        values_from = log_return
      )
    
  })
  
  
  correlation_matrix <- reactive({
    
    x <- correlation_data()
    
    req(
      nrow(x) >= 2,
      ncol(x) >= 3
    )
    
    r <- cor(
      x %>%
        select(-date),
      use = "pairwise.complete.obs"
    )
    
    r
    
  })
  
  
  output$correlation_table <- renderTable({
    
    r <- correlation_matrix()
    
    result <- as.data.frame(
      round(
        r,
        3
      )
    )
    
    result$股票 <- rownames(
      result
    )
    
    result <- result %>%
      select(
        股票,
        everything()
      )
    
    names(result)[-1] <-
      vapply(
        names(result)[-1],
        function(s) {
          get_stock_label(s)
        },
        character(1)
      )
    
    result
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  output$correlation_heatmap <- renderPlot({
    
    r <- correlation_matrix()
    
    long <- as.data.frame(
      as.table(r)
    )
    
    names(long) <- c(
      "Stock1",
      "Stock2",
      "Correlation"
    )
    
    ggplot(
      long,
      aes(
        x = Stock1,
        y = Stock2,
        fill = Correlation
      )
    ) +
      geom_tile(
        color = "white"
      ) +
      geom_text(
        aes(
          label =
            sprintf(
              "%.2f",
              Correlation
            )
        ),
        size = 5
      ) +
      scale_fill_gradient2(
        limits = c(-1, 1),
        midpoint = 0
      ) +
      scale_x_discrete(
        labels = function(z) {
          vapply(
            z,
            get_stock_name,
            character(1)
          )
        }
      ) +
      scale_y_discrete(
        labels = function(z) {
          vapply(
            z,
            get_stock_name,
            character(1)
          )
        }
      ) +
      labs(
        title = "日對數報酬相關係數熱圖",
        x = NULL,
        y = NULL,
        fill = "相關係數"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold"
        ),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
  })
  
  
  # ==========================================================
  # 8. 投資組合
  # ==========================================================
  
  # ----------------------------------------------------------
  # 動態交易設定
  # ----------------------------------------------------------
  
  output$portfolio_trade_inputs <- renderUI({
    
    req(
      input$portfolio_stock
    )
    
    tagList(
      
      lapply(
        
        input$portfolio_stock,
        
        function(symbol) {
          
          x <- data() %>%
            filter(
              symbol == !!symbol,
              is.finite(adjusted)
            ) %>%
            arrange(date)
          
          if (nrow(x) == 0) {
            return(
              div(
                class = "warning-note",
                paste0(
                  get_stock_label(symbol),
                  " 沒有可用資料。"
                )
              )
            )
          }
          
          dates <- unique(
            x$date
          )
          buy_default <- nearest_date(
            dates,
            max(
              min(dates),
              max(dates) - 365
            )
          )
          
          sell_default <- nearest_date(
            dates,
            max(dates)
          )
          
          if (
            sell_default < buy_default
          ) {
            sell_default <- max(dates)
          }
          
          div(
            
            class = "portfolio-trade-box",
            
            h4(
              get_stock_label(symbol)
            ),
            
            fluidRow(
              
              column(
                
                4,
                
                dateInput(
                  paste0(
                    "portfolio_buy_",
                    symbol
                  ),
                  "買入日期：",
                  value = buy_default,
                  min = min(dates),
                  max = max(dates),
                  format = "yyyy-mm-dd",
                  language = "zh-TW"
                )
                
              ),
              
              column(
                
                4,
                
                dateInput(
                  paste0(
                    "portfolio_sell_",
                    symbol
                  ),
                  "賣出日期：",
                  value = sell_default,
                  min = min(dates),
                  max = max(dates),
                  format = "yyyy-mm-dd",
                  language = "zh-TW"
                )
                
              ),
              
              column(
                
                4,
                
                if (
                  input$portfolio_trade_mode ==
                  "以投入金額設定"
                ) {
                  
                  numericInput(
                    paste0(
                      "portfolio_amount_",
                      symbol
                    ),
                    "配置資金（元）：",
                    value = 100000,
                    min = 0,
                    step = 10000
                  )
                  
                } else {
                  
                  numericInput(
                    paste0(
                      "portfolio_shares_",
                      symbol
                    ),
                    "持有股數：",
                    value = 1000,
                    min = 0,
                    step = TRADE_UNIT
                  )
                }
                
              )
            ),
            
            p(
              class = "control-description",
              "若輸入日期不是交易日，計算時會自動使用該股票最近的有效交易日。"
            )
            
          )
          
        }
        
      )
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 手續費函數
  # ----------------------------------------------------------
  
  brokerage_fee <- function(
    value
  ) {
    
    rate <- max(
      0,
      input$brokerage_rate
    ) / 100
    
    minimum <- max(
      0,
      input$minimum_fee
    )
    
    if (
      !is.finite(value) ||
      value <= 0
    ) {
      return(0)
    }
    
    max(
      value * rate,
      minimum
    )
    
  }
  
  
  # ----------------------------------------------------------
  # 證交稅
  # ----------------------------------------------------------
  
  sell_tax <- function(
    symbol,
    value
  ) {
    
    if (
      !is.finite(value) ||
      value <= 0
    ) {
      return(0)
    }
    
    rate <- if (
      symbol == "0050"
    ) {
      input$etf_tax_rate
    } else {
      input$stock_tax_rate
    }
    
    value * max(
      0,
      rate
    ) / 100
    
  }
  
  
  # ----------------------------------------------------------
  # 股票最近有效價格
  # ----------------------------------------------------------
  
  price_on_or_before <- function(
    symbol,
    d
  ) {
    
    x <- data() %>%
      filter(
        symbol == !!symbol,
        date <= d,
        is.finite(adjusted)
      ) %>%
      arrange(
        desc(date)
      ) %>%
      slice_head(
        n = 1
      )
    
    if (
      nrow(x) == 0
    ) {
      return(NA_real_)
    }
    
    x$adjusted[1]
  }
  
  
  # ----------------------------------------------------------
  # 投資組合交易資料
  # ----------------------------------------------------------
  
  portfolio_trades <- reactive({
    
    req(
      input$portfolio_stock
    )
    
    result <- lapply(
      
      input$portfolio_stock,
      
      function(symbol) {
        
        x <- data() %>%
          filter(
            symbol == !!symbol,
            is.finite(adjusted)
          ) %>%
          arrange(date)
        
        if (
          nrow(x) == 0
        ) {
          return(NULL)
        }
        
        dates <- unique(
          x$date
        )
        
        buy_input <- input[[
          paste0(
            "portfolio_buy_",
            symbol
          )
        ]]
        
        sell_input <- input[[
          paste0(
            "portfolio_sell_",
            symbol
          )
        ]]
        
        if (
          is.null(buy_input) ||
          is.null(sell_input)
        ) {
          return(NULL)
        }
        
        buy_date <- nearest_date(
          dates,
          buy_input
        )
        
        sell_date <- nearest_date(
          dates,
          sell_input
        )
        
        if (
          is.na(buy_date) ||
          is.na(sell_date) ||
          sell_date < buy_date
        ) {
          
          return(
            data.frame(
              股票代號 = symbol,
              股票名稱 = get_stock_name(symbol),
              狀態 = "日期設定無效",
              stringsAsFactors = FALSE
            )
          )
        }
        
        buy_price <- price_on_or_before(
          symbol,
          buy_date
        )
        
        sell_price <- price_on_or_before(
          symbol,
          sell_date
        )
        
        if (
          !is.finite(buy_price) ||
          !is.finite(sell_price)
        ) {
          return(NULL)
        }
        
        if (
          input$portfolio_trade_mode ==
          "以投入金額設定"
        ) {
          
          amount_input <- input[[
            paste0(
              "portfolio_amount_",
              symbol
            )
          ]]
          
          amount_input <- gsub(
            ",",
            "",
            amount_input
          )
          
          amount <- suppressWarnings(
            as.numeric(amount_input)
          )
          
          if (
            !is.finite(amount)
          ) {
            amount <- 0
          }
          
          amount <- max(
            0,
            amount
          )
          
          shares <- floor(
            amount /
              (
                buy_price *
                  TRADE_UNIT
              )
          ) *
            TRADE_UNIT
          
          while (
            shares > 0 &&
            (
              shares *
              buy_price +
              brokerage_fee(
                shares *
                buy_price
              )
            ) > amount
          ) {
            
            shares <-
              shares -
              TRADE_UNIT
            
          }
          
          allocation <- amount
          
        } else {
          
          shares <- max(
            0,
            floor(
              input[[
                paste0(
                  "portfolio_shares_",
                  symbol
                )
              ]] /
                TRADE_UNIT
            ) *
              TRADE_UNIT
          )
          
          allocation <-
            shares *
            buy_price +
            brokerage_fee(
              shares *
                buy_price
            )
        }
        
        buy_value <-
          shares *
          buy_price
        
        buy_fee <-
          brokerage_fee(
            buy_value
          )
        
        sell_value <-
          shares *
          sell_price
        
        sell_fee <-
          brokerage_fee(
            sell_value
          )
        
        tax <-
          sell_tax(
            symbol,
            sell_value
          )
        
        net_sell <-
          sell_value -
          sell_fee -
          tax
        
        data.frame(
          
          股票代號 = symbol,
          
          股票名稱 =
            get_stock_name(symbol),
          
          買入日期 =
            buy_date,
          
          買入價格 =
            buy_price,
          
          賣出日期 =
            sell_date,
          
          賣出價格 =
            sell_price,
          
          股數 =
            shares,
          
          配置資金 =
            allocation,
          
          買入成交金額 =
            buy_value,
          
          買入手續費 =
            buy_fee,
          
          實際投入資金 =
            buy_value +
            buy_fee,
          
          賣出成交金額 =
            sell_value,
          
          賣出手續費 =
            sell_fee,
          
          證交稅 =
            tax,
          
          賣出後所得 =
            net_sell,
          
          狀態 = ifelse(
            shares > 0,
            "有效",
            "股數為 0"
          ),
          
          stringsAsFactors =
            FALSE
          
        )
      }
    )
    
    trades <- bind_rows(
      result
    )
    
    # --------------------------------------------------------
    # 確保交易資料具有固定欄位
    # --------------------------------------------------------
    
    required_cols <- c(
      "股票代號",
      "股票名稱",
      "買入日期",
      "買入價格",
      "賣出日期",
      "賣出價格",
      "股數",
      "配置資金",
      "買入成交金額",
      "買入手續費",
      "實際投入資金",
      "賣出成交金額",
      "賣出手續費",
      "證交稅",
      "賣出後所得",
      "狀態"
    )
    
    date_cols <- c(
      "買入日期",
      "賣出日期"
    )
    
    character_cols <- c(
      "股票代號",
      "股票名稱",
      "狀態"
    )
    
    for (
      nm in setdiff(
        required_cols,
        names(trades)
      )
    ) {
      
      if (
        nm %in% date_cols
      ) {
        
        trades[[nm]] <- as.Date(
          NA
        )
        
      } else if (
        nm %in% character_cols
      ) {
        
        trades[[nm]] <- NA_character_
        
      } else {
        
        trades[[nm]] <- NA_real_
        
      }
    }
    
    trades <- trades[
      ,
      required_cols,
      drop = FALSE
    ]
    
    trades
    
  })
  
  
  output$portfolio_trade_table <- renderTable({
    
    x <- portfolio_trades()
    
    req(
      nrow(x) > 0
    )
    
    x %>%
      mutate(
        買入日期 =
          ifelse(
            is.na(買入日期),
            "NA",
            format(
              as.Date(買入日期),
              "%Y-%m-%d"
            )
          ),
        賣出日期 =
          ifelse(
            is.na(賣出日期),
            "NA",
            format(
              as.Date(賣出日期),
              "%Y-%m-%d"
            )
          ),
        買入價格 =
          fmt_price(買入價格),
        賣出價格 =
          fmt_price(賣出價格),
        股數 =
          fmt_num(股數),
        配置資金 =
          fmt_num(配置資金, 2),
        買入成交金額 =
          fmt_num(買入成交金額, 2),
        買入手續費 =
          fmt_num(買入手續費, 2),
        實際投入資金 =
          fmt_num(實際投入資金, 2),
        賣出成交金額 =
          fmt_num(賣出成交金額, 2),
        賣出手續費 =
          fmt_num(賣出手續費, 2),
        證交稅 =
          fmt_num(證交稅, 2),
        賣出後所得 =
          fmt_num(賣出後所得, 2)
      )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 投資組合每日資產價值
  # ----------------------------------------------------------
  
  portfolio_daily <- reactive({
    
    trades <- portfolio_trades() %>%
      filter(
        狀態 == "有效",
        股數 > 0,
        is.finite(買入價格),
        is.finite(賣出價格)
      )
    
    req(
      nrow(trades) > 0
    )
    
    # --------------------------------------------------------
    # 初始資金
    # --------------------------------------------------------
    
    initial_capital <- if (
      input$portfolio_trade_mode ==
      "以投入金額設定"
    ) {
      
      sum(
        trades$配置資金,
        na.rm = TRUE
      )
      
    } else {
      
      sum(
        trades$實際投入資金,
        na.rm = TRUE
      )
    }
    
    req(
      is.finite(initial_capital),
      initial_capital > 0
    )
    
    
    # --------------------------------------------------------
    # 投資組合期間
    # 第一筆買入 → 最後一筆賣出
    # --------------------------------------------------------
    
    start_date <- min(
      trades$買入日期,
      na.rm = TRUE
    )
    
    end_date <- max(
      trades$賣出日期,
      na.rm = TRUE
    )
    
    req(
      is.finite(start_date),
      is.finite(end_date),
      start_date <= end_date
    )
    
    
    # 只使用投資組合期間內的交易日
    dates <- trading_dates()[
      trading_dates() >= start_date &
        trading_dates() <= end_date
    ]
    
    req(
      length(dates) > 0
    )
    
    
    # --------------------------------------------------------
    # 每一個交易日計算
    # --------------------------------------------------------
    
    result <- lapply(
      
      dates,
      
      function(d) {
        
        # ======================================================
        # 1. 從初始資金開始
        # ======================================================
        
        cash <- initial_capital
        
        
        # ======================================================
        # 2. 扣除截至今天已經完成的買入
        # ======================================================
        
        bought <- trades[
          trades$買入日期 <= d,
          ,
          drop = FALSE
        ]
        
        if (
          nrow(bought) > 0
        ) {
          
          cash <-
            cash -
            sum(
              bought$實際投入資金,
              na.rm = TRUE
            )
          
        }
        
        
        # ======================================================
        # 3. 加回截至今天已經完成的賣出
        # ======================================================
        
        sold <- trades[
          trades$賣出日期 <= d,
          ,
          drop = FALSE
        ]
        
        if (
          nrow(sold) > 0
        ) {
          
          cash <-
            cash +
            sum(
              sold$賣出後所得,
              na.rm = TRUE
            )
          
        }
        
        
        # ======================================================
        # 4. 計算目前仍然持有的股票市值
        # ======================================================
        
        holdings <- 0
        
        active_trades <- trades[
          trades$買入日期 <= d &
            trades$賣出日期 > d,
          ,
          drop = FALSE
        ]
        
        if (
          nrow(active_trades) > 0
        ) {
          
          for (
            i in seq_len(
              nrow(active_trades)
            )
          ) {
            
            tr <- active_trades[i, ]
            
            current_price <- price_on_or_before(
              tr$股票代號,
              d
            )
            
            if (
              is.finite(current_price)
            ) {
              
              holdings <-
                holdings +
                tr$股數 *
                current_price
              
            }
          }
        }
        
        
        # ======================================================
        # 5. 投資組合總資產
        # ======================================================
        
        portfolio_value <-
          cash +
          holdings
        
        
        data.frame(
          date = d,
          cash = cash,
          holdings = holdings,
          portfolio_value = portfolio_value,
          stringsAsFactors = FALSE
        )
        
      }
      
    )
    
    
    result <- bind_rows(
      result
    )
    
    
    # --------------------------------------------------------
    # 數值安全檢查
    # --------------------------------------------------------
    
    result <- result %>%
      mutate(
        cash = ifelse(
          is.finite(cash),
          cash,
          NA_real_
        ),
        holdings = ifelse(
          is.finite(holdings),
          holdings,
          NA_real_
        ),
        portfolio_value = ifelse(
          is.finite(portfolio_value),
          portfolio_value,
          NA_real_
        )
      )
    
    
    result
    
  })
  
  
  output$portfolio_value_chart <- renderPlot({
    
    x <- portfolio_daily()
    
    req(
      input$portfolio_value_date_range
    )
    
    x <- x %>%
      filter(
        date >= as.Date(
          input$portfolio_value_date_range[1]
        ),
        date <= as.Date(
          input$portfolio_value_date_range[2]
        )
      )
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      x,
      aes(
        x = date,
        y = portfolio_value
      )
    ) +
      geom_line(
        linewidth = 0.9
      ) +
      labs(
        title = "投資組合每日資產價值",
        x = "日期",
        y = "資產價值（元）"
      ) +
      scale_y_continuous(
        labels = function(z) {
          format(
            round(z),
            big.mark = ","
          )
        }
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold"
        ),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 投資組合績效摘要
  # ----------------------------------------------------------
  
  portfolio_metrics <- reactive({
    
    x <- portfolio_daily()
    trades <- portfolio_trades() %>%
      filter(
        狀態 == "有效",
        股數 > 0
      )
    
    initial <- if (
      input$portfolio_trade_mode ==
      "以投入金額設定"
    ) {
      sum(
        trades$配置資金,
        na.rm = TRUE
      )
    } else {
      sum(
        trades$實際投入資金,
        na.rm = TRUE
      )
    }
    
    final <- tail(
      x$portfolio_value,
      1
    )
    
    daily_ret <- log(
      x$portfolio_value /
        lag(x$portfolio_value)
    )
    
    daily_ret <-
      daily_ret[
        is.finite(daily_ret)
      ]
    
    total_return <-
      final /
      initial - 1
    
    ann_return <-
      annualized_return(
        initial,
        final,
        nrow(x)
      )
    
    ann_vol <-
      sd(
        daily_ret,
        na.rm = TRUE
      ) *
      sqrt(
        TRADING_DAYS_PER_YEAR
      )
    
    sharpe <-
      if (
        is.finite(ann_vol) &&
        ann_vol > 0
      ) {
        mean(
          daily_ret,
          na.rm = TRUE
        ) *
          TRADING_DAYS_PER_YEAR /
          ann_vol
      } else {
        NA_real_
      }
    
    dd <- drawdown_value(
      x$portfolio_value
    )
    
    max_dd <- min(
      dd,
      na.rm = TRUE
    )
    
    list(
      initial = initial,
      final = final,
      total_return = total_return,
      annual_return = ann_return,
      annual_volatility = ann_vol,
      sharpe = sharpe,
      max_drawdown = max_dd,
      days = nrow(x)
    )
    
  })
  
  
  output$portfolio_summary <- renderTable({
    
    m <- portfolio_metrics()
    
    data.frame(
      
      指標 = c(
        "初始配置資金",
        "期末資產價值",
        "累積報酬率",
        "年化報酬率",
        "有效交易日數"
      ),
      
      數值 = c(
        
        fmt_num(
          m$initial,
          0
        ),
        
        fmt_num(
          m$final,
          2
        ),
        
        fmt_pct(
          m$total_return
        ),
        
        fmt_pct(
          m$annual_return
        ),
        
        fmt_num(
          m$days
        )
      ),
      
      stringsAsFactors = FALSE
      
    )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  output$portfolio_performance_metrics <- renderTable({
    
    m <- portfolio_metrics()
    
    data.frame(
      
      指標 = c(
        "年化報酬率",
        "年化波動度",
        "最大回撤",
        "夏普比率"
      ),
      
      數值 = c(
        
        fmt_pct(
          m$annual_return
        ),
        
        fmt_pct(
          m$annual_volatility,
          2,
          FALSE
        ),
        
        fmt_pct(
          m$max_drawdown
        ),
        
        ifelse(
          is.finite(m$sharpe),
          sprintf(
            "%.3f",
            m$sharpe
          ),
          "NA"
        )
      ),
      
      stringsAsFactors = FALSE
      
    )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 投資組合與 0050 比較
  # ----------------------------------------------------------
  
  portfolio_benchmark <- reactive({
    
    p <- portfolio_daily()
    
    req(
      nrow(p) > 1
    )
    
    start <- min(
      p$date
    )
    
    end <- max(
      p$date
    )
    
    b <- data() %>%
      filter(
        symbol == "0050",
        date >= start,
        date <= end,
        is.finite(adjusted)
      ) %>%
      arrange(date)
    
    req(
      nrow(b) > 1
    )
    
    p_return <-
      p$portfolio_value /
      first(p$portfolio_value) - 1
    
    b_return <-
      b$adjusted /
      first(b$adjusted) - 1
    
    p2 <- data.frame(
      date = p$date,
      portfolio_return = p_return
    )
    
    b2 <- data.frame(
      date = b$date,
      benchmark_return = b_return
    )
    
    list(
      portfolio = p2,
      benchmark = b2
    )
    
  })
  
  
  output$portfolio_benchmark_summary <- renderTable({
    
    x <- portfolio_benchmark()
    
    p <- tail(
      x$portfolio$portfolio_return,
      1
    )
    
    b <- tail(
      x$benchmark$benchmark_return,
      1
    )
    
    data.frame(
      
      指標 = c(
        "投資組合",
        "0050",
        "超額報酬"
      ),
      
      累積報酬率 = c(
        fmt_pct(p),
        fmt_pct(b),
        fmt_pct(p - b)
      ),
      
      stringsAsFactors = FALSE
      
    )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  output$portfolio_benchmark_chart <- renderPlot({
    
    x <- portfolio_benchmark()
    
    p <- x$portfolio
    b <- x$benchmark
    
    merged <- full_join(
      p,
      b,
      by = "date"
    ) %>%
      arrange(date)
    
    ggplot(
      merged,
      aes(
        x = date
      )
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_line(
        aes(
          y = portfolio_return,
          color = "投資組合"
        ),
        linewidth = 1
      ) +
      
      geom_line(
        aes(
          y = benchmark_return,
          color = "0050"
        ),
        linewidth = 1,
        linetype = "dashed"
      ) +
      
      scale_color_manual(
        values = c(
          "投資組合" = "steelblue",
          "0050" = "red"
        )
      ) +
      
      labs(
        title = "投資組合與 0050 累積報酬比較",
        x = "日期",
        y = "累積報酬率",
        color = "比較標的"
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(z * 100, 1),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold"
        ),
        legend.position = "bottom",
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
  })
  
  
  # ==========================================================
  # 9. 資料明細
  # ==========================================================
  
  output$stock_table <- renderTable({
    
    x <- data()
    
    symbols <- c(
      "0050",
      stock_info$symbol
    )
    
    result <- lapply(
      symbols,
      function(s) {
        
        z <- x %>%
          filter(
            symbol == s,
            is.finite(adjusted)
          )
        
        if (
          nrow(z) == 0
        ) {
          return(NULL)
        }
        
        data.frame(
          
          股票代號 = s,
          
          股票名稱 =
            get_stock_name(s),
          
          產業 =
            get_stock_industry(s),
          
          最早日期 =
            format(
              min(z$date),
              "%Y-%m-%d"
            ),
          
          最新日期 =
            format(
              max(z$date),
              "%Y-%m-%d"
            ),
          
          有效觀測數 =
            nrow(z),
          
          stringsAsFactors =
            FALSE
          
        )
      }
    )
    
    bind_rows(
      result
    )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ==========================================================
  # 10. 日期同步
  # ==========================================================
  
  # ----------------------------------------------------------
  # 市場價格圖：資料更新後同步可選日期範圍
  # ----------------------------------------------------------
  
  observe({
    
    req(
      latest_date(),
      earliest_date()
    )
    
    updateDateRangeInput(
      session,
      "market_chart_date_range",
      min = earliest_date(),
      max = latest_date()
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 投資組合：跟著實際交易期間同步
  # ----------------------------------------------------------
  
  observeEvent(
    portfolio_trades(),
    {
      
      trades <- portfolio_trades() %>%
        filter(
          狀態 == "有效",
          股數 > 0,
          !is.na(買入日期),
          !is.na(賣出日期)
        )
      
      if (
        nrow(trades) == 0
      ) {
        return()
      }
      
      portfolio_start <- min(
        trades$買入日期,
        na.rm = TRUE
      )
      
      portfolio_end <- max(
        trades$賣出日期,
        na.rm = TRUE
      )
      
      if (
        !is.finite(portfolio_start) ||
        !is.finite(portfolio_end) ||
        portfolio_start > portfolio_end
      ) {
        return()
      }
      
      updateDateRangeInput(
        session,
        "portfolio_value_date_range",
        start = portfolio_start,
        end = portfolio_end,
        min = earliest_date(),
        max = latest_date()
      )
      
    },
    ignoreInit = FALSE
  )
  
  
  # ==========================================================
  # 11. 結束 Server
  # ==========================================================
  
}


# ============================================================
# 12. 啟動 Shiny
# ============================================================

shinyApp(
  ui = ui,
  server = server
)

}
