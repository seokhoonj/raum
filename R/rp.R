make_base_risk <- function(no = 0, risk = "base", gender = c(1L, 2L), age = 0:120, grade = 0, rate = 1.) {
  base_risk <- data.table(expand.grid(
    no = no, risk = risk, gender = as.factor(gender), age = age, grade = grade, rate = rate,
    stringsAsFactors = FALSE))
  setorder(base_risk, no, risk, gender, age, grade)
  return(base_risk)
}

join_info <- function(risk_info, claim_info) {
  tot_info <- risk_info[claim_info, on = .(risk)]
  tot_info[risk_info, on = .(risk2 = risk, age = age, gender = gender),
           rate2 := i.rate]
  # remove the risk having several payment condition
  unique(tot_info[, .(
    age, gender, grade,
    rn, rider,
    risk, risk2,
    rate, rate2,
    amount_mean,
    rp_times, claim_times,
    reduction_period_start,
    reduction_period_end,
    reduction_period_ratio,
    proportion)
  ])
}

to_monthly_lapse <- function(x) 1-(1-x)^(1/12)

lapse2dist <- function(x) {
  p <- cumprod(1-x)
  abs(diff(c(1, p, 0)))
}

dist2lapse <- function(x) {
  c(x[1L], 1-exp(diff(log(1-cumsum(x)))))
}

random_pay_num <- function(df, lapse, mon, seed) {
  uid <- unique(df$id)
  prob <- lapse2dist(lapse[1:(mon-1)])
  if (!missing(seed)) set.seed(seed)
  structure(colvec(sample(
    seq_len(mon), size = length(uid), replace = T, prob = prob)),
    dimnames = list(uid, NULL))
}

get_rp_matrix <- function(risk_info, claim_info, igender, iage, igrade, mon, prop = F, unit = 1, type = c("rider", "risk")) {
  rn <- unique(claim_info$rn)
  rider <- unique(claim_info$rider)
  rider_list <- vector(mode = "list", length(rider))
  for (j in seq_along(rider)) {
    # variables
    iages <- iage + seq_len((mon-1)%/%12 + 1L) - 1L
    igrades <- unique(c(0, igrade))
    # join info
    rid_info <- claim_info[rider == rider[j]]
    tot_info <- join_info(risk_info, rid_info)
    app_info <- tot_info[(gender == igender) & (age %in% iages) & (grade %in% igrades)]
    # info subset
    if (!nrow(app_info))
      stop("The insured has no risk rate")
    risk_list <- vector(mode = "list", length(nrow(rid_info)))
    for (i in seq_len(nrow(rid_info))) {
      app_tbl <- app_info[(risk == rid_info$risk[i] &
                           reduction_period_start == rid_info$reduction_period_start[i] &
                           reduction_period_end   == rid_info$reduction_period_end[i] &
                           reduction_period_ratio == rid_info$reduction_period_ratio[i])]
      rp_tbl <- app_tbl[, .(age, rp = rate * rate2 * amount_mean / 12)]
      rp_mat <- reprow(rowvec(rp_tbl$rp), 12)
      pd_mat <- numbers(dim(rp_mat))
      reduction_period_start  <- app_tbl$reduction_period_start
      reduction_period_end    <- app_tbl$reduction_period_end
      reduction_period_ratio  <- app_tbl$reduction_period_ratio
      rd_mat <- ratio_by_period(pd_mat,
                                reduction_period_start,
                                reduction_period_end,
                                reduction_period_ratio) # to be modified
      rp <- structure(rp_mat * rd_mat, dimnames = list(1:12, app_tbl$age))
      risk_list[[i]] <- structure(
        colvec(rp[seq_len(mon)]), dimnames = list(NULL, rid_info$risk[i]))
    } # waiting period?
    z <- row_min_by_cn(do.call("cbind", risk_list))
    if (type[[1L]] == "rider") {
      z <- structure(colvec(row_sum(z)), dimnames = list(NULL, rn[j]))
    }
    rider_list[[j]] <- z
  }
  zs <- do.call("cbind", rider_list)
  setrownames(zs, seq_len(mon))
  return(zs)
}

apply_expiration <- function(x, expiration) {
  #       1    1    1    1
  # 1     7   28   60   60
  # 2    51   52   60   60
  # expiration <- c(0, 0, 0, 0) row_max_by_cn 계속
  # expiration <- c(1, 1, 1, 1) row_min_by_cn 하나라도 해당하면 종료
  # expiration <- c(2, 2, 2, 2) row_max_by_cn 모두 해당하면 종료
  # expiration <- c(0, 0, 1, 1) row_min_by_cn 1인 것 중 하나라도 해당하면 종료
  # expiration <- c(2, 2, 0, 0) row_max_by_cn 2인 것이 모두 해당하면 종료
  rn <- colnames(x)
  ur <- unique(rn)
  ex <- structure(rowvec(expiration), dimnames = list(NULL, colnames(x))) # 0, 1, 2
  em <- row_max_by_cn(ex) # maximum expiration

  z <- vector(mode = "list", length = length(ur))
  for (i in seq_along(ur)) {
    col_em <- icol(em, ur[i])
    if (em[, col_em] == 0L) {
      col_x0 <- icol(x, ur[i])
      x0 <- x[, col_x0, drop = F]
      mx <- row_max_by_cn(x0)
      z[[i]] <- mx
    } else if (em[, col_em] == 1L) {
      # select columns
      col_ex1 <- icol(ex, ur[i])
      col_x1 <- icol(x , ur[i])
      # subset columns
      ex1 <- ex[, col_ex1, drop = F]
      x1 <- x[, col_x1, drop = F]
      z[[i]] <- row_min_by_cn(x1[, which(ex1 == 1L), drop = F])
    } else if (em[, col_em] == 2L) {
      # select columns
      col_ex2 <- icol(ex, ur[i])
      col_x2  <- icol(x, ur[i])
      # subset columns
      ex2 <- ex[, col_ex2, drop = F]
      x2  <- x[, col_x2, drop = F]
      z[[i]] <- row_max_by_cn(x2[, which(ex2 == 2L), drop = F])
    }
  }
  return(do.call("cbind", z))
}

count_pay_num <- function(claim_info, df, udate, mon, waiting = T) {
  # check claim information
  has_cols(claim_info, c("cvd_kcd", "rider", "rn", "one_time"))
  has_cols(df, c("id", "sdate", "edate", "kcd"))

  # variables (claim_info)
  rn         <- claim_info$rn
  rider      <- claim_info$rider
  cvd_kcd    <- claim_info$cvd_kcd
  one_time   <- claim_info$one_time
  expiration <- claim_info$expiration
  waiting_period_start <- claim_info$waiting_period_start
  waiting_period_end   <- claim_info$waiting_period_end

  # order
  setorder(df, id, sdate, edate)

  # variables (clm)
  id    <- df$id
  kcd   <- df$kcd
  sdate <- df$sdate

  # check the last claim point
  clm <- check_cvd_kcd(cvd_kcd, kcd)
  set_only_first(clm, id, one_time)

  # check the month from the underwriting
  period <- diff_period(udate, sdate)
  period[is.na(period)] <- mon # no sdate (for the data with no claims)
  clm_prd <- as_integer(repcol(colvec(period), length(rn)))
  # check waiting period
  if (waiting) {
    clm_wai <- as_integer(ratio_by_period(as_double(clm_prd), waiting_period_start*12, waiting_period_end*12, rep(0, length(rn))))
    setmul(clm, clm_wai)
  }
  # payment before the first claim (Don't be confused due to 'max')
  replace_val_in_mat(clm_prd, 0L, clm, 0L) # set_num_on_other_mat_loc
  setdimnames(clm_prd, list(id, rn))
  clm_prd_max <- row_max_by_rn(clm_prd) # 한 ID에 여러 개의 month 가 있으면 가장 큰 월도를 가져옴 (one_time이 아닌 column이므로 추후 통일 됨)
  replace_val_in_mat(clm_prd_max, as_integer(mon), clm_prd_max, 0L)
  # modify no of premium payment by method of benefit payment
  clm_prd_cap <- structure(ones(dim(clm_prd_max)) * mon, dimnames = dimnames(clm_prd_max))
  m <- overlap_matrix(clm_prd_cap, clm_prd_max, one_time)
  z <- apply_expiration(m, expiration)
  z[z > mon] <- mon # set maximum month
  return(z)
}

rp_simulation <- function(risk_info, claim_info, df, udate, mon = 60, group = 1L, waiting = T,
                          lapse, unit = 1L, seed = 123) {
  # check columns
  has_cols(df, c("id", "gender", "age", "grade"))
  # order
  setorder(df, id, sdate, edate, kcd)
  # the insured having claim data
  insured <- unique(df[, .(id, age, gender, grade)])
  demo <- insured[, .(count = .N), keyby = .(gender, age, grade)]
  set(demo, j = "scale", value = minmax_scaler(demo$count))
  # count risk premium payment
  cat("Counting number of payments...\n")
  pay_count <- count_pay_num(claim_info, df, udate, mon, waiting = waiting)
  if (!missing(lapse)) {
    cat("Applying random lapse...\n")
    lapse_point <- random_pay_num(df, lapse, mon, seed = seed)
    lapse_point <- structure(repcol(lapse_point, each = ncol(pay_count)), dimnames = dimnames(pay_count))
    pay_count <- pmin(lapse_point, pay_count)
  }

  pay_count <- cbind(insured, pay_count)
  rp_list <- vector(mode = "list", length = nrow(demo))

  # repeat
  cat("Start calculating...\n")
  for (i in seq_len(nrow(demo))) {
    # variables
    count   <- demo$count[i]
    scale   <- demo$scale[i]
    iage    <- demo$age[i]
    igender <- demo$gender[i]
    igrade  <- unique(c(0, demo$grade[i]))
    # risk premium matrix
    rp  <- get_rp_matrix(risk_info, claim_info, igender, iage, igrade, mon, unit = unit) # Male: 1, Female: 2
    # subset pay_count
    ipay <- pay_count[age == iage & gender == igender & grade %in% igrade]
    # create variables
    pd <- rep((seq_len(mon)-1L) %/% group + 1L, nrow(ipay))
    id_mon <- rep(ipay$id, each = mon) # by month
    id_pd  <- rep(ipay$id, each = unilen(pd)) # by period (months grouped)
    pd_pd  <- rep(unique(pd), times = nrow(ipay))
    # subset ipay
    cols <- diff_cols(ipay, c("id", "age", "gender", "grade"))
    pay <- as.matrix(ipay[, ..cols])
    pay <- upper(rp, pay[,, drop = F])
    pay <- structure(pay, dimnames = list(paste(id_mon, pd), colnames(rp)))
    pay <- row_sum_by_rn(pay)
    # sum by scenario
    rp_list[[i]] <- data.table(id = id_pd, period = pd_pd, pay)
    # print
    cat(sprintf("Group %3d (gender: %d, age: %2d, grade: %d): %s %s\n",
                i, igender, iage, max(igrade),
                str_pad(comma(count), width = 9L), draw_line(scale*20)))
  }
  z <- do.call("rbind", rp_list)
  setnames(z, c("id", "period", sprintf(paste0("rp%0", max(nchar(claim_info$rn)), "s"), colnames(rp))))
  return(z)
}

apply_weight <- function(df, weight) {
  # columns
  loss_cols <- regmatch_cols(df, "^loss[0-9]+")
  rp_cols   <- regmatch_cols(df, "^rp[0-9]+")
  pre_cols  <- diff_cols(df, c(loss_cols, rp_cols))
  # weighted
  loss <- as.matrix(df[, ..loss_cols])
  rp <- as.matrix(df[, ..rp_cols])
  setmul(loss, weight, axis = 1)
  setmul(rp, weight, axis = 1)
  cbind(df[, ..pre_cols], data.table(loss), data.table(rp))
}

set_loss_sum <- function(df) {
  # columns
  loss_cols <- regmatch_cols(df, "^loss[0-9_]")
  s_loss <- paste0(paste0("df$", loss_cols), collapse = " + ")
  df[, loss := eval(parse(text = s_loss))]
}

# set_rp_sum <- function(df) {
#   # columns
#   rp_cols   <- regmatch_cols(df, "^rp[0-9_]")
#   s_rp   <- paste0(paste0("df$", rp_cols), collapse = " + ")
#   df[, rp   := eval(parse(text = s_rp  ))]
# }
#
# set_loss_rp_sum <- function(df) {
#   # columns
#   loss_cols <- regmatch_cols(df, "^loss[0-9_]")
#   rp_cols   <- regmatch_cols(df, "^rp[0-9_]")
#   pre_cols  <- diff_cols(df, c(loss_cols, rp_cols))
#
#   if (length(rp_cols) != length(loss_cols))
#     stop("loss columns != rp columns.", .call = FALSE)
#
#   s_loss <- paste0(paste0("df$", loss_cols), collapse = " + ")
#   s_rp   <- paste0(paste0("df$", rp_cols), collapse = " + ")
#   s_pre  <- paste0(pre_cols, collapse = ", ")
#   df[, loss := eval(parse(text = s_loss))]
#   df[, rp   := eval(parse(text = s_rp  ))]
# }
#
# group_loss_rp_sum <- function(df, group) {
#   # group
#   group <- match_cols(df, vapply(substitute(group), deparse, "character"))
#
#   # columns
#   loss_cols   <- regmatch_cols(df, "^loss")
#   rp_cols     <- regmatch_cols(df, "^rp")
#   ratio_cols  <- gsub("loss", "ratio", loss_cols)
#   sub_rp_cols <- gsub("loss", "rp", loss_cols)
#
#   # if (length(rp_cols) != length(loss_cols))
#   #   stop("loss columns != rp columns.", .call = FALSE)
#
#   # expression
#   s_n     <- paste("n = .N")
#   s_loss  <- paste(sprintf("%s = sum(%s)", loss_cols, loss_cols), collapse = ", ")
#   s_rp    <- paste(sprintf("%s = sum(%s)", rp_cols  , rp_cols  ), collapse = ", ")
#   s_ratio <- paste(sprintf("%s = sum(%s) / sum(%s)", ratio_cols, loss_cols, sub_rp_cols), collapse = ", ")
#   s_combi <- paste(c(s_n, s_loss, s_rp, s_ratio), collapse = ", ")
#   s_group <- paste(group, collapse = ", ")
#   s_final <- sprintf("df[, .(%s), .(%s)]", s_combi, s_group)
#
#   z <- eval(parse(text = s_final))
#   return(z)
# }
