
as_logical   <- function(x) .Call(raum_as_logical, x)
as_integer   <- function(x) .Call(raum_as_integer, x)
as_double    <- function(x) .Call(raum_as_double, x)
as_character <- function(x) .Call(raum_as_character, x)

# vector to matrix --------------------------------------------------------

colvec <- function(x) array(x, dim = c(length(x), 1L), dimnames = list(names(x), NULL))
rowvec <- function(x) array(x, dim = c(1L, length(x)), dimnames = list(NULL, names(x)))

# matrix ------------------------------------------------------------------

ones    <- function(dim) array(1L, dim = dim)
zeros   <- function(dim) array(0L, dim = dim)
strings <- function(dim) array(NA_character_, dim = dim)
numbers <- function(dim) array(seq_len(prod(dim)), dim = dim)
randoms <- function(dim, x = c(0L, 1L), replace = TRUE, prob = NULL)
  array(sample(x, size = prod(dim), replace = replace, prob = prob), dim = dim)

# names -------------------------------------------------------------------

setdimnames  <- function(x, dimnames) setattr(x, "dimnames", dimnames)
setrownames  <- function(x, rownames) setattr(x, "dimnames", list(rownames, colnames(x)))
setcolnames  <- function(x, colnames) setattr(x, "dimnames", list(rownames(x), colnames))
setfakenames <- function(x, dim) {
  rn <- k_split(seq_len(nrow(x)), k = dim[1L], number = TRUE)
  cn <- k_split(seq_len(ncol(x)), k = dim[2L], number = TRUE)
  setdimnames(x, list(rn, cn))
}

# transform ---------------------------------------------------------------

reverse  <- function(x) invisible(.Call(raum_reverse, x))
traverse <- function(x, y) .Call(raum_traverse, x, y)
rotate   <- function(x, angle) .Call(raum_rotate, x, angle)
repcol   <- function(x, each) .Call(raum_repcol, x, each)
upper    <- function(x, y) .Call(raum_upper, x, y)
uncumprod <- function(x) c(x[1L], exp(diff(log(x))))
only_first <- function(x, id, ot) .Call(raum_only_first, x, id, ot)
set_only_first <- function(x, id, ot) invisible(.Call(raum_set_only_first, x, id, ot))
one_upper_first <- function(x, id) {
  z <- .Call(raum_one_upper_first, x, id)
  setdimnames(z, dimnames(x))
  return(z)
}
set_one_upper_first <- function(x, id) invisible(.Call(raum_set_one_upper_first, x, id))


# replication functions ---------------------------------------------------

#' Data replication function
#'
#' This function is for replicating data rows
#' @title Data replication function
#' @param x matrix, data.frame, data.table.
#' @param ... times, each
#' @examples
#' reprow(iris, times = 3)
#' reprow(iris, each  = 3)
#' @export
reprow <- function(x, ...) UseMethod("reprow")

#' @rdname reprow
#' @export
reprow.matrix <- function(x, ...) do.call(cbind, lapply(seq_len(ncol(x)), function(s) rep(x[, s], ...)))
#' @rdname reprow
#' @export
reprow.data.frame <- function(x, ...) as.data.frame(lapply(x, rep, ...)) # times, each

#' @rdname reprow
#' @export
reprow.data.table <- function(x, ...) as.data.table(lapply(x, rep, ...)) # times, each

# row-based statistics ----------------------------------------------------

row_min <- function(x) .Call(raum_row_min, x)
row_max <- function(x) .Call(raum_row_max, x)
row_sum <- function(x) .Call(raum_row_sum, x)

row_min_by_rn <- function(x, na.rm = TRUE) {
  g <- rownames(x)
  uniqueg <- unique(g)
  maxval <- max(x)
  .Call(raum_row_min_by_rn, x, g, uniqueg, na.rm, maxval)
}
row_max_by_rn <- function(x, na.rm = TRUE) {
  g <- rownames(x)
  uniqueg <- unique(g)
  minval <- min(x)
  .Call(raum_row_max_by_rn, x, g, uniqueg, na.rm, minval)
}
row_sum_by_rn <- function(x, na.rm = TRUE) {
  g <- rownames(x)
  uniqueg <- unique(g)
  .Call(raum_row_sum_by_rn, x, g, uniqueg, na.rm)
}

row_min_by_cn <- function(x, na.rm = TRUE) {
  g <- colnames(x)
  uniqueg <- unique(g)
  maxval <- max(x)
  .Call(raum_row_min_by_cn, x, g, uniqueg, na.rm, maxval)
}
row_max_by_cn <- function(x, na.rm = TRUE) {
  g <- colnames(x)
  uniqueg <- unique(g)
  minval <- min(x)
  .Call(raum_row_max_by_cn, x, g, uniqueg, na.rm, minval)
}
row_sum_by_cn <- function(x, na.rm = TRUE) {
  g <- colnames(x)
  uniqueg <- unique(g)
  .Call(raum_row_sum_by_cn, x, g, uniqueg, na.rm)
}

row_min_by_dn <- function(x, na.rm = TRUE) {
  row_min_by_rn(row_min_by_cn(x, na.rm = na.rm), na.rm = na.rm)
}
row_max_by_dn <- function(x, na.rm = TRUE) {
  row_max_by_rn(row_max_by_cn(x, na.rm = na.rm), na.rm = na.rm)
}
row_sum_by_dn <- function(x, na.rm = TRUE) {
  row_sum_by_rn(row_sum_by_cn(x, na.rm = na.rm), na.rm = na.rm)
}

# unique ------------------------------------------------------------------

unilen <- function(x) .Call(raum_unilen, x)
sort_group_by <- function(x) .Call(raum_sort_group_by, x)

# hash --------------------------------------------------------------------

lookup  <- function(g, uniqueg) .Call(raum_lookup, g, uniqueg)
hashfun <- function(key, K) bitwShiftL(as.integer(3141592653 * key), (32 - K))

# replace -----------------------------------------------------------------

replace_vec_in_mat <- function(x, col, vec)
  invisible(.Call(raum_replace_vec_in_mat, x, col, vec))
replace_val_in_mat <- function(mat, val, refmat, refval)
  invisible(.Call(raum_replace_val_in_mat, mat, val, refmat, refval))
replace_num_na <- function(df) {
  class <- sapply(df, class)
  cols <- names(class)[which(class == "numeric")]
  df[, (cols) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = cols]
}
replace_chr_na <- function(df) {
  class <- sapply(df, class)
  cols <- names(class)[which(class == "character")]
  df[, (cols) := lapply(.SD, function(x) ifelse(is.na(x), "", x)), .SDcols = cols]
}


# calculation -------------------------------------------------------------

setmul <- function(x, y, axis = 1) {
  if (!is.matrix(x) | !is.numeric(x) | !is.numeric(y))
    stop("invalid input")
  ylen <- length(y)
  mdim <- if (!is.null(dim(y))) min(dim(y), na.rm = TRUE) else 0
  if (mdim > 1)
    return(invisible(.Call(raum_setmul_mat, x, y)))
  if (mdim <= 1 & ylen > 1 & axis == 1)
    return(invisible(.Call(raum_setmul_row, x, rowvec(y))))
  if (mdim <= 1 & ylen > 1 & axis == 2)
    return(invisible(.Call(raum_setmul_col, x, colvec(y))))
  if (ylen == 1)
    return(invisible(.Call(raum_setmul_num, x, y)))
}

#' Unique Length of Stay
#'
#' This function calculates unique length of stay between `from` to `to`.
#' @param df data.frame, data.table
#' @param id_var id variables
#' @param from start date
#' @param to end date
#' @examples
#' @export
count_stay <- function(df, id, from, to) {
  id   <- match_cols(df, vapply(substitute(id), deparse, "character"))
  from <- deparse(substitute(from))
  to   <- deparse(substitute(to))
  assert_class(df[[from]], "Date")
  assert_class(df[[to]], "Date")
  has_missing(df[[from]])
  has_missing(df[[to]])
  if (!is.data.table(df)) df <- as.data.table(df)
  setorderv(df, c(id, from, to))
  df_id <- df[, ..id]
  df_from <- as.integer(df[[from]])
  df_to <- as.integer(df[[to]])
  if (any(df_to - df_from < 0))
    stop("Some `from` are greater than `to`.")
  stay <- .Call(`raum_count_stay`, df_id, df_from, df_to)
  z <- cbind(unique(df_id), stay = stay)
  return(z)
}

# rcpp_count_stay <- function(df, id, from, to) {
#   id   <- match_cols(df, vapply(substitute(id), deparse, "character"))
#   from <- deparse(substitute(from))
#   to   <- deparse(substitute(to))
#   assert_class(df[[from]], "Date")
#   assert_class(df[[to]], "Date")
#   has_missing(df[[from]])
#   has_missing(df[[to]])
#   if (!is.data.table(df)) df <- as.data.table(df)
#   setorderv(df, c(id, from, to))
#   df_id <- df[, ..id]
#   df_from <- df[[from]]
#   df_to <- df[[to]]
#   if (any(df_to - df_from < 0))
#     stop("Some `from` are greater than `to`.")
#   stay <- .Call(`raum_rcpp_count_stay`, df_id, df_from, df_to)
#   z <- cbind(unique(df_id), stay = stay)
#   return(z)
# }

count_limit <- function(x, limit, waiting) {
  .Call(raum_count_limit, x, limit, waiting)
}

ratio_by_period <- function(x, start, end, ratio)
  .Call(raum_ratio_by_period, x, start, end, ratio)

set_ratio_by_period <- function(x, start, end, ratio)
  invisible(.Call(raum_set_ratio_by_period, x, start, end, ratio))

# kcd code functions ------------------------------------------------------

pste_code <- function(x, collapse = "|") paste0(x, collapse = collapse)
glue_code <- function(x, collapse = "|") paste0(unique(x[!is.na(x)]), collapse = collapse)
sort_code <- function(x, collapse = "|") paste0(sort(unique(x[!is.na(x)])), collapse = collapse)
splt_code <- function(x, split = "\\|") {z <- strsplit(x, split = split)[[1L]]; z[!z %in% c(NA, "NA", "")]}
srch_code <- function(x) glue_code(paste0(x, "$"))
melt_code <- function(x) srch_code(splt_code(pste_code(x)))
excl_code <- function(x) paste0('^((?!', x, ').)*$')
remv_code <- function(code, x) gsub(code, "", x)
pull_code <- function(code, x) {
  r <- regexpr(code, x, perl = TRUE)
  z <- rep(NA, length(x))
  z[r != -1] <- regmatches(x, r)
  return(z)
}
add_kcd <- function(df, col, dot = TRUE, lang = c("ko", "en")) {
  copybook <- copy(vuw::kcd_book)
  if (dot) rm_dot(copybook, kcd)
  col <- deparse(substitute(col))
  setnames(copybook, "kcd", col)
  new_col <- paste0(col, "_", lang[[1L]])
  if (lang[[1L]] == "ko") {
    df[copybook, on = col, (new_col) := i.ko]
  } else {
    df[copybook, on = col, (new_col) := i.en]
  }
}
