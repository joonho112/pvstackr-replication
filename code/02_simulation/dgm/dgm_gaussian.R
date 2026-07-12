# Gaussian-shortcut DGM for Phase 4 simulation infrastructure.

if (!exists("%||%", mode = "function", inherits = TRUE)) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

.dgm_find_project_root <- function() {
  starts <- c(getwd(), dirname(getwd()), dirname(dirname(getwd())),
              dirname(dirname(dirname(getwd()))))
  starts <- unique(normalizePath(starts, winslash = "/", mustWork = FALSE))
  hits <- starts[file.exists(file.path(starts, "DESCRIPTION")) &
                  dir.exists(file.path(starts, "R"))]
  if (!length(hits)) stop("[dgm_gaussian] could not locate project root",
                          call. = FALSE)
  hits[[1L]]
}

.dgm_with_seed <- function(seed, code) {
  if (is.null(seed)) return(code())
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      !is.finite(seed)) {
    stop("[dgm_gaussian] seed must be one finite integer-like value",
         call. = FALSE)
  }
  old_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (old_exists) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (old_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  code()
}

.dgm_check_unit_interval <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x <= 0 || x >= 1) {
    stop(sprintf("[dgm_gaussian] %s must be one finite value in (0, 1)", name),
         call. = FALSE)
  }
  invisible(x)
}

.dgm_check_positive_integer <- function(x, name, min_value = 1L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x != as.integer(x) || x < min_value) {
    stop(sprintf("[dgm_gaussian] %s must be an integer >= %d",
                 name, as.integer(min_value)), call. = FALSE)
  }
  as.integer(x)
}

.dgm_check_positive_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x <= 0) {
    stop(sprintf("[dgm_gaussian] %s must be one positive finite value", name),
         call. = FALSE)
  }
  invisible(x)
}

dgm_gaussian_oracle <- function(icc_y,
                                beta1 = 0,
                                beta_W = 0.40,
                                beta_B = 0.60,
                                sigma2 = 1) {
  .dgm_check_unit_interval(icc_y, "icc_y")
  .dgm_check_positive_scalar(sigma2, "sigma2")
  tau2 <- icc_y * sigma2 / (1 - icc_y)
  list(
    beta1 = as.numeric(beta1),
    beta_W = as.numeric(beta_W),
    beta_B = as.numeric(beta_B),
    Delta = as.numeric(beta_B - beta_W),
    sigma2 = as.numeric(sigma2),
    tau2 = as.numeric(tau2),
    icc_y = as.numeric(icc_y)
  )
}

.dgm_builtin_cell_map <- function() {
  data.frame(
    cell_id = c(
      "USA-base", "KOR-base", "USA-icx-hi", "KOR-icx-lo",
      "USA-rhoPV-lo", "USA-rhoPV-hi", "KOR-rhoPV-lo", "KOR-rhoPV-hi",
      "Q7-USA-as-KOR", "Q7-KOR-as-USA", "Q7-USA-mid", "Q7-KOR-mid",
      "Q8-small-school", "USA-pilot-IRT", "KOR-pilot-IRT"
    ),
    route = c(rep("Gaussian", 13L), "IRT", "IRT"),
    ICC_y = c(0.20, 0.45, 0.20, 0.45, 0.20, 0.20, 0.45, 0.45,
              0.20, 0.45, 0.20, 0.45, 0.45, 0.20, 0.45),
    ICC_x = c(0.15, 0.30, 0.30, 0.15, 0.15, 0.15, 0.30, 0.30,
              0.15, 0.30, 0.15, 0.30, 0.30, 0.15, 0.30),
    rho_PV = c(0.90, 0.90, 0.90, 0.90, 0.85, 0.95, 0.85, 0.95,
               0.90, 0.90, 0.90, 0.90, 0.90, NA_real_, NA_real_),
    tau2 = c(0.250, 0.818, 0.250, 0.818, 0.250, 0.250, 0.818, 0.818,
             0.250, 0.818, 0.250, 0.818, 0.818, 0.250, 0.818),
    sigma2 = rep(1, 15L),
    xi2 = c(0.150, 0.300, 0.300, 0.150, 0.150, 0.150, 0.300, 0.300,
            0.150, 0.300, 0.150, 0.300, 0.300, 0.150, 0.300),
    omega2 = c(0.850, 0.700, 0.700, 0.850, 0.850, 0.850, 0.700, 0.700,
               0.850, 0.700, 0.850, 0.700, 0.700, 0.850, 0.700),
    beta_W = rep(0.40, 15L),
    beta_B = rep(0.60, 15L),
    n_items = c(rep(NA_integer_, 13L), 40L, 40L),
    n_bar_j = c(rep(NA_integer_, 12L), 10L, NA_integer_, NA_integer_),
    purpose = c(
      "USA-like primary; Q1/Q2 anchor; chapter 16 main grid base",
      "KOR-like primary; Q1/Q2 anchor; chapter 16 main grid base",
      "USA + high SES-clustering variant (Q3 sub)",
      "KOR + low SES-clustering variant (Q3 sub)",
      "PV precision low boundary (Q3-Plus PV axis)",
      "PV precision high boundary",
      "KOR + PV low precision; F4 tail probe",
      "KOR + PV high precision",
      "Q7 misspec: DGM USA-ICC, model assumes KOR-ICC (over-cluster)",
      "Q7 misspec: DGM KOR-ICC, model assumes USA-ICC (under-cluster)",
      "Q7 misspec: DGM USA-ICC, model assumes ICC=0.30 (mild over)",
      "Q7 misspec: DGM KOR-ICC, model assumes ICC=0.30 (mild under)",
      "Q8 small-school: KOR-ICC x n_bar_j=10 x J=100 forced override",
      "Pilot: USA-base cross-route calibration",
      "Pilot: KOR-base cross-route calibration"
    ),
    stringsAsFactors = FALSE
  )
}

dgm_gaussian_cell_map <- function(path = NULL) {
  if (is.null(path)) {
    root <- .dgm_find_project_root()
    path <- file.path(root, "sim", "dgm", "dgm_gaussian_cells.csv")
  }
  if (file.exists(path)) {
    out <- utils::read.csv(path, stringsAsFactors = FALSE,
                           na.strings = c("", "NA"))
  } else {
    out <- .dgm_builtin_cell_map()
  }
  required <- c("cell_id", "route", "ICC_y", "ICC_x", "rho_PV", "tau2",
                "sigma2", "xi2", "omega2", "beta_W", "beta_B")
  missing <- setdiff(required, names(out))
  if (length(missing)) {
    stop("[dgm_gaussian_cell_map] missing columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  out
}

dgm_gaussian_cell_spec <- function(cell_id) {
  if (!is.character(cell_id) || length(cell_id) != 1L || !nzchar(cell_id)) {
    stop("[dgm_gaussian_cell_spec] cell_id must be one non-empty string",
         call. = FALSE)
  }
  cmap <- dgm_gaussian_cell_map()
  hit <- cmap[cmap$cell_id == cell_id, , drop = FALSE]
  if (nrow(hit) != 1L) {
    stop("[dgm_gaussian_cell_spec] unknown or non-unique cell_id: ", cell_id,
         call. = FALSE)
  }
  hit
}

dgm_gaussian <- function(J,
                         nbar,
                         icc_y,
                         icc_x,
                         beta1 = 0,
                         beta_W = 0.40,
                         beta_B = 0.60,
                         sigma2 = 1,
                         seed = NULL,
                         cell_id = NA_character_,
                         population_id = NULL,
                         school_sizes = NULL,
                         school_size_cv = 0) {
  J <- .dgm_check_positive_integer(J, "J", min_value = 2L)
  nbar <- .dgm_check_positive_integer(nbar, "nbar", min_value = 2L)
  .dgm_check_unit_interval(icc_y, "icc_y")
  .dgm_check_unit_interval(icc_x, "icc_x")
  .dgm_check_positive_scalar(sigma2, "sigma2")
  if (!is.numeric(school_size_cv) || length(school_size_cv) != 1L ||
      is.na(school_size_cv) || !is.finite(school_size_cv) ||
      school_size_cv < 0) {
    stop("[dgm_gaussian] school_size_cv must be one non-negative finite value",
         call. = FALSE)
  }

  oracle <- dgm_gaussian_oracle(
    icc_y = icc_y,
    beta1 = beta1,
    beta_W = beta_W,
    beta_B = beta_B,
    sigma2 = sigma2
  )
  xi2 <- icc_x
  omega2 <- 1 - icc_x

  .dgm_with_seed(seed, function() {
    if (is.null(school_sizes)) {
      if (school_size_cv == 0) {
        n_j <- rep(nbar, J)
      } else {
        sdlog <- sqrt(log1p(school_size_cv ^ 2))
        meanlog <- log(nbar) - 0.5 * sdlog ^ 2
        n_j <- pmax(2L, as.integer(round(stats::rlnorm(J, meanlog, sdlog))))
      }
    } else {
      if (!is.numeric(school_sizes) || length(school_sizes) != J ||
          anyNA(school_sizes) || any(!is.finite(school_sizes)) ||
          any(school_sizes != as.integer(school_sizes)) ||
          any(school_sizes < 2L)) {
        stop("[dgm_gaussian] school_sizes must be integer counts >= 2 with length J",
             call. = FALSE)
      }
      n_j <- as.integer(school_sizes)
    }

    school_num <- rep(seq_len(J), times = n_j)
    n <- length(school_num)
    student_num <- sequence(n_j)
    school_id_levels <- sprintf("S%05d", seq_len(J))
    school_id <- school_id_levels[school_num]
    student_id <- sprintf("%s-%04d", school_id, student_num)

    x_between <- stats::rnorm(J, mean = 0, sd = sqrt(xi2))
    zeta <- stats::rnorm(J, mean = 0, sd = sqrt(oracle$tau2))
    x_within_latent <- stats::rnorm(n, mean = 0, sd = sqrt(omega2))
    x <- x_between[school_num] + x_within_latent
    xbar_by_school <- ave(x, school_id, FUN = mean)
    x_within <- x - xbar_by_school
    epsilon <- stats::rnorm(n, mean = 0, sd = sqrt(sigma2))
    theta <- beta1 + beta_W * x_within + beta_B * xbar_by_school +
      zeta[school_num] + epsilon

    row_support_key <- paste(school_id, student_id, sep = "::")
    row_support_hash <- if (requireNamespace("digest", quietly = TRUE)) {
      digest::digest(row_support_key, algo = "sha256")
    } else {
      NA_character_
    }
    cluster_hash <- if (requireNamespace("digest", quietly = TRUE)) {
      digest::digest(split(student_id, school_id), algo = "sha256")
    } else {
      NA_character_
    }

    out <- data.frame(
      cell_id = as.character(cell_id %||% NA_character_),
      population_id = as.character(population_id %||%
                                     paste0("gaussian_", seed %||% "unseeded")),
      route = "Gaussian",
      school_id = school_id,
      student_id = student_id,
      school_index = as.integer(school_num),
      student_index = as.integer(student_num),
      N_j = as.integer(n_j[school_num]),
      x_between_j = as.numeric(x_between[school_num]),
      x_within_latent = as.numeric(x_within_latent),
      x_ij = as.numeric(x),
      xbar_j = as.numeric(xbar_by_school),
      x_within = as.numeric(x_within),
      zeta_j = as.numeric(zeta[school_num]),
      epsilon_ij = as.numeric(epsilon),
      theta = as.numeric(theta),
      stringsAsFactors = FALSE
    )
    attr(out, "dgm_gaussian") <- list(
      oracle = oracle,
      icc_x = as.numeric(icc_x),
      xi2 = as.numeric(xi2),
      omega2 = as.numeric(omega2),
      J = J,
      n = n,
      nbar = nbar,
      school_sizes = n_j,
      seed = seed,
      cell_id = cell_id,
      route = "Gaussian",
      row_support_hash = row_support_hash,
      cluster_hash = cluster_hash,
      pv_cols = character(0)
    )
    out
  })
}

dgm_gaussian_from_cell <- function(cell_id,
                                   J,
                                   nbar = NULL,
                                   M = NULL,
                                   seed = NULL,
                                   include_pv = TRUE,
                                   ...) {
  spec <- dgm_gaussian_cell_spec(cell_id)
  if (!identical(spec$route[[1L]], "Gaussian")) {
    stop("[dgm_gaussian_from_cell] cell is not a Gaussian route: ", cell_id,
         call. = FALSE)
  }
  nbar <- nbar %||% spec$n_bar_j[[1L]] %||% 30L
  if (is.na(nbar)) nbar <- 30L
  out <- dgm_gaussian(
    J = J,
    nbar = nbar,
    icc_y = spec$ICC_y[[1L]],
    icc_x = spec$ICC_x[[1L]],
    beta_W = spec$beta_W[[1L]],
    beta_B = spec$beta_B[[1L]],
    sigma2 = spec$sigma2[[1L]],
    seed = seed,
    cell_id = cell_id,
    ...
  )
  if (isTRUE(include_pv)) {
    M <- M %||% 10L
    dgm_gaussian_pv(out, M = M, rho_PV = spec$rho_PV[[1L]],
                    seed = if (is.null(seed)) NULL else as.integer(seed) + 1L)
  } else {
    out
  }
}

dgm_gaussian_pv <- function(data,
                            M,
                            rho_PV,
                            seed = NULL,
                            pv_prefix = "PV",
                            theta_col = "theta") {
  if (!is.data.frame(data)) {
    stop("[dgm_gaussian_pv] data must be a data.frame", call. = FALSE)
  }
  M <- .dgm_check_positive_integer(M, "M", min_value = 2L)
  .dgm_check_unit_interval(rho_PV, "rho_PV")
  if (!theta_col %in% names(data)) {
    stop("[dgm_gaussian_pv] theta_col is not present in data", call. = FALSE)
  }
  theta <- data[[theta_col]]
  if (!is.numeric(theta) || anyNA(theta) || any(!is.finite(theta)) ||
      stats::var(theta) <= 0) {
    stop("[dgm_gaussian_pv] theta must be finite numeric with positive variance",
         call. = FALSE)
  }
  .dgm_with_seed(seed, function() {
    n <- length(theta)
    theta_var <- stats::var(theta)
    meas_var <- theta_var * (1 - rho_PV) / rho_PV
    noise <- matrix(stats::rnorm(n * M, mean = 0, sd = sqrt(meas_var)),
                    nrow = n, ncol = M)
    pv <- sweep(noise, 1L, theta, "+")
    pv_cols <- paste0(pv_prefix, seq_len(M))
    for (m in seq_len(M)) data[[pv_cols[[m]]]] <- pv[, m]

    pv_order_hash <- if (requireNamespace("digest", quietly = TRUE)) {
      digest::digest(pv_cols, algo = "sha256")
    } else {
      NA_character_
    }
    pv_value_hash <- if (requireNamespace("digest", quietly = TRUE)) {
      digest::digest(data[, pv_cols, drop = FALSE], algo = "sha256")
    } else {
      NA_character_
    }

    meta <- attr(data, "dgm_gaussian") %||% list()
    meta$pv_cols <- pv_cols
    meta$rho_PV <- as.numeric(rho_PV)
    meta$M <- M
    meta$pv_seed <- seed
    meta$pv_measurement_variance <- as.numeric(meas_var)
    meta$pv_correlation_definition <- "mean raw Pearson correlation among plausible-value columns"
    meta$pv_order_hash <- pv_order_hash
    meta$pv_value_hash <- pv_value_hash
    attr(data, "dgm_gaussian") <- meta
    data
  })
}

dgm_gaussian_pv_correlation <- function(data, pv_cols = NULL) {
  if (!is.data.frame(data)) {
    stop("[dgm_gaussian_pv_correlation] data must be a data.frame",
         call. = FALSE)
  }
  pv_cols <- pv_cols %||% (attr(data, "dgm_gaussian") %||% list())$pv_cols
  if (is.null(pv_cols) || length(pv_cols) < 2L ||
      !all(pv_cols %in% names(data))) {
    stop("[dgm_gaussian_pv_correlation] need at least two PV columns",
         call. = FALSE)
  }
  cm <- stats::cor(as.matrix(data[, pv_cols, drop = FALSE]))
  mean(cm[upper.tri(cm)])
}

dgm_gaussian_recovery_summary <- function(data,
                                          pv_cols = NULL,
                                          REML = FALSE,
                                          lme4_control = NULL) {
  if (!is.data.frame(data)) {
    stop("[dgm_gaussian_recovery_summary] data must be a data.frame",
         call. = FALSE)
  }
  required <- c("theta", "x_within", "xbar_j", "school_id")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("[dgm_gaussian_recovery_summary] missing columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("[dgm_gaussian_recovery_summary] lme4 is required", call. = FALSE)
  }
  ctl <- lme4_control %||% lme4::lmerControl(
    calc.derivs = FALSE,
    check.conv.singular = "ignore"
  )
  fit <- lme4::lmer(
    theta ~ x_within + xbar_j + (1 | school_id),
    data = data,
    REML = REML,
    control = ctl
  )
  beta <- lme4::fixef(fit)
  vc <- as.data.frame(lme4::VarCorr(fit))
  tau2_hat <- vc$vcov[vc$grp == "school_id"][[1L]]
  sigma2_hat <- vc$vcov[vc$grp == "Residual"][[1L]]
  pv_cor <- NA_real_
  if (!is.null(pv_cols)) pv_cor <- dgm_gaussian_pv_correlation(data, pv_cols)
  oracle <- (attr(data, "dgm_gaussian") %||% list())$oracle %||% list()
  data.frame(
    beta1_hat = unname(beta[["(Intercept)"]]),
    beta_W_hat = unname(beta[["x_within"]]),
    beta_B_hat = unname(beta[["xbar_j"]]),
    tau2_hat = tau2_hat,
    sigma2_hat = sigma2_hat,
    icc_y_hat = tau2_hat / (tau2_hat + sigma2_hat),
    pv_cor_hat = pv_cor,
    beta1_true = oracle$beta1 %||% NA_real_,
    beta_W_true = oracle$beta_W %||% NA_real_,
    beta_B_true = oracle$beta_B %||% NA_real_,
    tau2_true = oracle$tau2 %||% NA_real_,
    sigma2_true = oracle$sigma2 %||% NA_real_,
    icc_y_true = oracle$icc_y %||% NA_real_,
    stringsAsFactors = FALSE
  )
}
