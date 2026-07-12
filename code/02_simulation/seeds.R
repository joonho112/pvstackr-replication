# Deterministic seed family used by the public simulation replay.

if (!exists("%||%", mode = "function", inherits = TRUE)) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

seed_root <- function() 20260514L

.seed_require_digest <- function() {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("[seeds] digest package is required", call. = FALSE)
  }
}

seed_role_registry <- function() {
  data.frame(
    role = c("population", "school_sample", "student_sample", "pv",
             "nonresponse", "weight_trim", "rep_weights", "brr",
             "mcmc_cdirect", "mcmc_cpv", "mcmc_A", "freq_perpv"),
    seed_scope = c(rep("paired_substrate", 8L),
                   rep("mcmc_pipeline_specific", 3L),
                   "analysis_rng"),
    paired_across_pipelines = c(rep(TRUE, 8L), rep(FALSE, 4L)),
    stringsAsFactors = FALSE
  )
}

shared_seed_roles <- function() {
  registry <- seed_role_registry()
  registry$role[registry$paired_across_pipelines]
}

.seed_check_scalar_character <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("[seeds] %s must be one non-empty string", label),
         call. = FALSE)
  }
  x
}

.seed_check_positive_integer <- function(x, label) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x != as.integer(x) || x < 1L) {
    stop(sprintf("[seeds] %s must be one positive integer", label),
         call. = FALSE)
  }
  as.integer(x)
}

.seed_pack_value <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.logical(x)) return(paste(ifelse(x, "TRUE", "FALSE"), collapse = ","))
  if (is.numeric(x)) {
    return(paste(format(as.numeric(x), digits = 17, scientific = FALSE,
                        trim = TRUE), collapse = ","))
  }
  if (is.character(x)) return(paste(x, collapse = ","))
  if (is.factor(x)) return(paste(as.character(x), collapse = ","))
  paste(utils::capture.output(str(x, give.attr = FALSE)), collapse = " ")
}

.seed_pack_dots <- function(dots) {
  if (!length(dots)) return(character(0))
  nms <- names(dots)
  if (is.null(nms) || any(!nzchar(nms))) {
    stop("[seeds] extra seed fields must be named", call. = FALSE)
  }
  nms <- sort(nms)
  paste0("extra.", nms, "=", vapply(dots[nms], .seed_pack_value,
                                    character(1L)))
}

seed_payload_string <- function(cell,
                                rep,
                                role,
                                pipeline = NULL,
                                stream = NULL,
                                chain = NULL,
                                root_seed = seed_root(),
                                ...) {
  cell <- .seed_check_scalar_character(cell, "cell")
  rep <- .seed_check_positive_integer(rep, "rep")
  root_seed <- .seed_check_positive_integer(root_seed, "root_seed")
  role <- .seed_check_scalar_character(role, "role")
  registry <- seed_role_registry()
  if (!role %in% registry$role) {
    stop("[seeds] unknown seed role: ", role, call. = FALSE)
  }
  scope <- registry$seed_scope[match(role, registry$role)]
  pipeline_label <- pipeline %||% ""
  if (!is.null(pipeline)) {
    pipeline_label <- .seed_check_scalar_character(pipeline, "pipeline")
  }
  pipeline_key <- if (role %in% shared_seed_roles()) {
    "paired_substrate"
  } else if (nzchar(pipeline_label)) {
    pipeline_label
  } else {
    role
  }
  stream_label <- if (is.null(stream)) "" else .seed_pack_value(stream)
  chain_label <- if (is.null(chain)) "" else
    as.character(.seed_check_positive_integer(chain, "chain"))
  lines <- c(
    "version=step4.5_seed_family_v1",
    paste0("root_seed=", root_seed),
    paste0("cell=", cell),
    paste0("rep=", sprintf("%010d", rep)),
    paste0("role=", role),
    paste0("scope=", scope),
    paste0("pipeline_key=", pipeline_key),
    paste0("stream=", stream_label),
    paste0("chain=", chain_label),
    .seed_pack_dots(list(...))
  )
  paste(lines, collapse = "\n")
}

.seed_int_from_payload <- function(payload,
                                   root_seed = seed_root(),
                                   collision_probe = 0L) {
  .seed_require_digest()
  root_seed <- .seed_check_positive_integer(root_seed, "root_seed")
  if (!is.numeric(collision_probe) || length(collision_probe) != 1L ||
      is.na(collision_probe) || !is.finite(collision_probe) ||
      collision_probe != as.integer(collision_probe) || collision_probe < 0L) {
    stop("[seeds] collision_probe must be one non-negative integer",
         call. = FALSE)
  }
  probe_payload <- paste(payload,
                         paste0("collision_probe=", as.integer(collision_probe)),
                         sep = "\n")
  raw <- as.numeric(digest::digest2int(probe_payload,
                                       seed = as.integer(root_seed)))
  as.integer((raw %% (.Machine$integer.max - 1L)) + 1L)
}

make_seed <- function(cell,
                      rep,
                      role,
                      pipeline = NULL,
                      stream = NULL,
                      chain = NULL,
                      root_seed = seed_root(),
                      collision_probe = 0L,
                      ...) {
  payload <- seed_payload_string(
    cell = cell,
    rep = rep,
    role = role,
    pipeline = pipeline,
    stream = stream,
    chain = chain,
    root_seed = root_seed,
    ...
  )
  .seed_int_from_payload(payload, root_seed = root_seed,
                         collision_probe = collision_probe)
}

# Development-only ledger and substrate-audit helpers from the source archive
# depended on package-layout paths absent here. They are not part of the live
# replay contract and are intentionally omitted from the public package.
