# extract effective sample sizes from beast log file
extractESS <- function(trait, fit, id) {
  # get beast log
  log <-
    parse_beast_tracelog_file(
      paste0("temp/log_", paste0(trait, "_", id), ".log")
    )
  # extract ESS
  bind_cols(
    tibble(trait = trait),
    calc_esses(
      remove_burn_ins(log, burn_in_fraction = 0.1), sample_interval = 1000
    )
  )
}
