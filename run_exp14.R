# run_exp14.R -- production one-at-a-time sensitivity sweep (Reviewer R1.4).
# Parallelised with mclapply (fork; inherits sourced functions on macOS/Linux).
# Writes results/exp14_sensitivity.csv. The driver lives in R/sim_exp14.R.
suppressMessages({library(tidyverse); library(parallel)})
purrr::walk(list.files("R", full.names = TRUE, pattern = "[.][Rr]$"), source)

topologies <- c("sp", "entangled")
seeds      <- 1:5
N          <- 60L
load_level <- "high"
n_rounds   <- 200L

sweeps <- list(
  cap_scale        = c(0.7, 1.0, 1.3),
  integ_eta        = c(0.10, 0.15, 0.20),
  lambda_l_default = c(0.0025, 0.005, 0.0075),
  integ_efficiency = c(0.65, 0.75, 0.85)
)
baseline <- list(cap_scale = 1.0, integ_eta = 0.15,
                 lambda_l_default = 0.005, integ_efficiency = NULL)

# Flat grid of every (param, level, topology, seed) cell.
cells <- list()
for (param in names(sweeps)) for (lvl in sweeps[[param]])
  for (tp in topologies) for (sd in seeds)
    cells[[length(cells) + 1L]] <- list(param = param, lvl = lvl, tp = tp, sd = sd)

reduction_cell <- function(cell) {
  args_over <- baseline
  args_over[[cell$param]] <- cell$lvl
  r <- exp14_reduction_one(
    graph_type = cell$tp, load_level = load_level, N = N, seed = cell$sd,
    cap_scale = args_over$cap_scale, integ_efficiency = args_over$integ_efficiency,
    integ_eta = args_over$integ_eta, lambda_l_default = args_over$lambda_l_default,
    n_rounds = n_rounds
  )
  data.frame(parameter = cell$param, level = cell$lvl, topology = cell$tp,
             seed = cell$sd, reduction = r, stringsAsFactors = FALSE)
}

t0 <- proc.time()
res <- mclapply(cells, reduction_cell, mc.cores = 6L)
raw <- do.call(rbind, res)

tab <- raw %>%
  filter(!is.na(reduction)) %>%
  group_by(parameter, level) %>%
  summarise(n_volatile = n(),
            median_reduction = median(reduction),
            min_reduction = min(reduction),
            max_reduction = max(reduction),
            .groups = "drop") %>%
  mutate(is_baseline = (parameter == "cap_scale"        & level == 1.0)    |
                       (parameter == "integ_eta"        & level == 0.15)   |
                       (parameter == "lambda_l_default" & level == 0.005)  |
                       (parameter == "integ_efficiency" & level == 0.75))

dir.create("results", showWarnings = FALSE)
write.csv(raw, "results/exp14_sensitivity_raw.csv", row.names = FALSE)
write.csv(tab, "results/exp14_sensitivity.csv", row.names = FALSE)

cat(sprintf("Done in %.0f s. Overall reduction band: %.0f%%--%.0f%% (median %.0f%%).\n",
            (proc.time() - t0)["elapsed"],
            100 * min(tab$min_reduction), 100 * max(tab$max_reduction),
            100 * median(raw$reduction[!is.na(raw$reduction)])))
print(as.data.frame(tab), row.names = FALSE)
