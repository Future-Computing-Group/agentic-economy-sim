# test-hygiene.R
# ---------------------------------------------------------------------------
# Release guard: no tracked file may carry authoring-environment or
# review-process text into the public repository.
#
# The scan runs over `git ls-files`, so it sees exactly what a release
# publishes, and reports file:line:text for every hit.
#
# Two exemptions, both necessary rather than convenient:
#
#   1. This file is skipped. It has to spell every needle out, so scanning it
#      would make the guard fail on itself forever.
#   2. `.gitignore`'s own `OPSLOG.md` entry is allowed. That line is the rule
#      that keeps the internal campaign log out of the repo; deleting it would
#      cause the leak it is accused of.
#
# Scoping notes:
#
#   - The lesson-number pattern `\bL[0-9]{2,3}\b` was checked against the whole
#     tracked tree before being applied to code as well as comments: R integer
#     literals (`200L`) carry no word boundary before the `L`, and no
#     identifier in R/, tests/, emul/ or agentic/ matches it, so no code
#     exemption is needed.
#   - "round" on its own is this simulator's own vocabulary (tatonnement
#     rounds, `n_rounds`) in a few hundred tracked lines, so the round pattern
#     matches only in review context. Review rounds leak through the
#     reviewer / referee / revision / R&R / `R1.4`-style code patterns instead.
# ---------------------------------------------------------------------------

leak_patterns <- c(
  "absolute workspace path" = "/Users/|Dropbox|5 Support|Groups/FCG",
  "internal campaign log"   = "OPSLOG",
  "lesson number"           = "\\bL[0-9]{2,3}\\b",
  "review-round code"       = "\\bR[123]\\.[0-9]\\b|R&R",
  "review vocabulary"       = "(?i)\\b(reviewers?|referees?|revisions?|rebuttals?)\\b",
  "ladder label"            = "(?i)\\brungs?\\b",
  "review round"            = "(?i)(review|editorial|rebuttal)[ -]round|round[ -]?[12][ -](review|revision)",
  "internal directory"      = "\\bReview/|\\bPlans?/|editorial-round",
  "internal task id"        = "\\bT[0-9]\\.[0-9]\\b|\\bD1[0-4]\\b|\\bNA-[0-9]",
  "process vocabulary"      = "(?i)\\b(amendments?|registered|frozen|pre-registration)\\b",
  "internal host"           = "(?i)\\b(nrouter|puhti|mahti|roihu|5gtn)\\b|csc\\.fi",
  "AI co-author trailer"    = "Co-Authored-By"
)

# Binary fixtures carry no prose; `.rds` in particular is gzip, so a text scan
# of it is noise. They were checked by decompressing and scanning the strings.
binary_fixture_pattern <- "\\.(rds|png|jpe?g|gif|pdf|zip|gz|ico|woff2?)$"

guard_file <- "tests/testthat/test-hygiene.R"

# (file, exact line) pairs the scan must not report. See exemption 2 above.
allowed_line <- function(file, line) {
  file == ".gitignore" & trimws(line) == "OPSLOG.md"
}

scan_for_leaks <- function(root, files) {
  hits <- character()
  for (f in files) {
    lines <- readLines(file.path(root, f), warn = FALSE)
    for (nm in names(leak_patterns)) {
      i <- grep(leak_patterns[[nm]], lines, perl = TRUE)
      i <- i[!allowed_line(f, lines[i])]
      if (length(i)) {
        hits <- c(hits, sprintf("%s:%d: [%s] %s", f, i, nm, trimws(lines[i])))
      }
    }
  }
  sort(hits)
}

test_that("no tracked file leaks internal or review-process text", {
  root <- here::here()
  # An archived release is a tarball with no .git, where "what is tracked" is
  # not a question that can be asked. The guard runs in the checkout it is
  # meant to gate and stands down elsewhere rather than failing for a reader.
  in_checkout <- nzchar(Sys.which("git")) && identical(
    suppressWarnings(system2("git", c("-C", shQuote(root), "rev-parse",
                                      "--is-inside-work-tree"),
                             stdout = TRUE, stderr = FALSE)), "true")
  skip_if_not(in_checkout, "not a git checkout: nothing tracked to scan")

  files <- system2("git", c("-C", shQuote(root), "ls-files"), stdout = TRUE)
  expect_gt(length(files), 0)          # git ls-files must actually have run

  files <- files[!grepl(binary_fixture_pattern, files)]
  files <- setdiff(files, guard_file)

  expect_equal(scan_for_leaks(root, files), character(0))
})

test_that("the guard's patterns fire on the leaks they are meant to catch", {
  # A typo in a regex above would let the guard pass on a dirty tree, so each
  # class is checked against a sample of the text it exists to catch.
  samples <- c(
    "# ad-hoc dev probe (L73 - not for public release)",
    "Exp.9 (R2.2): instrument a REAL multi-step LLM tool-use agent",
    "# the deviation, which is the one the reviewers asked about.",
    "# rung 2 replays the admitted sequence on the sleep transport.",
    "# cut or merged during revision",
    "source('/Users/someone/Dropbox/workspace/thing.R')",
    "log written to OPSLOG.md by the campaign",
    "see Review/editorial-round-2/audits for the ledger",
    "blocked on T3.1 and D12 until NA-4 lands",
    "the analysis plan was frozen before the pre-registration",
    "scp results to puhti.csc.fi",
    "Co-Authored-By: a tool <noreply@example.com>"
  )
  fires <- vapply(samples, function(s) {
    any(vapply(leak_patterns, grepl, logical(1), x = s, perl = TRUE))
  }, logical(1))
  expect_true(all(fires), info = paste(samples[!fires], collapse = "\n"))

  # ... and not on the simulator's own vocabulary.
  clean <- c(
    "  # the round-1 gain whatever the horizon; a mean would divide it by n_rounds",
    "  N <- 200L",
    "  tar_target(exp11_summary_table, exp11_aggregate(exp11_results_raw))",
    "  OLLAMA <- 'http://localhost:11434/api/generate'"
  )
  quiet <- vapply(clean, function(s) {
    !any(vapply(leak_patterns, grepl, logical(1), x = s, perl = TRUE))
  }, logical(1))
  expect_true(all(quiet), info = paste(clean[!quiet], collapse = "\n"))
})
