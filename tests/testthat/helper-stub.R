# Swap a binding in the global environment (where R/ is sourced) for one test.
#
# The modules are sourced into globalenv() rather than loaded as a package, so a
# test that needs to substitute one of them -- a deliberately non-exact packer,
# say -- has to reach there, and has to put the original back whatever the test
# does.
local_global_stub <- function(name, value, env = parent.frame()) {
  original <- get(name, envir = globalenv())
  assign(name, value, envir = globalenv())
  withr::defer(assign(name, original, envir = globalenv()), envir = env)
}
