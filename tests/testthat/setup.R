# Keep every test's config writes inside a suite-owned scratch home.
original_dualdag_home <- Sys.getenv("DUALDAG_HOME", unset = NA_character_)
suite_dualdag_home <- tempfile("dualdag-test-home-")
dir.create(suite_dualdag_home, recursive = TRUE)
Sys.setenv(DUALDAG_HOME = suite_dualdag_home,
           DUALDAG_TEST_HOME = suite_dualdag_home,
           DUALDAG_TEST_ORIGINAL_HOME = if (is.na(original_dualdag_home)) "__UNSET__" else original_dualdag_home)

# Forking inside the test process is covered once, in test-extract-layers.R.
options(dualdag.extract_cores = 1L)
