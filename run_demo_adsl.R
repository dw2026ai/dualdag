# One-click launcher for the ADSL demo.
local({
  app_dir <- NULL
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(fa)) app_dir <- dirname(normalizePath(sub("^--file=", "", fa[1]), mustWork = FALSE))
  if (is.null(app_dir)) {
    for (i in rev(seq_len(sys.nframe()))) {
      f <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
      if (!is.null(f)) { app_dir <- dirname(normalizePath(f)); break }
    }
  }
  if (is.null(app_dir) || !file.exists(file.path(app_dir, "run.R"))) app_dir <- getwd()
  sys.source(file.path(app_dir, "tools", "launch_demo.R"), envir = environment())
  launch_demo("adsl", app_dir)
})
