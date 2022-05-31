check_col_spec <- function(col_spec)
{
  unique_spec <- unique(col_spec)
  assertthat::assert_that(all(unique_spec %in% c("continuous", "dummy", "zz", "wec")),
                          msg = "col_spec must be either continuous, dummy, zz or wec")
}
