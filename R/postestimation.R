## WTP

## Elasticities / MPE

## Partworth

## Hit rates

## Individual level parameter distribution

## Compare models -> tex output


################################################


vcov <- function(model)
{
  mixl:::vcov.mixl(model)
}


robvcov <- function(model)
{
  betas <- model$estimate
  meat <- sandwich::meat(model)
  bread <- sandwich::bread(model)
  meat[is.na(meat)] <- 0
  bread[is.na(bread)] <- 0
  sandwich::sandwich(model, bread, meat)
}


#' Delta method
#'
#' @param model
#' @param formula
#' @param robust
#' @param ses
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'   deltamethod(model, ~ B_tt + B_cost, ses = F)
#'   cov <- robvcov(model)
#'   cov["B_tt", "B_tt"] + cov["B_cost", "B_cost"] + 2 * cov["B_tt", "B_cost"]
#' }
deltamethod <- function(model, formula, robust = TRUE, ses = TRUE)
{
  if(robust) {
    cov <- robvcov(model)
  } else {
    cov <- vcov(model)
  }

  delta(formula, model$estimate, cov, ses)

}



#' Generic delta method
#'
#' Adjusted from `msm` package
#'
#' @param g formula (without LHS)
#' @param mean named vector (of coefficients)
#' @param cov covariance matrix
#' @param ses standard errors or (co) variance
#'
#' @keywords internal
#'
#' @return (co) variance of function
#' @export
delta <- function(g, mean, cov, ses = TRUE)
{
  cov <- as.matrix(cov)
  n <- length(mean)
  if (!is.list(g))
    g <- list(g)
  if ((dim(cov)[1] != n) || (dim(cov)[2] != n))
    stop(paste("Covariances should be a ", n, " by ", n,
               " matrix"))
  syms <- names(mean)
  names(syms) <- syms
  for (nm in syms) assign(syms[nm], mean[nm])
  gdashmu <- t(sapply(g, function(form) {
    as.numeric(attr(eval(deriv(form, syms)), "gradient"))
  }))
  new.covar <- gdashmu %*% cov %*% t(gdashmu)
  if (ses) {
    new.se <- sqrt(diag(new.covar))
    new.se
  }
  else new.covar
}




mean_probs <- function(model, data, ...)
{
  probs <- as.data.frame(mixl::probabilities(model = model, data = data, ...))
  nm <- colnames(probs)
  nm_ind <- c("i", "ID", "choice_index")
  nm_probs <- nm[!(nm %in% nm_ind)]

  mean_probs <-
    apply(as.data.frame(probs), MARGIN = 2, mean)[nm_probs]

  mean_probs
}


get_col_specs <- function(col_spec, cols)
{
  full_col_spec <- sapply(cols, function(x) {
    nm <- names(col_spec)
    default <- col_spec["default"]
    if(x %in% nm) col_spec[x] else default
  })

  names(full_col_spec) <- cols
  full_col_spec
}



mpe <- function(model, col_spec, increase_factor = NULL, ...)
{
  check_col_spec(col_spec)

  if("zz" %in% unique(col_spec) | "wec" %in% unique(col_spec)) stop("zz and wec not yet implemented")

  data <- model$data
  data_cols <- model$model_spec$data_cols

  ## handle default
  full_col_spec <- get_col_specs(col_spec, data_cols)

  baseprobs <- mean_probs(model, data)
  mpe_all <- matrix(nrow = length(data_cols),
                    ncol = model$model_spec$num_utility_functions + 1,
                    dimnames = list(data_cols, names(baseprobs)))
  for(col in names(full_col_spec))
  {
    spec <- full_col_spec[col]
    tmp_data <- data

    ## mean probs before
    ## if dummy: compare 0 to 1
    if(spec == "dummy")
    {
      tmp_data[col] <- 0
      mpb <- mean_probs(model, tmp_data, ...)
    }
    else
    {
      mpb <- baseprobs
    }


    ## change
    if(spec == "dummy")
    {
      tmp_data[col] <- 1
    }
    else if(spec == "continuous")
    {
      tmp_data[col] <- tmp_data[col] * increase_factor
    }

    ## mean probs after
    mpa <- mean_probs(model, tmp_data, ...)

    ## marginal probability effect
    mpe <- mpa - mpb
    mpe <- as.matrix(mpe, dimnames = list(col))
    mpe_all[col, ] <- mpe
  }

  mpe_all

}


magic_parser <- function(utility_script)
{
  us <- stringr::str_remove_all(utility_script, "\n")
  us <- unlist(strsplit(us, ";"))
  us <- strsplit(us, "\\+|\\=")
  us <- sapply(us, trimws)
}

## only for simplest of MNL
vi <- function(model, x_k, na = 9999)
{
  data <- model$data
  data[data == na] <- NA
  data_cols <- model$model_spec$data_cols
  mean_x <- apply(data[data_cols], MARGIN = 2, function(x) mean(x, na.rm = TRUE))
  betas <- model$estimate
  utility_script <- model$model_spec$source

  # magic_parser()

  ## utility script parser -> raise error if interactions!
  ## special chars for parser: $ @ draw_ operators (_RND)
  ## see mixl:::convert_to_valid_cpp() and mixl:::extract_variables for inspiration
  ## what about mixed parameters? -> use posterior means?

}



#' Generate inputs for partworth analysis
#'
#' @param model output from `mixl::estimate`
#' @param na NA encodings; defaults to 9999
#' @param file full path to file (should end with .xlsx); defaults to `NULL`
#'
#' @return
#' @export
partworth <- function(model, na = 9999, file = NULL)
{
  data <- model$data
  data[data == na] <- NA
  data_cols <- model$model_spec$data_cols
  mean_x <- apply(data[data_cols], MARGIN = 2, function(x) abs(mean(x, na.rm = TRUE)))
  betas <- abs(model$estimate)

  if(!is.null(file))
  {
    is_xlsx <- stringr::str_detect(file, ".xlsx")
    assertthat::assert_that(is_xlsx,
                            msg = "file should end with .xlsx")

    wb <- xlsx::createWorkbook()
    sheet_mean <- xlsx::createSheet(wb, "mean_x")
    sheet_betas <- xlsx::createSheet(wb, "betas")

    xlsx::addDataFrame(as.data.frame(mean_x), sheet = sheet_mean, startColumn = 1, row.names = TRUE)
    xlsx::addDataFrame(as.data.frame(betas), sheet = sheet_betas, startColumn = 1, row.names = TRUE)

    xlsx::saveWorkbook(wb, file)
  }

  out <- list(mean_x = mean_x, betas = betas)

  return(out)
}


#' A fashion show is a model comparison
#'
#' @param models named list (names are used in table output)
#' @param output_file .txt file
#' @param stars defaults `c(0.01, 0.05, 0.1)`
#' @param ... passed to `texreg::texreg`
#'
#' @return LaTex table output
#' @export
fashion_show <- function(models, output_file = NULL, stars = c(0.01, 0.05, 0.1), ...)
{
  catwalk <-
    sapply(models, function(x) {
      mixl::summary_tex(x)
    }, simplify = FALSE)

  multitex <-
    texreg::texreg(catwalk, stars = stars, custom.model.names = names(catwalk), dcolumn = TRUE, ...)

  if(!is.null(output_file))
    cat(multitex, file = output_file, append=FALSE)

  multitex

}


