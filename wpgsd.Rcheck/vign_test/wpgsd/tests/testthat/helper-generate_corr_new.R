# Helper functions used by test-independent-generate_corr_new.R

#' Generate correlation matrix based on event counts
#'
#' @param event Event count of each hypothesis at each analysis, including
#'   event count of the intersection of hypotheses.
#'   It contains 4 columns: `H1`, `H2`, `Analysis`, `Event`.
#'   `H1` needs to be listed as 1, 2, 3, etc. as numbers.
#'
#' @return A correlation matrix.
#'
#' @importFrom dplyr filter select %>%
#'
#' @export
#'
#' @examples
#' # Build the transition matrix
#' m <- matrix(c(
#'   0, 0.5, 0.5,
#'   0.5, 0, 0.5,
#'   0.5, 0.5, 0
#' ), nrow = 3, byrow = TRUE)
#' # initialize weights
#' w <- c(1 / 3, 1 / 3, 1 / 3)
#'
#' # Input event count of intersection of paired hypotheses - Table 2
#' event <- tibble::tribble(
#'   ~H1, ~H2, ~Analysis, ~Event,
#'   1, 1, 1, 155,
#'   2, 2, 1, 160,
#'   3, 3, 1, 165,
#'   1, 2, 1, 85,
#'   1, 3, 1, 85,
#'   2, 3, 1, 85,
#'   1, 1, 2, 305,
#'   2, 2, 2, 320,
#'   3, 3, 2, 335,
#'   1, 2, 2, 170,
#'   1, 3, 2, 170,
#'   2, 3, 2, 170
#' )
#'
#' # Generate correlation from events
#' gs_corr <- generate_corr_new(event)
generate_corr_new <- function(event) {
  elem <- event %>% subset(H1 == H2)
  inter <- event %>% subset(H1 != H2)
  n_hypotheses <- max(as.numeric(elem$H1))
  n_analyses <- max(elem$Analysis)

  # Diagonal
  D <- diag(elem$Event)

  # Within hypothesis across analyses
  if (n_analyses > 1) {
    for (i in 1:n_hypotheses) {
      for (j in 1:(n_analyses - 1)) {
        count <- D[(j - 1) * n_hypotheses + i, (j - 1) * n_hypotheses + i]
        for (k in (j + 1):n_analyses) {
          D[(j - 1) * n_hypotheses + i, (k - 1) * n_hypotheses + i] <- count
          D[(k - 1) * n_hypotheses + i, (j - 1) * n_hypotheses + i] <- count
        }
      }
    }
  }

  # Between hypotheses and analyses
  for (i in 1:(n_hypotheses - 1)) {
    for (j in c((i + 1):n_hypotheses)) {
      for (k in 1:n_analyses) {
        count1 <- as.numeric(event %>%
          subset(((H1 == i & H2 == j) | (H1 == j & H2 == i)) & Analysis == k) %>%
          select(Event))[1]
        for (l in (k:n_analyses)) {
          D[n_hypotheses * (l - 1) + i, n_hypotheses * (k - 1) + j] <- count1
          D[n_hypotheses * (l - 1) + j, n_hypotheses * (k - 1) + i] <- count1
          D[n_hypotheses * (k - 1) + j, n_hypotheses * (l - 1) + i] <- count1
          D[n_hypotheses * (k - 1) + i, n_hypotheses * (l - 1) + j] <- count1
        }
      }
    }
  }

  corr_mat <- d_corr(D)

  col_names <- NULL
  for (k in 1:n_analyses) {
    for (i in 1:n_hypotheses) {
      name_tmp <- paste("H", i, "_A", k, sep = "")
      col_names <- c(col_names, name_tmp)
    }
  }

  colnames(corr_mat) <- col_names

  return(corr_mat)
}

test_generate_corr_new <- function() {
  a1 <- 80
  b1 <- 100
  ab1 <- 60
  a2 <- 120
  b2 <- 150
  ab2 <- 80

  event <- tibble::tribble(
    ~H1, ~H2, ~Analysis, ~Event,
    1, 1, 1, a1,
    2, 2, 1, b1,
    1, 2, 1, ab1,
    1, 1, 2, a2,
    2, 2, 2, b2,
    1, 2, 2, ab2
  )

  list(
    "a1" = a1,
    "b1" = b1,
    "ab1" = ab1,
    "a2" = a2,
    "b2" = b2,
    "ab2" = ab2,
    "event" = event
  )
}
