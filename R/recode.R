#' @import vctrs
#' @import rlang
NULL

#' @export
keys <- function(new, old) {
  tibble::tibble(.new = new, .old = old)
}
is_keys <- function(x) {
  is.data.frame(x) && all(names(x) %in% c(".new", ".old"))
}

#' @export
vec_recode <- function(x, spec, ..., default = NULL, ptype = NULL) {
  ellipsis::check_dots_empty(...)

  if (!is_keys(spec)) {
    abort("`spec` must be a data frame with `.new` and `.old` columns")
  }
  new <- spec$.new
  old <- spec$.old

  ptype <- ptype %||% vec_ptype_common(new, default)
  default <- default %||% x

  c(x, new) %<-% vec_cast_common(x, new, .to = ptype)

  # Handle list-columns so multiple values can be mapped to a single key
  if (is_bare_list(old)) {
    old <- vec_cast_common(!!!old, .to = ptype)
  } else {
    old <- vec_cast(old, to = ptype)
  }

  out <- vec_init(x, vec_size(x))
  done <- rep_along(out, FALSE)

  for (i in seq_along(old)) {
    where <- vec_in(x, old[[i]])
    done <- done | where
    vec_slice(out, where) <- new[[i]]
  }

  todo <- !done
  if (any(todo)) {
    default <- vec_recycle(default, vec_size(x))
    vec_slice(out, todo) <- vec_slice(default, todo)
  }

  out
}
