#' @import vctrs
#' @import rlang
NULL

#' @export
keys <- function(key, value) {
  tibble::tibble(.key = key, .value = value)
}
is_keys <- function(x) {
  is.data.frame(x) && all(names(x) %in% c(".key", ".value"))
}

#' @export
vec_recode <- function(x, spec, ..., default = NULL, ptype = NULL) {
  ellipsis::check_dots_empty(...)

  if (!is_keys(spec)) {
    abort("`spec` must be a data frame with `.key` and `.value` columns")
  }
  key <- spec$.key
  value <- spec$.value

  ptype <- ptype %||% vec_ptype_common(key, default)
  default <- default %||% x

  c(x, key) %<-% vec_cast_common(x, key, .to = ptype)

  # Handle list-columns so multiple values can be mapped to a single key
  if (is_bare_list(value)) {
    value <- vec_cast_common(!!!value, .to = ptype)
  } else {
    value <- vec_cast(value, to = ptype)
  }

  out <- vec_init(x, vec_size(x))
  done <- rep_along(out, FALSE)

  for (i in seq_along(value)) {
    where <- vec_in(x, value[[i]])
    done <- done | where
    vec_slice(out, where) <- key[[i]]
  }

  todo <- !done
  if (any(todo)) {
    default <- vec_recycle(default, vec_size(x))
    vec_slice(out, todo) <- vec_slice(default, todo)
  }

  out
}
