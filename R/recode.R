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
  c(new, default) %<-% vec_cast_common(new, default, .to = ptype)

  if (is_bare_list(old)) {
    old_ptype <- vec_ptype_common(!!!old, x)
    old <- vec_cast_common(!!!old, .to = old_ptype)
    x <- vec_cast(x, old_ptype)
  } else {
    c(old, x) %<-% vec_cast_common(old, x)
  }

  out <- vec_init(ptype, vec_size(x))
  done <- rep_along(out, FALSE)

  for (i in seq_along(old)) {
    if (is_bare_list(old)) {
      haystack <- old[[i]]
    } else {
      haystack <- vec_slice(old, i)
    }
    where <- vec_in(x, haystack)
    done <- done | where
    vec_slice(out, where) <- vec_slice(new, i)
  }

  todo <- !done
  if (any(todo)) {
    default <- default %||% x
    default <- vec_cast(default, ptype)
    default <- vec_recycle(default, vec_size(x))
    vec_slice(out, todo) <- vec_slice(default, todo)
  }

  out
}
