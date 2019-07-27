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
    ns <- vapply(old, vec_size, integer(1))

    # FIXME: Use `vctrs::repeat()` once vectorised over `each`
    new <- rep(new, times = ns)

    old <- vec_c(!!!old)
  }

  c(old, x) %<-% vec_cast_common(old, x)

  idx <- vec_match(x, old)
  out <- vec_slice(new, idx)

  todo <- is.na(idx)
  if (any(todo)) {
    # User must supply `default` to avoid this second conversion
    if (is_null(default)) {
      default <- x
      c(out, default) %<-% vec_cast_common(out, default)
    }

    default <- vec_recycle(default, vec_size(x))
    vec_slice(out, todo) <- vec_slice(default, todo)
  }

  out
}
