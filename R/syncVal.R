
syncVal <- function(value = NULL, fun = identity, label = NULL, ...,
	millis = 1000, priority = 100, domain = getDefaultReactiveDomain())
{
	sv <- reactiveVal(value)
	sv_t <- throttle(sv, millis=millis,
		priority=priority, domain=domain)
	structure(function(x) {
		if ( missing(x) ) {
			sv_t()
		} else {
			val <- fun(x, ...)
			prev <- isolate(sv())
			if ( !isTRUE(all.equal(val, prev)) )
				sv(val)
		}
	}, class = "syncVal")
}
