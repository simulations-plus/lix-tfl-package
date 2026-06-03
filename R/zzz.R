# Package load hook.
#
# lixoftConnectors must be initialized via initializeLixoftConnectors() BEFORE
# any other package that uses the `zip` package is loaded into the session;
# otherwise the connectors silently stop working for the rest of the session.
# flextable (loaded lazily via requireNamespace() in TableStyles.R and
# InternalHelpers.R) pulls in `zip`, so we initialize the connectors here at
# namespace load time — this runs after all Imports are loaded but before any
# user code can trigger a flextable/zip load.
#
# Wrapped in try() so the package still loads on systems without a working
# MonolixSuite installation; downstream functions will surface clearer errors
# when they actually need the connectors. NOTE: keep flextable in Suggests
# (not Imports), otherwise it would load before this hook runs and defeat
# the fix.
.onLoad <- function(libname, pkgname) {
	try(
		suppressMessages(lixoftConnectors::initializeLixoftConnectors()),
		silent = TRUE
	)
}
