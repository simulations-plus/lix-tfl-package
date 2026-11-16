# Package load hooks.
#
# lixoftConnectors must be initialized via initializeLixoftConnectors() BEFORE
# any other package that uses the `zip` package is loaded into the session;
# otherwise the MonolixSuite library fails to load for the rest of the session.
# flextable (loaded lazily via requireNamespace() in TableStyles.R and
# InternalHelpers.R) pulls in `zip`, so we initialize the connectors here at
# namespace load time — this runs after all Imports are loaded but before any
# user code can trigger a flextable/zip load. NOTE: keep flextable in Suggests
# (not Imports), otherwise it would load before this hook runs and defeat
# the fix.
#
# lixoftConnectors is in Suggests (it ships with MonolixSuite, not CRAN), so
# styleTFL() and the other helpers that do not read project results work
# without it. See .initializeConnectorsOnLoad() in InternalHelpers.R: it skips
# the initialization if lixoftConnectors is not installed or already
# initialized, uses the TFLMonolixSuite.lixoftPath option for MonolixSuite
# installations other than the one in lixoft.ini, and never makes loading fail.
.onLoad <- function(libname, pkgname) {
	.initializeConnectorsOnLoad()
}

# Report a failed initialization when the package is attached, so users with a
# MonolixSuite installation in a non-default folder learn how to set it before
# flextable is loaded (after that, R must be restarted).
.onAttach <- function(libname, pkgname) {
	err <- .tflState$connectorInitError
	if (!is.null(err)) {
		packageStartupMessage(
			"MonolixSuiteTFL: ", err, "\n",
			"Functions that read MonolixSuite projects will not work until lixoftConnectors ",
			"is initialized; styleTFL() is not affected.\n",
			.connectorInitHelp()
		)
	}
}
