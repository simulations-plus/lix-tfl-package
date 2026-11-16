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
# flextable is loaded. If zip is already loaded (flextable or officer loaded
# before this package), the connectors cannot be initialized in this session,
# so only restart instructions are given.
.onAttach <- function(libname, pkgname) {
	msg <- .connectorStartupMessage()
	if (!is.null(msg)) packageStartupMessage(msg)
}

#' Startup message for a failed load-time initialization of the connectors
#'
#' @return The message, or `NULL` if the initialization did not fail or the
#'   connectors have been initialized since (e.g. the namespace was loaded via
#'   `::`, the user fixed the folder and read a project, then attached it).
#' @noRd
.connectorStartupMessage <- function() {
	err <- .tflState$connectorInitError
	if (is.null(err) || .connectorsInitialized()) return(NULL)
	restart <- .zipLoaded()
	paste0(
		"MonolixSuiteTFL: ", err, "\n",
		if (restart) {
			paste0("Functions that read MonolixSuite projects will not work in this R session ",
						 "because the zip package was already loaded; styleTFL() is not affected.\n")
		} else {
			paste0("Functions that read MonolixSuite projects will not work until lixoftConnectors ",
						 "is initialized; styleTFL() is not affected.\n")
		},
		.connectorInitHelp(restart = restart)
	)
}
