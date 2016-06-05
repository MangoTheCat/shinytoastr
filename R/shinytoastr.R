
#' Initialize the toastr notification engine
#'
#' Call this function once, from the top of your Shiny UI definition.
#' Here is an example: \preformatted{  ui <- shinyUI(fluidPage(
#'     useToastr(),
#'     pageWithSidebar(
#'       headerPanel("Header"),
#'       sidebarPanel(
#'         ...
#'       ),
#'       mainPanel(
#'         ...
#'       )
#'     )
#'   ))
#' }
#'
#' @return The HTML tags to put into the \code{<head>} of the HTML file.
#'
#' @seealso \code{\link{toastr_success}}, \code{\link{toastr_info}},
#'   \code{\link{toastr_warning}}, \code{\link{toastr_error}}
#' @export
#' @importFrom shiny addResourcePath tags
#' @examples
#' ## See above

useToastr <- function() {
  addResourcePath("toastr", system.file("toastr", package = "shinytoastr"))
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "toastr/toastr.min.css"
    ),
    tags$script(
      src = "toastr/toastr.min.js"
    ),
    tags$script(
      src = "toastr/shinytoastr.js"
    )
  )
}

toastr_fun <- function(toast_type) {
  function(
    message,
    title = "",
    closeButton = FALSE,
    newestOnTop = FALSE,
    progressBar = FALSE,
    position = c("top-right", "top-center", "top-left", "top-full-width",
      "bottom-right", "bottom-center", "bottom-left", "bottom-full-width"),
    preventDuplicates = FALSE,
    showDuration = 300,
    hideDuration = 1000,
    timeout = 5000,
    extendedTimeOut = 1000,
    showEasing = c("swing", "linear"),
    hideEasing = c("swing", "linear"),
    showMethod = c("fadeIn", "slideDown", "show"),
    hideMethod = c("fadeOut", "hide")
  ) {

    options <- list(
      closeButton = isTRUE(closeButton),
      newestonTop = isTRUE(newestOnTop),
      progressBar = isTRUE(progressBar),
      positionClass = paste0("toast-", match.arg(position)),
      preventDuplicates = isTRUE(preventDuplicates),
      showDuration = as_count(showDuration),
      hideDuration = as_count(hideDuration),
      timeout = as_count(timeout),
      extendedTimeOut = as_count(extendedTimeOut),
      showEasing = match.arg(showEasing),
      hideEasing = match.arg(hideEasing),
      showMethod = match.arg(showMethod),
      hideMethod = match.arg(hideMethod)
    )

    session <- getSession()
    session$sendCustomMessage(
      type = 'toastr',
      message = list(
        type = toast_type,
        message = message,
        title = title,
        options = options
      )
    )
  }
}

#' Create toastr notifications
#'
#' There are four functions to create notifications:
#' \code{toastr_success}, \code{toastr_info}, \code{toastr_warning} and
#' \code{toastr_error}. They have exactly the same arguments and API
#' in general, but they create different kinds of notifications, styled
#' appropriately.
#'
#' By default, the notifications disappear automatically after a timeout,
#' unless the mouse cursor is over them.
#'
#' @param message Message to show.
#' @param title Optional title, shown on the top.
#' @param closeButton Whether to show a close button. Even if there
#'   is a close button, the notification can still be closed by clicking
#'   on it. For sticky notifications, it is good practice to show the close
#'   button, to tell the user that the notification can be closed.
#' @param newestOnTop Whether to have the newest notification on the top.
#' @param progressBar Whether to show a progress bar.
#' @param position Where to put the notification. Possible values:
#'   \code{top-right}, \code{top-center}, \code{top-left},
#'   \code{top-full-width}, and the corresponding \code{bottom-right}, etc.
#' @param preventDuplicates Whether to prevent showing exactly the
#'   same message as the previous one. Note that only the message matters
#'   here, the title is ignored.
#' @param showDuration How long the initial show transition should take,
#'   in milliseconds.
#' @param hideDuration How long the final hide transition should take,
#'   in milliseconds.
#' @param timeout How long the notification should be kept on the screen,
#'   in milliseconds. Set it to zero to keep it on the screen until it is
#'   clicked. Note that if the mouse cursor is over the notification, then
#'   it is kept on the screen for \code{extendedTimeOut} milliseconds,
#'   after the cursor has left.
#' @param extendedTimeOut How long to keep the notification on the screen
#'   after the mouse cursor leaves it, in milliseconds.
#' @param showEasing Animation easing to show the notification.
#'   Possible values: \code{swing}, \code{linear}.
#' @param hideEasing Animation easing to hide the notification.
#'   Possible values: \code{swing}, \code{linear}.
#' @param showMethod Animation to show the notification.
#'   Possible values: \code{fadeIn}, \code{slideDown}, \code{show}.
#' @param hideMethod Animation to hide the notification.
#'   Possible values: \code{fadeOut}, \code{hide}.
#'
#' @section Example:
#'
#' You typically use these functions in the definition of the Shiny
#' server program, as a response to an event, i.e. in the output functions,
#' reactives, or in \code{observe} or \code{observeEvent}. Here is
#' an example that shows a note when the data was successfully written to
#' a database, and an error otherwise. \preformatted{  observeEvent(
#'     input$save_button,
#'     {
#'       tryCatch(
#'         {
#'           writeToDB(data)
#'           toastr_success("Saved to database")
#'         },
#'         error = function(e) {
#'           toastr_error(title = "Database error", conditionMessage(e))
#'         }
#'       }
#'     }
#'   )
#'
#' @seealso \code{\link{useToastr}}
#' @export
#' @examples
#' ## See above

toastr_success <- toastr_fun("success")

#' @rdname toastr_success
#' @family toastr functions
#' @export

toastr_info <- toastr_fun("info")

#' @rdname toastr_success
#' @family toastr functions
#' @export

toastr_warning <- toastr_fun("warning")

#' @rdname toastr_success
#' @family toastr functions
#' @export

toastr_error <- toastr_fun("error")

#' @importFrom shiny getDefaultReactiveDomain

getSession <- function() {
  session <- getDefaultReactiveDomain()
  if (is.null(session)) {
    stop("could not find the Shiny session object. This usually happens ",
         "when toastr is called from a context that wasn't set up by a ",
         "Shiny session.")
  }
  session
}

as_count <- function(x) {
  x <- as.integer(x)
  stopifnot(length(x) == 1, ! is.na(x))
  x
}
