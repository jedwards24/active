# exploring source code
# to try to find the cause for the error with "device_info" messages on wahoo data

#' @rdname FitFile-accessors
#' @importFrom dplyr filter
#' @importFrom magrittr %>% extract2
setMethod("listMessageTypes",
          signature = c("FitFile"),
          function(fitFile) {
            all_gmn <- vapply( messages(fitFile), FUN = globalMessageNumber,
                               FUN.VALUE = integer(1))
            filter(fit_data_types$mesg_num, key %in% unique(all_gmn)) %>%
              magrittr::extract2('value')
          }
)
all_gmn <- vapply( FITfileR:::messages(eb), FUN = FITfileR:::globalMessageNumber,
                   FUN.VALUE = integer(1))


types <- FITfileR:::fit_data_types # table of data types codes
types$mesg_num
types$mesg_count

msg <- tibble(key = all_gmn) %>%
  left_join(types$mesg_num) %>%
  mutate(id = row_number())
count(msg, value)
ids <- filter(msg, value == "device_info")$id

etMethod("getMessagesByType",
         signature = c("FitFile", "integer"),
         function(fitFile, message_type) {

           idx <- vapply(messages(fitFile), FUN = globalMessageNumber,
                         FUN.VALUE = integer(1)) == message_type

           if(any(idx)) {
             messages <- messages(fitFile)[ idx ]

             signatures <- vapply(messages,
                                  function(x) { paste0(x@definition@field_defs$field_def_num, collapse = "") },
                                  FUN.VALUE = character(1))

             ## this
             if(message_type != 78) {
               messages2 <- split(messages, signatures)
             } else {
               messages2 <- list(messages)
             }

             messages3 <- lapply(messages2, FUN = .processFieldsList, message_type)

             if(length(messages3) == 1) {
               messages3 <- messages3[[1]]
             } else {
               gm_name <- .translateGlobalMessageNumber( message_type )
               names(messages3) <- paste(gm_name, seq_along(messages3), sep = "_")
             }
             return(messages3)
           } else {
             message("No messages of this type found in the file.\n",
                     "You can use listMessageTypes() to identify the message types present.")
             return(NULL)
           }

         }
)

ids
idx <- vapply(FITfileR:::messages(eb), FUN = FITfileR:::globalMessageNumber,
              FUN.VALUE = integer(1)) == 23
length(FITfileR:::messages(eb))
x <- list(FITfileR:::messages(eb)[idx])
length(x)
map(x, ~FITfileR:::.processFieldsList(., 23))

x3 <- list(FITfileR:::messages(eb)[22])
x3 <- FITfileR:::messages(eb)[25]

FITfileR:::.processFieldsList(x3, 23) %>% glimpse

#--------------
# session issue with eb #270
types$mesg_num %>% filter(value == "session")
idx <- vapply(FITfileR:::messages(fit), FUN = FITfileR:::globalMessageNumber,
              FUN.VALUE = integer(1)) == 18
sum(idx)
which(idx == 1)
length(FITfileR:::messages(fit))
x <- FITfileR:::messages(fit)[17835][[1]]
length(x)
slotNames(x)
x@fields
x@definition
x <- list(FITfileR:::messages(fit)[idx])
length(x)
map(x, ~FITfileR:::.processFieldsList(., 23))

x3 <- list(FITfileR:::messages(eb)[22])
x3 <- FITfileR:::messages(eb)[25]

FITfileR:::.processFieldsList(x3, 23) %>% glimpse
