library(HypothesisManagerPlugin)

# Somehow, this does not actually override the function when `parse` is called.
# uuidCounter <<- 0
# UUIDgenerate <<- function () {
#     uuidCounter <<- uuidCounter + 1
#     print(uuidCounter)
#     uuidCounter
# }

snapshot <- function (fileName) {
    code <- readChar(fileName, file.info(fileName)$size)
    eval_ <<- FALSE
    result <- parse(code)
    snapshotName <- paste0(fileName, "-snapshot")
    saveRDS(result, snapshotName)
}