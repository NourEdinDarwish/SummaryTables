describe("HTML rendering", {
  it("renders without writing to user or temp directories", {
    # jamovi Cloud mounts R user directories and tempdir() read-only.
    # Redirect R_user_dir() to an empty folder and verify that rendering
    # under default sass.cache settings writes nothing to either directory.
    userDir <- withr::local_tempdir()
    withr::local_envvar(
      R_USER_CACHE_DIR = userDir,
      R_USER_DATA_DIR = userDir,
      R_USER_CONFIG_DIR = userDir
    )
    withr::local_options(sass.cache = NULL)
    tempBefore <- list.files(
      tempdir(),
      all.files = TRUE,
      recursive = TRUE,
      include.dirs = TRUE,
      no.. = TRUE
    )

    tblCross(data = data.frame(), row = NULL, col = NULL)
    expect_null(getOption("sass.cache"))

    tblCross(data = gtsummary::trial, row = "grade", col = "trt")
    expect_null(getOption("sass.cache"))

    expect_length(list.files(userDir, all.files = TRUE, no.. = TRUE), 0L)
    expect_identical(
      list.files(
        tempdir(),
        all.files = TRUE,
        recursive = TRUE,
        include.dirs = TRUE,
        no.. = TRUE
      ),
      tempBefore
    )
  })
})
