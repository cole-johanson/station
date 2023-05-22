pdf_update <- function(file, new_title=NULL, bookmarks_data=NULL) {
  tmpfile = fs::path(tempdir(),'tmp.txt')

  # Output the current file metadata
  cmd_line = str_glue(
    'pdftk',
    '{file}',
    'dump_data_utf8 output {tmpfile}',
    .sep = " "
  )
  system(cmd_line)

  file_metadata = readLines(tmpfile)
  if(!is.null(new_title)) {
    file_metadata[which(file_metadata == 'InfoKey: Title')+1] = str_glue("InfoValue: {new_title}")
  }

  if(!is.null(bookmarks_data)) {
    # bookmarks_data should be a data frame with BookmarkTitle=character(), BookmarkPageNumber=integer(),
    # and an optional BookmarkLevel=integer() (defaults to 1)
    bookmarks_data_txt = character()
    for(i in 1:nrow(bookmarks_data)) {
      bookmarks_data_txt = bookmarks_data_txt %>%
        append('BookmarkBegin') %>%
        append(str_glue('BookmarkTitle: {bookmarks_data[i,1]}')) %>%
        append(str_glue('BookmarkLevel: 1')) %>%
        append(str_glue('BookmarkPageNumber: {bookmarks_data[i,2]}'))
    }
    file_metadata = file_metadata %>% append(bookmarks_data_txt)
  }

  writeLines(file_metadata, tmpfile)

  # Update the metadata
  tmp_pdf = fs::path(tempdir(),'tmp.pdf')
  cmd_line = str_glue(
    'pdftk',
    '{file}',
    'update_info_utf8 {tmpfile}',
    'output {tmp_pdf}',
    .sep = " "
  )
  system(cmd_line)

  # Copy tmp_pdf to combined path
  file.copy(tmp_pdf, file, overwrite = T)

  file.remove(tmp_pdf)
  file.remove(tmpfile)
}
