#' A simplistic HTML parser for Synop data from the Ogimet webpage
#'
#' Parses an HTML table into a data.frame using base R only with no external dependencies
#' The code relies purely on regular expressions and basic string and regex functions.
#' 
#' @details
#' By default the function grabs the first `TABLE` element it finds. If the HTML code
#' has several tables and you need a specific one, pass a regex that
#' matches its opening tag via `table_start_pattern`, e.g.:
#'
#' \preformatted{
#' parse_html_table(html, table_start_pattern = '<TABLE align="center" border=0[^>]*>')
#' }
#'
#' @param html a single string containing the HTML body (or any larger HTML chunk) that includes the table you want to parse.
#' 
#' 
#' @export
#' @return description
#' @return data.frame with automatically detected column names (built from
#         the header's rowspan/colspan structure) and numeric columns
#         auto-converted where possible. Any column made up entirely of
#         <img> cells (e.g. weather icons) is dropped automatically.
#' @keywords internal
#' @importFrom stats ave

parse_html_table = function(html, table_start_pattern = "<TABLE[^>]*>") {
  
  extract_cell_text = function(cell) {
    txt = gsub("<[^>]+>", "", cell)
    txt = gsub("&nbsp;", " ", txt)
    txt = gsub("[\x01-\x08\x0B\x0C\x0E-\x1F\x7F]", "", txt, perl = TRUE)
    trimws(txt)
  }
  
  split_html_rows = function(html_chunk) {
    starts = gregexpr("(?s)<TR[^>]*>", html_chunk, perl = TRUE, ignore.case = TRUE)[[1]]
    if (starts[1] == -1) return(character(0))
    ends = c(starts[-1] - 1, nchar(html_chunk))
    substring(html_chunk, starts, ends)
  }
  
  raw_cells_from_row = function(row) {
    regmatches(
      row,
      gregexpr("(?s)<T[DH][^>]*>.*?</T[DH]>", row, perl = TRUE, ignore.case = TRUE)
    )[[1]]
  }
  
  parse_data_row = function(row) {
    raw = raw_cells_from_row(row)
    is_image = grepl("<img", raw, ignore.case = TRUE)
    text = vapply(raw, function(cell) {
      if (grepl("<img", cell, ignore.case = TRUE)) NA_character_ else extract_cell_text(cell)
    }, character(1), USE.NAMES = FALSE)
    list(text = text, is_image = is_image)
  }
  
  extract_table = function(html, start_pattern) {
    start_pos = regexpr(start_pattern, html, ignore.case = TRUE)
    if (start_pos == -1) stop("No table matching the given pattern was found.")
    remainder = substr(html, start_pos, nchar(html))
    end_pos = regexpr("(?s)</TABLE>", remainder, perl = TRUE, ignore.case = TRUE)
    substr(remainder, 1, end_pos + attr(end_pos, "match.length") - 1)
  }
  
  extract_numeric_attr = function(tag, attr_name, default = 1L) {
    pattern = paste0(attr_name, "\\s*=\\s*[\"']?([0-9]+)[\"']?")
    m = regmatches(tag, regexec(pattern, tag, ignore.case = TRUE))[[1]]
    if (length(m) < 2) default else as.integer(m[2])
  }
  
  extract_headers = function(table_html) {
    thead_match = regmatches(
      table_html,
      regexpr("(?s)<THEAD>.*?</THEAD>", table_html, perl = TRUE, ignore.case = TRUE)
    )
    
    if (length(thead_match) > 0) {
      thead_html = thead_match
    } else {
      all_rows = split_html_rows(table_html)
      n_header_rows = 0L
      for (row in all_rows) {
        if (grepl("<TH", row, ignore.case = TRUE)) {
          n_header_rows = n_header_rows + 1L
        } else break
      }
      thead_html = paste(all_rows[seq_len(n_header_rows)], collapse = "\n")
    }
    
    header_rows = split_html_rows(thead_html)
    n_rows = length(header_rows)
    
    reserved_until = new.env()
    grid = new.env()
    max_col = 0L
    
    for (r in seq_len(n_rows)) {
      cells = regmatches(
        header_rows[r],
        gregexpr("(?s)<TH[^>]*>.*?</TH>", header_rows[r], perl = TRUE, ignore.case = TRUE)
      )[[1]]
      
      current_col = 1L
      for (cell in cells) {
        key = as.character(current_col)
        while (!is.null(reserved_until[[key]]) && reserved_until[[key]] >= r) {
          current_col = current_col + 1L
          key = as.character(current_col)
        }
        
        rowspan = extract_numeric_attr(cell, "rowspan", 1L)
        colspan = extract_numeric_attr(cell, "colspan", 1L)
        text    = extract_cell_text(cell)
        
        for (c in current_col:(current_col + colspan - 1L)) {
          for (rr in r:(r + rowspan - 1L)) {
            grid[[paste0(rr, "_", c)]] = text
          }
          reserved_until[[as.character(c)]] = r + rowspan - 1L
        }
        max_col = max(max_col, current_col + colspan - 1L)
        current_col = current_col + colspan
      }
    }
    
    names_out = character(max_col)
    for (c in seq_len(max_col)) {
      parts = character(0)
      for (r in seq_len(n_rows)) {
        t = grid[[paste0(r, "_", c)]]
        if (!is.null(t) && (length(parts) == 0 || parts[length(parts)] != t)) {
          parts = c(parts, t)
        }
      }
      names_out[c] = paste(parts, collapse = "_")
    }
    
    # de-duplicate repeated names (e.g. 8x "Daily weather summary")
    ave(names_out, names_out, FUN = function(x) {
      if (length(x) == 1) x else paste0(x, "_", seq_along(x))
    })
  }
  
  table_html = extract_table(html, table_start_pattern)
  column_names = extract_headers(table_html)
  
  all_rows = split_html_rows(table_html)
  data_rows = all_rows[!grepl("<TH", all_rows, ignore.case = TRUE)]
  
  parsed_rows = lapply(data_rows, parse_data_row)
  
  image_matrix = do.call(rbind, lapply(parsed_rows, function(row) row$is_image))
  image_mask = apply(image_matrix, 2, any)
  
  filtered_rows = lapply(parsed_rows, function(row) row$text[!image_mask])
  filtered_names = column_names[!image_mask]
  
  row_lengths = lengths(filtered_rows)
  if (length(unique(row_lengths)) > 1) {
    bad_row = which(row_lengths != row_lengths[1])[1]
    stop(sprintf(
      "Inconsistent number of cells across data rows: row 1 has %d cells, row %d has %d. Check the source HTML for malformed rows.",
      row_lengths[1], bad_row, row_lengths[bad_row]
    ))
  }
  
  df = as.data.frame(do.call(rbind, filtered_rows), stringsAsFactors = FALSE)
  colnames(df) = filtered_names
  
  # auto-convert to numeric where safe
  for (col in colnames(df)) {
    values = df[[col]]
    values[values %in% c("----", "")] = NA
    numeric_try = suppressWarnings(as.numeric(values))
    
    if (sum(is.na(numeric_try)) == sum(is.na(values))) {
      df[[col]] = numeric_try
    } else {
      df[[col]] = values
    }
  }
  
  return(df)
}
