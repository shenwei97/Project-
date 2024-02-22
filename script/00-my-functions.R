# 目的:
# 描述:常用函数


# 1. gtsummary设定 ----------------------------------------------------------


# list(
#   "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 3),
#   "pkgwide-fn:prependpvalue_fun" = function(x) style_pvalue(x, digits = 3, prepend_p = TRUE),
#   "tbl_summary-fn:addnl-fn-to-run" =
#     function(x) {
#       if (!is.null(x$by)) {
#         x <- add_p(x)
#       } # add_p if there is a by variable
#       x <- add_stat_label(x)
#       x <- italicize_levels(x)
#       x <- bold_labels(x)
#     }
# ) |> set_gtsummary_theme()

# 2 flextable导出------------------------------------------------------


save_flextable_as_docx <- function(table, filename) {
  library(flextable)
  # 指定保存的路径
  output_path <- paste0(here(), "/output")
  # 创建完整的文件路径
  file_path <- file.path(output_path, paste0(filename, ".docx"))
  # 保存表格为 Word 文档
  table |>
    as_flex_table() |>
    flextable::save_as_docx(path = file_path)
}

save_ggplot <- function(plot, filename) {
  # 指定保存的路径
  output_path <- paste0(here(),'/figure')
  # 创建完整的文件路径
  file_path <- file.path(output_path, paste0(filename, ".pdf"))
  # 保存图表
  ggsave(file_path, plot = plot,  units = "in",width = 11.68, height = 8.27, dpi = 300)
}

# 3 拼接变量集合 -------------------------------------------------


paste_data_vars <- function(data) {
  paste0("'", paste(data |> colnames(), collapse = "','"), "'")
}


# 4. 识别分类变量 ---------------------------------------------------------------

select_fct_vars <-
  function(data, level) {
    data |>
      summarise_all(n_distinct) |>
      select_if(~ all(. <= level)) |>
      paste_data_vars()
  }

