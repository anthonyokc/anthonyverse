highchart_label_formatter <- function(variable_name,
                                      prefix = "",
                                      suffix = "",
                                      value_identifier = "this.value",
                                      digits = 0,
                                      dataClass = FALSE
                                      ) {

    if (str_detect(variable_name, "[P|p]ercent")) {
        formatted_value <- "Highcharts.numberFormat(this.value * 100, 0) + '%'"
    } else if (str_detect(variable_name, "[I|i]ncome|[A|a]mount")) {
        formatted_value <- "'$' + Highcharts.numberFormat(this.value, 0)"
    } else {
        formatted_value <- "Highcharts.numberFormat(this.value, 0)"
    }

    if (value_identifier != "this.value") {
        formatted_value <-
            str_replace_all(formatted_value, "this.value", value_identifier)
    }

    if (digits != 0) {
        formatted_value <-
            str_replace_all(formatted_value, "0\\)", paste0(digits, "\\)"))
    }

    if (dataClass == TRUE) {
        modified_from <- str_replace_all(formatted_value, "this.value", "this.from")
        modified_to <- str_replace_all(formatted_value, "this.value", "this.to")

        return(JS(paste0("function() {
                return ", prefix,
                modified_from, " +  ' — '  + ", modified_to,
                suffix, ";", "
            }"))
        )
    }

    return(JS(paste0("function() {
            return ", prefix, formatted_value, suffix, ";", "
        }"))
    )

}
