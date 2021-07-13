#' Make a field chart from df
#'
#' Field Chart를 만들기 위한 함수
#'
#' @param wd 작업 디렉토리, 기본값 = getwd()
#' @param df 데이터프레임(group, x축 좌표(문자), y축 좌표(숫자), 기본값 = hedxTools::ffr_fdr_sample
#' @param yCol y축 좌표가 위치한 행, 기본값 = "value"
#' @param xCol x축 좌표가 위치한 행, 기본값 = "PURC_MON_NEW"
#' @param groupCol 데이터 Group이 위치한 행, 기본값 = "group"
#' @param xType x축의 타입 : "datetime" 또는 "category", 기본값 = "datetime"
#' @param xLeftMargin x축 좌측의 여백값으로 타입에 따라 값을 변경 해야함, 기본값 = 0.15
#' @param yMax y축의 최대값으로 설정하지 않으면 최대 값의 140%로 설정됨, 기본값 = FALSE
#' @param y2Max 우측 y축의 최대값으로 설정하지 않으면 최대 값의 140%로 설정됨, 기본값 = FALSE
#' @param yLeftText y축 좌측 문구, 기본값 = "FFR(%)"
#' @param yRightText y축 우측 문구, 기본값 = "FDR(%)"
#' @param addName Group name 우측의 추가 문구, 기본값 = NULL
#' @param lineWidth 라인 두께, 기본값 = 1
#' @param tickIntervalY y축 라벨 표기 간격, 기본값 = 0.5
#' @param tickIntervalX x축 라벨 표기 간격으로 datetime 타입의 경우 초단위로 설정
#' @param useCustomize 라인 색상, Symbol 등 사용자 지정 속성을 적용할지 여부, 기본값 = TRUE
#' @param yAxis 각 라인별 y축 선택 0:좌측, 1:우측, 기본값 = yAxis = c(0, 0, 0, 1, 1, 1)
#'  기본값 = 30 * 24 * 3600 * 1000
#' @param linelabelSignals 라인라벨의 시그널 색상,
#'  기본값 = c("", "", "", "green", "", "green"),
#' @param linelabelSymbols 라인라벨 시그널의 모양, 기본값 = c("", "", "", "●", "", "●"),
#' @param weeklabelDate 주간 실적 라벨에 표기될 날짜, 기본값 = c("(3/4)", "(3/11)")
#' @param weeklabelValue 주간 실적 라벨에 표기될 수치 : c(지난주 실적, 금주 실적), 기본겂 = c(1.06, 1.04)
#' @param lineSymbols 라인의 심볼 : circle, diamond 또는 square ,
#'  기본값 = c('circle', 'circle', 'circle', 'diamond', 'diamond', 'square'),
#' @param lineSymbolColors 라인 심볼의 색상으로 설정하지 않으면 라인 색상을 따라감,
#'  기본값 = c('white', '', 'white', '', '', 'white'),
#' @param markerHover 라인에 마우스를 올렸을 때 라인 심볼의 표시 여부 : TRUE 또는 FALSE, 기본값 = TRUE
#' @param groupColors 그룹별 라인과 라인라벨의 색상 : 색상코드 또는 FALSE,
#'  기본값 = c("#000000", "#FF0000", "#008000", "#FF00FF", "#7F7F7F", "#FFC000"),
#' @param useDatalabels 데이터 라벨의 표시 여부 : TRUE 또는 FALSE, 기본값 = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
#' @param datalabelsOrder 데이터 라벨의 우선 순위 : 기본값 = c("'20(R)", "'21(T)", "'21(R)", "L3M('20)", "L3M", "L6M"),
#' @param yRightUse 우측 Y축을 사용할지 여부, 기본값 = TRUE
#' @param useLeftlabels 좌측 라벨을 사용할지 여부, 기본값 = TRUE
#' @param useLinelabels 라인별 라벨을 사용할지 여부, 기본값 = FALSE
#' @param useWeeklabels 주간 라벨을 사용할지 여부, 기본값 = TRUE
#' @param titleSignal Title 좌측의 시그널 색상, 기본값 = "green"
#' @param fontFamily 타이틀, 라벨, 데이터라벨의 폰트, 기본값 = "LG스마트체 Regular"
#' @param titleText 타이틀 문구, 기본값 = "Global OLED (Product)"
#' @param titleFontWeight 타이틀 폰트 스타일 : 'normal' 또는 bold', 기본값 = 'bold'
#' @param titleFontSize 타이틀 폰트 사이즈, 기본값 = "16px"
#' @param linelabelFontWeight 라인라벨의 폰트 스타일 : 'normal' 또는 bold', 기본값 = 'bold'
#' @param linelabelFontSize 라인라벨의 폰트 사이즈, 기본값 = "12px"
#' @param weeklabelFontWeight 주간 라벨의 폰트 스타일 : 'normal' 또는 bold', 기본값 = 'bold'
#' @param weeklabelFontSize 주간 라벨의 폰트 사이즈, 기본값 = "12px"
#' @param datalabelFontWeight 데이터 라벨의 폰트 스타일 : 'normal' 또는 bold', 기본값 = "normal"
#' @param datalabelOutline 데이터 라벨의 아웃라인 : "사이즈 색상", 기본값 = "1px white"
#' @param imageHeight base64 이미지의 높이, 기본값 = 400
#' @param imageWidth base64 이미지의 넓이, 기본값 = 640
#' @param base64 base64 이미지 또는 htmlwidget object 출력을 선택, 기본값 = TRUE
#' @return base64 문자열 또는 htmlwidget object.
#'
#' @rdname makeFieldChart
#' @importFrom magrittr %>%
#' @export

# 컬럼 정의 ----
makeFieldChart <- function(wd = getwd(),
                           df = hedxTools::ffr_fdr_sample,
                           yCol = "value",
                           xCol = "PURC_MON_NEW",
                           barCol = NA,
                           groupCol = "group",
                           xType = "datetime",
                           xLeftMargin = 0.15,
                           yMax = FALSE,
                           y2Max = FALSE,
                           yLeftText = "FFR(%)",
                           yRightText = "FDR(%)",
                           addName = NULL,
                           lineWidth = 1,
                           tickIntervalY = 0.5,
                           tickIntervalX = 30 * 24 * 3600 * 1000,
                           useCustomize = TRUE,
                           yAxis = c(0, 0, 0, 1, 1, 1), #
                           linelabelSignals = c("", "", "", "", "green", "green"),
                           linelabelSymbols = c("", "", "", "", "●", "●"),
                           weeklabelDate = c("(3/4)", "(3/11)"),
                           weeklabelValue = c(1.06, 1.04),
                           lineSymbols = c("circle", "circle", "circle", "diamond", "diamond", "square"),
                           lineSymbolColors = c("white", "white", "", "", "", "white"),
                           markerHover = TRUE,
                           groupColors = c("#000000", "#008000", "#FF0000", "#7F7F7F", "#FF00FF", "#FFC000"),
                           useDatalabels = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE),
                           datalabelsOrder = c("'20(R)", "'21(T)", "'21(R)", "L3M('20)", "L3M", "L6M"),
                           dataLabelsOverlap = TRUE,
                           yRightUse = TRUE,
                           labelLocation = "left",
                           leftLabelGrid = 20,
                           useLeftlabels = TRUE,
                           useLinelabels = FALSE,
                           useWeeklabels = TRUE,
                           useLegend = TRUE,
                           useRound = TRUE,
                           useExport = FALSE,
                           titleSignal = "green", #
                           fontFamily = "LG스마트체2.0 Light",
                           titleText = "Global OLED (Product)",
                           fileName = "temp",
                           titleFontWeight = "bold",
                           titleFontSize = "16px",
                           linelabelFontWeight = "bold",
                           linelabelFontSize = "12px",
                           weeklabelFontWeight = "bold",
                           weeklabelFontSize = "12px",
                           legendFontSize = "12px",
                           xAxisLabelsSize = "12px",
                           datalabelFontWeight = "normal",
                           datalabelOutline = "1px white",
                           imageHeight = 400,
                           imageWidth = 640,
                           base64 = TRUE,
                           clickSeries = NULL,
                           deleteTmp = TRUE) {

  # Main function------------------------------------------

  setwd(wd)
  df <- dplyr::rename(df, "yCol" = yCol, "xCol" = xCol, "group" = groupCol)

  if (!is.na(barCol)) df <- dplyr::rename(df, "barCol" = barCol)

  # x 좌표는 소수점 둘째자리로 반올림, y좌표는 datetime으로 변경
  if (useRound == TRUE) {
    df["yCol"] <- round(df["yCol"], digit = 3)
  } 

  if (xType == "datetime") {
    df["xCol"] <- as.Date(paste0(df[["xCol"]], 1), "%Y%m%d")
  }

  # group별로 df를 분리 해주고 정렬
  df_group <- split(df, df$group)

  if ((length(unique(df$group)) != length(unique(datalabelsOrder)) || !all(datalabelsOrder %in% unique(df$group)))) {
    unique_group <- sort(unique(df$group))
  } else {
    unique_group <- datalabelsOrder
  }


  # y축 최대값을 정하기 위해 NA를 제외한 value의 최대값을 구하고 1.4를 곱함
  y_max <- ifelse(yMax, yMax, max(df$yCol[!is.na(df$yCol)]) * 1.4)
  if(y_max == 0) y_max <- 10

  y2_max <- ifelse(y2Max, y2Max, y_max)

  # 주간 실적의 시그널과 라벨을 구하기 위한 데이터 프레임을 만듬

  # 금주, 지난주 날짜와 실적을 dataframe으로 만들고 desc를 기준으로 group을 나눠줌
  ffr_week <- data.frame(weeklabelDate, weeklabelValue)

  # 금주, 지난주 실적을 기준으로 signal을 구해줌
  ffr_signal <- hedxTools::checkWeekSignal(
    ffr_week[1, "weeklabelValue"],
    ffr_week[2, "weeklabelValue"]
  )

  # x축의 가장 마지막 좌표를 주해줌
  top_label_x <- ifelse(
    xType == "datetime",
    highcharter::datetime_to_timestamp(sort(df$xCol)[length(df$xCol)]),
    length(unique(df$xCol))
  )

  label_y <- c()
  label_x <- c()

  for (group_name in unique_group) {
    group_name <- as.character(group_name)
    value_y <- df %>%
      dplyr::filter(!is.na(yCol), group == group_name) %>%
      dplyr::select(yCol)
    value_x <- df %>%
      dplyr::filter(!is.na(yCol), group == group_name) %>%
      dplyr::select(xCol)
    # label_text[group_name] <- as.character(df_group[[as.character(group_name)]][["group"]][1])
    if(labelLocation == "right") {
      label_x[group_name] <- ifelse(xType == "datetime", highcharter::datetime_to_timestamp(rev(value_x$xCol)[1]), length(value_x$xCol))
      label_y[group_name] <- rev(value_y$yCol)[1]
    } else {
      label_x[group_name] <- ifelse(xType == "datetime", highcharter::datetime_to_timestamp(value_x$xCol[1]), value_x$xCol[1])
      label_y[group_name] <- value_y$yCol[1]
    }
  }
  label_loc <- (c(1:leftLabelGrid) - 0.5) * y_max / leftLabelGrid
  label_y <- sort(label_y, na.last = TRUE)
  
  for(index in 1:length(label_y)) {
    if(!is.na(label_y[index])) {
      label_y[index] <- label_loc[which.min(abs(label_loc - label_y[index]))]
      label_loc <- label_loc[label_loc != label_y[index]]
    }
  }
  
  label_y <- label_y[unique_group]

  if (!useCustomize) {
    yAxis <- 0
    linelabelSignals <- "black"
    linelabelSymbols <- ""
    useDatalabels <- FALSE
    lineSymbols <- FALSE
    lineSymbolColors <- FALSE
  }

  if(length(lineSymbols) != 1) {
    label_x <- ifelse(xType == "datetime", highcharter::datetime_to_timestamp(df[["xCol"]][1]), 1)
  }
  group_colors <- c()
  group_colors[1:length(unique_group)] <- "#A6A6A6"
  group_colors_index <- length(group_colors) - length(groupColors) + 1
  group_colors[group_colors_index:length(group_colors)] <- groupColors

  label_df <- data.frame(
    label_x,
    label_y,
    yAxis,
    linelabelSignals,
    linelabelSymbols,
    group_colors,
    useDatalabels,
    lineSymbols,
    lineSymbolColors
  ) %>% dplyr::filter(!is.na(label_y))

  label <- list()
  # 옵션값을 가지고 라인 좌측 라벨 구조 생성
  for (group in rownames(label_df)) {
    label[[length(label) + 1]] <- list(
      point = list(x = label_df[group, ]$label_x - 1, y = label_df[group, ]$label_y, xAxis = 0, yAxis = label_df[group, ]$yAxis),
      borderWidth = 0,
      text = paste0(
        "<span style='color:",
        label_df[group, ]$linelabelSignals, ";'>",
        label_df[group, ]$linelabelSymbols,
        "</span>",
        "<p style='color:",
        label_df[group, ]$group_colors, ";'>",
        group,
        "</span>"
      )
    )
  }

  # each series Chart ----
  dxChart <- highcharter::highchart() %>%
    highcharter::hc_chart(zoomType = "x", plotBorderWidth = 1, animation = if (base64) FALSE) %>%
    highcharter::hc_legend(enabled = useLegend, itemStyle = list(fontSize = legendFontSize)) %>%
    highcharter::hc_yAxis_multiples(
      list(
        title = list(text = yLeftText, align = 'high'),
        min = 0,
        max = y_max,
        tickInterval = tickIntervalY,
        endOnTick = FALSE,
        gridLineColor = ""
      ),
      list(
        title = list(text = yRightText, align = 'low'),
        visible = yRightUse,
        min = 0,
        max = y2_max,
        endOnTick = FALSE,
        gridLineColor = "",
        opposite = TRUE
      )
    ) %>%
    highcharter::hc_xAxis(
      minPadding = xLeftMargin,
      type = xType,
      showFirstLabel = ifelse(xType == "datetime", FALSE, TRUE),
      tickInterval = tickIntervalX,
      crosshair = list(
        width = 1,
        color = "#DFDFDF",
        dashStyle = "shortdot"
      ),
      labels = list(
        format = ifelse(xType == "datetime", "{value:%b}", "{value}"),
        style = list(fontSize = xAxisLabelsSize)
      )
    ) %>%
    highcharter::hc_plotOptions(
      series = list(
        dataLabels = list(
          allowOverlap = dataLabelsOverlap,
          format = "{point.yCol:.2f}",
          style = list(fontWeight = datalabelFontWeight, textOutline = datalabelOutline)
        ),
        lineWidth = lineWidth,
        animation = if (base64) FALSE,
        cursor = 'pointer',
        events = list(click = clickSeries)
      )
    ) %>%
    # dataLabels 전역 설정
    highcharter::hc_tooltip(
      shared = TRUE,
      useHTML = TRUE,
      headerFormat = '<small>{point.key}</small><table>',
      pointFormat = '<tr><td style="color: {series.color}">{series.name}: </td><td style="text-align: right"><b>{point.yCol:.2f}%</b></td></tr>',
      footerFormat = '</table>'
    ) %>%
    highcharter::hc_title(
      text = paste0(
        "<span style='color:",
        titleSignal,
        ";'>",
        if (titleSignal == "black") {
          "○ "
        } else if (titleSignal == "white") {
          ""
        } else {
          "● "
        },
        "</span>",
        titleText
      ),
      margin = 10, align = "center",
      style = list(fontFamily = fontFamily, fontWeight = titleFontWeight, useHTML = TRUE, fontSize = titleFontSize)
    ) %>%
    hc_exporting(
      enabled = useExport,
      buttons = list(contextButton = list(menuItems = list("viewFullscreen", "separator", "downloadPNG", "downloadPDF", "downloadCSV"))),
      filename = paste0(titleText, "_", Sys.Date())
    )


  if (useLeftlabels) {
    dxChart <- dxChart %>% highcharter::hc_add_annotation(
      labelOptions = list(
        y = ifelse(labelLocation == "right", -10, 0),
        x = ifelse(labelLocation == "right", 10, -13),
        verticalAlign = "middle",
        allowOverlap = TRUE,
        overflow = 'none',
        align = "right",
        padding = 1,
        style = list(fontFamily = fontFamily, fontWeight = linelabelFontWeight, fontSize = linelabelFontSize),
        backgroundColor = ""
      ),
      labels = label
    )
  }

  if (useWeeklabels) {
    dxChart <- dxChart %>%
      highcharter::hc_add_annotation(
        draggable = FALSE,
        labelOptions = list(
          y = -8,
          x = -179,
          verticalAlign = "middle",
          allowOverlap = TRUE,
          align = "left",
          style = list(fontFamily = fontFamily, fontWeight = weeklabelFontWeight, fontSize = weeklabelFontSize),
          backgroundColor = "white"
        ),
        labels = list(
          point = list(x = top_label_x, y = y_max, xAxis = 0, yAxis = 0),
          borderWidth = 0,
          text = paste(ffr_week[1, "weeklabelValue"], ffr_week[1, "weeklabelDate"], "→")
        )
      ) %>%
      highcharter::hc_add_annotation(
        draggable = FALSE,
        labelOptions = list(
          y = -8,
          x = 0,
          verticalAlign = "middle",
          allowOverlap = TRUE,
          align = "left",
          style = list(
            fontFamily = fontFamily,
            fontWeight = weeklabelFontWeight,
            fontSize = weeklabelFontSize,
            color = ffr_signal
          ),
          backgroundColor = rgb(217 / 255, 217 / 255, 217 / 255)
        ),
        labels = list(
          point = list(x = top_label_x, y = y_max, xAxis = 0, yAxis = 0),
          borderWidth = 0,
          text = paste(ffr_week[2, "weeklabelValue"], ffr_week[2, "weeklabelDate"])
        )
      )
  }

  # group별 데이터, 라벨 이름, 마커 옵션을 넣어줌
  for (group in rownames(label_df)) {
    dxChart <- dxChart %>%
      highcharter::hc_add_series(
        data = df_group[[group]],
        name = paste(group, addName[1]),
        highcharter::hcaes(x = xCol, y = yCol),
        marker = list(
          enabled = ifelse(lineSymbols, TRUE, FALSE),
          states = list(hover = list(enabled = markerHover)),
          symbol = label_df[group, ]$lineSymbols,
          fillColor = label_df[group, ]$lineSymbolColors,
          lineWidth = 1,
          lineColor = NULL
        ),
        dataLabels = list(
          enabled = label_df[group, ]$useDatalabels,
          color = label_df[group, ]$group_colors
        ),
        label = list(enabled = useLinelabels, style = list(fontWeight = "nomal")),
        color = label_df[group, ]$group_colors,
        yAxis = label_df[group, ]$yAxis,
        type = "line",
      )

    if (!is.na(barCol)) {
      dxChart <- dxChart %>%
        highcharter::hc_add_series(
          data = df_group[[group]],
          name = paste(group, addName[2]),
          yAxis = 1,
          highcharter::hcaes(x = xCol, y = barCol),
          color = label_df[group, ][["group_colors"]],
          type = "column"
        )
    }
  }
  if (base64 == TRUE) {
    hash <- hashids::encode(as.integer(Sys.time()), hashids::hashid_settings(titleText))
    if (!dir.exists(paste0("tmp_", hash))) {
      dir.create(paste0("tmp_", hash))
    }
    html_path <- sprintf("./tmp_%s/%s_%s.html", hash, fileName, format(Sys.Date(), "%y%m%d"))
    tf1 <- sprintf("./tmp_%s/%s_%s.png", hash, fileName, format(Sys.Date(), "%y%m%d"))

    htmlwidgets::saveWidget(widget = dxChart, file = html_path)
    if (!webshot::is_phantomjs_installed()) {
      webshot::install_phantomjs()
    }
    webshot::webshot(url = html_path, vheight = imageHeight, vwidth = imageWidth, file = tf1)
    # png를 base64로 변경
    base64 <- RCurl::base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")
    if (deleteTmp) unlink(paste0("tmp_", hash), recursive = TRUE)
    return(base64)
  } else {
    return(dxChart)
  }
}

#' Base64 str to Html page
#'
#' base64 문자열을 html 페이지로 띄워서 보기 위한 함수
#'
#' @param base64Chart Base64 문자열
#'
#' @return Html page.
#' @rdname base64ToHtml
#' @export

base64ToHtml <- function(base64Chart = makeFieldChart()) {
  base64 <- base64Chart
  html <- sprintf('<html><body><img src="data:image/png;base64,%s"></body></html>', base64)
  cat(html, file = tf2 <- tempfile(fileext = ".html"))
  browseURL(tf2)
}

#' Make Chart by preset
#'
#' Preset Argument를 넣어서 차트를 만드는 함수
#'
#' @param preset ffr, hazard
#' @param df 데이터프레임(group, x축 좌표(문자), y축 좌표(숫자), 기본값 = NULL
#' @param title 타이틀 문구, 기본값 = ""
#' @param base64 base64 이미지 또는 htmlwidget object 출력을 선택, 기본값 = TRUE
#'
#' @return base64 str or htmlwidget object
#' @rdname makePresetChart
#' @export
#'
makePresetChart <- function(preset = "ffr", df = NaN, title = "", base64 = TRUE) {
  if (preset == "ffr") {
    if (is.na(df)) {
      df <- hedxTools::ffr_fdr_sample
    }
    dxChart <- hedxTools::makeFieldChart(titleText = title, base64 = base64)
    return(dxChart)
  }
  if (preset == "hazard") {
    if (is.na(df)) {
      df <- hedxTools::hazard_accumulate_sample
    }
    dxChart <- hedxTools::makeFieldChart(
      lineSymbols = FALSE,
      useCustomize = FALSE,
      xLeftMargin = 0,
      df = df,
      titleText = title,
      yLeftText = "Hazard (%)",
      xCol = "SVC_MON_NEW_ind_cal",
      yCol = "svc_rate_value",
      groupCol = "CALC_PROD_DT_ind",
      xType = "category",
      useLeftlabels = FALSE,
      yRightUse = FALSE,
      tickIntervalX = 1,
      useLinelabels = TRUE,
      markerHover = FALSE,
      base64 = base64
    )
    return(dxChart)
  }
}

#' Check Week Signal
#'
#' 지난주 금주 실적 시그널 확인 함수
#'
#' @param last 지난주 실적, 기본값 = 1
#' @param this 금주 실적, 기본값 = 1
#'
#' @return color str
#' @rdname checkWeekSignal
#' @export
#'
checkWeekSignal <- function(last = 1, this = 1) {
  if (last >= this) {
    if (last == this) {
      signal <- "black"
    } else {
      signal <- "green"
    }
  } else {
    signal <- "red"
  }
  return(signal)
}

#' Check Signal
#'
#' 시그널 확인 함수
#'
#' @param df group, x축 좌표, y축 좌표 구조로 대상 그룹으로 Filtering 후 입력 필요
#' @param target 목표값으로 L3M의 경우 전년 실적 입력 필요
#' @param type 시그널을 확인하고자 하는 차트 종류 ffr, L6M 또는 L3M
#'
#' @return color str
#' @rdname checkSignal
#' @export
#'
checkSignal <- function(df, target, type, yCol = "value", xCol = "PURC_MON_NEW", groupCol = "group") {
  df <- df %>%
    dplyr::rename(yCol = yCol, xCol = xCol, group = groupCol) %>%
    dplyr::filter(!is.na(yCol)) %>%
    dplyr::arrange(desc(xCol))

  compare_continuity <- function(df, times) {
    df <- df$yCol
    if (length(df) < times + 1) {
      message("비교할 대상이 ", times, "주기 보다 짧습니다.")
    }
    result <- TRUE
    for (index in 1:times) {
      result <- result && df[index] > df[index + 1]
    }
    return(result)
  }

  compare_target <- function(df, target, percent) {
    target <- target %>%
      dplyr::rename(yCol = yCol, xCol = xCol, group = groupCol) %>%
      dplyr::filter(xCol == df$xCol[1])
    if (is.na(target$yCol)) {
      return(FALSE)
    } else if (sum(2, -(df$yCol[1] / target$yCol[1]), na.rm = TRUE) < (percent / 100)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  if (type == "ffr") {
    if (df$yCol[1] < 1.5) {
      message("FFR : 1.5% 미만")
      signal <- "black"
    } else if (compare_target(df, target, 95) || compare_continuity(df, 3)) {
      message("FFR : 목표대비 95%↓, 3개월 연속 악화")
      signal <- "red"
    } else if (!compare_target(df, target, 100)) {
      message("FFR : 목표대비 100%↑")
      signal <- "green"
    } else if (!compare_target(df, target, 95) || compare_continuity(df, 2)) {
      message("FFR : 목표대비 95%↑, 2개월 연속 악화")
      signal <- "yellow"
    }
  }

  if (type == "L6M") {
    if (compare_continuity(df, 1)) {
      message("L6M : 전월대비 악화")
      signal <- "red"
    } else {
      message("L6M : 전월대비 개선")
      signal <- "green"
    }
  }

  if (type == "L3M") {
    if (compare_continuity(df, 1) && compare_target(df, target, 100)) {
      message("L3M : 전월대비 악화 및 전년 동기대비 악화")
      signal <- "red"
    } else if (compare_continuity(df, 1) || compare_target(df, target, 100)) {
      message("L3M : 전월대비 악화, 전년 동기대비 악화")
      signal <- "yellow"
    } else {
      message("L6M : 전월대비 개선 및 전년 동기대비 개선")
      signal <- "green"
    }
  }

  return(signal)
}
