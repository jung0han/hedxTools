#' Make a field chart from df
#'
#' Field Chart를 만들기 위한 함수
#'
#' @param wd 작업 디렉토리, 기본값 = getwd()
#' @param df 데이터프레임(group, x축 좌표(문자), y축 좌표(숫자), 기본값 = read.csv("./data/sample.csv", header = T)
#' @param xColumn x축 좌표가 위치한 행, 기본값 = "value"
#' @param yColumn y축 좌표가 위한 행, 기본값 = "PURC_MON_NEW"
#' @param xType x축의 타입 : "datetime" 또는 "category", 기본값 = "datatime"
#' @param xLeftMargin x축 좌측의 여백값으로 타입에 따라 값을 변경 해야함, 기본값 = 0.15
#' @param yMaxRate y축 좌표의 최대값 대비 y축의 최대값 비율, 기본값 = 1.4
#' @param yLeftText y축 좌측 문구, 기본값 = "FFR(%)"
#' @param yRightText y축 우측 문구, 기본값 = "FDR(%)"
#' @param lineWidth 라인 두께, 기본값 = 1
#' @param tickIntervalY y축 라벨 표기 간격, 기본값 = 0.5
#' @param tickIntervalX x축 라벨 표기 간격으로 datetime 타입의 경우 초단위로 설정, 기본값 = 30 * 24 * 3600 * 1000
#' @param fontFamily 타이틀, 라벨, 데이터라벨의 폰트, 기본값 = "LG스마트체 Regular"
#' @param titleText 타이틀 문구, 기본값 = "Global OLED (Product)"
#' @param titleFontWeight 타이틀 폰트 스타일 : 'normal' 또는 bold', 기본값 = 'bold'
#' @param titleFontSize 타이틀 폰트 사이즈, 기본값 = "16px"
#' @param linelabelFontWeight 라인라벨의 폰트 스타일 : 'normal' 또는 bold', 기본값 = 'bold'
#' @param linelabelFontSize 라인라벨의 폰트 사이즈, 기본값 = "12px"
#' @param weeklabelFontWeight 주간 라벨의 폰트 스타일 : 'normal' 또는 bold', 기본값 = 'bold'
#' @param weeklabelFontSize 주간 라벨의 폰트 사이즈, 기본값 = "12px"
#' @param useDatalabels 데이터 라벨의 사용 여부, 기본값 = TRUE
#' @param datalabelFontWeight 데이터 라벨의 폰트 스타일 : 'normal' 또는 bold', 기본값 = "normal"
#' @param datalabelOutline 데이터 라벨의 아웃라인 : "사이즈 색상", 기본값 = "1px white"
#' @param imageHeight base64 이미지의 높이, 기본값 = 400
#' @param imageWidth base64 이미지의 넓이, 기본값 = 640
#' @param base64 base64 이미지 또는 htmlwidget object 출력을 선택, 기본값 = TRUE
#' @return base64 문자열 또는 htmlwidget object.
#'
#' @rdname makeFieldChart
#' @export

# library("highcharter", "stringr", "webshot", "RCurl")

# 컬럼 정의 ----
makeFieldChart <- function(
  wd = getwd(),
  df = read.csv("./data/sample.csv", header = T),
  xColumn = "value",
  yColumn = "PURC_MON_NEW",
  xType = "datatime",
  xLeftMargin = 0.15,
  yMaxRate = 1.4,
  yLeftText = "FFR(%)",
  yRightText = "FDR(%)",
  lineWidth = 1,
  tickIntervalY = 0.5,
  tickIntervalX = 30 * 24 * 3600 * 1000,
  fontFamily = "LG스마트체 Regular",
  titleText = "Global OLED (Product)",
  titleFontWeight = 'bold',
  titleFontSize = "16px",
  linelabelFontWeight = 'bold',
  linelabelFontSize = "12px",
  weeklabelFontWeight = 'bold',
  weeklabelFontSize = "12px",
  useDatalabels = TRUE,
  datalabelFontWeight = "normal",
  datalabelOutline = "1px white",
  imageHeight = 400,
  imageWidth = 640,
  base64 = TRUE
) {
  # =======================================================
  # Utility functions
  # =======================================================

  # 차트 생성 조건 설정
  makeFFRWeekTable <- function() {
    desc <- c("last", "this")
    date <- c("(3/4)", "(3/11)")
    value <- c(1.06, 1.04)
    df <- data.frame(desc, date, value)
    df <- split(df, df$desc)

    return(df)
  }

  # 지난주 금주 실적 시그널 확인
  checkWeekSignal <- function(last, this) {
    if(last >= this) {
      if(last == this) {
        signal = "black"
      } else {
        signal = "green"
      }
    } else {
      signal = "red"
    }
    print(paste("Week signal :", signal))
    return(signal)
  }

  # =======================================================
  # Main function
  # =======================================================

  setwd(wd)

  # x 좌표는 소수점 2째자리로 반올림, y좌표는 datatime으로 변경
  df[xColumn] <- round(df[xColumn], digit=2)
  df[yColumn] <- as.Date(paste0(df[[yColumn]],1),"%Y%m%d")

  # group별로 df를 분리 해주고 정렬
  df_group <- split(df, df$group)
  unique_group <- sort(unique(df$group))

  # y축 최대값을 정하기 위해 NA를 제외한 value의 최대값을 구하고 1.4를 곱함
  y_max <- max(df$value[!is.na(df$value)]) * yMaxRate

  ffr_week <- makeFFRWeekTable()
  ffr_signal <- checkWeekSignal(
    ffr_week[['last']][['value']],
    ffr_week[['this']][['value']]
    )

  #
  label_x <- datetime_to_timestamp(df[[yColumn]][1])
  top_label_x <- datetime_to_timestamp(df[[yColumn]][length(df[[yColumn]])])
  label_y <- c()
  label_text <- c()
  # group 시그널 변수, 계산식 추가 필요!
  label_signal <- c("red", "green", "blue", "green", "red")
  label_signal_symbol <- c("", "", "●", "", "●")
  group_colors <- c(
    rgb(0,0,0),
    rgb(1,0,0),
    rgb(1,0,1),
    rgb(127/255,127/255,127/255),
    rgb(1,192/255,0)
    )
  use_datalabels <- c(TRUE, TRUE, TRUE, FALSE, TRUE)
  line_symbols <- c('circle', 'circle', 'diamond', 'diamond', 'square')
  line_symbols_color <- c('white', '', '', '', 'white')

  for(group in unique_group) {
    label_text <- c(label_text, as.character(df_group[[group]][["group"]][1]))
    label_y <- c(label_y, df_group[[group]][["value"]][1])
  }

  label_df <- data.frame(label_text, label_y, label_signal, label_signal_symbol, group_colors, use_datalabels, line_symbols, line_symbols_color)

  label <- list()

  # 옵션값을 가지고 라인 좌측 라벨 구조 생성
  for(group in label_df$label_text) {
    label[[length(label)+1]] <- list(
      point = list(x = label_x, y = label_df[label_df$label_text == group,][['label_y']], xAxis = 0, yAxis = 0),
      borderWidth=0,
      text = paste0(
        "<span style='color:",
        label_df[label_df$label_text == group,][['label_signal']], ";'>",
        label_df[label_df$label_text == group,][['label_signal_symbol']],
        "</span>",
        "<p style='color:",
        label_df[label_df$label_text == group,][['group_colors']], ";'>",
        group,
        "</span>"
        )
    )
  }

  # each series Chart ----
  dxChart <- highchart() %>%
    hc_chart(plotBorderWidth = 1) %>%
    hc_yAxis_multiples(
    list(title = list(text = "FFR(%)"), min=0, max=y_max, tickInterval = tickIntervalY, endOnTick=FALSE, gridLineColor=""),
    list(title = list(text = "FDR(%)"), min=0, max=y_max, endOnTick=FALSE, gridLineColor="", showLastLabel = FALSE, opposite = TRUE)
  ) %>%
    hc_xAxis(
      minPadding = xLeftMargin,
      type = xType,
      showFirstLabel = FALSE,
      tickInterval = tickIntervalX,
      labels = list(format = "{value:%b}")
      ) %>%
    hc_plotOptions(
      series = list(
        dataLabels = list(
          enabled = useDatalabels,
          allowOverlap = TRUE,
          format = "{point.value:.2f}",
          style = list(fontWeight = datalabelFontWeight, textOutline = datalabelOutline)
          ),
        lineWidth = lineWidth
      )
    ) %>% # dataLabels 전역 설정
    hc_tooltip(
      crosshairs = TRUE,
      sort = TRUE,
      table = TRUE
    ) %>%
    hc_title(
      text = paste0("<span style='color:",
                    ffr_signal,
                    ";'>●</span> ",
                    titleText),
      margin = 10, align = "center",
      style = list(fontFamily = fontFamily, fontWeight = titleFontWeight, useHTML = TRUE, fontSize = titleFontSize)
    ) %>%
    hc_annotations(
      list(
        draggable = TRUE,
        labelOptions = list(
          y = 0,
          x = -60,
          verticalAlign="middle",
          allowOverlap=TRUE,
          align="left",
          padding=1,
          style = list(fontFamily = fontFamily, fontWeight = linelabelFontWeight, fontSize = linelabelFontSize),
          backgroundColor = ""
        ),
        labels = label
      ), # 차트 좌측 라벨
      list(
        draggable = FALSE,
        labelOptions = list(
          y = 0,
          x = -160,
          verticalAlign="middle",
          allowOverlap=TRUE,
          align="left",
          style = list(fontFamily = fontFamily, fontWeight = weeklabelFontWeight, fontSize = weeklabelFontSize),
          backgroundColor = "white"
        ),
        labels = list(
          point = list(x = top_label_x, y = y_max, xAxis = 0, yAxis = 0),
          borderWidth=0,
          text = paste(ffr_week[['last']][['value']], ffr_week[['last']][['date']], "→")
        )
      ),
      list(
        draggable = FALSE,
        labelOptions = list(
          y = 0,
          x = 0,
          verticalAlign="middle",
          allowOverlap=TRUE,
          align="left",
          style = list(fontFamily = fontFamily, fontWeight = weeklabelFontWeight, fontSize = weeklabelFontSize, color = ffr_signal),
          backgroundColor = rgb(217/255,217/255,217/255)
        ),
        labels = list(
          point = list(x = top_label_x, y = y_max, xAxis = 0, yAxis = 0),
          borderWidth=0,
          text = paste(ffr_week[['this']][['value']], ffr_week[['this']][['date']])
        )
      )
    )

  for(group in label_df$label_text) {
    dxChart <- dxChart %>%
      hc_add_series(data = df_group[[group]],
                    name = group,
                    hcaes(x = PURC_MON_NEW, y = value),
                    marker = list(symbol = label_df[label_df$label_text == group,][['line_symbols']], fillColor=label_df[label_df$label_text == group,][['line_symbols_color']], lineWidth=1, lineColor=NULL),
                    dataLabels = list(enabled = label_df[label_df$label_text == group,][['use_datalabels']],
                                      color = label_df[label_df$label_text == group,][['group_colors']]),
                    color=label_df[label_df$label_text == group,][['group_colors']],
                    yAxis=1,
                    type = "line")
  }

  if(base64 == TRUE) {
    htmlwidgets::saveWidget(widget = dxChart, file = "./tmp/dxChart.html")
    if(!webshot::is_phantomjs_installed()) {
      webshot::install_phantomjs()
    }
    tf1 <- "dxChart.png"
    if(!dir.exists('tmp')) {
      dir.create('tmp')
    }
    webshot::webshot(url = "./tmp/dxChart.html", vheight = 400, vwidth = 640, file = tf1, delay = 1)
    # png를 base64로 변경
    base64 <- RCurl::base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")
    return(base64)
  } else {
    return(dxChart)
  }

}

base64ToHtml <- function(base64Chart = makeFieldChart()) {
  base64 <- base64Chart
  html <- sprintf('<html><body><img src="data:image/png;base64,%s"></body></html>', base64)
  cat(html, file = tf2 <- tempfile(fileext = ".html"))
  browseURL(tf2)
}
