#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' @export
add <- function(x, y) {
 x + y
}


#' Make highchart from df
#' 
#' FFR, FDR Chart를 만들고 base64 형태로 출력하기 위한 함수
#' 
#' @param df
#' @param x_column
#' @param y_column
#' @return A base64 img string.
#' 
#' @rdname makeFieldChart
#' @export

# 컬럼 정의 ----
makeFieldChart <- function(
  wd = getwd(),
  df = read.csv("dd.csv", header = T),
  x_column = "value",
  y_column = "PURC_MON_NEW",
  y_max_rate = 1.4
) {
  # =======================================================
  # Sample data
  # =======================================================



  # =======================================================
  # Utility functions
  # =======================================================

  # 필요한 패키지 설치
  loadLibrary <- function(...) {
    if (!require(pacman)) {
      install.packages("pacman")
    }

    pacman::p_load(...)
    webshot::install_phantomjs()
  }

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

  loadLibrary("highcharter", "stringr", "webshot", "RCurl")
  setwd(wd)

  # x 좌표는 소수점 2째자리로 반올림, y좌표는 datatime으로 변경
  df[x_column] <- round(df[x_column], digit=2)
  df[y_column] <- as.Date(paste0(df[[y_column]],1),"%Y%m%d")

  # group별로 df를 분리 해주고 정렬
  df_group <- split(df, df$group)
  unique_group <- sort(unique(df$group))

  # y축 최대값을 정하기 위해 NA를 제외한 value의 최대값을 구하고 1.4를 곱함
  y_max <- max(df$value[!is.na(df$value)]) * y_max_rate

  ffr_week <- makeFFRWeekTable()
  ffr_signal <- checkWeekSignal(ffr_week[['last']][['value']], ffr_week[['this']][['value']])

  # 
  label_x <- datetime_to_timestamp(df[[y_column]][1])
  top_label_x <- datetime_to_timestamp(df[[y_column]][length(df[[y_column]])])
  label_y <- c()
  label_text <- c()
  label_signal <- c("red", "green", "blue", "green", "red") # group 시그널 변수, 계산식 추가 필요!
  label_signal_symbol <- c("", "", "●", "", "●")
  group_colors <- c(rgb(0,0,0), rgb(1,0,0), rgb(1,0,1), rgb(127/255,127/255,127/255), rgb(1,192/255,0))
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
  ffr_fdr_chart <- highchart() %>% 
    hc_chart(plotBorderWidth = 1) %>%
    hc_yAxis_multiples(
    list(title = list(text = "FFR(%)", align = "high"), min=0, max=y_max, tickInterval = 0.5, endOnTick=FALSE, gridLineColor=""),
    list(title = list(text = "FDR(%)"), min=0, max=y_max, endOnTick=FALSE, gridLineColor="", showLastLabel = FALSE, opposite = TRUE)
  ) %>%
    hc_xAxis(minPadding = 0.15, type = "datetime", showFirstLabel = FALSE, tickInterval = 30 * 24 * 3600 * 1000, labels = list(format = "{value:%b}")) %>%
    hc_plotOptions(
      series = list(
        dataLabels = list(enabled = TRUE, allowOverlap = TRUE, format = "{point.value:.2f}", style = list(fontWeight = "none", textOutline = "1px white")),
        lineWidth = 1
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
                    ";'>●</span> Global OLED (Product)"),
      margin = 10, align = "center",
      style = list(fontFamily = "LG스마트체 Regular", fontWeight = 'bold', useHTML = TRUE, fontSize = "16px")
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
          style = list(fontFamily = 'LG스마트체 Regular', fontWeight = 'bold', fontSize = "12px"),
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
          style = list(fontFamily = 'LG스마트체 Regular', fontWeight = 'bold', fontSize = "12px"),
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
          style = list(fontFamily = 'LG스마트체 Regular', fontWeight = 'bold', fontSize = "12px", color = ffr_signal),
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
    ffr_fdr_chart <- ffr_fdr_chart %>%
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

  htmlwidgets::saveWidget(widget = ffr_fdr_chart, file = "ffr_fdr_chart.html")
  tf1 <- "ffr_fdr_chart.png"
  webshot(url = "ffr_fdr_chart.html", vheight = 400, vwidth = 640, file = tf1, delay = 1)

  base64 <- base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt") # png를 base64로 변경

  return(base64)
}

base64 <- makeFieldChart()
html <- sprintf('<html><body><img src="data:image/png;base64,%s"></body></html>', base64)
cat(html, file = tf2 <- tempfile(fileext = ".html"))
browseURL(tf2)

# load csv(사전 작업 필요) ----

df <- read.csv("dd.csv", header = T)

# Make label table ----
label_x <- datetime_to_timestamp(df[[y_column]][1])
top_label_x <- datetime_to_timestamp(df[[y_column]][length(df[[y_column]])])
label_y <- c()
label_text <- c()

# group 시그널 변수, 계산식 추가 필요!
label_signal <- c("red", "green", "blue", "green", "red")

# group 시그널 형태와 라인 색상, 옵션 선택, Shiny input과 연결 필요!
label_signal_symbol <- c("", "", "●", "", "●")
group_colors <- c(rgb(0,0,0), rgb(1,0,0), rgb(1,0,1), rgb(127/255,127/255,127/255), rgb(1,192/255,0))
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
ffr_fdr_chart <- highchart() %>% 
  hc_chart(plotBorderWidth = 1) %>%
  hc_yAxis_multiples(
  list(title = list(text = "FFR(%)", align = "high"), min=0, max=y_max, tickInterval = 0.5, endOnTick=FALSE, gridLineColor=""),
  list(title = list(text = "FDR(%)"), min=0, max=y_max, endOnTick=FALSE, gridLineColor="", showLastLabel = FALSE, opposite = TRUE)
) %>%
  hc_xAxis(minPadding = 0.15, type = "datetime", showFirstLabel = FALSE, tickInterval = 30 * 24 * 3600 * 1000, labels = list(format = "{value:%b}")) %>%
  hc_plotOptions(
    series = list(
      dataLabels = list(enabled = TRUE, allowOverlap = TRUE, format = "{point.value:.2f}", style = list(fontWeight = "none", textOutline = "1px white")),
      lineWidth = 1
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
                  ";'>●</span> Global OLED (Product)"),
    margin = 10, align = "center",
    style = list(fontFamily = "LG스마트체 Regular", fontWeight = 'bold', useHTML = TRUE, fontSize = "16px")
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
        style = list(fontFamily = 'LG스마트체 Regular', fontWeight = 'bold', fontSize = "12px"),
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
        style = list(fontFamily = 'LG스마트체 Regular', fontWeight = 'bold', fontSize = "12px"),
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
        style = list(fontFamily = 'LG스마트체 Regular', fontWeight = 'bold', fontSize = "12px", color = ffr_signal),
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
  ffr_fdr_chart <- ffr_fdr_chart %>%
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

ffr_fdr_chart

htmlwidgets::saveWidget(widget = ffr_fdr_chart, file = "ffr_fdr_chart.html")
tf1 <- "ffr_fdr_chart.png"
webshot(url = "ffr_fdr_chart.html", vheight = 400, vwidth = 640, file = tf1, delay = 1)

base64 <- base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt") # png를 base64로 변경
html <- sprintf('<html><body><img src="data:image/png;base64,%s"></body></html>', base64)
cat(html, file = tf2 <- tempfile(fileext = ".html"))
browseURL(tf2)

hchart(df, "line", hcaes(x = PURC_MON_NEW, y = value, group = group)) %>%
  hc_title(
    text = "<font style='font-family:\'LG스마트체 Regular\';'> <span style='color:red;'>●</span> Global OLED (Product)</font>",
    margin = 20, align = "center",
    style = list(color = "black", useHTML = TRUE)
  ) %>%
  hc_xAxis(title = list(text = ""), offset = 1) %>%
  hc_yAxis(max = y_max) %>%
  hc_colors(c(rgb(0,0,0), rgb(1,0,0), rgb(127/255,127/255,127/255), rgb(1,0,1), rgb(1,192/255,0))) %>%
  hc_annotations(
    list(
      labels = list(
        list(point = list(x = 1, y = 1, xAxis = 0, yAxis = 0), borderWidth=0, text = "<span style='color:red;'>●</span>`20(R)"),
        list(point = list(x = 1, y = 1, xAxis = 0, yAxis = 0), text = "Start")
      )
    )
  )


hc <- highchart()%>%
  hc_xAxis(type = "datetime", labels = list(format = '{value:%m/%d}')) %>%
  hc_yAxis_multiples(list(title = list(text = "IPR"),labels=list(format = '{value}%'),min=0,
                          max=100,showFirstLabel = TRUE,showLastLabel=TRUE,opposite = FALSE),
                     list(title = list(text = "Total Subscribers"),min=0,max = max(df$total_users),
                          labels = list(format = "{value}"),showLastLabel = FALSE, opposite = TRUE)) %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_add_series(df,type="column",hcaes(x=received_dt,y=total_users,group=isp),yAxis=1) %>%
  hc_add_series(df,type="line",hcaes(x=received_dt,y=inbox_rate,group=isp))
    
highchart() %>% 
  hc_yAxis_multiples(
    list(lineWidth = 1),
    list(lineWidth = 1, showLastLabel = FALSE, opposite = TRUE)
  ) %>% 
  hc_add_series(data = rnorm(10)) %>% 
  hc_add_series(data = rexp(10), type = "spline", yAxis = 1)



df <- data.frame(
  total_inbox = c(2, 3, 4, 5, 6),
  total_volume = c(30, 30, 30, 30, 30),
  total_users = c(300, 400, 20, 340, 330),
  received_dt = c("20180202", "20180204", "20180206", "20180210", "20180212"),
  isp = "ProviderXY"
)

df$inbox_rate <- df$total_inbox / df$total_volume
df$inbox_rate <- round((df$inbox_rate*100),0)
df$received_dt <- as.character(df$received_dt)
df$received_dt <- as.Date(df$received_dt, "%Y%m%d")
df <- df[order(df$received_dt),]

hc <- highchart()%>%
  hc_xAxis(type = "datetime", labels = list(format = '{value:%m/%d}')) %>%
  hc_yAxis_multiples(list(title = list(text = "IPR"),labels=list(format = '{value}%'),min=0,
                          max=100,showFirstLabel = TRUE,showLastLabel=TRUE,opposite = FALSE),
                     list(title = list(text = "Total Subscribers"),min=0,max = max(df$total_users),
                          labels = list(format = "{value}"),showLastLabel = FALSE, opposite = TRUE)) %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_add_series(df,type="column",hcaes(x=received_dt,y=total_users,group=isp),yAxis=1) %>%
  hc_add_series(df,type="line",hcaes(x=received_dt,y=inbox_rate,group=isp))

hc

# base64 file을 브라우저로 보여주기 ----

png(tf1 <- tempfile(fileext = ".png")); plot(0); dev.off()
tf1
library(RCurl)
txt <- base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")

html <- sprintf('<html><body><img src="data:image/png;base64,%s"></body></html>', txt)
cat(html, file = tf2 <- tempfile(fileext = ".html"))
browseURL(tf2)

# 차트를 이미지로 저장 ----

# install.packages("webshot")
# webshot::install_phantomjs()
library(webshot)
library(highcharter)
library(plyr)

data("citytemp")

plot <- highchart() %>% 
  hc_add_series(name = "London", data = citytemp$london, type = "area") %>% 
  hc_rm_series(name = "New York")

htmlwidgets::saveWidget(widget = plot, file = "~/plot.html")
setwd("~")
webshot(url = "plot.html", 
                 file = "plot.png")

# ----