#' query field data from DB
#'
#' Field 불량율을 계산하는 함수
#'
#' @param date 검색 기준일, Datetime(yyyy-mm-dd)
#' @param period 검색 기간(년), Int
#' @param type 생산/판매 법인 기준 선택, prod or sales
#' @param cal 계산 기준, ffr or fdr
#' @param accNum 누적 기간으로 fdr를 계산 시 사용, Int
#' @param svcFilters SVC 데이터 쿼리의 Where문에 삽인되는 필터, list()
#' @param sellinFilters Sellin 데이터 쿼리의 Where문에 삽인되는 필터, list()
#' @return svc 수량, sales 수량, 불량율
#'
#' @rdname queryFieldData
#' @importFrom magrittr %>%
#' @export

queryFieldData <- function(date, period = 2, type = "prod", cal = "ffr", accNum = 6, svcFilters, sellinFilters) {
        # Utility function ----------------------------------------

        # apply 함수에 사용할 월 뺄셈 함수, 지정된 월에서 해당 월만큼 뺀 후 YYYYmm형태로 출력
        month_subtract <- function(x, month) {
                result <- as.Date(paste0(month, 1), format="%Y%m%d") - months(x)
                return(format(result, format = "%Y%m"))
        }
        # 누적 서비스 건수 계산
        cal_acc_svc <- function(df, endDate, period, accNum = 12) {
                months <- sapply(0:(period*12), month_subtract, month = format(endDate, format="%Y%m"))

                acc_svc <- c()
                for(month in months) {
                        acc_months <- sapply(0:(accNum-1), month_subtract, month = month)
                        
                        acc_svc[month] <- df %>% dplyr::filter(
                                ISSUE_MON %in% acc_months,
                                PURC_MON_NEW %in% acc_months
                        ) %>% dplyr::select(SVC_COUNT) %>% colSums()
                }
                return(acc_svc)
        }
        # 누적 판매 수량 계산
        cal_acc_sales <- function(df, endDate, period, accNum = 12) {
                months <- sapply(0:(period*12), month_subtract, month = format(endDate, format="%Y%m"))

                acc_sales <- c()
                for(month in months) {
                        acc_months <- sapply(0:(accNum-1), month_subtract, month = month)
                        acc_sales[month] <- df %>% dplyr::filter(
                                PURC_MON %in% acc_months
                        ) %>% dplyr::select(SELLIN_SUM) %>% colSums()
                }
                return(acc_sales)
        }
        # 가중치를 반영한 누적 판매 수량 계산
        cal_w.acc_sales <- function(df, endDate, period, accNum = 12) {
                months <- sapply(0:(period*12), month_subtract, month = format(endDate, format="%Y%m"))

                w.acc_sales <- c()
                for(month in months) {
                        acc_months <- sapply(0:(accNum-1), month_subtract, month = month)
                        w_sales <- 0
                        for(index in 1:accNum) {
                                w_sales <- w_sales + df %>% dplyr::filter(
                                        PURC_MON == acc_months[index]
                                ) %>% 
                                        dplyr::select(SELLIN_SUM) %>% colSums() * (index / accNum)
                        }
                        w.acc_sales[month] <- w_sales
                }
                return(w.acc_sales)
        }

        # Main function ----------------------------------------

        end_date <- as.Date(date)
        start_date <- end_date - lubridate::years(period + 1) - 1

        start_month <- format(start_date, format="%Y%m")
        end_month <- format(end_date, format="%Y%m")
        end_day <- format(end_date, format="%Y%m%d")

        # 입력 받은 필터로 쿼리문을 만듬
        query_filters_svc <- ""
        query_filters_sellin <- ""
        for(colname in names(svcFilters)) {
                filters <- paste(svcFilters[[colname]], collapse = ",")
                query_filters_svc <- paste(query_filters_svc, "\nAND", colname, "IN (", filters, ")")
        }

        for(colname in names(sellinFilters)) {
                filters <- paste(sellinFilters[[colname]], collapse = ",")
                query_filters_sellin <- paste(query_filters_sellin, "\nAND", colname, "IN (", filters, ")")
        }
        
        # DB 커넥션 개수 확인 후 15개 이상일 경우 모두 종료
        all_cons <- DBI::dbListConnections(RMySQL::MySQL())
        message(paste("연결된 DB Connection :", length(all_cons)))
        if(length(all_cons) > 4) {
                for(con in all_cons)
                        +  DBI::dbDisconnect(con)
        }

        # 데이터베이스 연결하기 (updated)
        con = DBI::dbConnect(RMySQL::MySQL(),
                        user='cs_task_shiny',
                        password='lge123!!',
                        dbname='cs_task_he',
                        host='10.185.152.89',
                        port = 3306,
                        client.flag = RMySQL::CLIENT_MULTI_RESULTS)

        # DB character set을 utf-8 로 지정
        DBI::dbSendQuery(con, 'set character set "utf8"')

        # svc data query
        svc_query <- paste(
        "SELECT ISSUE_MON
                ,PURC_MON_NEW
                ,PRODUCT_DIVIDE
                ,PROD_CORP
                ,SALES_CORP
                ,LEVEL1_L12M         AS DETAIL
                ,COUNT(PURC_MON_NEW) AS SVC_COUNT
        FROM he_tv_svc_global_output_summary
        WHERE ISSUE_MON >=",start_month,
        "\nAND ISSUE_MON <=", end_month,
        "\nAND PURC_MON_NEW >=", start_month,
        "\nAND PURC_MON_NEW <=", end_month,
        "\nAND DATA_DT <= ", end_day,
        query_filters_svc, "
        GROUP BY  ISSUE_MON
                ,PURC_MON_NEW
                ,SALES_CORP
                ,PROD_CORP"
        )
        message(svc_query)

        sellin_query <- paste(
        "SELECT PURC_MON 
        ,PROD_CORP
        ,SALES_CORP
        ,SUM(QTY) AS SELLIN_SUM",
        ifelse(type == "sales", "\nFROM `he_tv_sellin_sales_output_summary`", "\nFROM `he_tv_sellin_prod_output_summary`"),
        "\nWHERE PURC_MON >=", start_month,
        "\nAND PURC_MON <=", end_month,
        "\nAND DATA_DT <=", end_day,
        query_filters_sellin, "
        GROUP BY  PURC_MON
                ", ifelse(type == "sales", ",SALES_CORP", ",PROD_CORP")
        )
        message(sellin_query)

        svc_df <- DBI::dbGetQuery(con, svc_query)
        sellin_df <- DBI::dbGetQuery(con, sellin_query)

        if(cal == "ffr") {
                acc_svc <- cal_acc_svc(svc_df, end_date, period)
                w.acc_sales <- cal_w.acc_sales(sellin_df, end_date, period)
                ffr_rate <- acc_svc / w.acc_sales * 100
                return(cbind(acc_svc, w.acc_sales, ffr_rate))
        }
        
        if(cal == "fdr") {
                acc_svc <- cal_acc_svc(svc_df, end_date, period, accNum)
                acc_sales <- cal_acc_sales(sellin_df, end_date, period, accNum)
                fdr_rate <- acc_svc / acc_sales * 100
                return(cbind(acc_svc, acc_sales, fdr_rate))
        }
}
