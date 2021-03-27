library(DBI)
library(RMySQL)

# 데이터베이스 연결하기 (updated)
con = dbConnect(RMySQL::MySQL(),
                user='cs_task_shiny',
                password='lge123!!',
                dbname='cs_task_he',
                host='10.185.152.89',
                port = 3306,
                client.flag = CLIENT_MULTI_RESULTS)

# DB character set을 utf-8 로 지정
dbSendQuery(con, 'set character set "utf8"')

# filter mapping table 불러오는 코드
# ref_he_tv_filter <- dbGetQuery(con, "SELECT * FROM `ref_he_tv_filter_value`;")
# 
# filter_names <- unique(ref_he_tv_filter$COL_NAME)
# 
# for(filter_name in filter_names) {
#         filters_list[[filter_name]] <- ref_he_tv_filter$VALUE1[ref_he_tv_filter$COL_NAME == filter_name]
# }

# prod or sales 중 선택
type <- "prod"

# filter 변수 예시, shiny에서 list로 출력하고 각 문자열은 작은 따옴표로 묶여있어야 함
filters_svc <- list()
filters_svc[["PRODUCT_DIVIDE"]] <- c("'LED_UHD'", "'LED_FHD'", "'OLED'")
filters_svc[["LEVEL1_L12M"]] <- c("'01.모듈'")
filters_svc[["SALES_CORP"]] <- c("'LGEIN'")
filters_svc[["PROD_CORP"]] <- c("")
filters_svc[["CONTINENT_E"]] <- c("")
filters_svc[["TOOL4"]] <- c("")
filters_svc[["ATTRIBUTE6"]] <- c("")
#filters_svc[["DEV_YEAR"]] <- c("'18년'", "'19년'")
#filters_svc[["INCH"]] <- c("'55'", "'65'")

# filter 변수가 적용되지 않은 리스트를 제외
filters_svc <- filters_svc[lapply(filters_svc, function(x) nchar(x[1])) > 0]

# sellin filter 변수 예시
cols_sellin <- c("SALES_CORP", "PROD_CORP", "CONTINENT_E", "TOOL4", "ATTRIBUTE6")
filters_sellin <- list()
for(colname in cols_sellin) {
        filters_sellin[[colname]] <- filters_svc[[colname]]
}

# shiny로 시작일 입력 받음
end_date <- as.Date("2021-03-01")
start_date <- end_date - years(3) - 1


query_filters_svc <- ""
query_filters_sellin <- ""
for(colname in names(filters_svc)) {
        filters <- paste(filters_svc[[colname]], collapse = ",")
        query_filters_svc <- paste(query_filters_svc, "\nAND", colname, "IN (", filters, ")")
}

for(colname in names(filters_sellin)) {
        filters <- paste(filters_svc[[colname]], collapse = ",")
        query_filters_sellin <- paste(query_filters_sellin, "\nAND", colname, "IN (", filters, ")")
}

start_month <- format(start_date, format="%Y%m")
end_month <- format(end_date, format="%Y%m")
end_day <- format(end_date, format="%Y%m%d")
end_year <- format(end_date, format="%Y")

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

svc_df <- dbGetQuery(con, svc_query)
sellin_df <- dbGetQuery(con, sellin_query)

# range_month <- sapply(1:13, function(x) format(as.Date(paste0(end_year,"-10-01")) - months(x), format="%Y%m"))

month_subtract <- function(x, month) {
        result <- as.Date(paste0(month, 1), format="%Y%m%d") - months(x)
        return(format(result, format = "%Y%m"))
}

range_month <- sapply(1:13, month_subtract, month = end_month)

acc_svc <- c()
for(month in range_month) {
        acc_svc[month] <- svc_df %>% filter(
                ISSUE_MON <= month,
                ISSUE_MON >= month_subtract(11, month),
                PURC_MON_NEW <= month,
                PURC_MON_NEW >= month_subtract(11, month)
        ) %>% select(SVC_COUNT) %>% colSums()
}
print(acc_svc)

w.acc_sales <- c()
acc_sales <- c()
for(month in range_month) {
        acc_sales[month] <- sellin_df %>% filter(PURC_MON == month) %>% select(SELLIN_SUM) %>% colSums()

}

rowSums(test)
sellin_df %>% filter(PURC_MON == "202") %>% select(SELLIN_SUM) %>% colSums()
ifelse(test, 1, 2)
