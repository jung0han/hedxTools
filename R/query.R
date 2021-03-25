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


filters <- "\nAND PRODUCT_DIVIDE IN('LED_UHD', 'LED_FHD')"

query <- paste("SELECT ISSUE_MON
        ,PURC_MON_NEW
        ,PRODUCT_DIVIDE
        ,PROD_CORP
        ,SALES_CORP
        ,LEVEL1_L12M         AS DETAIL
        ,COUNT(PURC_MON_NEW) AS SVC_COUNT
FROM he_tv_svc_global_output_summary
WHERE ISSUE_MON >=", 201810,
"\nAND ISSUE_MON <=", 202109,
"\nAND PURC_MON_NEW >=", 201810,
"\nAND PURC_MON_NEW <=", 202109,
"\nAND DATA_DT <= ", 20210323,
"\nAND CONTINENT_E = 'KOREA'
AND INCH = '55'
AND DEV_YEAR = '19년'
AND PRODUCT_DIVIDE = 'LED_UHD'
AND LEVEL1_L12M = '01.모듈'",
filters, "
GROUP BY  ISSUE_MON
          ,PURC_MON_NEW
          ,PROD_CORP")
message(query)
