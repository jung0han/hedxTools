# hedxTools

### 설치 방법

처음 사용하는 경우:

    devtools::install_gitlab("heqdx/hedxTools", host = "http://localhost:9999/gitlab", auth_token = "x6zy6vwoAg97hV7UdG-W")

패키지를 업데이트 하는 경우:

    devtools::install_gitlab("heqdx/hedxTools", host = "http://localhost:9999/gitlab", auth_token = "x6zy6vwoAg97hV7UdG-W")
    detach("package:hedxTools", unload = TRUE)
    library("hedxTools")

### Chart 사용 방법

Hazard 차트 작성 예시:

    hedxTools::makeFieldChart(
        lineSymbols = FALSE,
        useCustomize = FALSE,
        xLeftMargin = 0,
        df = dxChart::hazard_accumulate_sample,
        titleText = "Global OLED (Module)",
        yLeftText = "Hazard (%)",
        xCol="SVC_MON_NEW_ind_cal",
        yCol="svc_rate_value",
        groupCol = "CALC_PROD_DT_ind",
        xType = "category",
        useLeftlabels = FALSE,
        yRightUse = FALSE,
        tickIntervalX = 1,
        useLinelabels = TRUE,
        markerHover = FALSE,
        base64 = FALSE
    )

Daily SVC 차트 작성 예시:

    df <- hedxTools::svc_sellin %>% dplyr::mutate(group = "svc_sellin")

    hedxTools::makeFieldChart(
        yAxis = 0,
        linelabelSignals = FALSE,
        linelabelSymbols = FALSE,
        groupColors = FALSE,
        useDatalabels = FALSE,
        lineSymbols = FALSE,
        lineSymbolColors = FALSE,
        xLeftMargin = 0,
        df = df,
        titleText = "Daily Hazard",
        yLeftText = "SVC Count",
        yRightText = "Sellin Count",
        xCol="PROD_MON",
        yCol="issue_cnt",
        y2Max = 1500000,
        barCol="sell_in",
        groupCol = "group",
        useLeftlabels = FALSE,
        tickIntervalX = 1,
        useWeeklabels = FALSE,
        tickIntervalY = 10000,
        markerHover = FALSE,
        base64 = FALSE
    )

FFR, Hazard 차트를 쉽게 만들 수 있는 Preset 함수:

    hedxTools::makePresetChart(preset = "ffr", title = "Global OLED (Module)", base64 = FALSE)
    hedxTools::makePresetChart(preset = "hazard", title = "Global OLED (Module)", base64 = FALSE)

FFR, L6M, L3M 시그널 확인 및 반영 함수:

    df <- hedxTools::ffr_fdr_sample

    ffr_signal <- hedxTools::checkSignal(df[df['group'] == "'21(R)",], df[df['group'] == "'21(T)",], "ffr")
    L6M_signal <- hedxTools::checkSignal(df[df['group'] == "L6M",], ,"L6M")
    L3M_signal <- hedxTools::checkSignal(df[df['group'] == "L3M",], df[df['group'] == "L3M('20)",], "L3M")

    linelabel_signals <- c("", "", "", L3M_signal, "", L6M_signal)

    hedxTools::makeFieldChart(
        df = df,
        titleSignal = ffr_signal,
        linelabelSignals = linelabel_signals,
        titleText = "Korea UHD (Product)",
        base64 = FALSE
    )

차트를 base64 문자열로 리턴받고 html파일로 확인:

    base64 <- hedxTools::makePresetChart(preset = "ffr", title = "Global OLED (Module)")
    hedxTools::base64ToHtml(base64)

### Query 사용 방법

Query에 사용할 필터를 List로 작성:

    # 예시 : 각 문자열은 작은 따옴표로 묶여있어야 함
    filters_svc <- list()
    filters_svc[["PRODUCT_DIVIDE"]] <- c("'LED_UHD'", "'LED_FHD'", "'OLED'")
    #filters_svc[["LEVEL1_L12M"]] <- c("'01.모듈'")
    filters_svc[["SALES_CORP"]] <- c("'LGEIN'")
    filters_svc[["PROD_CORP"]] <- c("")
    filters_svc[["CONTINENT_E"]] <- c("")
    filters_svc[["TOOL4"]] <- c("")
    filters_svc[["ATTRIBUTE6"]] <- c("")
    #filters_svc[["DEV_YEAR"]] <- c("'18년'", "'19년'")
    #filters_svc[["INCH"]] <- c("'55'", "'65'")

    # filter 변수가 적용되지 않은 리스트 제외
    filters_svc <- filters_svc[lapply(filters_svc, function(x) nchar(x[1])) > 0]

    # sellin filter에 동일하게 적용할 Field 설정
    cols_sellin <- c("PRODUCT_DIVIDE", "SALES_CORP", "PROD_CORP", "CONTINENT_E", "TOOL4", "ATTRIBUTE6")

    filters_sellin <- list()
    for(colname in cols_sellin) {
    filters_sellin[[colname]] <- filters_svc[[colname]]
    }

필터를 전달하여 함수 실행:

    # 3월 1일 기준 4년치 FFR 데이터 출력
    queryFieldData(date = "2021-03-01", cal="ffr", period = 4, svcFilters = filters_svc, sellinFilters = filters_sellin)

    # 2월 1일 기준 L3M(accNum = 3) 출력
    queryFieldData(date = "2021-03-01", cal="fdr", accNum = 3, svcFilters = filters_svc, sellinFilters = filters_sellin,)

### Argument 상세

패키지 설치 후 Help 메뉴에서 함수명으로 조회하여 확인
