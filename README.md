# dxChart

### 설치 방법

처음 사용하는 경우:

    devtools::install_gitlab("dongwoo.jeong/dxChart", host = "http://mod.lge.com/hub")

패키지를 업데이트 하는 경우:

    devtools::install_gitlab("dongwoo.jeong/dxChart", host = "http://mod.lge.com/hub")
    detach("package:dxChart", unload = TRUE)
    library("dxChart")

### 사용 방법

Hazard 차트 작성 예시:

    dxChart::makeFieldChart(
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

FFR, Hazard 차트를 쉽게 만들 수 있는 Preset 함수:

    dxChart::makePresetChart(preset = "ffr", title = "Global OLED (Module)", base64 = FALSE)
    dxChart::makePresetChart(preset = "hazard", title = "Global OLED (Module)", base64 = FALSE)

FFR, L6M, L3M 시그널 확인 및 반영 함수:

    df <- dxChart::ffr_fdr_sample

    ffr_signal <- dxChart::checkSignal(df[df['group'] == "'21(R)",], df[df['group'] == "'21(T)",], "ffr")
    L6M_signal <- dxChart::checkSignal(df[df['group'] == "L6M",], ,"L6M")
    L3M_signal <- dxChart::checkSignal(df[df['group'] == "L3M",], df[df['group'] == "L3M('20)",], "L3M")

    linelabel_signals <- c("", "", "", L3M_signal, "", L6M_signal)

    dxChart::makeFieldChart(
        df = df,
        titleSignal = ffr_signal,
        linelabelSignals = linelabel_signals,
        titleText = "Korea UHD (Product)",
        base64 = FALSE
    )

차트를 base64 문자열로 리턴받고 html파일로 확인:

    base64 <- dxChart::makePresetChart(preset = "ffr", title = "Global OLED (Module)")
    dxChart::base64ToHtml(base64)

### Argument 상세

패키지 설치 후 Help 메뉴에서 함수명으로 조회하여 확인
