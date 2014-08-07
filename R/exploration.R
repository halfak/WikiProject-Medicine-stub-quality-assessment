source("loader/predictions.R")

predictions = load_predictions(reload=T)

wiki.table(
    predictions[,
        list(
            articles=length(title)
        ),
        pred_class
    ]
)


non_stub = predictions[pred_class!="Stub",
    list(
        title,
        not_stub=prob_ga+prob_fa+prob_b+prob_c+prob_start
    )
]


wiki.table(
    non_stub[order(not_stub, decreasing=T),
        list(
            title=paste("[[:en:", title, "|", title, "]]", sep=""),
            not_stub=round(not_stub*100, 1)
        )
    ]
)
