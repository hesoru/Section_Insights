import {performQuery} from "./Api";

async function pieChartQuery(id) {
    const query = {
        "WHERE": {},
        "OPTIONS": {
            "COLUMNS": [
                `${id}_dept`,
                "totalPass",
                "totalFail",
                "totalAudit"
            ]
        },
        "TRANSFORMATIONS": {
            "GROUP": [
                `${id}_dept"`
            ],
            "APPLY": [
                {
                    "totalPass": {
                        "SUM": `${id}_pass"`
                    }
                },
                {
                    "totalFail": {
                        "SUM": `${id}_fail"`
                    }
                },
                {
                    "totalAudit": {
                        "SUM": `${id}_audit"`
                    }
                }
            ]
        }
    }

    const results = await performQuery(query);

}