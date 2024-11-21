import {performQueryAPI} from "./Api";

export async function getPieChartData(id) {
    const query = {
        "WHERE": {},
        "OPTIONS": {
            "COLUMNS": [
                `${id}_dept`,
                "totalPass",
                "totalFail",
                "totalAudit"
            ],
            "ORDER": {
                "dir": "DOWN",
                "keys": ["totalPass"]
            }
        },
        "TRANSFORMATIONS": {
            "GROUP": [
                `${id}_dept`
            ],
            "APPLY": [
                {
                    "totalPass": {
                        "SUM": `${id}_pass`
                    }
                },
                {
                    "totalFail": {
                        "SUM": `${id}_fail`
                    }
                },
                {
                    "totalAudit": {
                        "SUM": `${id}_audit`
                    }
                }
            ]
        }
    }

    const res = await performQueryAPI(query);
    const results = res.result
    return results.map(result => {
        const firstKey = Object.keys(result)[0]
        return {
            data: {
                labels: ["Pass", "Fail", "Audit"],
                datasets: [{
                    data: [result.totalPass, result.totalFail, result.totalAudit],
                    backgroundColor: ["blue", "green", "red"],
                    hoverOffset: 4
                }],
            },
            departmentName: result[firstKey]
        };
    });
}


