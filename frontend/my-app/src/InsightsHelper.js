import {performQueryAPI} from "./Api";

export async function getPieChartData(id) {
    console.log("this is the id" + id)
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
    console.log("this is the query" + JSON.stringify(query, null, 2));
    const res = await performQueryAPI(query);
    console.log("looking at getPieChartData" + JSON.stringify(res))
    const results = res.result
    console.log("testing results" + Array.isArray(results))
    const testing = results.map(result => {
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
    console.log("after map testing array" + JSON.stringify(testing));
    return testing;
}


