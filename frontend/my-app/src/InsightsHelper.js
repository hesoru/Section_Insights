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
    const res = await performQueryAPI(query);
    const results = res.body.result
    const chartData = results.map(result => {
        return {
            labels: ["Pass", "Fail", "Audit"],
            datasets: [{
                data: [result.totalPass, result.totalFail, result.totalAudit],
                backgroundColor: ["blue", "green", "red"],
                hoverOffset: 4
            }],
            departmentName: result[0]
        };
    });
}

export async function getBarGraphData(id) {
	const query_asc = {
		"WHERE": {},
		"OPTIONS": {
		"COLUMNS": [`${id}_dept`, "avgGrade"],
			"ORDER": {
			"dir": "UP",
				"keys": ["avgGrade"]
		}
	},
		"TRANSFORMATIONS": {
		"GROUP": [`${id}_dept`],
			"APPLY": [
			{ "avgGrade": { "AVG": `${id}_avg` } }
			]
		}
	}
	const query_desc = {
		"WHERE": {},
		"OPTIONS": {
			"COLUMNS": [`${id}_dept`, "avgGrade"],
			"ORDER": {
				"dir": "DOWN",
				"keys": ["avgGrade"]
			}
		},
		"TRANSFORMATIONS": {
			"GROUP": [`${id}_dept`],
			"APPLY": [
				{ "avgGrade": { "AVG": `${id}_avg` } }
			]
		}
	}
	const res_asc = await performQueryAPI(query_asc);
	const res_desc = await performQueryAPI(query_desc);
	const results_asc = res_asc.body.result
	const results_desc = res_desc.body.result
	const bottomFive = results_asc.map(result => {
		return {
			labels: ["Pass", "Fail", "Audit"],
			datasets: [{
				data: [result.totalPass, result.totalFail, result.totalAudit],
				backgroundColor: ["blue", "green", "red"],
				hoverOffset: 4
			}],
			departmentName: result[0]
		};
	});
	const topFive = results_desc.map(result => {
		return {
			labels: ["Pass", "Fail", "Audit"],
			datasets: [{
				data: [result.totalPass, result.totalFail, result.totalAudit],
				backgroundColor: ["blue", "green", "red"],
				hoverOffset: 4
			}],
			departmentName: result[0]
		};
	});
}



