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
        // const firstKey = Object.keys(result)[0]
        return {
            data: {
                labels: ["Pass", "Fail", "Auditing"],
                datasets: [{
                    data: [result.totalPass, result.totalFail, result.totalAudit],
                    backgroundColor: ['rgb(68,172,103)', 'rgb(255,134,134)', 'rgb(75,192,192)'],
                    hoverOffset: 4
                }],
            },
            departmentName: result[`${id}_dept`]
        };
    });
    console.log("after map testing array" + JSON.stringify(testing));
    return testing;
}

export async function getBottomFiveBarChartData(id) {
	console.log("this is the id" + id);
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
	console.log("this is the query_asc" + JSON.stringify(query_asc, null, 2));
	const res_asc = await performQueryAPI(query_asc);
	const results_asc = res_asc.result;
	const results_bottom_five = results_asc.slice(0, 5);
	console.log("looking at bottom 5" + JSON.stringify(results_bottom_five));
	return {
		data: {
			labels: results_bottom_five.map(result => result[`${id}_dept`]),
			datasets: [{
				label: "Average Grade",
				data: results_bottom_five.map(result => result.avgGrade),
				backgroundColor: 'rgb(255,134,134)',
				borderColor: 'rgb(255,134,134)',
				borderWidth: 1
			}]
		}
	};
}

export async function getTopFiveBarChartData(id) {
	console.log("this is the id" + id);
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
	console.log("this is the query_desc" + JSON.stringify(query_desc, null, 2));
	const res_desc = await performQueryAPI(query_desc);
	const results_desc = res_desc.result;
	const results_top_five = results_desc.slice(0, 5);
	console.log("looking at top 5" + JSON.stringify(results_top_five));
	return {
		data: {
			labels: results_top_five.map(result => result[`${id}_dept`]),
			datasets: [{
				label: "Average Grade",
				data: results_top_five.map(result => result.avgGrade),
				backgroundColor: 'rgb(68,172,103)',
				borderColor: 'rgb(68,172,103)',
				borderWidth: 1
			}]
		}
	};
}

export async function getLineGraphData(id) {
	console.log("this is the id" + id);
	const query = {
		"WHERE": {},
		"OPTIONS": {
			"COLUMNS": [`${id}_dept`, `${id}_year`, "avgGrade"],
			"ORDER": {
				"dir": "UP",
				"keys": [`${id}_dept`, `${id}_year`]
			}
		},
		"TRANSFORMATIONS": {
			"GROUP": [`${id}_dept`, `${id}_year`],
			"APPLY": [
				{ "avgGrade": { "AVG": `${id}_avg` } }
			]
		}
	}
	console.log("this is the query" + JSON.stringify(query, null, 2));
	const res = await performQueryAPI(query);
	const results = res.result;
	console.log("looking at avgByYear" + JSON.stringify(results));
	const avgByYear = results.map(result => {
		return {
			data: {
				labels: [result[`${id}_year`]],
				datasets: [{
					label: `Average Grade for ${result[`${id}_dept`]}`,
					data: [result.avgGrade],
					backgroundColor: 'rgb(75, 192, 192)',
					pointBackgroundColor: 'rgb(75, 192, 192)',
					borderColor: 'rgb(75, 192, 192)',
					tension: 0.1
				}]
			},
			departmentName: result[`${id}_dept`]
		};
	});
	console.log("after map testing array" + JSON.stringify(avgByYear));
	return avgByYear;
}
