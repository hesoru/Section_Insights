import { InsightError, InsightResult } from "../controller/IInsightFacade";
import { ApplyRule, ApplyToken, Query } from "../models/Section";

export function groupBy(
	filteredResults: InsightResult[],
	query: Query
): Map<(string | number)[], InsightResult[]> | InsightResult[] {
	const groups = new Map<(string | number)[], InsightResult[]>();

	//1 Extract keys to group by
	let keys: string[] = [];
	if (query.TRANSFORMATIONS) {
		keys = query.TRANSFORMATIONS.GROUP;
	} else {
		return filteredResults;
	}

	for (const result of filteredResults) {
		const values = [];
		for (const key of keys) {
			values.push(result[key]);
		}
		const mapEntry = groups.get(values);
		if (!mapEntry) {
			groups.set(values, [result]);
		} else {
			mapEntry.push(result);
		}
	}
	return groups;
}

export function applyResult(group: InsightResult[], applyRules: ApplyRule[]): InsightResult {
	const result: InsightResult = {};
	for (const rule of applyRules) {
		const keyValues = group.map((item) => item[rule.key]);
		if (keyValues.every((value) => typeof value === "number")) {
			result[rule.applyKey] = applyNumericOperation(keyValues as number[], rule.applyToken);
		} else {
			if (rule.applyToken !== "COUNT") {
				throw new InsightError("invalid apply token provided with non numeric key");
			}
			result[rule.applyKey] = keyValues.length;
		}
	}
	return result;
}

export function applyNumericOperation(keyValues: number[], applyToken: ApplyToken): number {
	const dec = 2;
	switch (applyToken) {
		case "MAX":
			return Math.max(...keyValues);
		case "MIN":
			return Math.min(...keyValues);
		case "AVG":
			return +(keyValues.reduce((a: number, b: number) => a + b, 0) / keyValues.length).toFixed(dec);
		case "SUM":
			return +keyValues.reduce((a: number, b: number) => a + b, 0).toFixed(dec);
		case "COUNT":
			return keyValues.length;
		default:
			throw new InsightError("invalid apply token in query");
	}
}
