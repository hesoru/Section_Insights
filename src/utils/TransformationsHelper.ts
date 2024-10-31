import { InsightError, InsightResult } from "../controller/IInsightFacade";
import { ApplyRule, ApplyToken, Query } from "../models/Query";
import Decimal from "decimal.js";

export function groupBy(filteredResults: InsightResult[], query: Query): Map<string, InsightResult[]> {
	const groups = new Map<string, InsightResult[]>();

	//1 Extract keys to group by
	let keys: string[] = [];
	if (query.TRANSFORMATIONS) {
		keys = query.TRANSFORMATIONS.GROUP;
	}

	for (const result of filteredResults) {
		const values = keys.map((key) => result[key]);
		const mapKey = values.join("_");
		const mapEntry = groups.get(mapKey);
		if (!mapEntry) {
			groups.set(mapKey, [result]);
		} else {
			mapEntry.push(result);
		}
	}
	return groups;
}

export function apply(group: InsightResult[], applyRules: ApplyRule[]): InsightResult {
	const result: InsightResult = {};
	for (const rule of applyRules) {
		const applyKey = Object.keys(rule)[0]; //first key
		const applyTokenObject = rule[applyKey]; //{APPLYTOKEN: key}
		const applyToken = Object.keys(applyTokenObject)[0] as ApplyToken;
		const key = applyTokenObject[applyToken];

		if (!key) {
			throw new InsightError("no key found in applyTokenObject");
		}
		const keyValues = group.map((item) => item[key]);

		if (applyToken !== "COUNT") {
			if (!keyValues.every((value) => typeof value === "number")) {
				throw new InsightError("invalid apply token provided with non numeric key");
			}
			result[applyKey] = applyNumericOperation(keyValues as number[], applyToken);
		} else {
			//Count
			const uniqueValues = new Set(keyValues);
			result[applyKey] = uniqueValues.size;
		}
	}
	return result;
}

export function applyNumericOperation(keyValues: number[], applyToken: ApplyToken): number {
	const dec = 2;
	let decValues;
	let total;
	let avg;

	switch (applyToken) {
		case "MAX":
			return Math.max(...keyValues);
		case "MIN":
			return Math.min(...keyValues);
		case "AVG":
			decValues = keyValues.map((v: number) => new Decimal(v));
			total = decValues.reduce((acc: Decimal, val: Decimal) => acc.add(val), new Decimal(0));
			avg = total.toNumber() / keyValues.length;
			return Number(avg.toFixed(dec));
		case "SUM":
			return +keyValues.reduce((a: number, b: number) => a + b, 0).toFixed(dec);
		default:
			throw new InsightError("invalid apply token in query");
	}
}
