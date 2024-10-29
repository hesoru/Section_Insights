import { InsightError, InsightResult } from "../controller/IInsightFacade";
import { Options } from "../models/Query";

/**
 * @returns - InsightResult[], takes already filtered and selected results and sorts them according to options.ORDER
 * if options.ORDER exists, if not throws an error as sort Results should not have been called. If types of values
 * between 2 sections.ORDER differs throw InsightError
 * @param options
 * @param results
 */
export function sortResults(options: Options, results: InsightResult[]): InsightResult[] {
	const order = options.ORDER;
	if (!order) {
		throw new InsightError("invalid options passed to sortResults");
	}
	if (typeof order === "object") {
		//list must contain at least one key
		return multiSort(results, order.dir, order.keys);
	} else {
		return results.sort((a, b) => sortCompare(a, b, order, 1));
	}
}

export function multiSort(results: InsightResult[], direction: string, keys: string[]): InsightResult[] {
	const dir = direction === "UP" ? 1 : -1;
	results.sort((a, b) => {
		for (const key of keys) {
			const comparison = sortCompare(a, b, key, dir);
			if (comparison !== 0) {
				return comparison;
			}
		}
		return 0;
	});
	return results;
}

export function sortCompare(a: InsightResult, b: InsightResult, order: string, dir: number): number {
	const aValue = a[order];
	const bValue = b[order];

	if (typeof aValue === "string" && typeof bValue === "string") {
		// string comparison (case-insensitive)
		return dir * aValue.localeCompare(bValue, undefined, { sensitivity: "base" });
	}

	if (typeof aValue === "number" && typeof bValue === "number") {
		// numeric comparison
		return dir * (aValue - bValue);
	}
	throw new InsightError("Comparison column contains strings and numbers together!");
}
