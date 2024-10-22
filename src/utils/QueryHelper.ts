import { InsightError, InsightResult, NotFoundError } from "../controller/IInsightFacade";
import { Query, Section, Body, Options } from "../models/Section";
import fs from "fs-extra";
import path from "node:path";
import { parseSectionObject } from "./JsonHelper";

/**
 * @returns - Query, validates that the query param conforms to Query structure, if not throws InsightError
 * @param filter
 * @param data
 */

export function handleFilter(filter: Body, data: InsightResult[]): InsightResult[] {
	if (Object.keys(filter).length === 0) {
		return data;
	}
	if (filter.AND || filter.OR) {
		return handleLogicComparison(filter, data);
	} else if (filter.LT || filter.GT || filter.EQ) {
		return handleMComparison(filter, data);
	} else if (filter.IS) {
		return handleSComparison(filter, data);
	} else if (filter.NOT) {
		return handleNegation(filter, data);
	} else {
		throw new InsightError("Invalid filter");
	}
}

function handleLogicComparison(filter: Body, data: InsightResult[]): InsightResult[] {
	//return array of insight result.
	let results: InsightResult[] = [];
	let alreadyAnd = false;

	if (filter.AND) {
		// intersection of all results: all filters must be true
		// return filter.AND.reduce((acc, subBody) => handleBody(subBody, acc), data);
		for (const subFilter of filter.AND) {
			const subResults = handleFilter(subFilter, data);
			if (!alreadyAnd) {
				results = subResults;
				alreadyAnd = true;
			} else {
				results = results.filter((section) => subResults.includes(section)); // Intersection of results
			}
		}
	} else if (filter.OR) {
		results = [];
		for (const subFilter of filter.OR) {
			const subResults = handleFilter(subFilter, data);
			results = results.concat(subResults); // Union of results
		}
		results = Array.from(new Set(results)); // Remove duplicates
	} else {
		throw new InsightError("Invalid logic filter");
	}
	return results;
}

export function handleMComparison(filter: any, data: InsightResult[]): InsightResult[] {
	if (filter.GT) {
		const mKey = Object.keys(filter.GT)[0];
		const value = Object.values(filter.GT)[0];
		return data.filter((section) => section[mKey] > (value as number));
	}
	if (filter.LT) {
		const mKey = Object.keys(filter.LT)[0];
		const value = Object.values(filter.LT)[0];
		return data.filter((section) => section[mKey] < (value as number));
	}
	if (filter.EQ) {
		const mKey = Object.keys(filter.EQ)[0];
		const value = Object.values(filter.EQ)[0];
		return data.filter((section) => section[mKey] === (value as number));
	}
	throw new InsightError("Invalid MComparator operator.");
}

export function handleSComparison(filter: any, data: InsightResult[]): InsightResult[] {
	const sKey = Object.keys(filter.IS)[0];
	const value = Object.values(filter.IS)[0] as string;

	const regex = new RegExp(/^\*?[^*]*\*?$/); // Handle wildcards
	if (!regex.test(value)) {
		throw new InsightError("invalid SComparator operator, bad wildcard.");
	}
	const validValue = new RegExp(`^${value.replace(/\*/g, ".*")}$`); // Handle wildcards
	return data.filter((section) => validValue.test(section[sKey] as string));
}

export function handleNegation(filter: any, data: InsightResult[]): InsightResult[] {
	// Exclude matching sections
	//handleNotFilter(filter.NOT, data)
	const notData = handleFilter(filter.NOT, data);
	return data.filter((section) => !notData.includes(section));
}

// export function handleNotFilter(filter: any, data: InsightResult[]): InsightResult[] {
// 	let mkey;
// 	let
// 	switch (filter.NOT.keys()[0]) {
// 		case "IS":
//
// 			break;
// 		case "GT":
// 			const mKey = Object.keys(filter.GT)[0];
// 			const value = Object.values(filter.GT)[0];
// 			return data.filter((section) => section[mKey] <= (value as number));
// 		case "LT":
//
// 	}
// }

export async function getAllSections(query: Query, datasets: Map<string, number>): Promise<Set<InsightResult>> {
	const idString = extractDatasetId(query);
	const fileName = String(datasets.get(idString));
	const allSections = await loadDatasets(idString, fileName);

	return parseToInsightResult(allSections, idString);
}

/**
 * @returns - InsightResult[], takes already filtered and selected results and sorts them according to options.ORDER
 * if options.ORDER exists, if not throws an error as sort Results should not have been called. If types of values
 * between 2 sections.ORDER differs throw InsightError
 * @param options
 * @param results
 */
export function sortResults(options: Options, results: InsightResult[]): InsightResult[] {
	return results.sort((a, b) => {
		if (!options.ORDER) {
			throw new InsightError("invalid options passed to sortResults");
		}

		const aValue = a[options.ORDER];
		const bValue = b[options.ORDER];

		if (typeof aValue === "string" && typeof bValue === "string") {
			// string comparison (case-insensitive)
			return aValue.localeCompare(bValue, undefined, { sensitivity: "base" });
		}

		if (typeof aValue === "number" && typeof bValue === "number") {
			// numeric comparison
			return aValue - bValue;
		}

		// types differ
		throw new InsightError("Comparison column contains strings and numbers together!");
	});
}

/**
 * @returns - string, extracts dataset id from first key found in OPTIONS.COLUMNS
 * @param query
 */
export function extractDatasetId(query: Query): string {
	const keys: string[] = query.OPTIONS.COLUMNS;
	const keyParts = keys[0].split("_");
	return keyParts[0];
}

/**
 * @returns - Promise<Section[]> an array of the sections contained in the data specified by param id.
 * If dataset with name data/fileName can not be found throws NotFoundError
 * @param fileName
 * @param id
 */
export async function loadDatasets(id: string, fileName: string): Promise<Set<Section>> {
	const datasetPath = path.resolve("./data", fileName);
	let dataset;
	try {
		dataset = await fs.readJson(datasetPath);
	} catch (error) {
		throw new NotFoundError(`Could not find dataset with - id=${id};` + error);
	}
	const parsedSections = new Set<Section>();
	for (const file of dataset.files) {
		for (const section of file.result) {
			const newSection = parseSectionObject(section);
			parsedSections.add(newSection);
		}
	}
	return parsedSections;
}

/**
 * @returns - InsightResult[], takes the filtered InsightResults array and selects the columns to keep according to OPTIONS.COLUMNS
 * @param filteredResults
 * @param validatedQuery
 */
export function selectColumns(filteredResults: InsightResult[], validatedQuery: Query): InsightResult[] {
	return filteredResults.map((section) => {
		// can be string | number
		const result: any = {};
		const columns = validatedQuery.OPTIONS.COLUMNS;
		for (const column of columns) {
			result[column] = section[column];
		}
		return result;
	});
}

export function parseToInsightResult(allSections: Set<Section>, idString: string): Set<InsightResult> {
	const columns = new Set<string>([
		"uuid",
		"id",
		"title",
		"instructor",
		"dept",
		"year",
		"avg",
		"pass",
		"fail",
		"audit",
	]);
	const allResults = new Set<InsightResult>();
	for (const section of allSections) {
		const sectionResult: InsightResult = {};
		for (const item of columns) {
			//iterate through indicies
			sectionResult[`${idString}_${item}`] = section[item as keyof Section];
		}
		allResults.add(sectionResult);
	}
	return allResults;
}
