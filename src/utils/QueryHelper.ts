import { InsightError, InsightResult } from "../controller/IInsightFacade";
import { Query, Section, Body, Options } from "../models/Section";
import fs from "fs-extra";
import path from "node:path";
import { parseSectionObject } from "./JsonHelper";

/**
 * Takes Body portion of a valid Query and filters InsightResult[] according to specified conditions in Query
 *
 * @param filter - Body portion of a valid Query
 * @param data - InsightResult[] data to filter
 * @return InsightResult[]
 *
 * Returns filtered results, or an InsightError for any errors with filtering.
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
		throw new InsightError("Invalid filter.");
	}
}

/**
 * Takes logic comparison portions of the query and filters InsightResult[] according to specified conditions.
 * Recursively calls handleFilter() for inner operators (eg. LT/NOT).
 *
 * @param filter - logic (AND/OR) operations within Body portion of a valid Query
 * @param data - InsightResult[] data to filter
 * @returns InsightResult[]
 *
 * Returns filtered results, or an InsightError for any errors with filtering.
 */
export function handleLogicComparison(filter: Body, data: InsightResult[]): InsightResult[] {
	let results: InsightResult[] = [];

	// Inspired by ChatGPT responses
	if (filter.AND) {
		for (const subFilter of filter.AND) {
			const subResults = handleFilter(subFilter, data);
			if (results.length === 0) {
				results = subResults;
			} else {
				results = results.filter((section) => subResults.includes(section)); // Intersection of results
			}
		}
	} else if (filter.OR) {
		for (const subFilter of filter.OR) {
			const subResults = handleFilter(subFilter, data);
			results = results.concat(subResults); // Union of results
		}
		results = Array.from(new Set(results)); // Remove duplicates
	} else {
		throw new InsightError("Invalid logic filter.");
	}
	return results;
}

/**
 * Takes numeric (M) comparison portions of the query and filters InsightResult[] according to specified conditions.
 *
 * @param filter - numeric (LT/GT/EQ) operations within Body portion of a valid Query
 * @param data - InsightResult[] data to filter
 * @returns InsightResult[]
 *
 * Returns filtered results, or an InsightError for any errors with filtering.
 */
export function handleMComparison(filter: any, data: InsightResult[]): InsightResult[] {
	// Inspired by ChatGPT responses
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
	throw new InsightError("Invalid numeric comparison filter.");
}

/**
 * Takes string (S) comparison portions of the query and filters InsightResult[] according to specified conditions.
 *
 * @param filter - string (IS) operations within Body portion of a valid Query
 * @param data - InsightResult[] data to filter
 * @returns InsightResult[]
 *
 * Returns filtered results, or an InsightError for any errors with filtering.
 */
export function handleSComparison(filter: any, data: InsightResult[]): InsightResult[] {
	const sKey = Object.keys(filter.IS)[0];
	const value = Object.values(filter.IS)[0] as string;

	// Inspired by ChatGPT responses
	const regex = new RegExp(/^\*?[^*]*\*?$/); // Handle wildcards
	if (!regex.test(value)) {
		throw new InsightError("Invalid string comparison operator: bad wildcard.");
	}
	const validValue = new RegExp(`^${value.replace(/\*/g, ".*")}$`); // Handle wildcards
	return data.filter((section) => validValue.test(section[sKey] as string));
}

/**
 * Takes negation portions of the query and filters InsightResult[] according to specified conditions.
 * Recursively calls handleFilter() for inner operators (eg. LT/NOT).
 *
 * @param filter - negation (NOT) operations within Body portion of a valid Query
 * @param data - InsightResult[] data to filter
 * @returns InsightResult[]
 *
 * Returns filtered results, or an InsightError for any errors with filtering.
 */
export function handleNegation(filter: any, data: InsightResult[]): InsightResult[] {
	// Exclude matching sections
	const notData = handleFilter(filter.NOT, data);
	return data.filter((section) => !notData.includes(section));
}

/**
 * Returns InsightResult[] containing all sections in dataset specified by query.
 *
 * @param query - valid Query
 * @param datasets - map of datasetIds for your current InsightFacade
 * @returns Promise<InsightResult[]>
 *
 * Will return InsightResult[] containing all sections in a dataset upon resolving,
 * or an InsightError for any errors with retrieving data.
 */
export async function getAllSections(query: Query, datasets: Map<string, number>): Promise<InsightResult[]> {
	const idString = extractDatasetId(query);
	const fileName = String(datasets.get(idString));
	const allSections = await loadDatasets(idString, fileName);

	const columns = Object.keys(allSections[0]);

	const allResults: InsightResult[] = [];
	for (const section of allSections) {
		const sectionResult: InsightResult = {};
		for (const item of columns) {
			// Iterate through indices
			sectionResult[`${idString}_${item}`] = section[item as keyof Section];
		}
		allResults.push(sectionResult);
	}

	return allResults;
}

/**
 * Sorts given InsightResult[] based on column specified in ORDER within OPTIONS portion of valid Query.
 *
 * @param options - OPTIONS portion of Query
 * @param results - InsightResult[] to sort
 * @returns InsightResult[]
 *
 * Will return sorted InsightResult[], or an InsightError for any errors with sorting data
 * (e.g. columns contains string and numbers, ORDER does not exist).
 */
export function sortResults(options: Options, results: InsightResult[]): InsightResult[] {
	// Inspired by ChatGPT responses
	return results.sort((a, b) => {
		if (!options.ORDER) {
			throw new InsightError("invalid options passed to sortResults");
		}

		const aValue = a[options.ORDER];
		const bValue = b[options.ORDER];

		// Inspired by ChatGPT responses
		// String comparison (case-insensitive)
		if (typeof aValue === "string" && typeof bValue === "string") {

			return aValue.localeCompare(bValue, undefined, { sensitivity: "base" });
		}
		// Numeric comparison
		if (typeof aValue === "number" && typeof bValue === "number") {
			return aValue - bValue;
		}

		// If types differ
		throw new InsightError("Comparison column contains strings and numbers together!");
	});
}

/**
 * Extracts dataset id string from a valid Query.
 *
 * @param query - valid Query
 * @returns string - dataset id
 */
export function extractDatasetId(query: Query): string {
	const keys: string[] = query.OPTIONS.COLUMNS;
	const keyParts = keys[0].split("_");
	return keyParts[0];
}

/**
 * Loads a Section[] containing all the sections in a dataset specified by id in @param.
 *
 * @param id - id of dataset to load
 * @param fileName - name of file associated with dataset with id in @param
 * @returns Promise<Section[]>
 *
 * Will return Section[] upon resolving, or an InsightError for any errors with retrieving data.
 */
export async function loadDatasets(id: string, fileName: string): Promise<Section[]> {
	const datasetPath = path.resolve(__dirname, "../data", fileName);
	let dataset;
	try {
		dataset = await fs.readJson(datasetPath);
	} catch (error) {
		throw new InsightError(`Could not find fileName for dataset with - id=${id};` + error);
	}
	const parsedSections: Section[] = [];
	for (const file of dataset) {
		for (const section of file.result) {
			const newSection = parseSectionObject(section);
			parsedSections.push(newSection);
		}
	}
	return parsedSections;
}
