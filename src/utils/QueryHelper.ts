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

	if (filter.AND) {
		// intersection of all results: all filters must be true
		// return filter.AND.reduce((acc, subBody) => handleBody(subBody, acc), data);
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
		throw new InsightError("Invalid logic filter");
	}
	return results;
}

function handleMComparison(filter: any, data: InsightResult[]): InsightResult[] {
	if (filter.GT) {
		const mKey = Object.keys(filter.GT)[0];
		const parts = mKey.split("_");
		const field = parts[1];
		const value = Object.values(filter.GT)[0];
		return data.filter((section) => section[field] > (value as number));
	}
	if (filter.LT) {
		const mKey = Object.keys(filter.LT)[0];
		const parts = mKey.split("_");
		const field = parts[1];
		const value = Object.values(filter.LT)[0];
		return data.filter((section) => section[field] < (value as number));
	}
	if (filter.EQ) {
		const mKey = Object.keys(filter.EQ)[0];
		const parts = mKey.split("_");
		const field = parts[1];
		const value = Object.values(filter.EQ)[0];
		return data.filter((section) => section[field] === (value as number));
	}
	throw new InsightError("Invalid MComparator operator.");
}

function handleSComparison(filter: any, data: InsightResult[]): InsightResult[] {
	const sKey = Object.keys(filter.IS)[0];
	const parts = sKey.split("_");
	const field = parts[1];
	const value = Object.values(filter.IS)[0] as string;

	const regex = new RegExp(/^\*?[^*]*\*?$/); // Handle wildcards
	if (!regex.test(value)) {
		throw new InsightError("invalid SComparator operator, bad wildcard.");
	}
	const validValue = new RegExp(`^${value.replace(/\*/g, ".*")}$`); // Handle wildcards
	return data.filter((section) => validValue.test(section[field] as string));
}

function handleNegation(filter: any, data: InsightResult[]): InsightResult[] {
	// Exclude matching sections
	const notData = handleFilter(filter.NOT, data);
	return data.filter((section) => !notData.includes(section));
}

export async function getAllSections(query: Query, datasets: Map<string, number>): Promise<InsightResult[]> {
	const idString = extractDatasetId(query);
	const fileName = String(datasets.get(idString));
	const allSections = await loadDatasets(idString, fileName);

	const columns = Object.keys(allSections[0]);

	const allResults: InsightResult[] = [];
	for (const section of allSections) {
		const sectionResult: InsightResult = {};
		for (const item of columns) {
			//iterate through indicies
			sectionResult[item] = section[item as keyof Section];
		}
		allResults.push(sectionResult);
	}

	return allResults;
}

export function sortResults(options: Options, results: InsightResult[]): InsightResult[] {
	return results.sort((a, b) => {
		if (!options.ORDER) {
			throw new InsightError("invalid options passed to sortResults");
		}
		const parts = options.ORDER.split("_");
		const field = parts[1];

		const aValue = a[field];
		const bValue = b[field];

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

export function extractDatasetId(query: Query): string {
	const keys: string[] = query.OPTIONS.COLUMNS;
	const keyParts = keys[0].split("_");
	return keyParts[0];
}

//THIS IS THE HELPER TO CALL IN EACH BASE CASE FOR WHERE FUNCTION: MCOMPARATOR, SCOMPARATOR
// /**
//  * @returns - Section[] of sections returned by loadDatasets contained in the data set specified in the id string of
//  * param field. Sections have been filtered according to op and Mfield|Sfield.
//  * @param op
//  * @param record
//  */
// export async function getMatchingSections(op: string, record: [MKey, number] | [SKey, string]): Promise<Section[]> {
//
// 	const key = record[0];
// 	const value = record[1];
// 	const parts = key.split('_');
// 	const idString = parts[0];
// 	const field = parts[1];
//
// 	//Adapted form ChatGPT generated response
// 	const comp = generateCompFunction(op);
//
// 	const sections = await loadDatasets(idString, 'sections');
// 	const results: Section[] = [];
// 	for (const section of sections) {
// 		if (filterSection(section, comp, value, field)) {
// 			results.push(section);
// 		}
// 	}
// 	return results;
// }
//
// export function generateCompFunction(op: string): CompFunction {
// 	switch(op) {
// 		case 'GT':
// 			return ((a: number, b: number) => a > b) as CompFunction;
// 		case 'LT':
// 			return ((a: number, b: number) => a < b) as CompFunction;
// 		case 'EQ':
// 			return ((a: number, b: number) => a === b) as CompFunction;
// 		case 'IS':
// 			return ((a: string, b: string) => a === b) as CompFunction;
// 		default:
// 			throw new InsightError('not good very bad, no teneís un Quixote')
// 	}
// }
//
// export function filterSection(section: Section, comp: CompFunction, value: string | number, field: string): boolean {
// 	switch(field) {
// 		case 'avg':
// 			return comp(section.avg, value);
// 		case 'pass':
// 			return comp(section.pass, value);
// 		case 'fail':
// 			return comp(section.fail, value);
// 		case 'audit':
// 			return comp(section.audit, value);
// 		case 'year':
// 			return comp(section.year, value);
// 		case 'dept':
// 			return comp(section.dept, value);
// 		case 'id':
// 			return comp(section.id, value);
// 		case 'instructor':
// 			return comp(section.instructor, value);
// 		case 'title':
// 			return comp(section.title, value);
// 		case 'uuid':
// 			return comp(section.uuid, value);
// 		default:
// 			throw new InsightError('invalid key passed to getMatchingSections');
// 	}
// }

/**
 * @returns - Promise<Section[]> an array of the sections contained in the data specified by param id.
 * If dataset with name data/fileName can not be found throws NotFoundError
 * @param fileName
 * @param id
 */
export async function loadDatasets(id: string, fileName: string): Promise<Section[]> {
	const datasetPath = path.resolve(__dirname, "../data", fileName);
	let dataset;
	try {
		dataset = await fs.readJson(datasetPath);
	} catch (error) {
		throw new NotFoundError(`Could not find dataset with - id=${id};` + error);
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

/**
 * @returns - InsightResult[], selects columns from array of sections as specified by options.COLUMNS and parses them
 * into InsightResult. Creates one Insight Result per section. Final InsightResult[] ordered according to column specified
 * by options.ORDER if applicable.
 * @param options
 * @param sections
 */
// export function handleOptions(options: Options, sections: Section[]): InsightResult[] {
// 	const columns = options.COLUMNS;
//
// 	const column_keys: string[] = [];
// 	for (const column of columns) {
// 		if (!isMKey(column) && !isSKey(column)) {
// 			throw new InsightError('invalid keys passed to OPTIONS.COLUMNS, NOTE THIS WAS NOT CAUGHT IN VALIDATE QUERY');
// 		}
// 		const parts = column.split('_');
// 		column_keys.push(parts[1]);
// 	}
//
// 	const insight_results: InsightResult[] = [];
// 	for (const section of sections) {
// 		const section_result: InsightResult = {};
// 		for (let i = 0; i < column_keys.length; i++) {  //iterate through indices
// 			section_result[columns[i]] = section[column_keys[i] as keyof Section]; //WILL THIS LINE WORK? PROBABLY BUG HERE
// 		}
// 		insight_results.push(section_result);
// 	}
//
// 	if (options.ORDER) {
// 		const order = options.ORDER;
// 		if (!isMKey(order) && !isSKey(order)) {
// 			throw new InsightError('invalid keys passed to OPTIONS.COLUMNS, NOTE THIS WAS NOT CAUGHT IN VALIDATE QUERY');
// 		}
// 		const parts = order.split('_');
// 		const field = parts[1];
//
// 		//Adapted from ChatGPT generated response:
// 		insight_results.sort((a, b) => {
// 			const valueA = a[field];
// 			const valueB = b[field];
//
// 			if (typeof valueA === 'number' && typeof valueB === 'number') {
// 				return valueA - valueB;
// 			} else if (typeof valueA === 'string' && typeof valueB === 'string') {
// 				return valueA.localeCompare(valueB, undefined, {sensitivity: 'base'});
// 			} else {
// 				throw new InsightError('fields in handleOptions.ORDER not of same type.')
// 			}
// 		})
//
// 	}
// 	return insight_results;
// }
