import {
	InsightDataset,
	InsightDatasetKind,
	InsightError,
	InsightResult,
	NotFoundError,
	ResultTooLargeError,
} from "../controller/IInsightFacade";
import { Section } from "../models/Section";
import fs from "fs-extra";
import path from "node:path";
import { parseSectionObject } from "./JsonHelper";
import { Building, Room } from "../models/Room";
import { sortResults } from "./SortHelper";
import { Body, Query } from "../models/Query";
import { apply, groupBy } from "./TransformationsHelper";

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

export async function getAllData(
	query: Query,
	datasets: Map<string, number>,
	kind: InsightDatasetKind
): Promise<Set<InsightResult>> {
	const idString = extractDatasetId(query, datasets);
	const fileName = String(datasets.get(idString));
	const dataset = await loadDataset(idString, fileName, kind);

	if (kind === InsightDatasetKind.Sections) {
		return parseSectionsToInsightResult(dataset as Set<Section>, idString);
	} else {
		return parseRoomsToInsightResult(dataset as Set<Room>, idString);
	}
}

/**
 * @returns - string, extracts dataset id from first key found in OPTIONS.COLUMNS
 * @param query
 * @param datasetIds
 */
export function extractDatasetId(query: any, datasetIds: Map<string, number>): string {
	if (query.OPTIONS) {
		const options = query.OPTIONS;
		if (options.COLUMNS) {
			for (const key of options.COLUMNS) {
				const keyParts = key.split("_");
				if (datasetIds.has(keyParts[0])) {
					return keyParts[0];
				}
			}
		}
	}
	throw new InsightError("invalid query structure could not extract dataset ID");
}

/**
 * @returns - Promise<Section[]> an array of the sections contained in the data specified by param id.
 * If dataset with name data/fileName can not be found throws NotFoundError
 * @param fileName
 * @param id
 * @param kind
 */
export async function loadDataset(
	id: string,
	fileName: string,
	kind: InsightDatasetKind
): Promise<Set<Section> | Set<Room>> {
	const datasetPath = path.resolve("./data", fileName);
	let dataset;
	try {
		dataset = await fs.readJson(datasetPath);
	} catch (error) {
		throw new NotFoundError(`Could not find dataset with - id=${id};` + error);
	}

	if (kind === InsightDatasetKind.Sections) {
		const parsedSections = new Set<Section>();
		for (const file of dataset.files) {
			for (const section of file.result) {
				const newSection = parseSectionObject(section);
				parsedSections.add(newSection);
			}
		}
		return parsedSections;
	} else {
		const parsedRooms = new Set<Room>();
		for (const file of dataset.files) {
			for (const room of file.result) {
				const newRoom = JSON.parse(room);
				parsedRooms.add(newRoom);
			}
		}
		return parsedRooms;
	}
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
			// if (section[column] === undefined) {
			// 	throw new InsightError("could not find column in columns");
			// }
			result[column] = section[column];
		}
		return result;
	});
}

export function parseSectionsToInsightResult(allSections: Set<Section>, idString: string): Set<InsightResult> {
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
			sectionResult[`${idString}_${item}`] = section[item as keyof Section];
		}
		allResults.add(sectionResult);
	}
	return allResults;
}

export function parseRoomsToInsightResult(allRooms: Set<Room>, idString: string): Set<InsightResult> {
	const roomColumns = new Set<string>(["name", "number", "type", "furniture", "seats", "href"]);
	const buildingColumns = new Set<string>(["fullname", "shortname", "address", "lat", "lon"]);
	const allResults = new Set<InsightResult>();
	for (const room of allRooms) {
		const roomResult: InsightResult = {};
		for (const item of roomColumns) {
			const value = room[item as keyof Room];
			if (typeof value === "string" || typeof value === "number") {
				roomResult[`${idString}_${item}`] = value;
			}
			//this should always be the case, but I had to put the explicit type guard in because typescript was complaining about the building field.
		}
		for (const item of buildingColumns) {
			const value = room.building[item as keyof Building];
			if (value) {
				//since lat or lon will be undefined
				roomResult[`${idString}_${item}`] = value;
			}
		}
		allResults.add(roomResult);
	}
	return allResults;
}

export function queryInsightResults(allResults: Set<InsightResult>, validatedQuery: Query): InsightResult[] {
	const MAX_SIZE = 5000;
	//4) Filter results according to WHERE
	let filteredResults: InsightResult[];
	filteredResults = handleFilter(validatedQuery.WHERE, Array.from(allResults));

	//5) Group results and apply
	if (validatedQuery.TRANSFORMATIONS) {
		const groupedResults = groupBy(filteredResults, validatedQuery);
		if (validatedQuery.TRANSFORMATIONS.APPLY) {
			for (const group of groupedResults.values()) {
				const calculation = apply(group, validatedQuery.TRANSFORMATIONS.APPLY);
				Object.assign(group[0], calculation); //will this add it to group inplace?
			}
		}
		filteredResults = Array.from(groupedResults.values()).map((group) => group[0]);
	}

	// 5) handle results that are too large
	if (filteredResults.length > MAX_SIZE) {
		throw new ResultTooLargeError("Query results exceed maximum size (5000 sections).");
	}

	// 7) sort results if necessary (OPTIONS.ORDER)
	let sortedFilteredResults: InsightResult[];
	if (validatedQuery.OPTIONS.ORDER) {
		sortedFilteredResults = sortResults(validatedQuery.OPTIONS, filteredResults);
	} else {
		sortedFilteredResults = filteredResults;
	}

	// 6) select only specified columns (OPTIONS.COLUMNS)
	return selectColumns(sortedFilteredResults, validatedQuery);
}

export async function loadMeta(): Promise<Map<string, InsightDataset>> {
	const datasetPath = path.resolve("./data", "meta");
	let dataset;
	try {
		dataset = await fs.readJson(datasetPath);
	} catch (error) {
		throw new NotFoundError(`Could not find dataset meta` + error);
	}
	const datasetInfo: Map<string, InsightDataset> = new Map<string, InsightDataset>();
	for (const data of dataset) {
		const id = data.id;
		const kind = data.kind;
		const numRows = data.numRows;
		const meta = {
			id: id,
			kind: kind,
			numRows: numRows,
		};
		datasetInfo.set(id, meta);
	}
	return datasetInfo;
}
