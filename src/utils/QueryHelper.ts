import {InsightError, InsightResult, NotFoundError} from "../controller/IInsightFacade";
import {
	Filter,
	LogicComparison,
	MComparison,
	MKey,
	Negation,
	SComparison,
	Section,
	SKey,
	Where
} from "../models/Section";
import fs from "fs-extra";
import path from "node:path";
import {parseJSONtoSections} from "./JsonHelper";

export function validateQuery(query: unknown): boolean {
	return true;
	//only accept things that are in the form of JSON object.
}

/**
 * @returns -
 * @param query - ASSUME that query is a valid and in JSON format
 */
export function extractDatasetId(query: unknown): string {
	//Adapted from ChatGPT:
	// const regex = /"([^_]+)_(mfield|sfield)"/g
	// const id = regex.exec(query);
	// if (!id) {
	// 	throw new InsightError('Could not find idString within query passed to performQuery');
	// }
	// return id[1];
	console.log(query);
	return "stub";
}

export async function loadDatasets(id: string, datasetIds: Map<string, number>): Promise<Section[]> {
	const name = datasetIds.get(id)
	const datasetPath = path.resolve(__dirname, "../data", String(name));
	let dataset;
	try {
		dataset = await fs.readJson(datasetPath)
	} catch (error) {
		throw new NotFoundError(`Could not find dataset with - id=${id};` + error)
	}

	return parseJSONtoSections(dataset);
}

export async function processQueryOnDataset(query: unknown): Promise<InsightResult[]> {
	//query should come in as a json object
	//handleWhere(query.WHERE);

	return [];
}

export function handleWhere(where: Where, id: string): void {
		if (where.FILTER !== undefined) {
			handleFilter(where.FILTER);
		} else {
			//if there is no filter specified so matches all entries
		}
	console.log(id)
	}


export function handleFilter(filter: Filter): void {

	if ('AND' in filter || 'OR' in filter) {
		handleLogicComparison(filter);
	} else if ('LT' in filter || 'GT' in filter || 'EQ' in filter) {
		handleMComparator(filter);
	} else if ('IS' in filter) {
		handleSComparator(filter);
	} else if ('NOT' in filter) {
		handleNegation(filter);
	} else {
		throw new InsightError('There was some problem here!')
	}

}

function handleLogicComparison(filter: LogicComparison): void {
	if('AND' in filter && Array.isArray(filter.AND)) {
		for(const f of filter.AND) {
			handleFilter(f);
		}

	} else if ('OR' in filter && Array.isArray(filter.OR)) {
		for(const f of filter.OR) {
			handleFilter(f);
		}
	} else {
		throw new InsightError('There was some problem here!')
	}
}

function handleMComparator(filter: MComparison): void {

}

function handleSComparator(filter: SComparison): void {


}

function handleNegation(filter: Negation): void {

}

function getMatchingSections(op: string, field: (Record<MKey, number> | Record<SKey, number>), sects: Section[]): void {
	//decode Record to find id and Mfield or Sfield.
	//loadDatasets(id)
	//searchSections and collect those that fit the criteria
	//Get sections that match
}



