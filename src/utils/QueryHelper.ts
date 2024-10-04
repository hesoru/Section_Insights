import {InsightError, InsightResult, NotFoundError} from "../controller/IInsightFacade";
import {
	MKey, Query,
	Section,
	SKey
} from "../models/Section";
import fs from "fs-extra";
import path from "node:path";
import {parseJSONtoSections} from "./JsonHelper";

const testQuery =  {
	"WHERE":{
		"GT":{
			"sections_avg":97
		}
	},
	"OPTIONS":{
		"COLUMNS":[
			"sections_dept",
			"sections_avg"
		],
		"ORDER":"sections_avg"
	}
}

// { "sections_dept": "math", "sections_avg": 97.09 },
// { "sections_dept": "math", "sections_avg": 97.09 },
// { "sections_dept": "epse", "sections_avg": 97.09 },
// { "sections_dept": "epse", "sections_avg": 97.09 },
// { "sections_dept": "math", "sections_avg": 97.25 },
// { "sections_dept": "math", "sections_avg": 97.25 },
// { "sections_dept": "epse", "sections_avg": 97.29 },
// { "sections_dept": "epse", "sections_avg": 97.29 },
// { "sections_dept": "nurs", "sections_avg": 97.33 },
// { "sections_dept": "nurs", "sections_avg": 97.33 },
// { "sections_dept": "epse", "sections_avg": 97.41 },
// { "sections_dept": "epse", "sections_avg": 97.41 },
// { "sections_dept": "cnps", "sections_avg": 97.47 },



export async function processQueryOnDataset(query: unknown): Promise<InsightResult[]> {
	try {

		//1) Check if query is of type object
		//isInstance(query, Object)

		//2) How do we map unknown query to Query
		//if Where exists

		const parsedQuery: Record<string, any> = {};

		//validate query syntax, if follows rules call it a query

		// try query.WHERE, if not reject
		if (query.WHERE) {
			parsedQuery.BODY = handleBody(query.WHERE);
		}
		// Parse OPTIONS
		if (query.OPTIONS) {
			parsedQuery.OPTIONS = handleOptions(query.OPTIONS);
		}

		return parsedQuery;
	} catch (error: any) {
		throw new Error(`Query not a valid format: ${error.message}`);
	}
}

export function validateQuery(query: Query): Query {
	return query;
	//only accept things that are in the form of JSON object.
	//can you return a Query Object here which can be used in processQuery
}

export function handleBody(body: Body): InsightResult[] {  //param is type body because we know it follows structure of body
	if (!body) {
		// no filter - return all entries
		return InsightResult[];
	}
	// handle filters within the body
	return handleFilter(body);
}

//Helena:
export function handleOptions(options: Options): object {
	const columns = options.COLUMNS;
	const order = options.ORDER;
	if (options.ORDER) {

	}
	return { COLUMNS: columns, ORDER: order };
}


//Helena:
export function handleFilter(filter: Body): object | string {
	if (filter.AND || filter.OR) {
		return handleLogicComparison(filter);
	} else if (filter.LT || filter.GT || filter.EQ) {
		return handleMComparison(filter);
	} else if (filter.IS) {
		return handleSComparison(filter);
	} else if (filter.NOT) {
		return handleNegation(filter);
	}
	return "Unknown filter";
}

handleLogicComparison(logic: Body): object | string {  //return array of insight result.
	if (logic.AND) {
		const filters = logic.AND;
		const parsedFilters = filters.map((f) => handleFilter(f));
		return { AND: parsedFilters };
	} else if (logic.OR) {
		const filters = logic.OR;
		const parsedFilters = filters.map((f) => handleFilter(f));
		return { OR: parsedFilters };
	}
	return "Invalid logic comparison";
}


export function handleFilter(filter: Filter): void {

	if (filter.type === 'AND') {
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

export async function loadDatasets(id: string, fileName: string): Promise<Section[]> {
	const datasetPath = path.resolve(__dirname, "../data", fileName);
	let dataset;
	try {
		dataset = await fs.readJson(datasetPath)
	} catch (error) {
		throw new NotFoundError(`Could not find dataset with - id=${id};` + error)
	}

	return parseJSONtoSections(dataset);
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


//THIS IS THE HELPER TO CALL IN EACH BASE CASE FOR WHERE FUNCTION: MCOMPARATOR, SCOMPARATOR
function getMatchingSections(op: string, field: (Record<MKey, number> | Record<SKey, number>), sects: Section[]): void {
	//decode Record to find id and Mfield or Sfield.
	//loadDatasets(id)
	//searchSections and collect those that fit the criteria
	//Get sections that match
}



