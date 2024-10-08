import {
	InsightDataset,
	InsightDatasetKind,
	InsightError,
	InsightResult,
	NotFoundError
} from "../controller/IInsightFacade";
import {
	MKey, Query,
	Section,
	SKey, Body, Options
} from "../models/Section";
import fs from "fs-extra";
import path from "node:path";
import {parseJSONtoSections} from "./JsonHelper";

// declare let query: Query;
// declare const columns = [];

export async function processQueryOnDataset(query: unknown): Promise<InsightResult[]> {
	// 1) validate query
	let validatedQuery: Query;
	try {
		validatedQuery = validateQuery(query);

	} catch (error) {
		throw new Error(`Query not a valid format: ` + error);
	}

	// 2) obtain data for all sections
	const allSections = getAllSections(validatedQuery);

	// 3) filter results if necessary (WHERE)
	let filteredResults: InsightResult[];
	if (validatedQuery.WHERE) {
		filteredResults = handleFilter(validatedQuery.WHERE, allSections);
	} else {
		//
	}

	// 4) select only specified columns (OPTIONS.COLUMNS)
	if (validatedQuery.OPTIONS.COLUMNS) {
		filteredResults = filteredResults.map((section) => {
			// can be string | number
			const result: any = {};
			validatedQuery.OPTIONS.COLUMNS.forEach((column) => {
				result[column] = section[column];
			});
			return result;
		});
	}

	// 5) sort results if necessary (OPTIONS.ORDER)
	let sortedFilteredResults: InsightResult[];
	if (validatedQuery.OPTIONS.ORDER) {
		sortedFilteredResults = sortResults(validatedQuery.OPTIONS, filteredResults);
	}

	return sortedFilteredResults;

	// //const allMatchingSections: Section[] = handleBody(validatedQuery.WHERE);  //should return InsightResult[] that matches Body specifications
	// //const orderedMatchingSections = handleOptions(validatedQuery.OPTIONS, allMatchingSections); //should return same InsightResult[] but sorted to Option specifications
	// return [];
}

/**
 * @returns - Query, validates that the query param conforms to Query structure, if not throws InsightError
 * @param query
 */
export function validateQuery(query: unknown): Query {
	//1) check that query is an object
	if (typeof query !== 'object' || query === null) {
		throw new InsightError('invalid query, query is not a non-null object');
	}

	//2) check WHERE fields
	if ('WHERE' in query) {
		if (query.WHERE === null || typeof query.WHERE !== 'object') {
			throw new InsightError('invalid query, query contains null WHERE field');
		}
		validateBody(query.WHERE);
	} else {
		throw new InsightError('invalid query, query does not contain WHERE field');
	}

	//3) check OPTION fields
	if ('OPTIONS' in query) {
		//is the OPTIONS field allowed to be null? I don't think so
		if (query.OPTIONS === null || typeof query.OPTIONS !== 'object') {
			throw new InsightError('invalid query, query contains invalid OPTIONS field');
		}
		validateOptions(query.OPTIONS);
	} else {
		//throw new InsightError('invalid query, query does not contain OPTION field');
		//this is okay right
	}
	return query as Query;
}

export function validateBody(filter: any): void {
	const keys = Object.keys(filter);
	if (keys.length !== 1) {
		throw new InsightError('invalid query, query.WHERE contains more than one key');
	}

	//checks type of each possible filter stopping at Mkey and Skey
	switch (keys[0]) {
		case 'OR':
			if (filter.OR === null || !Array.isArray(filter.OR)) {
				throw new InsightError('invalid query, query.WHERE.OR is invalid')
			}
			for (const body in filter.OR) {
				validateBody(filter.OR);
			}
			break;
		case 'AND':
			if (filter.AND === null || !Array.isArray(filter.AND)) {
				throw new InsightError('invalid query, query.WHERE.AND is invalid')
			}
			for (const body in filter.OR) {
				validateBody(filter.OR);
			}
			break;
		case 'GT':
			if (filter.GT === null || typeof filter.GT !== 'string') {
				throw new InsightError('invalid query, query.WHERE.GT is invalid')
			}
			break;
		case 'LT':
			if (filter.LT === null || typeof filter.LT !== 'string') {
				throw new InsightError('invalid query, query.WHERE.LT is invalid')
			}
			break;
		case 'EQ':
			if (filter.EQ === null || typeof filter.EQ !== 'string') {
				throw new InsightError('invalid query, query.WHERE.EQ is invalid')
			}
			break;
		case 'IS':
			if (filter.IS === null || typeof filter.IS !== 'string') {
				throw new InsightError('invalid query, query.WHERE.IS is invalid')
			}
			break;
		case 'NOT':
			if (filter.NOT === null || typeof filter.NOT !== 'object') {
				throw new InsightError('invalid query, query.WHERE.NOT is invalid')
			}
			validateBody(filter.NOT);
			break;
		default:
			throw new InsightError('invalid query, query.WHERE contains an invalid key');
	}
}

function validateOptions(options: any) {
	const keys = Object.keys(options);
	if (keys.length === 0 || keys.length > 2) {
		throw new InsightError('invalid query, query.OPTIONS incorrect number of keys');
	}
	//validate columns
	if (keys[0] === 'COLUMNS') { //!==?
		throw new InsightError('invalid query, query.OPTIONS does not contain COLUMNS');
	}
	if (!Array.isArray(options.COLUMNS) || options.COLUMNS.length === 0) {
		throw new InsightError('invalid query, query.OPTIONS.COLUMNS is not an array');
	}
	for (const key in options.COLUMNS) {
		validateKey(key);
	}

	//validate order
	if (keys[1] && keys[1] !== 'ORDER') {
		throw new InsightError('invalid query, query.OPTIONS does not contain ORDER as 2nd key');
	}
	validateKey(options.ORDER);
}

function validateKey(key: any) {
	if (typeof key === 'string') {
		throw new InsightError('invalid query, key is not a string');
	}
	if (!isMKey(key) || isSKey(key)) {
		throw new InsightError('invalid query, key is not an Mkey or Skey')
	}
}

function isMKey(key: string): boolean {
	if (!key.includes('_')) {
		return false;
	}
	const parts = key.split('_');
	const validId = /^[^_]+$/; //Adapted from chatGPT generated response.
	if (!validId.test(parts[0]) || parts[0].trim().length === 0) {
		return false;
	}
	const validMFields = ['avg', 'pass', 'fail', 'audit', 'year'];
	return validMFields.includes(parts[1]);
}

function isSKey(key: string): boolean {
	if (!key.includes('_')) {
		return false;
	}
	const parts = key.split('_');
	const validId = /^[^_]+$/; //Adapted from chatGPT generated response.
	if (!validId.test(parts[0]) || parts[0].trim().length === 0) {
		return false;
	}
	const validSFields = ['dept', 'id', 'instructor', 'title', 'uuid'];
	return validSFields.includes(parts[1]);
}

// export function handleBody(body: Body): InsightResult[] {  //param is type body because we know it follows structure of body
// 	if (!body || Object.keys(body).length === 0) {
// 		// no filter - return all sections
// 		return getAllSections();
// 	}
// 	// handle filters within the body
// 	return handleFilter(body);
// }

//Helena:
export function handleFilter(filter: Body, data: InsightResult[]): InsightResult[] {
	if (filter.AND || filter.OR) {
		return handleLogicComparison(filter, data);
	} else if (filter.LT || filter.GT || filter.EQ) {
		return handleMComparison(filter, data);
	} else if (filter.IS) {
		return handleSComparison(filter, data);
	} else if (filter.NOT) {
		return handleNegation(filter, data);
	} else {
		throw new InsightError('Invalid filter');
	}
}

function handleLogicComparison(filter: Body, data: InsightResult[]): InsightResult[] {  //return array of insight result.
	let results: InsightResult[] = [];

	if (filter.AND) {
		// intersection of all results: all filters must be true
		// return filter.AND.reduce((acc, subBody) => handleBody(subBody, acc), data);
		for (const subFilter of filter.AND) {
			const subResults = handleFilter(subFilter, data);
			results = results.filter(section => subResults.includes(section)); // Intersection of results
		}
	} else if (filter.OR) {
		for (const subFilter of filter.OR) {
			const subResults = handleFilter(subFilter, data);
			results = results.concat(subResults); // Union of results
		}
		results = Array.from(new Set(results)); // Remove duplicates

		// const filters = logic.OR;
		// // union of all results (any filter can match)
		// const results = filters.map(handleFilter);
		// return results.reduce((acc, curr) => acc.concat(curr.filter(section => !acc.includes(section))));
		// // const parsedFilters = filters.map((f) => handleFilter(f));
		// // return {OR: parsedFilters};
	} else {
		throw new InsightError('Invalid logic filter');
	}
	return results;
}

// /**
//  * @returns -
//  * @param query - ASSUME that query is a valid and in JSON format
//  */
// export function extractDatasetId(query: unknown): string {
// 	//Adapted from ChatGPT:
// 	// const regex = /"([^_]+)_(mfield|sfield)"/g
// 	// const id = regex.exec(query);
// 	// if (!id) {
// 	// 	throw new InsightError('Could not find idString within query passed to performQuery');
// 	// }
// 	// return id[1];
// 	console.log(query);
// 	return "stub";
// }


function handleMComparison(filter: any, data: InsightResult[]): InsightResult[] {
	if (filter.GT) {
		const [mKey, value] = filter.GT;
		return data.filter((section) => section[mKey] > value);
	}
	if (filter.LT) {
		const [mKey, value] = filter.LT;
		return data.filter((section) => section[mKey] < value);
	}
	if (filter.EQ) {
		const [mKey, value] = filter.EQ;
		return data.filter((section) => section[mKey] === value);
	}

	// const comparator = Object.keys(filter)[0];  // 'GT', 'LT', or 'EQ'
	// const mKey = Object.keys(filter[comparator])[0];  // Example: 'courses_avg'
	// const value = filter[comparator][mKey];
	//
	// return getMatchingSections(comparator, [mKey, value]);
}

function handleSComparison(filter: any, data: InsightResult[]): InsightResult[] {
	const [sKey, value] = filter.IS;
	const regex = new RegExp(`^${value.replace(/\*/g, ".*")}$`); // Handle wildcards
	return data.filter((section) => regex.test(section[sKey]));
}

function handleNegation(filter: any, data: InsightResult[]): InsightResult[] {
	// Exclude matching sections
	const notData = handleFilter(filter.NOT, data);
	return data.filter((section) => !notData.includes(section));
}

async function getAllSections(query: Query): Promise<InsightResult[]> {
	const columns = query.OPTIONS.COLUMNS;
	const columnKeys: string[] = [];
	for (const column of columns) {
		if (!isMKey(column) && !isSKey(column)) {
			throw new InsightError('invalid keys passed to OPTIONS.COLUMNS, NOTE THIS WAS NOT CAUGHT IN VALIDATE QUERY');
		}
		const parts = column.split('_');
		columnKeys.push(parts[1]);
	}

	const record = query.OPTIONS.COLUMNS[0]
	const key = record[0];
	const parts = key.split('_');
	const idString = parts[0];

	const allSections = await loadDatasets(idString, 'testname0');
	const allResults: InsightResult[] = [];
	for (const section of allSections) {
		const sectionResult: InsightResult = {};
		for (let i = 0; i < columnKeys.length; i++) {  //iterate through indicies
			sectionResult[columns[i]] = section[columnKeys[i] as keyof Section]; //WILL THIS LINE WORK? PROBABLY BUG HERE
		}
		allResults.push(sectionResult);
	}

	return allResults;
}

function sortResults(options: Options, results: InsightResult[]): InsightResult[] {
	if (options.ORDER) {
		return results.sort((a, b) => {
			const aValue = a[options.ORDER];
			const bValue = b[options.ORDER];

			if (typeof aValue === 'string' && typeof bValue === 'string') {
				// string comparison (case-insensitive)
				return aValue.localeCompare(bValue, undefined, { sensitivity: 'base' });
			}

			if (typeof aValue === 'number' && typeof bValue === 'number') {
				// numeric comparison
				return aValue - bValue;
			}

			// types differ
			throw new InsightError("Comparison column contains strings and numbers together!")
		});
	}
	return results;
}

//THIS IS THE HELPER TO CALL IN EACH BASE CASE FOR WHERE FUNCTION: MCOMPARATOR, SCOMPARATOR
/**
 * @returns - Section[] of sections returned by loadDatasets contained in the data set specified in the id string of
 * param field. Sections have been filtered according to op and Mfield|Sfield.
 * @param op
 * @param record
 */
function getMatchingSections(op: string, record: [MKey, number] | [SKey, string]): Section[] {

	const key = record[0];
	const value = record[1];
	const parts = key.split('_');
	const idString = parts[0];
	const field = parts[1];

	//Adapted form ChatGPT generated response
	type CompFunction = (a: any, b: any) => boolean;
	let comp: CompFunction;

	switch (op) {
		case 'GT':
			comp = (a: number, b: number) => a > b;
			break;
		case 'LT':
			comp = (a: number, b: number) => a < b;
			break;
		case 'EQ':
			comp = (a: number, b: number) => a === b;
			break;
		case 'IS':
			comp = (a: string, b: string) => a === b;
			break;
		default:
			throw new InsightError('not good very bad, no teneís un Quixote')
	}

	const sections = await loadDatasets(idString, 'testname0') // TODO: change file name?
	const results: Section[] = [];
	for (const section of sections) {
		let keep = false;
		switch (field) {
			case 'avg':
				keep = comp(section.avg, value);
				break;
			case 'pass':
				keep = comp(section.pass, value);
				break;
			case 'fail':
				keep = comp(section.fail, value);
				break;
			case 'audit':
				keep = comp(section.audit, value);
				break;
			case 'year':
				keep = comp(section.year, value);
				break;
			case 'dept':
				keep = comp(section.dept, value);
				break;
			case 'id':
				keep = comp(section.id, value);
				break;
			case 'instructor':
				keep = comp(section.instructor, value);
				break;
			case 'title':
				keep = comp(section.title, value);
				break;
			case 'uuid':
				keep = comp(section.uuid, value);
				break;
			default:
				throw new InsightError('invalid key passed to getMatchingSections');
		}

		if (keep) {
			results.push(section);
		}
	}
	return results;
}

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
		dataset = await fs.readJson(datasetPath)
	} catch (error) {
		throw new NotFoundError(`Could not find dataset with - id=${id};` + error)
	}

	return parseJSONtoSections(dataset);
}


/**
 * @returns - InsightResult[], selects columns from array of sections as specified by options.COLUMNS and parses them
 * into InsightResult. Creates one Insight Result per section. Final InsightResult[] ordered according to column specified
 * by options.ORDER if applicable.
 * @param options
 * @param sections
 */
export function handleOptions(options: Options, sections: Section[]): InsightResult[] {
	const columns = options.COLUMNS;

	const column_keys: string[] = [];
	for (const column of columns) {
		if (!isMKey(column) && !isSKey(column)) {
			throw new InsightError('invalid keys passed to OPTIONS.COLUMNS, NOTE THIS WAS NOT CAUGHT IN VALIDATE QUERY');
		}
		const parts = column.split('_');
		column_keys.push(parts[1]);
	}

	const insight_results: InsightResult[] = [];
	for (const section of sections) {
		const section_result: InsightResult = {};
		for (let i = 0; i < column_keys.length; i++) {  //iterate through indicies
			section_result[columns[i]] = section[column_keys[i] as keyof Section]; //WILL THIS LINE WORK? PROBABLY BUG HERE
		}
		insight_results.push(section_result);
	}

	if (options.ORDER) {
		const order = options.ORDER;
		if (!isMKey(order) && !isSKey(order)) {
			throw new InsightError('invalid keys passed to OPTIONS.COLUMNS, NOTE THIS WAS NOT CAUGHT IN VALIDATE QUERY');
		}
		const parts = order.split('_');
		const field = parts[1];

		//Adapted from ChatGPT generated response:
		insight_results.sort((a, b) => {
			const valueA = a[field];
			const valueB = b[field];

			if (typeof valueA === 'number' && typeof valueB === 'number') {
				return valueA - valueB;
			} else if (typeof valueA === 'string' && typeof valueB === 'string') {
				return valueA.localeCompare(valueB, undefined, {sensitivity: 'base'});
			} else {
				throw new InsightError('fields in handleOptions.ORDER not of same type.')
			}
		})

	}
	return insight_results;
}

