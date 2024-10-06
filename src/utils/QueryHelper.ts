import {InsightError, InsightResult, NotFoundError} from "../controller/IInsightFacade";
import {
	MKey, Query,
	Section,
	SKey, Body, Options
} from "../models/Section";
import fs from "fs-extra";
import path from "node:path";
import {parseJSONtoSections} from "./JsonHelper";

export async function processQueryOnDataset(query: unknown): Promise<InsightResult[]> {
	let validatedQuery: Query;
	try {
		validatedQuery = validateQuery(query);

	} catch (error) {
		throw new Error(`Query not a valid format: ` + error);
	}
	//const allMatchingSections: Section[] = handleBody(validatedQuery.WHERE);  //should return InsightResult[] that matches Body specifications
	//const orderedMatchingSections = handleOptions(validatedQuery.OPTIONS, allMatchingSections); //should return same InsightResult[] but sorted to Option specifications
	return [];
}


/**
 * @returns - Query, validates that the query param conforms to Query structure, if not throws InsightError
 * @param query
 */
export function validateQuery(query: unknown): Query {
	//1) check that query is an object
	if(typeof query !== 'object' || query === null) {
		throw new InsightError('invalid query, query is not a non-null object');
	}

	//2) check WHERE fields
	if('WHERE' in query)  {
		if(query.WHERE === null || typeof query.WHERE !== 'object')  {
			throw new InsightError('invalid query, query contains null WHERE field');
		}
		validateBody(query.WHERE);
	} else {
		throw new InsightError('invalid query, query does not contain WHERE field');
	}

	//3) check OPTION fields
	if('OPTIONS' in query)  {
		//is the OPTIONS field allowed to be null? I don't think so
		if(query.OPTIONS === null || typeof query.OPTIONS !== 'object')  {
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
	if(keys.length !== 1) {
		throw new InsightError('invalid query, query.WHERE contains more than one key');
	}

	//checks type of each possible filter stopping at Mkey and Skey
	switch(keys[0]) {
		case 'OR':
			if(filter.OR === null || !Array.isArray(filter.OR)) {
				throw new InsightError('invalid query, query.WHERE.OR is invalid')
			}
			for(const body in filter.OR) {
				validateBody(filter.OR);
			}
			break;
		case 'AND':
			if(filter.AND === null || !Array.isArray(filter.AND)) {
				throw new InsightError('invalid query, query.WHERE.AND is invalid')
			}
			for(const body in filter.OR) {
				validateBody(filter.OR);
			}
			break;
		case 'GT':
			if(filter.GT === null || typeof filter.GT !== 'string') {
				throw new InsightError('invalid query, query.WHERE.GT is invalid')
			}
			break;
		case 'LT':
			if(filter.LT === null || typeof filter.LT !== 'string') {
				throw new InsightError('invalid query, query.WHERE.LT is invalid')
			}
			break;
		case 'EQ':
			if(filter.EQ === null || typeof filter.EQ !== 'string') {
				throw new InsightError('invalid query, query.WHERE.EQ is invalid')
			}
			break;
		case 'IS':
			if(filter.IS === null || typeof filter.IS !== 'string') {
				throw new InsightError('invalid query, query.WHERE.IS is invalid')
			}
			break;
		case 'NOT':
			if(filter.NOT === null || typeof filter.NOT !== 'object') {
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
	if(keys.length === 0 || keys.length > 2) {
		throw new InsightError('invalid query, query.OPTIONS incorrect number of keys');
	}
	//validate columns
	if(keys[0] === 'COLUMNS') {
		throw new InsightError('invalid query, query.OPTIONS does not contain COLUMNS');
	}
	if(!Array.isArray(options.COLUMNS) || options.COLUMNS.length === 0) {
		throw new InsightError('invalid query, query.OPTIONS.COLUMNS is not an array');
	}
	for(const key in options.COLUMNS) {
		validateKey(key);
	}

	//validate order
	if(keys[1] && keys[1] !== 'ORDER') {
		throw new InsightError('invalid query, query.OPTIONS does not contain ORDER as 2nd key');
	}
	validateKey(options.ORDER);
}

function validateKey(key: any) {
	if(typeof key === 'string') {
		throw new InsightError('invalid query, key is not a string');
	}
	if(!isMKey(key) || isSKey(key)) {
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

export function handleBody(body: Body): InsightResult[] {  //param is type body because we know it follows structure of body
	if (!body) {
		// no filter - return all entries
		return InsightResult[];
	}
	// handle filters within the body
	return handleFilter(body);
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

	switch(op) {
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

	const sections = await loadDatasets(idString, 'testname0');
	let results: Section[] = [];
	for (const section of sections) {
		let keep: boolean = false;
		switch(field) {
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

		if(keep) {
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

	let column_keys: string[] = [];
	for(const column of columns) {
		if(!isMKey(column) && !isSKey(column)) {
			throw new InsightError('invalid keys passed to OPTIONS.COLUMNS, NOTE THIS WAS NOT CAUGHT IN VALIDATE QUERY');
		}
		const parts = column.split('_');
		column_keys.push(parts[1]);
	}

	const insight_results: InsightResult[] = [];
	for(const section of sections) {
		let section_result: InsightResult = {};
		for (let i = 0; i < column_keys.length; i++) {  //iterate through indicies
			section_result[columns[i]] = section[column_keys[i] as keyof Section]; //WILL THIS LINE WORK? PROBABLY BUG HERE
		}
		insight_results.push(section_result);
	}

	if (options.ORDER) {
		const order = options.ORDER;
		if(!isMKey(order) && !isSKey(order)) {
			throw new InsightError('invalid keys passed to OPTIONS.COLUMNS, NOTE THIS WAS NOT CAUGHT IN VALIDATE QUERY');
		}
		const parts = order.split('_');
		const field = parts[1];

		//Adapted from ChatGPT generated response:
		insight_results.sort((a, b) => {
			const valueA = a[field];
			const valueB = b[field];

			if(typeof valueA === 'number' && typeof valueB === 'number') {
				return valueA - valueB;
			} else if(typeof valueA === 'string' && typeof valueB === 'string') {
				return valueA.localeCompare(valueB, undefined, {sensitivity: 'base'});
			} else {
				throw new InsightError('fields in handleOptions.ORDER not of same type.')
			}
		})

	}
	return insight_results;
}

